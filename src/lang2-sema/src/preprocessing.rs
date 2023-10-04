use bumpalo::Bump;
use lang2_ast::{
    ADTsIn, AnnotatedExp, Exp, ExpKind, Fun, FunsIn, Id, Match, Parenthesized, RefState, SideTable,
    ValIn, Var, AST,
};
use petgraph::prelude::DiGraphMap;
use serde::Serialize;
use smallvec::SmallVec;
use tinyset::SetU32;

/// Information about function group accumulated throughout the pass
struct FunGroupInfo {
    /// Callgraph of functions within the group
    callgraph: DiGraphMap<Fun, ()>,
    /// Function that belongs to this group pass in currently inside of (if any)
    inside_of: Option<Fun>,
}

/// Worklist for function group typechecking
#[derive(Default, Serialize, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Worklist<'arena> {
    groups: &'arena [&'arena [Fun]],
}

/// Context of the preprocessing pass
struct Context<'arena> {
    /// Information about each function group currently being visited
    live_groups: Vec<FunGroupInfo>,
    /// Assembled worklists
    worklists: Vec<Worklist<'arena>>,
    /// Top-level info
    top: FunGroupInfo,
    /// Bump allocator
    bump: &'arena Bump,
    /// Global callgraph
    callgraph: DiGraphMap<Fun, ()>,
    /// Sets of free variables
    fvs: Vec<SetU32>,
    /// The function we are currently inside of
    inside_of: Fun,
}

/// Assembled worklists
#[derive(Default, Serialize, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Worklists<'arena> {
    /// Worklists for nested function groups
    worklists: Vec<Worklist<'arena>>,
    /// Worklist for the top level
    top_worklist: Worklist<'arena>,
}

/// Results of the preprocessing pass
#[derive(Default, Serialize, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Preprocessed<'arena> {
    /// Worklists for typechecking
    worklists: Worklists<'arena>,
    /// Approximate free variables sets
    fvs: &'arena [&'arena [Var]],
}

impl<'arena> Context<'arena> {
    /// Recursively visit expression, updating analysis information
    fn visit_exp(
        &mut self,
        exp: &'arena Exp<'arena>,
        side: &'arena SideTable<'arena>,
        bump: &'arena Bump,
    ) {
        match exp.kind {
            ExpKind::Parenthesized(Parenthesized { inner }) => self.visit_exp(inner, side, bump),
            ExpKind::Id(_) => {}
            ExpKind::FreeId(state) => match state.get() {
                RefState::Resolved(Id::Var(id)) => {
                    self.fvs[self.inside_of.index as usize].insert(id.index);
                }
                _ => {}
            },
            ExpKind::FnCall(call) => {
                if let RefState::Resolved(Id::Fun(fun)) = call.name.get() {
                    self.callgraph.add_edge(self.inside_of, fun, ());
                    let def = side[fun].ptr;
                    let group = match &def.fn_group {
                        Some(id) => &mut self.live_groups[id.index as usize],
                        None => &mut self.top,
                    };
                    if let Some(inside_of) = group.inside_of {
                        group.callgraph.add_edge(inside_of, fun, ());
                    }
                }
                for param in call.params {
                    self.visit_exp(param, side, bump);
                }
            }
            ExpKind::New(call) => {
                for param in call.params {
                    self.visit_exp(param, side, bump);
                }
            }
            ExpKind::Match(&Match { scrutinees, prongs }) => {
                for scrutinee in scrutinees {
                    self.visit_exp(scrutinee, side, bump);
                }
                for prong in prongs {
                    self.visit_exp(&prong.exp, side, bump);
                }
            }
            ExpKind::Annotated(AnnotatedExp { exp, .. }) => {
                self.visit_exp(exp, side, bump);
            }
            ExpKind::ValIn(ValIn { rhs, exp, .. }) => {
                self.visit_exp(rhs, side, bump);
                self.visit_exp(exp, side, bump);
            }
            ExpKind::FunsIn(funs) => {
                self.visit_funs_in(funs, side, bump);
            }
            ExpKind::ADTsIn(ADTsIn { exp, .. }) => self.visit_exp(exp, side, bump),
        }
    }
    fn visit_funs_in(
        &mut self,
        funs_in: &'arena FunsIn<'arena>,
        side: &'arena SideTable<'arena>,
        bump: &'arena Bump,
    ) {
        let currently_inside_of = self.inside_of;
        // Push a new function group
        assert_eq!(funs_in.idx.index as usize, self.live_groups.len());
        self.live_groups.push(FunGroupInfo {
            callgraph: DiGraphMap::with_capacity(funs_in.fns.len(), 0),
            inside_of: None,
        });
        for fun in funs_in.fns {
            self.callgraph.add_node(fun.id);
            self.live_groups
                .last_mut()
                .unwrap()
                .callgraph
                .add_node(fun.id);
            // Enter the function
            self.inside_of = fun.id;
            self.live_groups[funs_in.idx.index as usize].inside_of = Some(fun.id);
            // Run the analysis on the body
            self.visit_exp(&fun.body, side, bump);
        }
        // Pop function group
        let callgraph = std::mem::take(&mut self.live_groups.last_mut().unwrap().callgraph);
        let worklist = self.assemble_worklist(callgraph, side, bump);
        self.worklists.push(worklist);

        self.inside_of = currently_inside_of;
        self.visit_exp(&funs_in.exp, side, bump);
    }

    fn assemble_worklist(
        &mut self,
        callgraph: DiGraphMap<Fun, ()>,
        side: &'arena SideTable<'arena>,
        bump: &'arena Bump,
    ) -> Worklist<'arena> {
        let mut groups = SmallVec::<[&'arena [Fun]; 4]>::new();
        for mut scc in petgraph::algo::tarjan_scc(&callgraph) {
            scc.sort_by_key(|&fun| side[fun].ptr.decl_span);
            groups.push(bump.alloc_slice_fill_iter(scc));
        }
        Worklist {
            groups: bump.alloc_slice_fill_iter(groups.into_iter()),
        }
    }

    fn visit_ast(&mut self, ast: &'arena AST<'arena>) -> Worklist<'arena> {
        for fun in ast.top.funs {
            self.top.inside_of = Some(fun.id);
            self.inside_of = fun.id;
            self.visit_exp(&fun.body, &ast.side, self.bump);
        }
        let callgraph = std::mem::take(&mut self.top.callgraph);
        self.assemble_worklist(callgraph, &ast.side, self.bump)
    }
}

pub fn preprocess<'arena>(ast: &'arena AST<'arena>, bump: &'arena Bump) -> Preprocessed<'arena> {
    let mut fvs = vec![];
    fvs.resize_with(ast.side.fns_count(), || Default::default());
    let mut context: Context<'arena> = Context {
        live_groups: vec![],
        worklists: vec![],
        top: FunGroupInfo {
            callgraph: DiGraphMap::with_capacity(ast.top.funs.len(), 0),
            inside_of: None,
        },
        bump,
        fvs,
        callgraph: DiGraphMap::with_capacity(ast.side.fns_count(), ast.side.fns_count()),
        inside_of: Fun { index: 0 },
    };

    let top_worklist = context.visit_ast(ast);

    let condensed = petgraph::algo::condensation(context.callgraph.into_graph::<u32>(), true);
    let mut fvs_for_sccs: Vec<&'arena [Var]> = vec![&[]; condensed.node_count()];
    let fvs_for_funs: &mut [&[Var]] = bump.alloc_slice_fill_default(ast.side.fns_count());

    let mut top = petgraph::visit::Topo::new(&condensed);
    while let Some(scc) = top.next(&condensed) {
        let funs = &condensed[scc];
        let mut scc_fvs_set = SetU32::new();
        for fun in funs {
            scc_fvs_set.extend(std::mem::take(&mut context.fvs[fun.index as usize]));
        }
        for dep_scc in condensed.neighbors(scc) {
            scc_fvs_set.extend(fvs_for_sccs[dep_scc.index()].iter().map(|&var| var.index));
        }
        let fvs: Vec<Var> = scc_fvs_set.iter().map(|index| Var { index }).collect();
        fvs_for_sccs[scc.index()] = bump.alloc_slice_fill_iter(fvs);
        for fun in funs {
            fvs_for_funs[fun.index as usize] = fvs_for_sccs[scc.index()];
        }
    }

    let worklists = context.worklists;
    Preprocessed {
        worklists: Worklists {
            worklists: worklists,
            top_worklist: top_worklist,
        },
        fvs: fvs_for_funs,
    }
}

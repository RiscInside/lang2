use lang2_ast::{
    ADTsIn, AnnotatedExp, Exp, ExpKind, Fun, FunsIn, Id, Match, Parenthesized, RefState, SideTable,
    ValIn, AST,
};
use petgraph::prelude::DiGraphMap;
use serde::Serialize;
use smallvec::{SmallVec, ToSmallVec};

/// Information about function group accumulated throughout the pass
struct FunGroupInfo {
    /// Callgraph of functions within the group
    callgraph: DiGraphMap<Fun, ()>,
    /// Function that belongs to this group pass in currently inside of (if any)
    inside_of: Option<Fun>,
}

/// Worklist for function group typechecking
#[derive(Default, Serialize, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Worklist {
    groups: SmallVec<[SmallVec<[Fun; 1]>; 2]>,
}

/// Context of the preprocessing pass
struct Context {
    /// Information about each function group currently being visited
    live_groups: Vec<FunGroupInfo>,
    /// Assembled worklists
    worklists: Vec<Worklist>,
    /// Top-level info
    top: FunGroupInfo,
}

/// Assembled worklists
#[derive(Default, Serialize, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Worklists {
    /// Worklists for nested function groups
    worklists: Vec<Worklist>,
    /// Worklist for the top level
    top_worklist: Worklist,
}

/// Results of the preprocessing pass
#[derive(Default, Serialize, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Preprocessed {
    /// Worklists for typechecking
    worklists: Worklists,
}

impl Context {
    /// Recursively visit expression, updating analysis information
    fn visit_exp<'arena>(&mut self, exp: &'arena Exp<'arena>, side: &'arena SideTable<'arena>) {
        match exp.kind {
            ExpKind::Parenthesized(Parenthesized { inner }) => self.visit_exp(inner, side),
            ExpKind::Id(_) => {}
            ExpKind::FreeId(_) => {}
            ExpKind::FnCall(call) => {
                if let RefState::Resolved(Id::Fun(fun)) = call.name.get() {
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
                    self.visit_exp(param, side);
                }
            }
            ExpKind::New(call) => {
                for param in call.params {
                    self.visit_exp(param, side);
                }
            }
            ExpKind::Match(&Match { scrutinees, prongs }) => {
                for scrutinee in scrutinees {
                    self.visit_exp(scrutinee, side);
                }
                for prong in prongs {
                    self.visit_exp(&prong.exp, side);
                }
            }
            ExpKind::Annotated(AnnotatedExp { exp, .. }) => {
                self.visit_exp(exp, side);
            }
            ExpKind::ValIn(ValIn { rhs, exp, .. }) => {
                self.visit_exp(rhs, side);
                self.visit_exp(exp, side);
            }
            ExpKind::FunsIn(funs) => {
                self.visit_funs_in(funs, side);
            }
            ExpKind::ADTsIn(ADTsIn { exp, .. }) => self.visit_exp(exp, side),
        }
    }
    fn visit_funs_in<'arena>(
        &mut self,
        funs_in: &'arena FunsIn<'arena>,
        side: &'arena SideTable<'arena>,
    ) {
        // Push a new function group
        assert_eq!(funs_in.idx.index as usize, self.live_groups.len());
        self.live_groups.push(FunGroupInfo {
            callgraph: DiGraphMap::with_capacity(funs_in.fns.len(), 0),
            inside_of: None,
        });
        for fun in funs_in.fns {
            self.live_groups
                .last_mut()
                .unwrap()
                .callgraph
                .add_node(fun.id);
            // Enter the function
            self.live_groups[funs_in.idx.index as usize].inside_of = Some(fun.id);
            // Run the analysis on the body
            self.visit_exp(&fun.body, side);
        }
        // Pop function group
        assert_eq!(funs_in.idx.index as usize, self.live_groups.len() - 1);
        let group = self.live_groups.pop().unwrap();
        let callgraph = self.assemble_worklist(group.callgraph, side);
        self.worklists.push(callgraph);
    }

    fn assemble_worklist<'arena>(
        &mut self,
        callgraph: DiGraphMap<Fun, ()>,
        side: &'arena SideTable<'arena>,
    ) -> Worklist {
        let mut worklist = Worklist::default();
        for mut scc in petgraph::algo::tarjan_scc(&callgraph) {
            scc.sort_by_key(|&fun| side[fun].ptr.decl_span);
            worklist.groups.push(scc.to_smallvec());
        }
        worklist
    }

    fn visit_ast<'arena>(&mut self, ast: &'arena AST<'arena>) -> Worklist {
        for fun in ast.top.funs {
            self.top.inside_of = Some(fun.id);
            self.visit_exp(&fun.body, &ast.side);
        }
        let callgraph = std::mem::take(&mut self.top.callgraph);
        self.assemble_worklist(callgraph, &ast.side)
    }
}

pub fn preprocess<'arena>(ast: &'arena AST<'arena>) -> Preprocessed {
    let mut context = Context {
        live_groups: vec![],
        worklists: vec![],
        top: FunGroupInfo {
            callgraph: DiGraphMap::with_capacity(ast.top.funs.len(), 0),
            inside_of: None,
        },
    };

    let top_worklist = context.visit_ast(ast);
    Preprocessed {
        worklists: Worklists {
            worklists: context.worklists,
            top_worklist: top_worklist,
        },
    }
}

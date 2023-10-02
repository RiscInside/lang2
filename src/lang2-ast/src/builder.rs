use bumpalo::Bump;
use lang2_shadow::Table;
use smallvec::SmallVec;
use std::collections::{HashMap, HashSet};

use crate::*;

/// Sentinel value for reserving slots in the identifier side table
/// Should not be pointed to once AST construction is done
const ID_SENTINEL: IdPattern<'static> = IdPattern {
    name: "Congrats, you have found a bug, please report it",
    id: Id { index: 0xfffffffe },
};

/// Delayed resolution of the identifier
pub struct DelayedId<'arena> {
    /// Pointer to the cell to resolve
    idref: &'arena IdRef<'arena, Id>,
    /// Scope counter
    scope: usize,
}

/// Builder for the AST. Provides a convinient API for parsers to call into
pub struct Builder<'arena> {
    /// Arena to keep AST in
    bump: &'arena Bump,
    /// Side tables
    side: SideTable<'arena>,
    /// Table of type variables
    tyvar_table: Table<&'arena str, TypeVar>,
    /// Table of type constructors
    tycons_table: Table<&'arena str, TypeCons>,
    /// Table of constructors
    cons_table: Table<&'arena str, Cons>,
    /// Table of directly resolvable identifiers. This table is replaced with an empty one on entry into new
    /// mutual functions group.
    direct_id_table: Table<&'arena str, Id>,
    /// Delayed identifier resolutions
    delayed_id_resolutions: HashMap<&'arena str, SmallVec<[DelayedId<'arena>; 2]>>,
    /// Counter of queued delayed identifier resolutions
    delayed_id_resolutions_cnt: usize,
    /// Delayed constructor resolutions
    delayed_cons_resolutions: HashMap<&'arena str, SmallVec<[&'arena IdRef<'arena, Cons>; 2]>>,
    /// Top-level functions
    top_functions: Vec<Function<'arena>>,
    /// Top-level ADTs
    top_adts: Vec<ADT<'arena>>,
    /// ADT sentinel (fake ADT to point to to reserve the slot)
    adt_sentinel: &'arena ADT<'arena>,
    /// Variant sentinel (fake variant to point to to reserve the slot)
    variant_sentinel: &'arena Variant<'arena>,
}

/// Head of the val-in statement
pub struct ValInHead<'arena> {
    /// Delayed counter on entry
    delayed: usize,
    /// Pattern on the LHS
    lhs: Pattern<'arena>,
    /// Pattern on the RHS
    rhs: Exp<'arena>,
}

/// Prong head
pub struct ProngHead<'arena> {
    /// Delayed counter on entry
    delayed: usize,
    /// Patterns
    patterns: &'arena [Pattern<'arena>],
}

/// Function group head. This structure saves direct_id_table which is moved out.
pub struct FunGroupHead<'arena> {
    /// Saved direct_id_table
    direct_id_table: Table<&'arena str, Id>,
    /// Saved delayed resolutions counter
    delayed: usize,
}

/// Context maintained during ADT group parsing
pub struct ADTGroupContext<'arena> {
    /// Defined types (for detecting duplicates)
    defined_adts: HashSet<&'arena str>,
    /// Defined constructors (for detecting duplicates)
    defined_constructors: HashSet<&'arena str>,
}

/// Function declaration (without body)
pub struct FunctionHead<'arena> {
    /// Name
    name: &'arena str,
    /// Identifier referring to the function
    id: Id,
    /// Is a duplicate?
    duplicate: bool,
    /// Type parameters
    ty_params: TypeParamsList<'arena>,
    /// Parameters
    params: &'arena [Pattern<'arena>],
    /// Return type
    ret_ty: Option<Ty<'arena>>,
    /// Declaration span
    decl_span: Span,
}

impl<'arena> Builder<'arena> {
    /// Create a new builder
    pub fn new(bump: &'arena Bump) -> Self {
        Self {
            bump,
            side: SideTable {
                tyvar_table: vec![],
                tycons_table: vec![],
                cons_table: vec![],
                id_table: vec![],
                fn_groups_count: 0,
            },
            tyvar_table: Table::new(),
            tycons_table: Table::new(),
            cons_table: Table::new(),
            direct_id_table: Table::new(),
            delayed_id_resolutions: HashMap::new(),
            delayed_id_resolutions_cnt: 0,
            delayed_cons_resolutions: HashMap::new(),
            top_adts: vec![],
            top_functions: vec![],
            adt_sentinel: bump.alloc(ADT {
                name: "Congrats, you have found a bug, please report it",
                duplicate: false,
                id: TypeCons { index: 0xfffffffc },
                params: &[],
                decl_span: Span { start: 84, end: 19 },
                variants: &[],
            }),
            variant_sentinel: bump.alloc(Variant {
                name: "Congrats, you have found a bug, please report it",
                duplicate: false,
                id: Cons { index: 0xfffffffd },
                span: Span { start: 84, end: 19 },
                tys: &[],
            }),
        }
    }

    /// Delay resolution of the identifier
    fn delay_id_resolution(&mut self, id: &'arena str, idref: &'arena IdRef<'arena, Id>) {
        self.delayed_id_resolutions
            .entry(id)
            .or_default()
            .push(DelayedId {
                idref,
                scope: self.delayed_id_resolutions_cnt,
            });
        self.delayed_id_resolutions_cnt += 1;
    }

    /// Resolve all delayed resolutions for an identifier in scope
    fn resolve_delayed(&mut self, name: &'arena str, scope: usize, result: Id) {
        if let Some(delayed_stack) = self.delayed_id_resolutions.get_mut(name) {
            while let Some(tos) = delayed_stack.last() {
                if tos.scope < scope {
                    // All other resolutions do not belong to the scope
                    break;
                }
                tos.idref.replace(RefState::Resolved(result));
                delayed_stack.pop();
                self.delayed_id_resolutions_cnt -= 1;
            }
        }
    }

    /// Build a parenthesized type
    pub fn build_paren_ty(&mut self, inner: Ty<'arena>, span: Span) -> Ty<'arena> {
        Ty {
            span,
            kind: TyKind::Parenthesized(Parenthesized {
                inner: self.bump.alloc(inner),
            }),
        }
    }

    /// Build a never type
    pub fn build_never_ty(&mut self, span: Span) -> Ty<'arena> {
        return Ty {
            span,
            kind: TyKind::Never,
        };
    }

    /// Build an underscore type
    pub fn build_underscore_ty(&mut self, span: Span) -> Ty<'arena> {
        return Ty {
            span,
            kind: TyKind::Underscore,
        };
    }

    /// Build type variable monotype
    pub fn build_tvar_ty(&mut self, id: &str, span: Span) -> Ty<'arena> {
        Ty {
            span,
            kind: if let Some(&tvar) = self.tyvar_table.get(id) {
                TyKind::Var(tvar)
            } else {
                TyKind::UnresolvedVar(UnresolvedTyVar {
                    name: self.bump.alloc(self.bump.alloc_str(id) as &str),
                })
            },
        }
    }

    /// Build type application
    pub fn build_tapp(
        &mut self,
        cons_name: &str,
        span: Span,
        params: impl ExactSizeIterator<Item = Ty<'arena>>,
    ) -> Ty<'arena> {
        let params = self.bump.alloc_slice_fill_iter(params);
        Ty {
            span,
            kind: TyKind::App(if let Some(&tcons) = self.tycons_table.get(cons_name) {
                self.bump.alloc(TyApp {
                    cons: Cell::new(RefState::Resolved(tcons)),
                    params,
                })
            } else {
                let cons_name = self.bump.alloc_str(cons_name);
                let res = self.bump.alloc(TyApp {
                    cons: Cell::new(RefState::Unresolved(cons_name)),
                    params,
                });
                res
            }),
        }
    }

    /// Build parenthesized pattern
    pub fn build_paren_pat(&mut self, inner: Pattern<'arena>, span: Span) -> Pattern<'arena> {
        Pattern {
            kind: PatternKind::Parenthesized(Parenthesized {
                inner: self.bump.alloc(inner),
            }),
            span,
        }
    }

    /// Build variable pattern
    pub fn build_id_pat(&mut self, name: &str, span: Span) -> Pattern<'arena> {
        let name = self.bump.alloc_str(name);
        let id = Id {
            index: self.side.id_table.len().try_into().unwrap(),
        };
        let id_intro: &IdPattern<'arena> = self.bump.alloc(IdPattern { name, id });
        self.side.id_table.push(IdSide::IdPtr {
            ptr: id_intro,
            span,
        });
        Pattern {
            span,
            kind: PatternKind::IdIntro(&id_intro),
        }
    }

    /// Build destruction pattern
    pub fn build_destruct(
        &mut self,
        cons_name: &str,
        span: Span,
        subpatterns: impl ExactSizeIterator<Item = Pattern<'arena>>,
    ) -> Pattern<'arena> {
        let subpatterns = self.bump.alloc_slice_fill_iter(subpatterns);

        Pattern {
            span,
            kind: PatternKind::Destruct(if let Some(&cons) = self.cons_table.get(cons_name) {
                self.bump.alloc(Destruct {
                    name: Cell::new(RefState::Resolved(cons)),
                    subpatterns,
                })
            } else {
                let cons_name = self.bump.alloc_str(cons_name);
                let res = self.bump.alloc(Destruct {
                    name: Cell::new(RefState::Unresolved(cons_name)),
                    subpatterns,
                });
                self.delayed_cons_resolutions
                    .entry(cons_name)
                    .or_default()
                    .push(&res.name);
                res
            }),
        }
    }

    /// Build annotated pattern
    pub fn build_annotated_pat(
        &mut self,
        pat: Pattern<'arena>,
        ty: Ty<'arena>,
        span: Span,
    ) -> Pattern<'arena> {
        Pattern {
            span,
            kind: PatternKind::Annotated(self.bump.alloc(AnnotatedPat { pat, ty })),
        }
    }

    /// Build underscore pattern
    pub fn build_underscore_pat(&mut self, span: Span) -> Pattern<'arena> {
        Pattern {
            span,
            kind: PatternKind::Underscore,
        }
    }

    /// Build a parenthesized expression
    pub fn build_paren_exp(&mut self, exp: Exp<'arena>, span: Span) -> Exp<'arena> {
        Exp {
            span,
            kind: ExpKind::Parenthesized(Parenthesized {
                inner: self.bump.alloc(exp),
            }),
        }
    }

    /// Build an identifier expression
    pub fn build_id_exp(&mut self, id: &str, span: Span) -> Exp<'arena> {
        Exp {
            span,
            kind: if let Some(&resolved) = self.direct_id_table.get(id) {
                ExpKind::Id(resolved)
            } else {
                let arena_str: &'arena str = self.bump.alloc_str(id);
                let idref = self.bump.alloc(Cell::new(RefState::Unresolved(&arena_str)));
                self.delay_id_resolution(arena_str, idref);
                ExpKind::FreeId(idref)
            },
        }
    }

    /// Build a call expression
    pub fn build_call_exp(
        &mut self,
        fn_name: &str,
        span: Span,
        params: impl ExactSizeIterator<Item = Exp<'arena>>,
    ) -> Exp<'arena> {
        let params = self.bump.alloc_slice_fill_iter(params);
        Exp {
            span,
            kind: ExpKind::FnCall(if let Some(&id) = self.direct_id_table.get(fn_name) {
                self.bump.alloc(Call {
                    params,
                    name: Cell::new(RefState::Resolved(id)),
                })
            } else {
                let arena_str: &'arena str = self.bump.alloc_str(fn_name);
                let idref = Cell::new(RefState::Unresolved(&arena_str));
                let call: &'arena Call<'_, _> = self.bump.alloc(Call {
                    params,
                    name: idref,
                });
                // Register this identifier for later resolution
                self.delay_id_resolution(arena_str, &call.name);
                call
            }),
        }
    }

    /// Build a constructor call expression
    pub fn build_new(
        &mut self,
        cons_name: &str,
        span: Span,
        params: impl ExactSizeIterator<Item = Exp<'arena>>,
    ) -> Exp<'arena> {
        let params = self.bump.alloc_slice_fill_iter(params);

        Exp {
            span,
            kind: ExpKind::New(if let Some(&cons) = self.cons_table.get(cons_name) {
                self.bump.alloc(Call {
                    name: Cell::new(RefState::Resolved(cons)),
                    params,
                })
            } else {
                let cons_name = self.bump.alloc_str(cons_name);
                let res = self.bump.alloc(Call {
                    name: Cell::new(RefState::Unresolved(cons_name)),
                    params,
                });
                self.delayed_cons_resolutions
                    .entry(cons_name)
                    .or_default()
                    .push(&res.name);
                res
            }),
        }
    }

    /// Visit pattern
    fn visit_pattern(
        &mut self,
        pat: &Pattern<'arena>,
        visit_id: &mut impl FnMut(&mut Self, &'arena IdPattern<'arena>),
        reverse: bool,
    ) {
        match pat.kind {
            PatternKind::Parenthesized(Parenthesized { inner }) => {
                self.visit_pattern(inner, visit_id, reverse)
            }
            PatternKind::IdIntro(id) => visit_id(self, id),
            PatternKind::Destruct(&Destruct { subpatterns, .. }) => {
                self.visit_patterns(subpatterns.iter(), visit_id, reverse)
            }
            PatternKind::Annotated(AnnotatedPat { pat, .. }) => {
                self.visit_pattern(pat, visit_id, reverse)
            }
            PatternKind::Underscore => {}
        }
    }

    /// Visit list of patterns
    fn visit_patterns(
        &mut self,
        patterns: impl DoubleEndedIterator<Item = &'arena Pattern<'arena>>,
        visit_id: &mut impl FnMut(&mut Self, &'arena IdPattern<'arena>),
        reverse: bool,
    ) {
        if reverse {
            for pat in patterns.rev() {
                self.visit_pattern(pat, visit_id, reverse);
            }
        } else {
            for pat in patterns {
                self.visit_pattern(pat, visit_id, reverse);
            }
        }
    }

    /// Push bindings from a pattern
    fn pattern_intro(&mut self, pat: &Pattern<'arena>) {
        self.visit_pattern(
            pat,
            &mut |this, id| this.direct_id_table.push(id.name, id.id),
            false,
        );
    }

    /// Pop bindings from a pattern
    fn pattern_outro(&mut self, pat: &Pattern<'arena>) {
        self.visit_pattern(pat, &mut |this, id| this.direct_id_table.pop(id.name), true)
    }

    /// Pop bindings from a pattern, handing delayed resolutions
    fn pattern_outro_handle_delayed(&mut self, pat: &Pattern<'arena>, scope: usize) {
        self.visit_pattern(
            pat,
            &mut |this, id| {
                this.direct_id_table.pop(id.name);
                this.resolve_delayed(id.name, scope, id.id);
            },
            true,
        )
    }

    /// Introduce bindings for val in statements
    pub fn build_val_head(&mut self, lhs: Pattern<'arena>, rhs: Exp<'arena>) -> ValInHead<'arena> {
        self.pattern_intro(&lhs);
        ValInHead {
            delayed: self.delayed_id_resolutions_cnt,
            lhs,
            rhs,
        }
    }

    /// Finalize val in statement
    pub fn finalize_val(
        &mut self,
        head: ValInHead<'arena>,
        exp: Exp<'arena>,
        span: Span,
    ) -> Exp<'arena> {
        if self.delayed_id_resolutions_cnt == head.delayed {
            self.pattern_outro(&head.lhs);
        } else {
            self.pattern_outro_handle_delayed(&head.lhs, head.delayed);
        }
        Exp {
            span,
            kind: ExpKind::ValIn(self.bump.alloc(ValIn {
                lhs: head.lhs,
                rhs: head.rhs,
                exp,
            })),
        }
    }

    /// Introduce bindings for prong statements
    pub fn build_prong_head(
        &mut self,
        lhs: impl ExactSizeIterator<Item = Pattern<'arena>>,
    ) -> ProngHead<'arena> {
        let patterns = self.bump.alloc_slice_fill_iter(lhs) as &[Pattern<'_>];
        for pattern in patterns {
            self.pattern_intro(pattern);
        }
        ProngHead {
            delayed: self.delayed_id_resolutions_cnt,
            patterns,
        }
    }

    /// Build a prong
    pub fn finalize_prong(&mut self, head: ProngHead<'arena>, exp: Exp<'arena>) -> Prong<'arena> {
        if self.delayed_id_resolutions_cnt == head.delayed {
            for pattern in head.patterns.iter().rev() {
                self.pattern_outro(pattern);
            }
        } else {
            for pattern in head.patterns.iter().rev() {
                self.pattern_outro_handle_delayed(pattern, head.delayed);
            }
        }
        Prong {
            patterns: head.patterns,
            exp,
        }
    }

    /// Build a match statement
    pub fn build_match(
        &mut self,
        scrutinees: impl ExactSizeIterator<Item = Exp<'arena>>,
        prongs: impl ExactSizeIterator<Item = Prong<'arena>>,
        span: Span,
    ) -> Exp<'arena> {
        Exp {
            span,
            kind: ExpKind::Match(self.bump.alloc(Match {
                scrutinees: self.bump.alloc_slice_fill_iter(scrutinees),
                prongs: self.bump.alloc_slice_fill_iter(prongs),
            })),
        }
    }

    /// Build annotated expression
    pub fn build_annotated_exp(
        &mut self,
        exp: Exp<'arena>,
        ty: Ty<'arena>,
        span: Span,
    ) -> Exp<'arena> {
        Exp {
            span,
            kind: ExpKind::Annotated(self.bump.alloc(AnnotatedExp { ty, exp })),
        }
    }

    /// Build a type parameter list and add them into scope
    pub fn build_tys_list<'a>(
        &mut self,
        tys: impl ExactSizeIterator<Item = (&'a str, Span)>,
    ) -> TypeParamsList<'arena> {
        let mut cnt = 0;
        let mut duplicates = HashSet::new();
        let slice = self.bump.alloc_slice_fill_iter(tys.map(|(name, span)| {
            let arena_name = self.bump.alloc_str(name) as &'arena str;
            let id = TypeVar {
                index: (self.side.tyvar_table.len() + cnt).try_into().unwrap(),
            };
            let duplicate = !duplicates.insert(arena_name);
            cnt += 1;
            TypeParam {
                name: arena_name,
                id,
                span,
                duplicate,
            }
        }));
        for ty_param in slice.iter() {
            assert_eq!(
                ty_param.id.index,
                self.side.tyvar_table.len().try_into().unwrap()
            );
            self.side.tyvar_table.push(TypeVarSide { ptr: ty_param });
            if !ty_param.duplicate {
                self.tyvar_table.push(ty_param.name, ty_param.id);
            }
        }
        slice
    }

    /// Remove type variables from scope
    fn tys_outro(&mut self, tys: TypeParamsList<'arena>) {
        for ty in tys {
            if !ty.duplicate {
                self.tyvar_table.pop(ty.name);
            }
        }
    }

    /// Create new ADT group context
    pub fn new_adt_group(&mut self) -> ADTGroupContext<'arena> {
        ADTGroupContext {
            defined_adts: HashSet::new(),
            defined_constructors: HashSet::new(),
        }
    }

    /// Build a variant
    pub fn build_variant(
        &mut self,
        adts_group_ctx: &mut ADTGroupContext<'arena>,
        name: &str,
        span: Span,
        tys: impl ExactSizeIterator<Item = Ty<'arena>>,
    ) -> Variant<'arena> {
        let id = Cons {
            index: self.side.cons_table.len().try_into().unwrap(),
        };
        self.side.cons_table.push(ConsSide {
            variant: self.variant_sentinel,
            adt: TypeCons { index: 0xfffffffa },
        });
        let name = self.bump.alloc_str(name) as &str;
        let duplicate = !adts_group_ctx.defined_constructors.insert(&name);
        Variant {
            name,
            duplicate,
            id,
            span,
            tys: self.bump.alloc_slice_fill_iter(tys),
        }
    }

    /// Build ADT
    pub fn build_adt(
        &mut self,
        adts_group_ctx: &mut ADTGroupContext<'arena>,
        name: &str,
        params: TypeParamsList<'arena>,
        decl_span: Span,
        variants: impl ExactSizeIterator<Item = Variant<'arena>>,
    ) -> ADT<'arena> {
        let name = self.bump.alloc_str(name) as &str;
        let id = TypeCons {
            index: self.side.tycons_table.len().try_into().unwrap(),
        };
        self.side.tycons_table.push(TypeConsSide {
            ptr: self.adt_sentinel,
        });
        let variants = self.bump.alloc_slice_fill_iter(variants);
        let duplicate = !adts_group_ctx.defined_adts.insert(name);

        for variant in variants.iter() {
            self.side.cons_table[variant.id.index as usize] = ConsSide { variant, adt: id };
            if !duplicate {
                self.cons_table.push(variant.name, variant.id);
            }
        }

        self.tys_outro(params);
        if !duplicate {
            self.tycons_table.push(name, id);
        }

        ADT {
            name,
            duplicate,
            id,
            params,
            decl_span,
            variants,
        }
    }

    /// Build ADT in expression
    pub fn build_adt_in_exp(
        &mut self,
        adts: impl ExactSizeIterator<Item = ADT<'arena>>,
        exp: Exp<'arena>,
        span: Span,
    ) -> Exp<'arena> {
        let adts = self.bump.alloc_slice_fill_iter(adts);
        for adt in adts.iter() {
            self.side.tycons_table[adt.id.index as usize] = TypeConsSide { ptr: adt };
            if !adt.duplicate {
                for variant in adt.variants {
                    if !variant.duplicate {
                        self.cons_table.pop(variant.name);
                    }
                }
                self.tycons_table.pop(adt.name);
            }
        }
        Exp {
            span,
            kind: ExpKind::ADTsIn(self.bump.alloc(ADTsIn { adts, exp })),
        }
    }

    /// Begin parsing a group of functions
    pub fn fun_group_start(&mut self) -> FunGroupHead<'arena> {
        // In the beginning of the function group, no binding is undisputably available - virtually any outer binding
        // can be shadowed by some function defined later in a function group block. To reflect this, direct_id_table
        // is emptied, causing all identifier resolution for things in outer scopes to be delayed until we leave the
        // scope
        FunGroupHead {
            direct_id_table: std::mem::take(&mut self.direct_id_table),
            delayed: self.delayed_id_resolutions_cnt,
        }
    }

    /// Parse a function declaration
    pub fn build_fun_head(
        &mut self,
        name: &str,
        ty_params: TypeParamsList<'arena>,
        params: impl ExactSizeIterator<Item = Pattern<'arena>>,
        ret_ty: Option<Ty<'arena>>,
        decl_span: Span,
    ) -> FunctionHead<'arena> {
        // Reserve slot in the side table in the function, but don't fill it yet
        let name = self.bump.alloc_str(name) as &str;
        let id = Id {
            index: self.side.id_table.len().try_into().unwrap(),
        };
        let duplicate = self.direct_id_table.contains_key(&name);
        if !duplicate {
            self.direct_id_table.push(name, id);
        }
        self.side.id_table.push(IdSide::IdPtr {
            ptr: &ID_SENTINEL,
            span: Span { start: 19, end: 84 },
        });

        let params = self.bump.alloc_slice_fill_iter(params);
        for param in params.iter() {
            self.pattern_intro(param);
        }
        FunctionHead {
            name,
            id,
            duplicate,
            params,
            ty_params,
            ret_ty,
            decl_span,
        }
    }

    /// Parse a function
    pub fn build_function(
        &mut self,
        head: FunctionHead<'arena>,
        body: Exp<'arena>,
    ) -> Function<'arena> {
        self.tys_outro(head.ty_params);
        for param in head.params.iter().rev() {
            self.pattern_outro(param);
        }
        Function {
            name: head.name,
            id: head.id,
            duplicate: head.duplicate,
            ty_params: head.ty_params,
            params: head.params,
            ret_ty: head.ret_ty,
            decl_span: head.decl_span,
            body,
        }
    }

    /// Finish assembling function group
    pub fn build_function_group(
        &mut self,
        head: FunGroupHead<'arena>,
        functions: impl ExactSizeIterator<Item = Function<'arena>>,
    ) -> &'arena [Function<'arena>] {
        let functions = self.bump.alloc_slice_fill_iter(functions);
        // Process delayed resolutions if any were queued
        if self.delayed_id_resolutions_cnt != head.delayed {
            for function in functions.iter().rev() {
                if !function.duplicate {
                    self.resolve_delayed(function.name, head.delayed, function.id);
                }
            }
        }
        // Reroute side tables and make bindings available for the child expression
        let mut direct_id_table = head.direct_id_table;
        for function in functions.iter() {
            self.side.id_table[function.id.index as usize] = IdSide::FnDef(function);
            if !function.duplicate {
                direct_id_table.push(function.name, function.id);
            }
        }
        self.direct_id_table = direct_id_table;
        functions
    }

    /// Build function expression
    pub fn build_function_exp(
        &mut self,
        fns: &'arena [Function<'arena>],
        exp: Exp<'arena>,
        span: Span,
    ) -> Exp<'arena> {
        // Undefine functions
        for function in fns {
            if !function.duplicate {
                self.direct_id_table.pop(function.name);
            }
        }
        Exp {
            span,
            kind: ExpKind::FunsIn(self.bump.alloc(FunsIn {
                fns,
                exp,
                idx: {
                    self.side.fn_groups_count = self.side.fn_groups_count.checked_add(1).unwrap();
                    FunGroupIdx {
                        index: self.side.fn_groups_count,
                    }
                },
            })),
        }
    }

    /// Add a top-level function
    pub fn add_function(&mut self, fun: Function<'arena>) {
        if let Some(delayed) = self.delayed_id_resolutions.remove(fun.name) {
            //  Duplicates must have been resolved
            for DelayedId { idref, .. } in delayed {
                idref.set(RefState::Resolved(fun.id));
            }
        }
        self.top_functions.push(fun);
    }

    /// Add a top-level adt
    pub fn add_adt(&mut self, adt: ADT<'arena>) {
        for variant in adt.variants {
            if let Some(delayed) = self.delayed_cons_resolutions.remove(variant.name) {
                assert!(!variant.duplicate);
                for resolution in delayed {
                    resolution.set(RefState::Resolved(variant.id));
                }
            }
        }
        self.top_adts.push(adt);
    }

    /// Build top level
    pub fn ast(self) -> AST<'arena> {
        AST {
            top: TopLevel {
                adts: self.bump.alloc_slice_fill_iter(self.top_adts),
                funs: self.bump.alloc_slice_fill_iter(self.top_functions),
            },
            side: self.side,
        }
    }
}

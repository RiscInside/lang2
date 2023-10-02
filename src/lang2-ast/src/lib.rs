use serde::{Deserialize, Serialize};
use std::{cell::Cell, hash::Hash, ops::Index};

use lang2_span::{HasSpan, Span};

pub mod builder;

/// Type variable name
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Serialize, Deserialize, Hash)]
pub struct TypeVar {
    pub index: u32,
}

/// Type constructor name
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Serialize, Deserialize, Hash)]
pub struct TypeCons {
    pub index: u32,
}

/// Identifier
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Serialize, Deserialize, Hash)]
pub struct Id {
    pub index: u32,
}

/// Constructor name
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Serialize, Deserialize, Hash)]
pub struct Cons {
    pub index: u32,
}

/// Function group index
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct FunGroupIdx {
    pub index: u32,
}

pub static FUN_GROUP_TOP_LEVEL: FunGroupIdx = FunGroupIdx { index: 0 };

/// Parenthesized value
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct Parenthesized<'arena, T> {
    pub inner: &'arena T,
}

/// Type parameter
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct TypeParam<'arena> {
    /// Name of the type parameter
    pub name: &'arena str,
    /// True if duplicate
    pub duplicate: bool,
    /// Type variable ID
    pub id: TypeVar,
    /// Span
    pub span: Span,
}

/// Type parameters list
pub type TypeParamsList<'arena> = &'arena [TypeParam<'arena>];

/// Abstract data type definition
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct ADT<'arena> {
    /// Name of the type constructor
    pub name: &'arena str,
    /// True if ADT with this name has been defined
    pub duplicate: bool,
    /// Type constructor ID
    pub id: TypeCons,
    /// Type parameters to the type constructor
    pub params: TypeParamsList<'arena>,
    /// Declaration span
    pub decl_span: Span,
    /// List of variants
    pub variants: &'arena [Variant<'arena>],
}

/// One of the variants in abstract data type definition
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct Variant<'arena> {
    /// Name of the variant
    pub name: &'arena str,
    /// True if variant with this name has been defined
    pub duplicate: bool,
    /// Variant ID
    pub id: Cons,
    /// Span of the variant definition
    pub span: Span,
    /// List of types of values stored in the variant
    pub tys: &'arena [Ty<'arena>],
}

/// Monotype (no introduction of type variables)
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct Ty<'arena> {
    /// Kind of the monotype (not type kind, that's always "*" for now)
    #[serde(flatten)]
    pub kind: TyKind<'arena>,
    /// Type's span
    pub span: Span,
}

/// Type application
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct TyApp<'arena, Name> {
    /// Type constructor
    pub cons: Name,
    /// Parameters to the type constructor
    pub params: &'arena [Ty<'arena>],
}

/// Unresolved type variable
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct UnresolvedTyVar<'arena> {
    name: &'arena &'arena str,
}

/// State of the reference to the identifier
#[derive(PartialEq, Eq, Debug, Serialize, Clone, Copy)]
pub enum RefState<'arena, T> {
    /// Unresolved
    Unresolved(&'arena str),
    /// Resolved
    Resolved(T),
}

/// Reference to the identifier
pub type IdRef<'arena, T> = Cell<RefState<'arena, T>>;

/// Monotype kind. Not to be confused with type kinds
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
#[serde(tag = "kind")]
pub enum TyKind<'arena> {
    /// Parenthesized type
    Parenthesized(Parenthesized<'arena, Ty<'arena>>),
    /// Never type
    Never,
    /// Underscore (a.k.a. fill in for me)
    Underscore,
    /// Type variable
    Var(TypeVar),
    /// Unresolved type variable
    UnresolvedVar(UnresolvedTyVar<'arena>),
    /// Application of the type constructor
    App(&'arena TyApp<'arena, IdRef<'arena, TypeCons>>),
}

/// Function defined at top-level
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct Function<'arena> {
    /// Name of the function
    pub name: &'arena str,
    /// Identifier ID
    pub id: Id,
    /// True if duplicate
    pub duplicate: bool,
    /// Type parameters
    pub ty_params: TypeParamsList<'arena>,
    /// Function parameter patterns (have to be irrefutable)
    pub params: &'arena [Pattern<'arena>],
    /// Return type
    pub ret_ty: Option<Ty<'arena>>,
    // Function declaration span
    pub decl_span: Span,
    /// Function body
    pub body: Exp<'arena>,
}

/// Pattern with bindings
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct Pattern<'arena> {
    /// Pattern kind
    #[serde(flatten)]
    pub kind: PatternKind<'arena>,
    /// Span of the pattern
    pub span: Span,
}

// Id pattern
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct IdPattern<'arena> {
    /// Name of the identifier
    pub name: &'arena str,
    /// Introduced identifier
    pub id: Id,
}

/// Destructuring variant
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct Destruct<'arena, Name> {
    /// Variant's name
    pub name: Name,
    /// List of subpatterns
    pub subpatterns: &'arena [Pattern<'arena>],
}

/// Annotated pattern
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct AnnotatedPat<'arena> {
    /// Type
    pub ty: Ty<'arena>,
    /// Pattern
    pub pat: Pattern<'arena>,
}

/// Pattern kind
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
#[serde(tag = "kind")]
pub enum PatternKind<'arena> {
    /// Parenthesized parrern
    Parenthesized(Parenthesized<'arena, Pattern<'arena>>),
    /// Identifier binding
    IdIntro(&'arena IdPattern<'arena>),
    /// Destructing a variant
    Destruct(&'arena Destruct<'arena, IdRef<'arena, Cons>>),
    /// Type annotated pattern
    Annotated(&'arena AnnotatedPat<'arena>),
    /// Underscore
    Underscore,
}

/// Expression
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct Exp<'arena> {
    /// Expression kind
    #[serde(flatten)]
    pub kind: ExpKind<'arena>,
    /// Span of the expression
    pub span: Span,
}

/// Call expression
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct Call<'arena, IdOrCons> {
    /// Function being called/constructor being instantiated
    pub name: IdOrCons,
    /// Parameters
    pub params: &'arena [Exp<'arena>],
}

/// Match epxression
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct Match<'arena> {
    /// List of expressions being scrutinized
    pub scrutinees: &'arena [Exp<'arena>],
    /// Match prongs
    pub prongs: &'arena [Prong<'arena>],
}

/// One of the prongs of the match statement (patterns => expression pair)
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct Prong<'arena> {
    /// Patterns this prong matches against
    pub patterns: &'arena [Pattern<'arena>],
    /// Expression to evaluate if match succeeds
    pub exp: Exp<'arena>,
}

/// Type annotated expresssion
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct AnnotatedExp<'arena> {
    /// Type
    pub ty: Ty<'arena>,
    /// Expression
    pub exp: Exp<'arena>,
}

/// Value binding expression
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct ValIn<'arena> {
    /// Pattern of the LHS
    pub lhs: Pattern<'arena>,
    /// Expression on the RHS
    pub rhs: Exp<'arena>,
    /// Resulting expression
    pub exp: Exp<'arena>,
}

/// Functions group expression. Functions within a group can reference each other
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct FunsIn<'arena> {
    /// Defined functions
    pub fns: &'arena [Function<'arena>],
    /// Expression
    pub exp: Exp<'arena>,
    /// Function group index
    pub idx: FunGroupIdx,
}

/// ADT definition expression. ADTs within a group can reference each other
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct ADTsIn<'arena> {
    /// Definition of the ADTs
    pub adts: &'arena [ADT<'arena>],
    /// Expression
    pub exp: Exp<'arena>,
}

/// Expression kind
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
#[serde(tag = "kind")]
pub enum ExpKind<'arena> {
    /// Parenthesized expression
    Parenthesized(Parenthesized<'arena, Exp<'arena>>),
    /// Bound identifier expression
    Id(Id),
    /// Free identifier expression
    FreeId(&'arena IdRef<'arena, Id>),
    /// Function call expression
    FnCall(&'arena Call<'arena, IdRef<'arena, Id>>),
    /// Constructor call expression
    New(&'arena Call<'arena, IdRef<'arena, Cons>>),
    /// Match expression
    Match(&'arena Match<'arena>),
    /// Type annotated expression
    Annotated(&'arena AnnotatedExp<'arena>),
    /// Val-in expression
    ValIn(&'arena ValIn<'arena>),
    /// Functions group expression
    FunsIn(&'arena FunsIn<'arena>),
    /// ADT group expression
    ADTsIn(&'arena ADTsIn<'arena>),
}

/// Top-level
#[derive(PartialEq, Eq, Debug, Serialize, Clone)]
pub struct TopLevel<'arena> {
    /// Data declarations
    adts: &'arena [ADT<'arena>],
    /// Function declarations
    funs: &'arena [Function<'arena>],
}

/// Side information for type variables
pub struct TypeVarSide<'arena> {
    /// Pointer to the declaration
    pub ptr: &'arena TypeParam<'arena>,
}

/// Side information for type constructors
pub struct TypeConsSide<'arena> {
    /// Pointer to the declaration
    pub ptr: &'arena ADT<'arena>,
}

/// Side information for identifiers
pub enum IdSide<'arena> {
    /// Pointer to the pattern where ID has been declared
    IdPtr {
        ptr: &'arena IdPattern<'arena>,
        span: Span,
    },
    /// Pointer to the function definition
    FnDef(&'arena Function<'arena>),
}

/// Side information for type constructors
pub struct ConsSide<'arena> {
    /// Pointer to the variant
    pub variant: &'arena Variant<'arena>,
    /// ADT declaration ID
    pub adt: TypeCons,
}

/// Side table
pub struct SideTable<'arena> {
    /// Type variables side table
    pub(crate) tyvar_table: Vec<TypeVarSide<'arena>>,
    /// Type constructors side table
    pub(crate) tycons_table: Vec<TypeConsSide<'arena>>,
    /// Constructors side table
    pub(crate) cons_table: Vec<ConsSide<'arena>>,
    /// Identifier side table
    pub(crate) id_table: Vec<IdSide<'arena>>,
    /// Number of function groups. 0 slot is reserved for the top-level
    fn_groups_count: u32,
}

impl<'arena> Index<TypeVar> for SideTable<'arena> {
    type Output = TypeVarSide<'arena>;

    fn index(&self, tv: TypeVar) -> &Self::Output {
        &self.tyvar_table[tv.index as usize]
    }
}

impl<'arena> Index<TypeCons> for SideTable<'arena> {
    type Output = TypeConsSide<'arena>;

    fn index(&self, tcons: TypeCons) -> &Self::Output {
        &self.tycons_table[tcons.index as usize]
    }
}

impl<'arena> Index<Cons> for SideTable<'arena> {
    type Output = ConsSide<'arena>;

    fn index(&self, cons: Cons) -> &Self::Output {
        &self.cons_table[cons.index as usize]
    }
}

impl<'arena> Index<Id> for SideTable<'arena> {
    type Output = IdSide<'arena>;

    fn index(&self, id: Id) -> &Self::Output {
        &self.id_table[id.index as usize]
    }
}

impl SideTable<'_> {
    pub fn tyvar_count(&mut self) -> u32 {
        self.tyvar_table.len().try_into().unwrap()
    }

    pub fn tycons_count(&mut self) -> u32 {
        self.tycons_table.len().try_into().unwrap()
    }

    pub fn cons_count(&mut self) -> u32 {
        self.cons_table.len().try_into().unwrap()
    }

    pub fn id_count(&mut self) -> u32 {
        self.id_table.len().try_into().unwrap()
    }

    pub fn fn_groups_count(&mut self) -> u32 {
        self.fn_groups_count
    }
}

/// AST of lang2 program
pub struct AST<'arena> {
    pub top: TopLevel<'arena>,
    pub side: SideTable<'arena>,
}

impl HasSpan for Exp<'_> {
    fn span(&self) -> Span {
        self.span
    }
}

impl HasSpan for Pattern<'_> {
    fn span(&self) -> Span {
        self.span
    }
}

impl HasSpan for Ty<'_> {
    fn span(&self) -> Span {
        self.span
    }
}

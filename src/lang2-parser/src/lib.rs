use lang2_ast::{
    builder::{ADTGroupContext, Builder},
    Exp, Function, Pattern, Prong, Ty, TypeParamsList, Variant, ADT, AST,
};
use lang2_span::{from_to, HasSpan, Span};
use logos::{Lexer, Logos};
use serde::Serialize;
use smallvec::{smallvec, SmallVec};
use std::{iter::Peekable, ops::Range};

#[derive(Debug, Serialize)]
pub enum Error {
    ExpectedActual {
        expected: &'static str,
        actual: &'static str,
        pos: usize,
    },
    UnexpectedChar {
        pos: usize,
    },
    UnexpectedEof {
        pos: usize,
    },
}

pub type ParseResult<T> = Result<T, Error>;

#[derive(Logos, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[logos(skip r"[\n\t\f ]+|#.*\n")]
enum TokenKind {
    #[token("fun")]
    Fun,

    #[token("val")]
    Val,

    #[token("data")]
    Data,

    #[token("match")]
    Match,

    #[token("with")]
    With,

    #[token("end")]
    End,

    #[token(".")]
    Period,

    #[token(",")]
    Comma,

    #[token("(")]
    LParen,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,

    #[token(")")]
    RParen,

    #[token("=>")]
    Arrow,

    #[token("=")]
    EqualsSign,

    #[token(":")]
    Colon,

    #[token("_")]
    Underscore,

    #[regex(r"[a-z][0-9A-Za-z_]*")]
    LowercaseStartId,

    #[regex(r"[A-Z][0-9A-Za-z_]*")]
    UppercaseStartId,
}

impl TokenKind {
    fn describe(&self) -> &'static str {
        match self {
            TokenKind::Fun => "`fun` keyword",
            TokenKind::Val => "`val` keyword",
            TokenKind::Data => "`data` keyword",
            TokenKind::Match => "`match` keyword",
            TokenKind::With => "`with` keyword",
            TokenKind::End => "`end` keyword",
            TokenKind::Period => "`.` (dor or period)",
            TokenKind::Comma => "`,` (comma)",
            TokenKind::LParen => "`(` (left parenthesis)",
            TokenKind::LBracket => "`[` (left bracket)",
            TokenKind::RBracket => "`]` (right bracket)",
            TokenKind::RParen => "`)` (right parenthesis)",
            TokenKind::Arrow => "`=>` (right arrow)",
            TokenKind::EqualsSign => "`=` (equals sign)",
            TokenKind::Colon => "`:` (colon)",
            TokenKind::Underscore => "`_` (underscore)",
            TokenKind::LowercaseStartId => "non-capitalized identifier",
            TokenKind::UppercaseStartId => "capitalized identifier",
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct Token {
    kind: TokenKind,
    span: Span,
}

impl Token {
    fn describe(&self) -> &'static str {
        self.kind.describe()
    }

    fn from_spanned(
        raw: Option<(Result<TokenKind, ()>, Range<usize>)>,
        end: usize,
    ) -> ParseResult<Self> {
        let Some((res, span)) = raw else {
            return Err(Error::UnexpectedEof { pos: end });
        };
        match res {
            Ok(kind) => Ok(Token {
                kind,
                span: span.into(),
            }),
            Err(_) => Err(Error::UnexpectedChar { pos: span.start }),
        }
    }
}

impl Error {
    fn expected_actual(expected: &'static str, actual: Token) -> Self {
        Self::ExpectedActual {
            expected,
            actual: actual.describe(),
            pos: actual.span.start.try_into().unwrap(),
        }
    }
}

struct Parser<'src, 'arena, 'builder> {
    builder: &'builder mut Builder<'arena>,
    tokens: Peekable<logos::SpannedIter<'src, TokenKind>>,
    source: &'src str,
}

impl<'src, 'arena, 'builder> Parser<'src, 'arena, 'builder> {
    fn new(source: &'src str, builder: &'builder mut Builder<'arena>) -> Self {
        Self {
            builder,
            tokens: Lexer::new(source).spanned().peekable(),
            source,
        }
    }

    fn peek(&mut self) -> ParseResult<Token> {
        return Token::from_spanned(self.tokens.peek().cloned(), self.source.len());
    }

    fn peek_maybe_eof(&mut self) -> ParseResult<Option<Token>> {
        let peeked = self.peek();
        match peeked {
            Ok(tok) => Ok(Some(tok)),
            Err(Error::UnexpectedEof { .. }) => Ok(None),
            Err(e) => Err(e),
        }
    }

    fn consume(&mut self) {
        self.tokens.next();
    }

    fn expect_exact(&mut self, kind: TokenKind) -> ParseResult<Span> {
        let tok = self.peek()?;
        if tok.kind == kind {
            self.consume();
            Ok(tok.span)
        } else {
            Err(Error::expected_actual(kind.describe(), tok))
        }
    }

    /// Parse identifier, followed by a list of atoms in brackets
    /// TODO: come up with a better name
    #[inline(always)]
    fn parse_with_list<T, P>(
        &mut self,
        start_span: Span,
        mut single: impl FnMut(&mut Self, &'src str, Span) -> T,
        mut one: impl FnMut(&mut Self) -> ParseResult<P>,
        mut parametric: impl FnMut(&mut Self, &'src str, SmallVec<[P; 4]>, Span) -> T,
        left: TokenKind,
        right: TokenKind,
        not_right_or_comma: &'static str,
        require_one_if_parens: bool,
    ) -> ParseResult<T> {
        self.consume();
        let name = &self.source[start_span.as_usize_range()];
        match self.peek() {
            Ok(Token { kind, .. }) if kind == left => {
                self.consume();
                let mut params = smallvec![];
                let tok = self.peek()?;
                if !require_one_if_parens && tok.kind == right {
                    self.consume();
                    return Ok(parametric(
                        self,
                        name,
                        params,
                        from_to(start_span.start, tok.span.end),
                    ));
                }
                params.push(one(self)?);
                return loop {
                    let tok = self.peek()?;
                    match tok.kind {
                        TokenKind::Comma => {
                            self.consume();
                            params.push(one(self)?);
                        }
                        kind if kind == right => {
                            self.consume();
                            break Ok(parametric(
                                self,
                                name,
                                params,
                                from_to(start_span.start, tok.span.end),
                            ));
                        }
                        _ => break Err(Error::expected_actual(not_right_or_comma, tok)),
                    }
                };
            }
            _ => {}
        }
        Ok(single(self, name, start_span))
    }

    #[inline(always)]
    fn parse_parens<T>(
        &mut self,
        starting_span: Span,
        mut one: impl FnMut(&mut Self) -> ParseResult<T>,
        mut surround_with_parens: impl FnMut(&mut Self, T, Span) -> T,
    ) -> ParseResult<T> {
        self.consume();
        let inner = one(self)?;
        let rparen = self.expect_exact(TokenKind::RParen)?;
        Ok(surround_with_parens(
            self,
            inner,
            from_to(starting_span.start, rparen.end),
        ))
    }

    #[inline(always)]
    fn parse_typed<T: HasSpan>(
        &mut self,
        mut one: impl FnMut(&mut Self) -> ParseResult<T>,
        mut typed: impl FnMut(&mut Self, T, Ty<'arena>, Span) -> T,
    ) -> ParseResult<T> {
        let atom = one(self)?;
        let atom_start = atom.span().start;
        Ok(
            if let Ok(Token {
                kind: TokenKind::Colon,
                ..
            }) = self.peek()
            {
                self.consume();
                let ty = self.parse_ty()?;
                let ty_end = ty.span.end;
                typed(self, atom, ty, from_to(atom_start, ty_end))
            } else {
                atom
            },
        )
    }

    fn parse_lparen_ty(&mut self, span: Span) -> ParseResult<Ty<'arena>> {
        self.parse_parens(
            span,
            |this| this.parse_ty(),
            |this, ty, span| this.builder.build_paren_ty(ty, span),
        )
    }

    fn parse_ty_cons(&mut self, cons_span: Span) -> ParseResult<Ty<'arena>> {
        self.parse_with_list(
            cons_span,
            |this, name, span| this.builder.build_tapp(name, span, std::iter::empty()),
            |this| this.parse_ty(),
            |this, name, params, span| this.builder.build_tapp(name, span, params.into_iter()),
            TokenKind::LBracket,
            TokenKind::RBracket,
            "`]` (right bracket) or `,` (comma)",
            true,
        )
    }

    fn parse_ty(&mut self) -> ParseResult<Ty<'arena>> {
        let tok = self.peek()?;
        match tok.kind {
            TokenKind::LParen => self.parse_lparen_ty(tok.span),
            TokenKind::LowercaseStartId => {
                self.consume();
                Ok(self
                    .builder
                    .build_tvar_ty(&self.source[tok.span.as_usize_range()], tok.span))
            }
            TokenKind::Underscore => {
                self.consume();
                Ok(self.builder.build_underscore_ty(tok.span))
            }
            TokenKind::UppercaseStartId => self.parse_ty_cons(tok.span),
            _ => Err(Error::expected_actual("type", tok)),
        }
    }

    fn parse_lparen_pat(&mut self, span: Span) -> ParseResult<Pattern<'arena>> {
        self.parse_parens(
            span,
            |this| this.parse_pat(),
            |this, ty, span| this.builder.build_paren_pat(ty, span),
        )
    }

    fn parse_destruct(&mut self, cons_span: Span) -> ParseResult<Pattern<'arena>> {
        self.parse_with_list(
            cons_span,
            |this, name, span| this.builder.build_destruct(name, span, std::iter::empty()),
            |this| this.parse_pat(),
            |this, name, params, span| this.builder.build_destruct(name, span, params.into_iter()),
            TokenKind::LParen,
            TokenKind::RParen,
            "`,` (comma) or `)` (right parenthesis)",
            true,
        )
    }

    fn parse_pat_atom(&mut self) -> ParseResult<Pattern<'arena>> {
        let tok = self.peek()?;
        match tok.kind {
            TokenKind::LParen => self.parse_lparen_pat(tok.span),
            TokenKind::Underscore => {
                self.consume();
                Ok(self.builder.build_underscore_pat(tok.span))
            }
            TokenKind::LowercaseStartId => {
                self.consume();
                Ok(self
                    .builder
                    .build_id_pat(&self.source[tok.span.as_usize_range()], tok.span))
            }
            TokenKind::UppercaseStartId => self.parse_destruct(tok.span),
            _ => Err(Error::expected_actual("pattern", tok)),
        }
    }

    fn parse_pat(&mut self) -> ParseResult<Pattern<'arena>> {
        let atom = self.parse_pat_atom()?;
        let atom_start = atom.span.start;
        Ok(
            if let Ok(Token {
                kind: TokenKind::Colon,
                ..
            }) = self.peek()
            {
                self.consume();
                let ty = self.parse_ty()?;
                let ty_end = ty.span.end;
                self.builder
                    .build_annotated_pat(atom, ty, from_to(atom_start, ty_end))
            } else {
                atom
            },
        )
    }

    fn parse_fun(&mut self) -> ParseResult<Function<'arena>> {
        self.consume(); // skip fun
        let name_span = self.expect_exact(TokenKind::LowercaseStartId)?;
        let decl_start = name_span.start;
        let name = &self.source[name_span.as_usize_range()];
        let (_, ty_params) = self.parse_ty_list()?;

        self.expect_exact(TokenKind::LParen)?;
        let mut params = SmallVec::<[Pattern<'arena>; 8]>::new();

        let tok = self.peek()?;
        let mut decl_end = if tok.kind == TokenKind::RParen {
            self.consume();
            tok.span.end
        } else {
            params.push(self.parse_pat()?);
            loop {
                let tok = self.peek()?;
                match tok.kind {
                    TokenKind::RParen => {
                        self.consume();
                        break tok.span.end;
                    }
                    TokenKind::Comma => {
                        self.consume();
                        params.push(self.parse_pat()?);
                    }
                    _ => {
                        return Err(Error::expected_actual(
                            "`,` (comma) or `)` (right parenthesis)",
                            tok,
                        ))
                    }
                }
            }
        };

        let ret_ty = if self.peek()?.kind == TokenKind::Colon {
            self.consume();
            let ty = self.parse_ty()?;
            decl_end = ty.span.end;
            Some(ty)
        } else {
            None
        };

        let head = self.builder.build_fun_head(
            name,
            ty_params,
            params.into_iter(),
            ret_ty,
            from_to(decl_start, decl_end),
        );

        self.expect_exact(TokenKind::Arrow)?;
        let body = self.parse_exp()?;
        self.expect_exact(TokenKind::Period)?;

        Ok(self.builder.build_function(head, body))
    }

    fn parse_fun_group(&mut self, fun_span: Span) -> ParseResult<Exp<'arena>> {
        let head = self.builder.fun_group_start();
        let mut funs: SmallVec<[Function<'arena>; 4]> = smallvec![self.parse_fun()?];

        let mut tok = self.peek()?;
        while tok.kind == TokenKind::Fun {
            funs.push(self.parse_fun()?);
            tok = self.peek()?;
        }

        let funs = self.builder.build_function_group(head, funs.into_iter());
        let exp = self.parse_exp()?;
        let exp_end = exp.span.end;
        Ok(self
            .builder
            .build_function_exp(funs, exp, from_to(fun_span.start, exp_end)))
    }

    fn parse_val(&mut self, span: Span) -> ParseResult<Exp<'arena>> {
        self.consume(); // skip val
        let lhs = self.parse_pat()?;
        self.expect_exact(TokenKind::EqualsSign)?;
        let rhs: Exp<'_> = self.parse_exp()?;
        let head = self.builder.build_val_head(lhs, rhs);
        self.expect_exact(TokenKind::Period)?;

        let exp: Exp<'_> = self.parse_exp()?;
        let exp_end = exp.span.end;
        Ok(self
            .builder
            .finalize_val(head, exp, from_to(span.start, exp_end)))
    }

    fn parse_variant(
        &mut self,
        adt_group_context: &mut ADTGroupContext<'arena>,
    ) -> ParseResult<Variant<'arena>> {
        let name_span = self.expect_exact(TokenKind::UppercaseStartId)?;
        let name = &self.source[name_span.as_usize_range()];
        match self.peek() {
            Ok(Token {
                kind: TokenKind::LParen,
                ..
            }) => {
                self.consume();
                let mut params: SmallVec<[Ty<'arena>; 4]> = smallvec![self.parse_ty()?];
                return loop {
                    let tok = self.peek()?;
                    match tok.kind {
                        TokenKind::Comma => {
                            self.consume();
                            params.push(self.parse_ty()?);
                        }
                        TokenKind::RParen => {
                            self.consume();
                            break Ok(self.builder.build_variant(
                                adt_group_context,
                                name,
                                from_to(name_span.start, tok.span.end),
                                params.into_iter(),
                            ));
                        }
                        _ => {
                            break Err(Error::expected_actual(
                                "`,` (comma) or `)` (right parenthesis)",
                                tok,
                            ))
                        }
                    }
                };
            }
            _ => {}
        }
        Ok(self
            .builder
            .build_variant(adt_group_context, name, name_span, std::iter::empty()))
    }

    fn parse_tvar_input(&mut self) -> ParseResult<(&'src str, Span)> {
        let span = self.expect_exact(TokenKind::LowercaseStartId)?;
        let name = &self.source[span.as_usize_range()];
        Ok((name, span))
    }

    fn parse_ty_list(&mut self) -> ParseResult<(Option<Span>, TypeParamsList<'arena>)> {
        let mut params: SmallVec<[(&str, Span); 4]> = SmallVec::<[(&'src str, Span); 4]>::new();
        let tok = self.peek()?;
        if tok.kind == TokenKind::LBracket {
            let start = tok.span.start;
            self.consume();
            params.push(self.parse_tvar_input()?);
            loop {
                let tok = self.peek()?;
                match tok.kind {
                    TokenKind::Comma => {
                        self.consume();
                        params.push(self.parse_tvar_input()?);
                    }
                    TokenKind::RBracket => {
                        self.consume();
                        return Ok((
                            Some(from_to(start, tok.span.end)),
                            self.builder.build_tys_list(params.into_iter()),
                        ));
                    }
                    _ => {
                        return Err(Error::expected_actual(
                            "`,` (comma) or `]` (right bracket)",
                            tok,
                        ))
                    }
                }
            }
        };
        Ok((None, self.builder.build_tys_list(params.into_iter())))
    }

    fn parse_adt(
        &mut self,
        adt_group_context: &mut ADTGroupContext<'arena>,
        data_span: Span,
    ) -> ParseResult<ADT<'arena>> {
        self.consume(); // skip data
        let name_span = self.expect_exact(TokenKind::UppercaseStartId)?;
        let decl_start = data_span.start;
        let mut decl_end = decl_start;

        let (ty_param_span, ty_params) = self.parse_ty_list()?;
        if let Some(span) = ty_param_span {
            decl_end = span.end;
        }

        self.expect_exact(TokenKind::Colon)?;

        let mut variants: SmallVec<[Variant<'arena>; 4]> =
            smallvec![self.parse_variant(adt_group_context)?];
        let mut tok = self.peek()?;
        while tok.kind != TokenKind::Period {
            if tok.kind != TokenKind::Comma {
                return Err(Error::expected_actual(
                    "`,` (comma) or `.` (dot or period)",
                    tok,
                ));
            }
            self.consume();
            variants.push(self.parse_variant(adt_group_context)?);
            tok = self.peek()?;
        }

        self.consume();
        Ok(self.builder.build_adt(
            adt_group_context,
            &self.source[name_span.as_usize_range()],
            ty_params,
            from_to(decl_start, decl_end),
            variants.into_iter(),
        ))
    }

    fn parse_adts_in(&mut self, data_span: Span) -> ParseResult<Exp<'arena>> {
        let mut group = self.builder.new_adt_group();
        let mut adts: SmallVec<[ADT<'arena>; 4]> =
            smallvec![self.parse_adt(&mut group, data_span)?];

        let mut tok = self.peek()?;
        while tok.kind == TokenKind::Data {
            adts.push(self.parse_adt(&mut group, tok.span)?);
            tok = self.peek()?;
        }
        let exp = self.parse_exp()?;
        let exp_end = exp.span.end;
        Ok(self
            .builder
            .build_adt_in_exp(adts.into_iter(), exp, from_to(data_span.start, exp_end)))
    }

    fn parse_prong(&mut self) -> ParseResult<Prong<'arena>> {
        let mut patterns: SmallVec<[Pattern<'arena>; 4]> = smallvec![self.parse_pat()?];
        loop {
            let tok = self.peek()?;
            match tok.kind {
                TokenKind::Comma => {
                    self.consume();
                    patterns.push(self.parse_pat()?);
                }
                TokenKind::Arrow => {
                    self.consume();
                    let head = self.builder.build_prong_head(patterns.into_iter());
                    let exp = self.parse_exp()?;
                    break Ok(self.builder.finalize_prong(head, exp));
                }
                _ => {
                    break Err(Error::expected_actual(
                        "`=>` (right arrow) or `,` (comma)",
                        tok,
                    ))
                }
            }
        }
    }

    fn parse_match(&mut self, match_span: Span) -> ParseResult<Exp<'arena>> {
        self.consume(); // skip match

        let mut scrutinees: SmallVec<[Exp<'arena>; 4]> = smallvec![self.parse_exp()?];
        loop {
            let tok = self.peek()?;
            match tok.kind {
                TokenKind::Comma => {
                    self.consume();
                    scrutinees.push(self.parse_exp()?);
                }
                TokenKind::With => {
                    self.consume();
                    break;
                }
                _ => return Err(Error::expected_actual("`with` or `,` (comma)", tok)),
            }
        }

        let mut prongs: SmallVec<[Prong<'arena>; 4]> = smallvec![self.parse_prong()?];
        let end_span = loop {
            let tok = self.peek()?;
            match tok.kind {
                TokenKind::Comma => {
                    self.consume();
                    prongs.push(self.parse_prong()?);
                }
                TokenKind::End => {
                    self.consume();
                    break tok.span;
                }
                _ => {
                    return Err(Error::expected_actual(
                        "`end` or `,` (comma) followed by match prong",
                        tok,
                    ))
                }
            }
        };

        let span = from_to(match_span.start, end_span.end);
        Ok(self
            .builder
            .build_match(scrutinees.into_iter(), prongs.into_iter(), span))
    }

    fn parse_lparen_exp(&mut self, span: Span) -> ParseResult<Exp<'arena>> {
        self.parse_parens(
            span,
            |this| this.parse_exp(),
            |this, exp, span| this.builder.build_paren_exp(exp, span),
        )
    }

    fn parse_id_or_call(&mut self, span: Span) -> ParseResult<Exp<'arena>> {
        self.parse_with_list(
            span,
            |this, name, span| this.builder.build_id_exp(name, span),
            |this| this.parse_exp(),
            |this, name, params, span| this.builder.build_call_exp(name, span, params.into_iter()),
            TokenKind::LParen,
            TokenKind::RParen,
            "`,` (comma) or `)` (right parenthesis)",
            false,
        )
    }

    fn parse_cons(&mut self, cons_span: Span) -> ParseResult<Exp<'arena>> {
        self.parse_with_list(
            cons_span,
            |this, name, span| this.builder.build_new(name, span, std::iter::empty()),
            |this| this.parse_exp(),
            |this, name, params, span| this.builder.build_new(name, span, params.into_iter()),
            TokenKind::LParen,
            TokenKind::RParen,
            "`,` (comma) or `)` (right parenthesis)",
            true,
        )
    }

    fn parse_exp_atom(&mut self) -> ParseResult<Exp<'arena>> {
        let tok = self.peek()?;
        match tok.kind {
            TokenKind::Fun => self.parse_fun_group(tok.span),
            TokenKind::Val => self.parse_val(tok.span),
            TokenKind::Data => self.parse_adts_in(tok.span),
            TokenKind::Match => self.parse_match(tok.span),
            TokenKind::LParen => self.parse_lparen_exp(tok.span),
            TokenKind::LowercaseStartId => self.parse_id_or_call(tok.span),
            TokenKind::UppercaseStartId => self.parse_cons(tok.span),
            _ => Err(Error::expected_actual("expression", tok)),
        }
    }

    fn parse_exp(&mut self) -> ParseResult<Exp<'arena>> {
        self.parse_typed(
            |this| this.parse_exp_atom(),
            |this, exp, ty, span| this.builder.build_annotated_exp(exp, ty, span),
        )
    }

    fn parse_toplevel(&mut self) -> ParseResult<()> {
        let mut group = self.builder.new_adt_group();
        while let Some(tok) = self.peek_maybe_eof()? {
            match tok.kind {
                TokenKind::Fun => {
                    let fun = self.parse_fun()?;
                    self.builder.add_function(fun);
                }
                TokenKind::Data => {
                    let adt = self.parse_adt(&mut group, tok.span)?;
                    self.builder.add_adt(adt);
                }
                _ => return Err(Error::expected_actual("top-level declaration", tok)),
            }
        }
        Ok(())
    }
}

pub fn parse<'src, 'arena>(
    source: &'src str,
    mut builder: Builder<'arena>,
) -> ParseResult<AST<'arena>> {
    let mut parser = Parser::new(source, &mut builder);
    parser.parse_toplevel()?;
    drop(parser);
    Ok(builder.ast())
}

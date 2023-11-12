{-# LANGUAGE TupleSections #-}

module Parser where

import AST
  ( AExp,
    AExpKind (ACall, AExpCons, AExpId, AValIn),
    APat,
    APatKind (APatCons, APatId, APat_),
    ATy (ATy),
    ATyKind (ATyApp, ATyVar),
    Annotatable (annotate),
    HasSpan (..),
    Range,
    Span (Span),
    TextAExp,
    TextAPat,
    TextATy,
  )
import Control.Applicative (Alternative (empty), (<|>))
import Control.Monad (void, (>=>))
import Data.Functor ((<&>))
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (Parsec, SourcePos, getOffset, getSourcePos, notFollowedBy, single, (<?>))
import Text.Megaparsec.Char (alphaNumChar, lowerChar, space, space1, string, upperChar)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Prelude hiding (exp)

type Parser a = Parsec Void Text a

whitespace :: Parser ()
whitespace = Lexer.space space1 (Lexer.skipLineComment (pack "#")) empty

makeLexeme :: Parser a -> Parser a
makeLexeme = Lexer.lexeme whitespace

char :: Char -> Parser ()
char c = makeLexeme $ void (single c)

lparen :: Parser ()
lparen = char '('

rparen :: Parser ()
rparen = char ')'

lbracket :: Parser ()
lbracket = char '['

rbracket :: Parser ()
rbracket = char ']'

comma :: Parser ()
comma = char ','

colon :: Parser ()
colon = char ':'

underscore :: Parser ()
underscore = char '_'

equalsSign :: Parser ()
equalsSign = char '='

-- TODO: figure out what's the best way to lex identifiers with megaparsec
identifier :: Parser Char -> Parser Char -> Parser Text
identifier first others = makeLexeme $ do
  head <- first
  tail <- repeated others
  pure $ pack (head : tail)

keyword :: String -> Parser ()
keyword s = do
  string (pack s)
  notFollowedBy alphaNumChar

delimited :: Parser a -> Parser b -> Parser c -> Parser b
delimited l p r = (l *> p) <* r

parenthesized :: Parser a -> Parser a
parenthesized p = delimited lparen p rparen

bracketed :: Parser a -> Parser a
bracketed p = delimited lbracket p rbracket

withFallback :: a -> Parser a -> Parser a
withFallback a p = p <|> pure a

repeated :: Parser a -> Parser [a]
repeated p = withFallback [] $ p >>= \head -> repeated p >>= \tail -> pure $ head : tail

list1 :: Parser a -> Parser b -> Parser [b]
list1 sep elem = elem >>= \head -> repeated (sep *> elem) >>= \tail -> pure $ head : tail

list :: Parser a -> Parser b -> Parser [b]
list sep elem = list1 sep elem <|> pure []

commaList1 :: Parser a -> Parser [a]
commaList1 = list1 comma

commaList :: Parser a -> Parser [a]
commaList = list comma

reduce :: (a -> b -> a) -> Parser b -> a -> Parser a
reduce f p acc = withFallback acc $ p >>= \res -> reduce f p (f acc res)

class ParseSpan s where
  withSpan :: Parser a -> Parser (s, a)

instance ParseSpan () where
  withSpan = (((),) <$>)

instance ParseSpan Span where
  withSpan p = do
    start <- getOffset
    res <- p
    end <- getOffset
    pure (Span start end, res)

inSpan :: (HasSpan f p, ParseSpan s) => Parser (p s) -> Parser (f s)
inSpan p = uncurry addSpan <$> withSpan p

-----------------------------------------------------------------
-- lang2 specific stuff
-----------------------------------------------------------------

typeVarName :: Parser Text
typeVarName = identifier lowerChar alphaNumChar <?> "type variable name"

typeConsName :: Parser Text
typeConsName = identifier upperChar alphaNumChar <?> "type constructor name"

patVarName :: Parser Text
patVarName = identifier lowerChar alphaNumChar <?> "pattern variable name"

variantName :: Parser Text
variantName = identifier upperChar alphaNumChar <?> "variant name"

varName :: Parser Text
varName = identifier lowerChar alphaNumChar <?> "identifier"

ty :: (ParseSpan s) => Parser (TextATy s)
ty = inSpan (varTy <|> appTy) <|> parenthesized ty
  where
    appTy = do
      cons <- typeConsName
      params <- bracketed (commaList1 ty) <|> pure []
      pure $ ATyApp cons params

    varTy = typeVarName <&> ATyVar

withTy :: (ParseSpan s, Range s, Annotatable p) => Parser (p Text Text s) -> Parser (p Text Text s)
withTy = (>>= reduce annotate ty)

pat :: (ParseSpan s, Range s) => Parser (TextAPat s)
pat = withTy $ inSpan (underscorePat <|> varPat <|> consPat) <|> parenthesized pat
  where
    consPat = do
      var <- variantName
      params <- parenthesized (commaList1 pat) <|> pure []
      pure $ APatCons var params
    underscorePat = underscore >> pure APat_
    varPat = APatId <$> patVarName

exp :: (ParseSpan s, Range s) => Parser (TextAExp s)
exp = withTy $ inSpan (idOrCallExp <|> consExp <|> valInExp) <|> parenthesized exp
  where
    idOrCallExp = do
      name <- varName
      withFallback (AExpId name) $ ACall name <$> parenthesized (commaList exp)

    consExp = do
      name <- variantName
      params <- parenthesized (commaList1 exp) <|> pure []
      pure $ AExpCons name params

    valInExp = do
      keyword "val"
      lhs <- pat
      equalsSign
      rhs <- exp
      keyword "in"
      AValIn lhs rhs <$> exp

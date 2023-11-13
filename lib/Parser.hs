{-# LANGUAGE TupleSections #-}

module Parser where

import AST
  ( AADT (..),
    AExp,
    AExpKind (..),
    AFun (..),
    APat,
    APatKind (..),
    AST (..),
    ATopDecl (..),
    ATy (ATy),
    ATyKind (..),
    AVariant (..),
    Annotatable (annotate),
    HasSpan (..),
    Range,
    Span (Span),
    TextAADT,
    TextAExp,
    TextAFun,
    TextAPat,
    TextAST,
    TextATopDecl,
    TextATy,
    TextAVariant,
  )
import Control.Applicative (Alternative (empty), optional, (<|>))
import Control.Monad (void, (>=>))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (hidden), ParseErrorBundle, Parsec, SourcePos, eof, getOffset, getSourcePos, notFollowedBy, parse, single, (<?>))
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

dot :: Parser ()
dot = char '.'

equalsSign :: Parser ()
equalsSign = makeLexeme (void (single '=') >> notFollowedBy (single '>')) <?> "="

arrow :: Parser ()
arrow = makeLexeme . void $ string (pack "=>")

-- TODO: figure out what's the best way to lex identifiers with megaparsec
identifier :: Parser Char -> Parser Char -> Parser Text
identifier first others = makeLexeme $ do
  head <- first
  tail <- repeated others
  pure $ pack (head : tail)

keyword :: String -> Parser ()
keyword s = makeLexeme $ do
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

repeated1 :: Parser a -> Parser (NonEmpty a)
repeated1 p = do
  first <- p
  (first :|) <$> repeated p

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
typeVarName = hidden (identifier lowerChar alphaNumChar) <?> "type variable name"

typeConsName :: Parser Text
typeConsName = hidden (identifier upperChar alphaNumChar) <?> "type constructor name"

patVarName :: Parser Text
patVarName = hidden (identifier lowerChar alphaNumChar) <?> "pattern variable name"

variantName :: Parser Text
variantName = hidden (identifier upperChar alphaNumChar) <?> "variant name"

varName :: Parser Text
varName = hidden (identifier lowerChar alphaNumChar) <?> "identifier"

funName :: Parser Text
funName = hidden (identifier lowerChar alphaNumChar) <?> "function name"

ty :: (ParseSpan s) => Parser (TextATy s)
ty = inSpan (varTy <|> appTy) <|> parenthesized ty
  where
    appTy = do
      cons <- typeConsName
      params <- bracketed (commaList1 ty) <|> pure []
      pure $ ATyApp cons params

    varTy = typeVarName <&> ATyVar

withTy :: (ParseSpan s, Range s, Annotatable p) => Parser (p Text Text s) -> Parser (p Text Text s)
withTy = (>>= reduce annotate (colon >> ty))

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
exp =
  withTy $
    inSpan
      ( funGroupExp
          <|> adtGroupExp
          <|> valInExp
          <|> idOrCallExp
          <|> consExp
      )
      <|> parenthesized exp
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
      dot
      AValIn lhs rhs <$> exp

    funGroupExp = do
      funs <- (:| []) <$> repeated1 fun
      AExpFunGroup funs <$> exp

    adtGroupExp = do
      adts <- repeated1 adt
      AExpADTGroup adts <$> exp

fun :: (ParseSpan s, Range s) => Parser (TextAFun s)
fun = do
  keyword "fun"
  (span, name) <- withSpan funName
  tyParams <- optional (bracketed (commaList1 (withSpan typeVarName)))
  params <- parenthesized (commaList pat)
  retTy <- optional (colon >> ty)
  arrow
  body <- exp
  dot
  return $
    AFun
      { aFunName = name,
        aFunNameSpan = span,
        aFunTvs = tyParams,
        aFunParams = params,
        aFunRetTy = retTy,
        aFunBody = body
      }

variant :: (ParseSpan s, Range s) => Parser (TextAVariant s)
variant = do
  (span, name) <- withSpan variantName
  params <- parenthesized (commaList1 ty) <|> pure []
  pure $
    AVariant
      { aVariantName = name,
        aVariantNameSpan = span,
        aVariantParams = params
      }

adt :: (ParseSpan s, Range s) => Parser (TextAADT s)
adt = do
  keyword "data"
  name <- typeConsName
  params <- bracketed (commaList1 (withSpan typeVarName)) <|> pure []
  colon
  variants <- commaList1 variant
  dot
  pure $ AADT {aADTName = name, aADTParams = params, aADTVariants = variants}

topItem :: (ParseSpan s, Range s) => Parser (TextATopDecl s)
topItem = (ATopADT <$> adt) <|> (ATopFun <$> fun)

ast' :: (ParseSpan s, Range s) => Parser (TextAST s)
ast' = AST <$> repeated topItem

ast :: (ParseSpan s, Range s) => Parser (TextAST s)
ast = ast' <* eof

astNoSpans :: Parser (TextAST ())
astNoSpans = ast

astWithSpans :: Parser (TextAST Span)
astWithSpans = ast

parseASTNoSpans :: String -> Text -> Either (ParseErrorBundle Text Void) (TextAST ())
parseASTNoSpans = parse ast

parseASTWithSpans :: String -> Text -> Either (ParseErrorBundle Text Void) (TextAST Span)
parseASTWithSpans = parse ast

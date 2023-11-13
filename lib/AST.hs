{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}

module AST where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

data Span = Span Int Int

instance Show Span where
  show (Span start end) = show start ++ "-" ++ show end

class Range s where
  fromTo :: s -> s -> s

instance Range () where
  fromTo :: () -> () -> ()
  fromTo _ _ = ()

instance Range Span where
  fromTo (Span from _) (Span _ to) = Span from to

data ATyKind tv tc s
  = ATyVar tv
  | ATyApp tc [ATy tv tc s]
  deriving (Show)

data ATy tv tc s = ATy s (ATyKind tv tc s) deriving (Show)

type TextATy = ATy Text Text

data APatKind id c tv tc s
  = APatId id
  | APat_
  | APatCons c [APat id c tv tc s]
  | APatOfType (APat id c tv tc s) (ATy tv tc s)
  deriving (Show)

data APat id c tv tc s = APat s (APatKind id c tv tc s) deriving (Show)

type TextAPat = APat Text Text Text Text

data AExpKind id c tv tc s
  = AValIn (APat id c tv tc s) (AExp id c tv tc s) (AExp id c tv tc s)
  | ACall id [AExp id c tv tc s]
  | AExpCons c [AExp id c tv tc s]
  | AExpId id
  | AExpOfType (AExp id c tv tc s) (ATy tv tc s)
  | -- List of lists of functions is used here to indicate letrec groups - parser produces n singleton lists,
    -- renaming pass uses tarjan's algorithm to assemble SCCs together
    AExpFunGroup (NonEmpty (NonEmpty (AFun id c tv tc s))) (AExp id c tv tc s)
  | AExpADTGroup (NonEmpty (AADT c tv tc s)) (AExp id c tv tc s)
  deriving (Show)

data AExp id c tv tc s = AExp s (AExpKind id c tv tc s) deriving (Show)

type TextAExp = AExp Text Text Text Text

data AFun id c tv tc s = AFun
  { aFunName :: id,
    aFunNameSpan :: s,
    aFunTvs :: Maybe [(s, tv)],
    aFunRetTy :: Maybe (ATy tv tc s),
    aFunParams :: [APat id c tv tc s],
    aFunBody :: AExp id c tv tc s
  }
  deriving (Show)

type TextAFun = AFun Text Text Text Text

data AVariant c tv tc s = AVariant
  { aVariantName :: c,
    aVariantNameSpan :: s,
    aVariantParams :: [ATy tv tc s]
  }
  deriving (Show)

type TextAVariant = AVariant Text Text Text

data AADT c tv tc s = AADT
  { aADTName :: tc,
    aADTParams :: [(s, tv)],
    aADTVariants :: [AVariant c tv tc s]
  }
  deriving (Show)

type TextAADT = AADT Text Text Text

data ATopDecl id c tv tc s = ATopFun (AFun id c tv tc s) | ATopADT (AADT c tv tc s) deriving (Show)

type TextATopDecl = ATopDecl Text Text Text Text

newtype AST id c tv tc s = AST [ATopDecl id c tv tc s] deriving (Show)

type TextAST = AST Text Text Text Text

class HasSpan f p | f -> p where
  addSpan :: s -> p s -> f s
  removeSpan :: f s -> (s, p s)

instance HasSpan (ATy tv tc) (ATyKind tv tc) where
  addSpan = ATy
  removeSpan (ATy span kind) = (span, kind)

instance HasSpan (APat id c tv tc) (APatKind id c tv tc) where
  addSpan = APat
  removeSpan (APat span kind) = (span, kind)

instance HasSpan (AExp id c tv tc) (AExpKind id c tv tc) where
  addSpan = AExp
  removeSpan (AExp span kind) = (span, kind)

class Annotatable f where
  annotate :: (Range s) => f tv tc s -> ATy tv tc s -> f tv tc s

instance Annotatable (APat id c) where
  annotate pat@(APat start _) ty@(ATy end _) = APat (fromTo start end) $ APatOfType pat ty

instance Annotatable (AExp id c) where
  annotate exp@(AExp start _) ty@(ATy end _) = AExp (fromTo start end) $ AExpOfType exp ty

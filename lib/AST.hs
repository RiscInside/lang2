{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}

module AST
  ( ATyKind (..),
    ATy (..),
    APatKind (..),
    APat (..),
    AExpKind (..),
    AExp (..),
    Span (Span),
    Range,
    HasSpan,
    Annotatable,
    annotate,
    addSpan,
    removeSpan,
    TextATy,
    TextAPat,
    TextAExp,
  )
where

import Data.Text (Text)

data Span = Span Int Int

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

data APat id c tv tc s = APat s (APatKind id c tv tc s)

type TextAPat = APat Text Text Text Text

data AExpKind id c tv tc s
  = AValIn (APat id c tv tc s) (AExp id c tv tc s) (AExp id c tv tc s)
  | ACall id [AExp id c tv tc s]
  | AExpCons c [AExp id c tv tc s]
  | AExpId id
  | AExpOfType (AExp id c tv tc s) (ATy tv tc s)

data AExp id c tv tc s = AExp s (AExpKind id c tv tc s)

type TextAExp = AExp Text Text Text Text

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

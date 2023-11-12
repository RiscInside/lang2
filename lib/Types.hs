module Types where

import Control.Monad (forM_, join, void, (>=>))
import Control.Monad.Plus (MonadPlus, guard)
import Control.Monad.Ref (MonadRef, newRef, readRef, writeRef)
import Control.Monad.ST.Strict (runST)
import Control.Monad.Trans.Maybe (runMaybeT)
import Data.Functor ((<&>))
import Data.IORef (IORef)
import Data.List (sort)
import Data.Map (Map, empty, insert, lookup, toList)
import Data.Set (Set)
import Prelude hiding (lookup)

type ConsName = Int

newtype Level = Level Int deriving (Eq, Ord, Show)

data UVar r c v = Unbound {level :: Level, varId :: v} | Bound (UTy r c v)

type UVarRef r c v = r (UVar r c v)

data UTy r c v = UTyVar (UVarRef r c v) | UTyApp {uTyCons :: c, uTyParams :: [UTy r c v]}

ufQuery :: (MonadRef r m) => UTy r c v -> m (UTy r c v)
ufQuery ty@(UTyVar ref) = do
  state <- readRef ref
  case state of
    Bound inner -> do
      res <- ufQuery inner
      writeRef ref $ Bound res
      pure res
    Unbound _ _ -> pure ty
ufQuery ty = pure ty

levelAndVar :: (MonadRef r m) => r (UVar r c v) -> m (Level, v)
levelAndVar ref = do
  state <- readRef ref
  case state of
    Unbound {level = l', varId = v'} -> pure (l', v')
    Bound _ -> error "Non-final type passed to levelAndVar"

occurs :: (MonadRef r m, MonadPlus m, Eq v) => v -> Level -> UTy r c v -> m ()
occurs v l = ufQuery >=> occurs' v l
  where
    occurs' :: (MonadRef r m, MonadPlus m, Eq v) => v -> Level -> UTy r c v -> m ()
    occurs' v l (UTyApp {uTyParams = params}) = forM_ params (occurs v l)
    occurs' v l (UTyVar ref) = do
      (l', v') <- levelAndVar ref
      guard $ v' /= v
      writeRef ref Unbound {level = min l' l, varId = v'}

unify :: (MonadRef r m, MonadPlus m, Eq c, Eq v, Eq (UVarRef r c v)) => UTy r c v -> UTy r c v -> m ()
unify t1 t2 = join $ (unify' <$> ufQuery t1) <*> ufQuery t2
  where
    unify' :: (MonadRef r m, MonadPlus m, Eq c, Eq v, Eq (UVarRef r c v)) => UTy r c v -> UTy r c v -> m ()
    unify' (UTyVar ref1) (UTyVar ref2)
      | ref1 == ref2 = pure ()
      | otherwise = do
          (l1, v1) <- levelAndVar ref1
          (l2, v2) <- levelAndVar ref2
          writeRef ref1 $ Unbound {level = min l1 l2, varId = v1}
          writeRef ref2 $ Bound (UTyVar ref1)
    unify' (UTyVar ref) ty = do
      (l, v) <- levelAndVar ref
      occurs v l ty
      writeRef ref $ Bound ty
    unify' ty (UTyVar ref) = do
      (l, v) <- levelAndVar ref
      occurs v l ty
      writeRef ref $ Bound ty
    unify' (UTyApp c1 t1s) (UTyApp c2 t2s) = do
      guard $ c1 == c2
      guard $ length t1s == length t2s
      forM_ (zip t1s t2s) (uncurry unify)

data DemoTy = DVar String Int | DApp String [DemoTy] deriving (Show, Eq, Ord)

demoWrap :: (MonadRef r m) => r (Map String (UVarRef r String String)) -> DemoTy -> m (UTy r String String)
demoWrap mapPtr (DVar name lvl) = do
  map <- readRef mapPtr
  case lookup name map of
    Just res -> pure $ UTyVar res
    Nothing -> do
      ref <- newRef (Unbound {level = Level lvl, varId = name})
      writeRef mapPtr (insert name ref map)
      pure $ UTyVar ref
demoWrap mapPtr (DApp name params) = do
  params' <- mapM (demoWrap mapPtr) params
  pure $ UTyApp {uTyCons = name, uTyParams = params'}

demoUnwrap :: (MonadRef r m) => UTy r String String -> m DemoTy
demoUnwrap = ufQuery >=> demoUnwrap'
  where
    demoUnwrap' :: (MonadRef r m) => UTy r String String -> m DemoTy
    demoUnwrap' (UTyVar ref) = levelAndVar ref >>= \(Level l, v) -> pure $ DVar v l
    demoUnwrap' (UTyApp {uTyCons = cons, uTyParams = params}) = DApp cons <$> mapM demoUnwrap params

renderMap :: (MonadRef r m) => Map String (UVarRef r String String) -> m (Map String DemoTy)
renderMap = mapM (demoUnwrap . UTyVar)

unifyResult' :: (MonadRef r m, MonadPlus m, Eq (UVarRef r String String)) => DemoTy -> DemoTy -> m DemoTy
unifyResult' ty1 ty2 = do
  mapPtr <- newRef empty
  wty1 <- demoWrap mapPtr ty1
  wty2 <- demoWrap mapPtr ty2
  unify wty1 wty2
  uty1 <- demoUnwrap wty1
  uty2 <- demoUnwrap wty2
  pure $ if uty1 == uty2 then uty1 else error $ "Unequal types: " ++ show uty1 ++ " != " ++ show uty2

unifyEquations' :: (MonadRef r m, Eq (UVarRef r String String)) => DemoTy -> DemoTy -> m [(String, DemoTy)]
unifyEquations' ty1 ty2 = do
  mapPtr <- newRef empty
  wty1 <- demoWrap mapPtr ty1
  wty2 <- demoWrap mapPtr ty2
  runMaybeT $ unify wty1 wty2
  all <- toList <$> (readRef mapPtr >>= renderMap)
  pure $ sort [eq | eq@(_, DApp _ _) <- all] ++ [eq | eq@(l, DVar r _) <- all, l /= r]

unifyResultMaybeST :: DemoTy -> DemoTy -> Maybe DemoTy
unifyResultMaybeST ty1 ty2 = runST (runMaybeT (unifyResult' ty1 ty2))

unifyEquationsST :: DemoTy -> DemoTy -> [(String, DemoTy)]
unifyEquationsST ty1 ty2 = runST (unifyEquations' ty1 ty2)

unifyTest :: DemoTy -> DemoTy -> (Maybe DemoTy, [(String, DemoTy)])
unifyTest ty1 ty2 = (unifyResultMaybeST ty1 ty2, unifyEquationsST ty1 ty2)

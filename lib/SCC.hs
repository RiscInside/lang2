{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SCC
  ( MonadGraph (neighboursM),
    Graph (neighbours),
    computeSCCsM,
    computeSCCs,
    IVertex (IVertex),
    computeSCCsTest,
  )
where

import Control.Monad (forM_)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict
  ( StateT,
    evalStateT,
    get,
    put,
  )
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map, empty, insert, member, (!))

class (Monad m, Ord v) => MonadGraph m g v where
  neighboursM :: g -> v -> m [v]

data VertexData = VertexData {vId :: Int, lowlink :: Int, onStack :: Bool}

data TarjanState v = TarjanState {stack :: [v], vertexData :: Map v VertexData, unusedId :: Int, components :: [NonEmpty v]}

type TarjanT m v = StateT (TarjanState v) m

instance (MonadGraph m g v) => MonadGraph (StateT s m) g v where
  neighboursM v = lift . neighboursM v

isVisited :: (Monad m, Ord v) => v -> TarjanT m v Bool
isVisited v = member v . vertexData <$> get

fresh :: (Monad m) => TarjanT m v Int
fresh = do
  state <- get
  put $ state {unusedId = unusedId state + 1}
  return $ unusedId state

enter :: (Monad m, Ord v) => v -> TarjanT m v ()
enter v = do
  vId <- fresh
  state <- get
  put $
    state
      { stack = v : stack state,
        vertexData = insert v (VertexData {vId = vId, lowlink = vId, onStack = True}) $ vertexData state
      }

minLowlink :: (Monad m, Ord v) => v -> v -> TarjanT m v ()
minLowlink v nv = do
  state <- get
  let ndata = vertexData state ! nv
  if not (onStack ndata)
    then return ()
    else do
      let vdata = vertexData state ! v
      let newLowlink = lowlink vdata `min` lowlink ndata
      put $ state {vertexData = insert v (vdata {lowlink = newLowlink}) $ vertexData state}

endSCC :: (Monad m, Ord v) => v -> [v] -> TarjanT m v (NonEmpty v)
endSCC v !others = do
  state <- get
  case stack state of
    [] -> error "no vertex seen on the stack"
    (v' : vs) -> do
      let vdata = vertexData state ! v
      put $ state {stack = vs, vertexData = insert v' (vdata {onStack = False}) $ vertexData state}
      if v' == v then return $ v' :| others else endSCC v (v' : others)

dfs :: (MonadGraph m g v, Ord v) => g -> v -> TarjanT m v ()
dfs g v = do
  visited <- isVisited v
  if visited
    then return ()
    else do
      -- Enter the node and push it on the nodes stack
      enter v
      -- Visit all neighboursM of the node
      nvs <- neighboursM g v
      forM_ nvs $ \nv -> do
        dfs g nv
        minLowlink v nv
      state <- get
      -- Check if there is a strongly connected component rooted at v
      mdata <- (! v) . vertexData <$> get
      if lowlink mdata /= vId mdata
        then return ()
        else do
          -- Assemble list of all vertices in SCC
          scc <- endSCC v []
          state <- get
          put $ state {components = scc : components state}

dfsAll :: (MonadGraph m g v, Ord v) => g -> [v] -> TarjanT m v ()
dfsAll g = mapM_ (dfs g)

computeSCCsM :: (MonadGraph m g v, Ord v) => g -> [v] -> m [NonEmpty v]
computeSCCsM graph vertices =
  evalStateT (computeSCCs' graph vertices) $
    TarjanState {stack = [], vertexData = empty, unusedId = 0, components = []}
  where
    -- List of the components needs to be reversed for the components that were assembled first to be visited first
    computeSCCs' graph vertices = dfsAll graph vertices >> (reverse . components <$> get)

class Graph g v where
  neighbours :: g -> v -> [v]

instance (Graph g v, Ord v) => (MonadGraph Identity g v) where
  neighboursM g v = pure $ neighbours g v

computeSCCs :: (Graph g v, Ord v) => g -> [v] -> [NonEmpty v]
computeSCCs graph vertices = runIdentity $ computeSCCsM graph vertices

newtype IVertex = IVertex Int deriving (Eq, Ord)

instance Show IVertex where
  show (IVertex v) = show v

instance Graph [(IVertex, IVertex)] IVertex where
  neighbours edges v = [nv | (v', nv) <- edges, v' == v]

computeSCCsTest :: [IVertex] -> [(IVertex, IVertex)] -> [NonEmpty IVertex]
computeSCCsTest vertices edges = computeSCCs edges vertices

instance Num IVertex where
  fromInteger = IVertex . fromInteger

  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  negate = undefined

instance Enum IVertex where
  toEnum = IVertex
  fromEnum (IVertex e) = e

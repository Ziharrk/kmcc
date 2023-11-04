{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Tree where

import           Control.Applicative ( Alternative(empty, (<|>)) )
import           Control.Concurrent.Chan ( getChanContents, newChan, writeChan )
import           Control.Concurrent.MVar ( newEmptyMVar, putMVar, takeMVar )
import           Control.Concurrent (forkFinally)
import           Control.DeepSeq ( NFData )
import           Control.Monad ( MonadPlus )
import           Data.Maybe ( catMaybes, isJust )
import qualified Data.Sequence as Seq
import           GHC.Generics ( Generic )
import           GHC.Conc ( killThread )
import           System.IO.Unsafe ( unsafePerformIO )
import           System.Mem.Weak ( addFinalizer )

-- A normal tree implementation and bfs/dfs implementations
data Tree a = Empty
            | Leaf a
            | Node (Tree a) (Tree a)
  deriving (Functor, Show, Generic)

instance Applicative Tree where
  pure = Leaf
  Empty <*> _ = Empty
  Leaf f <*> a = fmap f a
  Node l r <*> a = Node (l <*> a) (r <*> a)

instance Alternative Tree where
  empty = Empty
  (<|>) = Node

instance Monad Tree where
  Empty    >>= _ = Empty
  Leaf a   >>= f = f a
  Node l r >>= f = Node (l >>= f) (r >>= f)

instance MonadPlus Tree

instance MonadFail Tree where
  fail = const Empty

instance NFData a => NFData (Tree a) where


dfs :: Tree a -> [a]
dfs t' = dfs' [t']
  where dfs' [] = []
        dfs' (t:ts) = case t of
                        Empty -> dfs' ts
                        Leaf x -> x : dfs' ts
                        Node l r -> dfs' ([l, r] ++ ts)

bfs :: Tree a -> [a]
bfs t' = bfs' (Seq.singleton t')
  where bfs' Seq.Empty = []
        bfs' (t Seq.:<| ts) = case t of
          Empty -> bfs' ts
          Leaf x -> x : bfs' ts
          Node l r -> bfs' (ts Seq.:|> l Seq.:|> r)

{-# NOINLINE fs #-}
fs :: Tree a -> [a]
fs t = unsafePerformIO $ do
  ch <- newChan
  mvarTids <- newEmptyMVar
  putMVar mvarTids []
  let go t' = case t' of
        Empty    -> return ()
        Leaf x   -> writeChan ch (Just x)
        Node l r -> do
          mvarL <- newEmptyMVar
          mvarR <- newEmptyMVar
          tids <- takeMVar mvarTids
          tidL <- forkFinally (go l) $ \_ -> putMVar mvarL ()
          tidR <- forkFinally (go r) $ \_ -> putMVar mvarR ()
          putMVar mvarTids $ [tidL, tidR] ++ tids
          takeMVar mvarL
          takeMVar mvarR
  tid <- forkFinally (go t) $ \_ -> writeChan ch Nothing
  result <- catMaybes . takeWhile isJust <$> getChanContents ch
  addFinalizer result $ do
    tids <- takeMVar mvarTids
    mapM_ killThread (tid : tids)
  return result

{-
fs :: Tree a -> [a]
fs t' = fs' t'
  where fs' t = case t of
                  Empty -> []
                  Leaf x -> [x]
                  Node l r -> let (x,y) = (fs' l, fs' r) in par x (pseq y (x ++ y))
-}

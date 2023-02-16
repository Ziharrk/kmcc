{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Tree where

import           Control.Applicative ( Alternative(empty, (<|>)) )
import           Control.Monad ( MonadPlus )
import           Control.DeepSeq ( NFData )
import           Data.Maybe
import qualified Data.Sequence as Seq
import           GHC.Generics ( Generic )
import           GHC.Conc
import           Control.Concurrent.MVar
import           Control.Concurrent.Chan
import           System.IO.Unsafe

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

fs :: Tree a -> [a]
fs t' = unsafePerformIO $ do
  ch <- newChan
  let fsIO t = case t of
        Empty -> return ()
        Leaf x -> writeChan ch (Just x)
        Node l r -> do
          mvarL <- newEmptyMVar
          mvarR <- newEmptyMVar
          _ <- forkIO $ fsIO l >> putMVar mvarL ()
          _ <- forkIO $ fsIO r >> putMVar mvarR ()
          takeMVar mvarL
          takeMVar mvarR
  forkIO $ fsIO (t') >> writeChan ch Nothing
  catMaybes . takeWhile isJust <$> getChanContents ch

{-
fs :: Tree a -> [a]
fs t' = fs' t'
  where fs' t = case t of
                  Empty -> []
                  Leaf x -> [x]
                  Node l r -> let (x,y) = (fs' l, fs' r) in par x (pseq y (x ++ y))
-}
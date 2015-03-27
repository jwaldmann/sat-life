{-# language NoMonomorphismRestriction #-}

module Step where

import Satchmo.Boolean

import Data.List ( inits, tails, sortBy )
import Data.Function (on)

import Prelude hiding (and, or, not, (||), (&&) )
import qualified Prelude
import Control.Monad ( forM, foldM, replicateM, guard )

step =
  step_mod
  -- step_orig

step_mod x xs = do
  out <- boolean
  forM (select' 4 xs) $ \ (ys,zs) -> -- at least 4 neighbours
    assert_implies ( ys               ) [ not out ]
  forM (select' 3 xs) $ \ (ys,zs) -> -- exactly 3
    assert_implies ( ys ++ map not zs ) [ out ]
  forM (select' 2 xs) $ \ (ys,zs) -> do -- exactly 2
    assert_implies ( ys ++ map not zs ) [ not x, out ]
    assert_implies ( ys ++ map not zs ) [ x, not out ]
  forM (select' 1 xs) $ \ (ys,zs) -> -- at most 1 neigh
    assert_implies (       map not zs ) [ not out ]
  return out

assert_implies xs ys = assert $ map not xs ++ ys

select :: Int -> [a] -> [[a]]
select 0 xs = [[]]
select k [] = []
select k (x:xs) =
  select k xs ++ (map (x:) $ select (k-1) xs)

select' :: Int -> [a] -> [([a],[a])]
select' 0 xs = [([],xs)]
select' k [] = []
select' k (x:xs) =
     map (\(l,r) -> (l,x:r)) (select'     k xs)
  ++ map (\(l,r) -> (x:l,r)) (select' (k-1) xs)


step_orig x xs = do
    cs <- counts 3 xs
    keep <- and [ x, cs !! 2 ]
    let birth = cs !! 3
    or [ keep, birth ]
    
-- | output !! k  == True
-- if exactly  k  of the inputs are True
counts :: MonadSAT m
       => Int -> [ Boolean ] 
       -> m [ Boolean ]
counts w xs = do
    t <- constant True ; f <- constant False
    let handle cs x = do
           ds <- forM cs $ \ c -> boolean
           forM ( zip cs ds ) $ \ (c,d) -> do
               assert_fun3 ( \ c d x -> Prelude.not x <= ( c == d ) ) c d x
           forM ( zip ( f : cs) ds ) $ \ (c,d) -> do
               assert_fun3 ( \ c d x -> x <= ( c == d ) ) c d x
           return ds
    foldM handle ( t : replicate w f ) xs

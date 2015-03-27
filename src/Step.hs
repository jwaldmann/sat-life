{-# language NoMonomorphismRestriction #-}

module Step where

import Satchmo.Boolean

import Data.List ( inits, tails, sortBy )
import Data.Function (on)

import Prelude hiding (and, or, not, (||), (&&) )
import qualified Prelude
import Control.Monad ( forM, foldM, replicateM, guard )

step = step_mod
  -- step_orig

step_mod x xs = do
  out <- boolean
  forM (select 4 xs) $ \ ys -> assert $ not out : map not ys
  forM (select 7 xs) $ \ ys -> assert $ not out : ys
  forM (select 6 xs) $ \ ys -> assert $ not out : x : ys
  forM (select' 3 xs) $ \ (ys,zs) -> assert $ out : map not ys ++ zs
  forM (select' 2 xs) $ \ (ys,zs) -> assert $ out : not x : map not ys ++ zs
  return out

select :: Int -> [a] -> [[a]]
select 0 xs = [[]]
select k [] = []
select k (x:xs) =
  select k xs ++ (map (x:) $ select (k-1) xs)

select' :: Int -> [a] -> [([a],[a])]
select' 0 xs = [([],[])]
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

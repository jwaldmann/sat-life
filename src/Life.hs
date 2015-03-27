-- | compute oscillator for Conway's game of life, 
-- cf. http://www.conwaylife.com/wiki/Category:Oscillators
-- example usage: ./dist/build/Life/Life 3 9 9 20
-- arguments are: period, width, height, number of life start cells

{-# language PatternSignatures #-}
{-# language FlexibleContexts #-}

module Life where

import Step ( step )

import Prelude hiding ( not, or, and )
import qualified Prelude

import Satchmo.Relation
import Satchmo.Code
import Satchmo.Boolean hiding ( equals, implies )
import Satchmo.Counting.Binary

import Satchmo.SAT.Mini

import qualified Config as C

import Data.List (sort)
import qualified Data.Array as A
import Control.Monad ( guard, when, forM, foldM, void )
import System.Environment
import Data.Ix ( range, inRange )

main_with c = do
    print c
    Just gs <- solve $ osc c
    void $ forM ( zip [ 0..  ] gs ) $ \ (t, g) -> do
        putStrLn $ unwords [ "time", show t ]
        printA g
    print c

printA :: A.Array (Int,Int) Bool -> IO ()
printA a = putStrLn $ unlines $ do
         let ((u,l),(o,r)) = A.bounds a
         x <- [u .. o]
         return $ unwords $ do 
             y <- [ l ..r ]
             return $ case a A.! (x,y) of
                  True -> "O " ; False -> ". "

osc :: C.Config -> SAT ( SAT [ A.Array (Int,Int) Bool ] )
osc c = do
    let w = C.width c; h = C.width c ; p = C.period c
    g0 <- relation ((1,1),(w,h))
    case C.cells c of
         Just c -> monadic assert [ atmost c $ map snd $ assocs g0 ]
         Nothing -> return ()
    when (p == 1) $ do
      assert $ elems g0
    let handle 0 gs = return gs
        handle k (g:gs) = do g' <- next c g ; handle (k-1) (g' : g : gs)
    gs <- handle p [ g0 ]
    forM gs bordered
    monadic assert [ equals ( head gs ) ( last gs ) ]
    forM [ d | d <- [1 .. p - 1] , 0 == mod p d ] $ \ d -> 
        monadic assert [ fmap not $ equals ( gs !! 0 ) ( gs !! d ) ]

    footprint <- foldM union (head gs) (tail gs)
    stator <- foldM intersection (head gs) (tail gs)
    rotor <- intersection footprint $ complement stator

    case C.stator c of
      Nothing -> return ()
      Just s -> do
        ok <- atmost s $ elems stator
        assert [ ok ]
    case C.rotor c of
      Nothing -> return ()
      Just s -> do
        ok <- atmost s $ elems rotor
        assert [ ok ]
        
    return $ decode $ reverse gs

bordered g = do
    let ((u,l),(d,r)) = bounds g
    forM [ u .. d ] $ \ x -> forM [ l  , r ] $ \ y -> assert [ not $ g!(x,y) ]
    forM [ u ,  d ] $ \ x -> forM [ l .. r ] $ \ y -> assert [ not $ g!(x,y) ]

next c g = do
    f <- constant False
    let bnd = bounds g
    let neighbours (i,j) = do
            i' <- [ i-1, i, i+1 ]
            j' <- [ j-1, j, j+1 ]
            guard $ i /= i' Prelude.|| j /= j'
            return $ if inRange bnd (i',j') 
               then g ! (i', j')
               else f
    pairs <- forM ( assocs g ) $ \ (p, x) -> do
        y <- step c x $ neighbours p
        return (p, y)
    return $ build bnd pairs


    


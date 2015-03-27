module Opt where

import qualified OBDD as O

import qualified Data.Set as S
import qualified Data.Map.Strict as M


vars = "out" : "mid" : map (\i -> "x" ++ show i) [1..8]

life = O.or $ do
  fs <- replicateM (length vars) [False,True]
  let out : mid : ns = fs
      c = length $ filter id ns
      out' = (c == 3) Prelude.|| (c == 2) Prelude.&& mid
  guard $ out == out'
  return $ O.and $ zipWith O.unit vars fs

form f = reduce $ S.unions $ implied f

reduce s =
  let h [] = []
      h (c:ds) = c : h (filter ( \d -> Prelude.not $ M.isSubmapOf c d ) ds )
  in h $ sortBy (compare `on` M.size) $ S.toList s

implied f = do
  m <- O.all_models $ O.not f
  return $ reduce_implied f $ M.toList $ M.map Prelude.not m

cl0 = [("out",False)
      ,("x1",False),("x2",False),("x3",False),("x4",False)]

reduce_implied f cl =
  let here = O.or $ map (uncurry  O.unit) cl
      sub = S.unions $ do
        (pre, _ : post) <- splits cl
        let cl' = pre ++ post
        return $ reduce_implied f cl'
  in if o_implies f here
     then if S.null sub
          then S.singleton (M.fromList cl)
          else sub
     else S.empty

splits xs = zip (inits xs) (tails xs)  

subs xs = do
  fs <- replicateM (length xs) [False,True]
  return $ map fst $ filter snd $ zip xs fs

o_implies f g = O.null $ f O.&& (O.not g)    


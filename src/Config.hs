module Config where

import Options.Applicative
import Options.Applicative.Types

data Config =
  Config { period :: Int
         , width :: Int
         -- , height :: Int
         , cells :: Maybe Int
         , stator :: Maybe Int
         }
  deriving Show

config0 = Config { period = 3
                 , width = 9 -- , height = 9
                 , cells = Nothing, stator = Nothing
                 }

config :: Parser Config
config = Config
  <$> option auto ( long "period" <> short 'p' )
  <*> option auto ( long "width" <> short 'w' )
  <*> option (Just <$> auto) ( long "cells" <> short 'c' <> value Nothing )
  <*> option (Just <$> auto) ( long "stator" <> short 's' <> value Nothing )

parse :: IO Config
parse = customExecParser (prefs disambiguate)
   $ info (helper <*> config) idm


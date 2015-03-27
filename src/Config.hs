module Config where

import Options.Applicative
import Options.Applicative.Types

data Method = Unary | Binary | Direct
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Config =
  Config { period :: Int
         , width :: Int
         -- , height :: Int
         , cells :: Maybe Int
         , stator :: Maybe Int
         , rotor :: Maybe Int
         , method :: Method
         }
  deriving Show

config0 = Config { period = 3
                 , width = 9 -- , height = 9
                 , cells = Nothing
                 , stator = Nothing, rotor = Nothing
                 , method = Direct                           
                 }

config :: Parser Config
config = Config
  <$> option auto ( long "period" <> short 'p' )
  <*> option auto ( long "width" <> short 'w' )
  <*> option (Just <$> auto) ( long "cells" <> short 'c' <> value Nothing )
  <*> option (Just <$> auto) ( long "stator" <> short 's' <> value Nothing )
  <*> option (Just <$> auto) ( long "rotor" <> short 'r' <> value Nothing )
  <*> option auto ( long "method" <> short 'm' <> value Direct
                    <> metavar (show [minBound..maxBound::Method] ))

parse :: IO Config
parse = customExecParser (prefs disambiguate)
   $ info (helper <*> config) idm


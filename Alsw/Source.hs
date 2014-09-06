{-# LANGUAGE OverloadedStrings #-}
module Alsw.Source
  ( Source
  , StaticSource
  , getSource
  , getSourceName
  , getActions
  , holyquintet
  )
where

import qualified Data.Text as T
import Alsw.Action as A

class Source a where
  getSource :: a -> [T.Text]
  getSourceName :: a -> T.Text
  getActions :: a -> [Action]

data StaticSource = StaticSource { source :: [T.Text]
                                 , name :: T.Text
                                 , actions :: [Action]
                                 }

instance Source StaticSource where
  getSource ss = source ss
  getSourceName ss = name ss
  getActions ss = actions ss

holyquintet :: StaticSource
holyquintet = StaticSource { source = ["mami", "madoka", "homhom", "sayaka", "kyoko"]
                           , name = "holyquintet"
                           , actions = [A.output]
                           }


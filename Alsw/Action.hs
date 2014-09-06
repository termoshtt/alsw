module Alsw.Action
  ( Action
  , output
  )
where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.IO
import System.Exit

type Action = T.Text -> IO ()

output :: Action
output s = T.hPutStr stdout s >> exitSuccess

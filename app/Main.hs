module Main
  ( main
  ) where

import Control.Concurrent.STM (newTVarIO)
import Controller.App (App (..))
import Data.Map qualified as M
import Yesod (warp)

main :: IO ()
main = do
  aDatabase <- newTVarIO M.empty
  aCounter <- newTVarIO 0

  warp 8081 App{..}

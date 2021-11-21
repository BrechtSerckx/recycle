module Recycle
  ( main
  )
where

import           Recycle.AppM

main :: IO ()
main = do
  let env = Env { .. }
  flip runRecycle env $ pure ()

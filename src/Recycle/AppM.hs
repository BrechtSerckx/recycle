module Recycle.AppM
  ( RecycleM(..)
  , Env(..)
  , runRecycle
  )
where

import           GHC.Generics
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Trans.Reader     ( ReaderT(..) )


data Env = Env
  { clientEnv :: ClientEnv
  } deriving Generic

type InnerM = ReaderT Env IO

newtype RecycleM a = RecycleM
  { runRecycleM :: InnerM a
  } deriving newtype (Functor, Applicative, Monad, MonadIO)

runRecycle :: RecycleM a -> Env -> IO a
runRecycle act env = runRecycleM act `runReaderT` env

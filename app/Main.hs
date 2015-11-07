module Main where

import           Control.Monad.State (execStateT)

import           Options (Options, defineOptions, runCommand, simpleOption)

import           LimitedHashMap (initialState)
import           Server (runServer)

-- | Possible command-line options.
data MainOptions = MainOptions
  { optDebug :: Bool -- ^ Enable debug output
  , optPort  :: Word -- ^ Port to listen on
  }

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "debug" False
        "Enable debug output"
    <*> simpleOption "port" 11211
        "Port to listen on (default 11211)"

-- | Main entry point.
main :: IO ()
main = runCommand $ \opts args -> do
  execStateT (runServer (optDebug opts) (optPort opts)) $ initialState 100
  return ()


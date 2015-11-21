module Main where

import           Options (Options, defineOptions, runCommand, simpleOption)

import           Server (initialState, runServer)

-- | Possible command-line options.
data MainOptions = MainOptions
  { optDebug :: Bool -- ^ Enable debug output
  , optPort  :: Word -- ^ Port to listen on
  , optSize  :: Int  -- ^ Size of the hashmap
  }

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> simpleOption "debug" False
        "Enable debug output"
    <*> simpleOption "port" 11211
        "Port to listen on"
    <*> simpleOption "size" 100
        "Size of the hashmap in keys"

-- | Main entry point.
main :: IO ()
main = runCommand $ \opts args -> do
  state <- initialState (optDebug opts) (optSize opts)
  runServer state $ optPort opts


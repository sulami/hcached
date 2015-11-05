module Server (
  runServer
) where

import           Control.Monad (when)

-- | Run the server on the specified port.
runServer :: Bool -> Word -> IO ()
runServer debug port = do
  when debug . putStrLn $ "Listening on port " ++ show port


module Server (
  runServer
) where

import           Control.Monad (when)

runServer :: Bool -> Word -> IO ()
runServer debug port = do
  when debug . putStrLn $ "Listening on port " ++ show port


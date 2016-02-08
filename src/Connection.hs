module Connection where

import Network
import System.IO
import Control.Monad (liftM)
import Data.Char

newtype IrcHandle = IrcHandle { netHandle :: Handle }

createConnection :: String -> Int -> IO IrcHandle
createConnection h p = IrcHandle <$> connectTo h (PortNumber $ fromIntegral p)

writeConnection :: IrcHandle -> String -> IO ()
writeConnection h s = hPutStrLn (netHandle h) s

readConnection :: IrcHandle -> IO String
readConnection h = do
    s <- hGetLine $ netHandle h
    if firstWord (toLower `liftM` s) == "ping" then
        writeConnection h ("PONG " ++ words s !! 1) >> readConnection h
    else  return s
    where firstWord = head . words

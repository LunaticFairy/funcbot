module Main where

import Control.Monad (forever)
import Data.Text (unpack)
import Text.Printf

import Connection
import Commands
import Config
import Parser
 
main = do
    c <- readConnectionInfo 
    h <- createConnection (unpack $ address c) (port c)
    writeConnection h $ printf "NICK %s" (nickname c)
    writeConnection h $ printf "USER %s %s 8 * :%s"(nickname c) (username c) (realname c)
    forever $ do
        s <- readConnection h
        case parseIrc s of
            Left l -> return ()
            Right r -> runIrcT (handle r) h c

handle :: IrcLine -> Irc ()
handle (IrcLine _ "376" _ _) = joinCfg
handle (IrcLine _ _ _ _) = io $ return ()

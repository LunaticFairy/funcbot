module Main where

import Control.Monad (forever)

import Connection
import Commands
import Parser
 
main = do
    h <- createConnection "irc.somesite.org" 6667
    writeConnection h "NICK bot"
    writeConnection h "USER bot bot 8 * :bot"
    forever $ do
        s <- readConnection h
        case parseIrc s of
            Left l -> return ()
            Right r -> runIrcT (handle r) h

handle :: IrcLine -> Irc ()
handle (IrcLine _ "376" _ _) = join "#channel"
handle (IrcLine _ _ _ _) = io $ return ()

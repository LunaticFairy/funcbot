module Commands where

import Control.Monad.Reader
import Text.Printf
import Connection
import Parser

data IrcClient = IrcClient { conn :: IrcHandle }
type Irc = ReaderT IrcClient IO

runIrcT f st = runReaderT f $ IrcClient st

io :: IO a -> Irc a
io = liftIO

join :: String -> Irc ()
join ch = do
    h <- asks conn
    io $ writeConnection h (printf "JOIN %s" ch)

privmsg :: String -> String -> Irc ()
privmsg dest msg = do
    h <- asks conn
    io $ writeConnection h (printf "PRIVMSG %s :%s" dest msg)

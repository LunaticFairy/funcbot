module Commands where

import Control.Monad
import Control.Monad.Reader
import Data.List (intercalate)
import Data.Text (unpack)
import Text.Printf

import Connection
import Config
import Parser

data IrcClient = IrcClient { conn :: IrcHandle, connInfo :: ConnectionInfo }
type Irc = ReaderT IrcClient IO

runIrcT f st ci = runReaderT f $ IrcClient st ci

io :: IO a -> Irc a
io = liftIO

join :: String -> Irc ()
join ch = do
    h <- asks conn
    io $ writeConnection h (printf "JOIN %s" ch)

joinCfg :: Irc ()
joinCfg = do
    h <- asks conn
    c <- asks connInfo
    io $ writeConnection h $ printf "JOIN %s" $ intercalate "," (liftM unpack $ channels c)

nickservCfg :: Irc ()
nickservCfg = do
    h <- asks conn
    c <- asks connInfo
    io $ writeConnection h $ printf "NICKSERV IDENTIFY %s" $ nickserv c

autoCfg :: Irc ()
autoCfg = nickservCfg >> joinCfg

privmsg :: String -> String -> Irc ()
privmsg dest msg = do
    h <- asks conn
    io $ writeConnection h (printf "PRIVMSG %s :%s" dest msg)

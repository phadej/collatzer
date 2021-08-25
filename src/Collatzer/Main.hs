{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Collatzer.Main (main) where

import Control.Applicative       (optional, (<**>))
import Control.Monad             (void)
import Data.Foldable             (for_)
import Data.Text                 (Text)
import Network.HTTP.Types.Status (ok200)
import Numeric.Natural           (Natural)
import System.Timeout            (timeout)
import System.IO (hPutStrLn, stderr)

import qualified Data.Aeson               as A
import qualified Data.Cache.LRU.IO        as LRU
import qualified Data.Text.Read           as T.Read
import qualified GHC.Eventlog.Socket
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative      as O

trace :: String -> IO ()
trace = hPutStrLn stderr

type Ctx = LRU.AtomicLRU Natural [Natural]

handler :: Ctx -> Text -> IO Wai.Response
handler ctx t = case T.Read.decimal t of
    Left _       -> step1 1000
    Right (n, _) -> step1 n
  where
    step1 :: Natural -> IO Wai.Response
    step1 n = do
        x <- LRU.lookup n ctx
        case x of
            Nothing -> step2 n
            Just r  -> step3 r

    step2 :: Natural -> IO Wai.Response
    step2 n = do
        LRU.insert n r ctx
        step3 r
      where
        r = collatz n

    step3 :: [Natural] -> IO Wai.Response
    step3 r = return $ Wai.responseLBS
        ok200
        [("Content-Type", "application/json")]
        (A.encode r)

    collatz :: Natural -> [Natural]
    collatz 0 = []
    collatz 1 = [1]
    collatz n = n : if p == 0 then collatz m else collatz (3 * n + 1)
      where
        (m, p) = n `divMod` 2

app :: Ctx -> Wai.Application
app ctx req res = case Wai.pathInfo req of
    []              -> res =<< handler ctx "1000"
    ["endpoint", t] -> res =<< handler ctx t
    _               -> res =<< handler ctx "1000"

data Opts = Opts
    { optQuiteAfter   :: Maybe Int
    , optEventLogSock :: Maybe FilePath
    }
  deriving Show

optsP :: O.Parser Opts
optsP = do
    optQuiteAfter <- optional $ O.option O.auto $
        O.long "quite-after" <> O.metavar "SECS" <> O.help "Quit after given time"
    optEventLogSock <- optional $ O.strOption $
        O.long "eventlog-socket" <> O.metavar "PATH" <> O.help "Eventlog socket"
    pure Opts {..}

main :: IO ()
main = do
    opts <- O.execParser $ O.info (optsP <**> O.helper) $
        O.fullDesc <> O.header "collatzer - demo web app"

    for_ (optEventLogSock opts) $ \fp -> do
        trace $ "Eventlog socket " ++ fp
        GHC.Eventlog.Socket.startWait (Just fp)

    let timeout' :: IO () -> IO ()
        timeout' = case optQuiteAfter opts of
            Nothing -> id
            Just t  -> void . timeout (t * 1_000_000)

    ctx <- LRU.newAtomicLRU (Just 10)

    trace "Starting app at http://localhost:8000"
    timeout' $ Warp.run 8000 (app ctx)

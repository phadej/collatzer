{-# LANGUAGE ApplicativeDo      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Main (main) where

import Control.Applicative (optional, (<**>))
import Control.DeepSeq     (force)
import Control.Exception   (catch, evaluate)
import Data.Word           (Word64)
import System.IO           (hPutStrLn, stderr)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.List (unfoldr)
import System.IO.Unsafe (unsafePerformIO)

import qualified Control.Concurrent.Async as Async
import qualified Network.HTTP.Client      as HTTP
import qualified Options.Applicative      as O
import qualified System.Random.SplitMix   as SM
import qualified System.Clock as Clock

traceMVar :: MVar ()
traceMVar = unsafePerformIO $ newMVar ()
{-# NOINLINE traceMVar #-}

trace :: String -> IO ()
trace str = withMVar traceMVar $ \_ -> hPutStrLn stderr str

makeRequest :: HTTP.Manager -> Word64 -> IO Bool
makeRequest mgr n = do
    req <- HTTP.parseRequest ("http://localhost:8000/endpoint/" ++ show n')
    run req mgr `catch` handler
  where
    n' = clamp (100, 1_000_000) n

    run req mgr = do
        res <- HTTP.httpLbs req mgr
        _ <- evaluate $ force $ HTTP.responseBody res
        return True

    handler :: HTTP.HttpException -> IO Bool
    handler _ = return False

clamp :: Ord a => (a,a) -> a -> a
clamp (mi, ma) x
    | x < mi    = mi
    | x > ma    = ma
    | otherwise = x

data Opts = Opts
    { optQuiteAfter :: Maybe Int
    , optThreads    :: Int
    }
  deriving Show

optsP :: O.Parser Opts
optsP = do
    optQuiteAfter <- optional $ O.option O.auto $
        O.long "quit-after" <> O.metavar "SECS" <> O.help "Quit after given time"
    optThreads <- O.option O.auto $
        O.long "threads" <> O.metavar "N" <> O.help "Thread count" <> O.value 1
    pure Opts {..}

worker :: HTTP.Manager -> Maybe Int -> Clock.TimeSpec -> SM.SMGen -> IO ()
worker mgr timeoutSecs start = loop 0 where
    loop n gen = do
        now <- Clock.getTime Clock.Monotonic
        if timedOut $ Clock.sec $ Clock.diffTimeSpec now start
        then
            trace $ "Timeout, terminating worker after " ++ show n ++ " requests"
        else do
            let ~(w64, gen') = SM.bitmaskWithRejection64 1_000_000 gen
            res <- makeRequest mgr w64
            if res then loop (n + 1) gen' else
                trace $ "Got error, terminating worker after " ++ show n ++ " requests"

    timedOut n = case timeoutSecs of
        Nothing -> False
        Just m  -> n > fromIntegral m

main :: IO ()
main = do
    opts <- O.execParser $ O.info (optsP <**> O.helper) $
        O.fullDesc <> O.header "collatzer-client - generate load for collatzer"

    print opts

    mgr <- HTTP.newManager HTTP.defaultManagerSettings
    now <- Clock.getTime Clock.Monotonic
    gen <- SM.newSMGen
    Async.mapConcurrently_ (worker mgr (optQuiteAfter opts) now) (makeGens (clamp (1, 10) (optThreads opts)) gen)

makeGens :: Int -> SM.SMGen -> [SM.SMGen]
makeGens n gen 
    | n <= 0    = []
    | otherwise = let ~(g1, g2) = SM.splitSMGen gen in g1 : makeGens (n - 1) gen


{-# LANGUAGE OverloadedStrings #-}

module Trace ( withTrace
             , TraceLevel(..)
             , traceT
             , traceS
             , traceB
             , traceAndThrow
             ) where

-- Execution tracing for multiple threads into file / stdout

import System.IO
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Console.ANSI as A
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Text.Encoding as E
import qualified Data.ByteString as B
import Data.Time
import Data.List
import Data.Monoid
import Text.Printf

data TraceLevel = TLNone | TLError | TLWarn | TLInfo
                  deriving (Eq, Enum)

data TraceSettings = TraceSettings { tsFile    :: Maybe Handle
                                   , tsEchoOn  :: Bool
                                   , tsColorOn :: Bool
                                   , tsLevel   :: TraceLevel
                                   }

-- The well-known unsafePerformIO hack. It would be a bit cumbersome to always pass the trace
-- record around or always be in a reader or special trace monad
{-# NOINLINE traceSettings #-}
traceSettings :: MVar TraceSettings
traceSettings = unsafePerformIO newEmptyMVar

withTrace :: Maybe FilePath -> Bool -> Bool -> Bool -> TraceLevel -> IO () -> IO ()
withTrace traceFn echoOn appendOn colorOn level f =
    bracket
        ( do h <- case traceFn of Just fn -> if   level /= TLNone
                                             then Just <$> openFile fn (if   appendOn
                                                                        then AppendMode
                                                                        else WriteMode)
                                             else return Nothing
                                  _ -> return Nothing
             let ts = TraceSettings { tsFile = h
                                    , tsEchoOn = echoOn
                                    , tsColorOn = colorOn
                                    , tsLevel = level
                                    }
             r <- tryPutMVar traceSettings ts
             unless r $ error "Double initialization of Trace module"
             return ts
        )
        ( \ts -> do traceT TLInfo "Shutting down trace system"
                    void . takeMVar $ traceSettings
                    case tsFile ts of Just h -> hClose h
                                      _      -> return ()
                    when (tsEchoOn ts) $ hFlush stdout
        )
        $ \_ -> f

trace :: TraceLevel -> T.Text -> IO ()
trace lvl msg = void $ withMVar traceSettings $ \ts -> -- TODO: Have to take an MVar even if
                                                       --       tracing is off, speed this up
   when (lvl /= TLNone && fromEnum lvl <= fromEnum (tsLevel ts)) $ do
       tid  <- printf "%-12s" . show <$> myThreadId
       time <- printf "%-26s" . show . zonedTimeToLocalTime <$> getZonedTime
       let lvlDesc color = (if color then concat else (!! 1)) $ case lvl of
               TLError -> [ mkANSICol A.Red   , "ERROR", reset ]
               TLWarn  -> [ mkANSICol A.Yellow, "WARN ", reset ]
               TLInfo  -> [ mkANSICol A.White , "INFO ", reset ]
               _       -> replicate 3 ""
           reset = A.setSGRCode []
           mkANSICol c = A.setSGRCode [ A.SetColor A.Foreground A.Vivid c ]
           header color = intercalate " | " [ lvlDesc color, tid, time ]
           handles = case tsFile ts of Just h -> [h]; _ -> []; ++ [stdout | tsEchoOn ts]
           oneLine = not (T.any (== '\n') msg) && T.length msg < 80
       forM_ handles $ \h -> do
           closed <- hIsClosed h
           hs     <- hShow h
           -- Use ANSI colors when outputting to the terminal
           color <- (&&) (tsColorOn ts) <$> hIsTerminalDevice h
           if   closed
           then TI.putStrLn $ "ERROR: Trace message lost, called trace after shutdown: " <> msg
                              <> "\n" <> T.pack hs <> "\n" <> T.pack (show h)
           else -- Display short, unbroken messages in a single line without padding newline
                if   oneLine
                then TI.hPutStrLn h $ T.pack (header color) <> " - " <> msg
                else do hPutStrLn h $ header color
                        TI.hPutStrLn h msg
                        hPutStrLn h ""

traceT :: TraceLevel -> T.Text -> IO ()
traceT = trace

traceS :: TraceLevel -> String -> IO ()
traceS lvl msg = trace lvl (T.pack msg)

traceB :: TraceLevel -> B.ByteString -> IO ()
traceB lvl msg = trace lvl (E.decodeUtf8 msg)

traceAndThrow :: String -> IO a
traceAndThrow err = traceS TLError err >> (throwIO $ ErrorCall err)


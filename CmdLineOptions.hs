
module CmdLineOptions ( Flag(..)
                      , parseCmdLineOpt
                      , defPort
                      , defPollInterval
                      ) where

import System.Console.GetOpt
import System.Environment
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid

-- Command line flag / option parsing, defaults

data Flag = FlagHelp
          | FlagTraceLevel String
          | FlagTraceFile String
          | FlagTraceAppend
          | FlagTraceNoEcho
          | FlagTraceDisableColor
          | FlagPort String
          | FlagLocalhost
          | FlagTraceHTTP
          | FlagPollInterval String
            deriving (Eq, Show)

defPort, defPollInterval :: Int
defPort = 8001
defPollInterval = 1

parseCmdLineOpt :: MonadIO m => m [Flag]
parseCmdLineOpt = liftIO $ do
    name <- getProgName
    args <- getArgs
    let header = "Usage: " <> name <> " [OPTION...]"
        usage  = usageInfo header options
    flags <- case getOpt Permute options args of
                 (f , [], [] ) -> return f
                 (_ , _ , err) -> do
                     void . throwIO . userError $ "\n" <> concat err <> usage
                     return []
    when (FlagHelp `elem` flags) $ do
        putStrLn usage
        throwIO . userError $ "Done, exiting"
    return flags
  where
    options :: [OptDescr Flag]
    options = [ Option ['p']
                       ["port"]
                       (ReqArg FlagPort "PORT")
                       ("network port (default: " <> show defPort <> ")")
              , Option []
                       ["localhost"]
                       (NoArg FlagLocalhost)
                       ("only bind to localhost")
              , Option ['i']
                       ["poll-interval"]
                       (ReqArg FlagPollInterval "SECONDS")
                       ("bridge poll interval (default: " <> show defPollInterval <> ")")
              , Option ['t']
                       ["trace-level"]
                       (ReqArg FlagTraceLevel "LEVEL")
                       (  "execution trace level (default: i)\n"
                       <> "  n = none\n"
                       <> "  e = errors only\n"
                       <> "  w = warnings and errors\n"
                       <> "  i = infos, warnings and errors"
                       )
              , Option []
                       ["trace-file"]
                       (ReqArg FlagTraceFile "FILE")
                       ("output file for execution trace (default: none)")
              , Option []
                       ["trace-no-color"]
                       (NoArg FlagTraceDisableColor)
                       "no ANSI colors for trace output"
              , Option ['e']
                       ["trace-no-echo"]
                       (NoArg FlagTraceNoEcho)
                       "disable echo execution trace to stdout"
              , Option []
                       ["trace-append"]
                       (NoArg FlagTraceAppend)
                       "append execution trace file instead of overwriting"
              , Option []
                       ["trace-http"]
                       (NoArg FlagTraceHTTP)
                       ("trace web server events")
              , Option ['h']
                       ["help"]
                       (NoArg FlagHelp)
                       "print usage information"
              ]


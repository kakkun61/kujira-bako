{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}

import Paths_kujira_bako (getDataDir)

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Char              (toLower)
import           Data.Default.Class     (Default (def))
import           Data.Semigroup         (Last (Last, getLast))
import           Data.Traversable       (for)
import           Main.Utf8              (withUtf8)
import qualified Options.Declarative    as Options
import           System.Directory       (copyFile, createDirectoryIfMissing)
import           System.Exit            (exitFailure)
import           System.FilePath        ((</>))
import           System.IO              (hFlush, stdout)
import qualified System.Process         as Process

data Option =
  Option
    { target  :: Target
    , volumes :: [Volume]
    }
  deriving (Show, Read, Eq, Ord)

instance Default Option where
  def = Option Windows []

data Target = Linux (Maybe Distribution) | Windows deriving (Show, Read, Eq, Ord)

type Distribution = String

data Volume = Volume { source :: FilePath, destination :: FilePath } deriving (Show, Read, Eq, Ord)

class OptionRepresentation a where
  decodeOpt :: String -> Maybe a
  encodeOpt :: a -> String

instance OptionRepresentation Volume where
  decodeOpt s =
    case span (/= ':') s of
      ([drive], ':':rest) ->
        case span (/= ':') rest of
          ([], _) -> Nothing
          (src', ':':dest) ->
            let
              src = drive:':':src'
            in
              Just $ Volume src dest
          (_, []) -> Nothing
      _ -> Nothing

  encodeOpt Volume { source, destination } = source ++ ":" ++ destination

instance Options.ArgRead Volume where
  argRead ss =
    getLast <$> (mconcat $ (Last <$>) . decodeOpt <$> ss)

logVerbose :: String -> Options.Cmd n ()
logVerbose = Options.logStr 1

logError   :: String -> Options.Cmd n ()
logError = Options.logStr 0

main :: IO ()
main =
  withUtf8 $ Options.run_ main'

main'
  :: Options.Flag "l" '["linux"] "" "Use Linux container" Bool
  -> Options.Flag "w" '["windows"] "" "Use Windows container" Bool
  -> Options.Flag "d" '["distribution"] "DISTRIBUTION" "Select a distribution on WSL 2" (Maybe String)
  -> Options.Flag "s" '["setup"] "" "Setup Docker for 鯨箱" Bool
  -> Options.Flag "V" '["volume"] "VOLUME" "Mount a volume" ([Volume])
  -> Options.Arg "ARGUMENTS" [String]
  -> Options.Cmd "鯨箱" ()
main' linuxFlag windowsFlag distributionFlag setupFlag volumesFlag args = do
  option <-
    case (Options.get linuxFlag, Options.get windowsFlag, Options.get distributionFlag) of
      (True, False, Just distribution) -> pure def { target = Linux (Just distribution) }
      (True, False, Nothing)           -> pure def { target = Linux Nothing }
      (False, True, Just _)            -> logVerbose "--distribution (-d) is ignored" >> pure def { target = Windows }
      (False, True, Nothing)           -> pure def { target = Windows }
      _                                -> logError "One of \"--linux\" and \"--windows\" must be selected." >> liftIO exitFailure
  if Options.get setupFlag
    then setup option
    else run option { volumes = Options.get volumesFlag } $ Options.get args

linuxHost :: String
linuxHost = "tcp://127.0.0.1:9266"

run :: Option -> [String] -> Options.Cmd n ()
run Option { target, volumes } (cmd:args) = do
  volumes <-
    case target of
      Linux _ ->
        for volumes $ \v@Volume { source } -> do
          source' <- wslpath source
          pure $ v { source = source' }
      _ -> pure volumes
  let
    volumeOpts = mconcat $ (\v -> ["--volume", encodeOpt v]) <$> volumes
    args' = cmd:volumeOpts ++ args
  case target of
    Linux _ -> callProcess "docker" ("--host":linuxHost:args')
    Windows -> callProcess "docker" args'

setup :: Option -> Options.Cmd n ()
setup Option { target = Linux (Just dist) } = do
  hash <- (toLower <$>) . init <$> readProcess "powershell" ["-Command", "& { (Get-FileHash " ++ confDirPath </> confName ++ ").Hash }"] "" -- init - remove a trailing newline character
  logVerbose $ "SHA256 hash: " ++ hash
  if hash == "6384c05915f20d4f6eaccddf26bbbe17baa44687132ad905c5ca5f3a02a402e5"
    then
      next
    else do
      answer <-
        liftIO $ do
          putStrLn "System V init configuration for Docker has been changed"
          putStr "Overwrite? [y/N]: "
          hFlush stdout
          getLine
      if "y" == (toLower <$> answer)
        then next
        else liftIO $ putStrLn $ "Edit \"" ++ wslConfDirPath ++ "/" ++ confName ++ "\" manually, add \"--host " ++ linuxHost ++ "\" to DOCKER_OPTS"
  where
    wslRoot = "\\\\wsl$\\" </> dist
    tmpDirPath = wslRoot </> "tmp\\kb"
    confDirPath = wslRoot </> "etc\\default"
    wslTmpDirPath = "/tmp/kb"
    wslConfDirPath = "/etc/default"
    confName = "docker"
    next = do
      liftIO $ createDirectoryIfMissing True tmpDirPath
      logVerbose $ "created directory: " ++ tmpDirPath

      dataDir <- liftIO getDataDir
      let src = dataDir </> "res\\etc\\default" </> confName
      liftIO $ copyFile src $ tmpDirPath </> confName
      logVerbose $ "copied configuration file: " ++ src ++ ", " ++ (tmpDirPath </> confName)

      callCommand $ "wsl --user root cp -f " ++ wslTmpDirPath ++ "/" ++ confName ++ " " ++ wslConfDirPath

      callCommand "wsl --user root service docker restart"

callProcess :: String -> [String] -> Options.Cmd n ()
callProcess cmd args = do
  liftIO $ Process.callProcess cmd args
  logVerbose $ "invoked: " ++ cmd ++ " " ++ unwords args

callCommand :: String -> Options.Cmd n ()
callCommand cmd = do
  liftIO $ Process.callCommand cmd
  logVerbose $ "invoked: " ++ cmd

readProcess :: String -> [String] -> String -> Options.Cmd n String
readProcess cmd args input = do
  output <- liftIO $ Process.readProcess cmd args input
  logVerbose $ "invoked: " ++ cmd ++ " " ++ unwords args ++ "\n\t" ++ output
  pure output

wslpath :: FilePath -> Options.Cmd n FilePath
wslpath winPath = init <$> readProcess "wsl" ["wslpath", winPath] "" -- drop the last "line feed" character

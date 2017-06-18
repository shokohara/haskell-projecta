{-# LANGUAGE OverloadedStrings #-}

module App where

import Data.Char
import Data.String.Here
import Control.Lens ((&), (.~), (<&>), (?~), (^.), (^..), (^?))
import Data.Text hiding (minimum, concat)
import Network.Google
import Network.Google.Compute
import Network.Google.Compute.Types
import Network.Google.PubSub
import Network.Google.PubSub.Types
import Network.Google.Resource.Compute.Instances.Insert
import Network.Google.Resource.PubSub.Projects.Subscriptions.Pull
import System.IO (stdout, hGetContents)
import qualified Data.Text as T
import qualified Network.Google as Google
import qualified Network.Google.Compute as Compute
import qualified Network.Google.PubSub as PubSub
import Data.Aeson.Encode.Pretty hiding (Config)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe
import GHC.Generics
import System.Process
import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:))
import Config
import qualified Config as C
import System.Exit

run :: IO ()
run = do
  mConfig <- run2
  case mConfig of
    Just config -> do
      run3 config
      run1 config
    Nothing -> die "error"

run3 config = do
  (_, Just hout, _, _) <- createProcess (proc "mkdir" ["-p", T.unpack $ directory config]) { cwd = Just ".", std_out = CreatePipe }
  b <- hGetContents hout
  putStrLn b
  (_, Just hout, _, _) <- createProcess (proc "ls" []) { cwd = Just $ T.unpack $ C.directory config, std_out = CreatePipe }
  a <- hGetContents hout
  putStrLn a
  (_, Just hout, _, _) <- createProcess (proc "ls" []) { cwd = Just ".", std_out = CreatePipe }
  c <- hGetContents hout
  putStrLn c

run1 :: Config -> IO ()
run1 config = do
  putStrLn "start"
  lgr <- Google.newLogger Google.Debug stdout
  env <- Google.newEnv <&> (Google.envLogger .~ lgr) . (Google.envScopes .~ PubSub.pubSubScope)
  r <- runResourceT . Google.runGoogle env $ Google.send $ projectsSubscriptionsPull (pullRequest & prMaxMessages ?~ 1) $ C.subscription config
  BS.putStrLn $ encodePretty (catMaybes (flip (^.) pmAttributes <$> catMaybes (flip (^.) rmMessage <$> concat (r ^.. prReceivedMessages))))
  return ()

run2 :: IO (Maybe Config)
run2 = Y.decodeFile "config.yaml" :: IO (Maybe Config)


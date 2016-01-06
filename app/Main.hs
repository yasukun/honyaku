{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Lib
import qualified Util as U
import System.Directory
import System.FilePath
import System.IO
import System.Console.CmdArgs

cryptpw :: String
cryptpw = "cryptpw"

data GTran = GTran {
      source :: String
    , target :: String
    , query :: String
    } deriving (Show, Data, Typeable)

gtran = GTran
        {
          source = "en" &= help "source language"
        , target = "ja" &= help "target language"
        , query = def &= help "source text"
        }

saveApiKey :: FilePath -> IO ()
saveApiKey path = do
  putStrLn "api key?"
  apikey <- getLine
  let ret = U.encrypt cryptpw apikey
  createDirectory $ fst $ splitFileName path
  writeFile path ret
  putStrLn ("crypt api key ["  ++ ret ++ " ] save to " ++ path)

loadApiKey :: FilePath -> IO String
loadApiKey path = U.decrypt cryptpw <$> readFile path

main :: IO ()
main = do
  opts <- cmdArgs gtran
  userdir <- getUserDocumentsDirectory
  let datapath = userdir </> ".honyaku" </> "apikey"
  isfile <- doesFileExist datapath
  if isfile
  then do
    apikey <- loadApiKey datapath
    tran apikey (query opts) (source opts) (target opts)
  else saveApiKey datapath

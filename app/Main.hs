module Main where

import Lib
import qualified Util as U
import System.Directory
import System.FilePath
import System.IO

cryptpw :: String
cryptpw = "cryptpw"

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
  userdir <- getUserDocumentsDirectory
  let datapath = userdir </> ".honyaku" </> "apikey"
  isfile <- doesFileExist datapath
  if isfile
  then tran =<< loadApiKey datapath
  else saveApiKey datapath

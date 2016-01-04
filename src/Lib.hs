module Lib
    ( someFunc
    , tran
    ) where

import Control.Monad
import qualified Data.Text.IO as T
import Data.Text
import Data.Maybe
import Web.Google.Translate

someFunc = putStr "someFunc"

tran apikey = do
  Right TranslationResponse { translations = xs } <-
      translate (Key (pack apikey)) (Just srcLang) trgLang (Body (pack "Hello"))
  forM_ xs $ \Translation { translatedText = TranslatedText txt } ->
       T.putStrLn txt
  where
    srcLang = Source English
    trgLang = Target Japanese

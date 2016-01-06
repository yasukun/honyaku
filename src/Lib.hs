module Lib
    ( tran
    , printLangs
    ) where

import Control.Monad
import qualified Data.Text.IO as T
import qualified Data.List as L
import Data.Text
import Data.Maybe
import Web.Google.Translate

tran apikey query source target = do
  Right TranslationResponse { translations = xs } <-
      translate (Key (pack apikey)) (Just srcLang) trgLang (Body (pack query))
  forM_ xs $ \Translation { translatedText = TranslatedText txt } ->
       T.putStrLn txt
  where
    srcLang = Source $ langFromS source
    trgLang = Target $ langFromS target

langFromS s = L.head $ L.filter (\x -> show x == s) langs

printLangs = do
  forM_ langs $ \x ->
      print x

langs = [  Afrikaans
   , Albanian
   , Arabic
   , Armenian
   , Azerbaijani
   , Basque
   , Belarusian
   , Bengali
   , Bosnian
   , Bulgarian
   , Catalan
   , Cebuano
   , Chichewa
   , ChineseSimplified
   , ChineseTraditional
   , Croatian
   , Czech
   , Danish
   , Dutch
   , English
   , Esperanto
   , Estonian
   , Filipino
   , Finnish
   , French
   , Galician
   , Georgian
   , German
   , Greek
   , Gujarati
   , HaitianCreole
   , Hausa
   , Hebrew
   , Hindi
   , Hmong
   , Hungarian
   , Icelandic
   , Igbo
   , Indonesian
   , Irish
   , Italian
   , Japanese
   , Javanese
   , Kannada
   , Kazakh
   , Khmer
   , Korean
   , Lao
   , Latin
   , Latvian
   , Lithuanian
   , Macedonian
   , Malagasy
   , Malay
   , Malayalam
   , Maltese
   , Maori
   , Marathi
   , Mongolian
   , MyanmarBurmese
   , Nepali
   , Norwegian
   , Persian
   , Polish
   , Portuguese
   , Punjabi
   , Romanian
   , Russian
   , Serbian
   , Sesotho
   , Sinhala
   , Slovak
   , Slovenian
   , Somali
   , Spanish
   , Sundanese
   , Swahili
   , Swedish
   , Tajik
   , Tamil
   , Telugu
   , Thai
   , Turkish
   , Ukrainian
   , Urdu
   , Uzbek
   , Vietnamese
   , Welsh
   , Yiddish
   , Yoruba
   , Zulu]

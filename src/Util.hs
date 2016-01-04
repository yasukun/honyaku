module Util (
             encrypt
           , decrypt
            ) where

import Data.Word
import Data.Bits
import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Codec.Encryption.DES as DES

type BlockWord = [[Word8]]

_W8_MASK :: Word64
_W8_MASK = 0xFF

unpackWord8 :: [Word8] -> String
unpackWord8 = BC.unpack . B.pack

packWord8 :: String -> [Word8]
packWord8 = B.unpack . BC.pack

packWord64 :: [Word8] -> Word64
packWord64 xs = if 8 < length xs then error "too long words."
                else bitShift 0 $ map fromIntegral xs
    where
      bitShift accum [] = accum
      bitShift accum (x:xs) = bitShift ((shiftL accum 8) .|. x) xs

fromWord64To8 :: Word64 -> [Word8]
fromWord64To8 w64 = dropWhile ((==) 0) $ reverse $ map go [0..7]
    where go i = fromIntegral $ (shiftR w64 (8*i)) .&. _W8_MASK

blockWord8sIn64 :: [Word8] -> BlockWord
blockWord8sIn64 = unfoldr go
    where
      go [] = Nothing
      go xs = Just (splitAt 8 xs)

encrypt :: String -> String -> String
encrypt = des DES.encrypt

decrypt :: String -> String -> String
decrypt = des DES.decrypt

des :: (Word64 -> Word64 -> Word64) -> String -> String -> String
des engine pw target =
    if 8 < length pw then error "password too long."
    else concat
             $ map unpackWord8
             $ map fromWord64To8
             $ map (engine pwW64) targetW64s
    where
      pwW64 = packWord64 . packWord8 $ pw
      targetW64s = map packWord64 $ blockWord8sIn64 $ packWord8 target

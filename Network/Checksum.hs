module Network.Checksum (inChecksum) where

import Data.Bits (complement, shiftL, shiftR, (.|.), (.&.))
import Data.Word (Word8, Word16, Word32)

inChecksum :: [Word8] -> Word16
inChecksum ws = fromIntegral . complement . addCarry . addHi16ToLow16 $ ws'
  where
    ws' = sum . map fromIntegral . packWord16 $ ws

packWord16 :: [Word8] -> [Word16]
packWord16 []         = []
packWord16 [w0]       = packWord16 [w0,0]
packWord16 (w0:w1:ws) = word8ToWord16 w0 w1 : packWord16 ws

word8ToWord16 :: Word8 -> Word8 -> Word16
word8ToWord16 high low = high' .|. low'
  where
    high' = (fromIntegral high) `shiftL` 8
    low'  = fromIntegral low

addHi16ToLow16 :: Word32 -> Word32
addHi16ToLow16 n = (n `shiftR` 16) + (n .&. 0xffff)

addCarry :: Word32 -> Word32
addCarry n = (n `shiftR` 16) + n

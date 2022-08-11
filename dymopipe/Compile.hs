{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
module Compile (compile) where

import Options
import Data.Singletons.Sigma
import qualified Data.Vinyl.ARec as V
import qualified Data.Vinyl as V
import qualified Control.Lens as L
import Control.Lens.Operators
import Data.Bits
import qualified Mason.Builder as M
import qualified Graphics.Image as G
import Data.Foldable (foldr1)

type CommonCompileOptions = V.ARec Attr CommonCompileOptionsF

compile :: SomeCompileOptions -> G.Image G.VS G.X G.Bit -> M.Builder
compile (SLabel :&: o) inp = labelSpecific o inp <> common (V.rcast o) inp
compile (STape :&: o) inp = tapeSpecific o inp <> common (V.rcast o) inp

labelSpecific :: Options (Compile Label) -> G.Image G.VS G.X G.Bit -> M.Builder
labelSpecific o _ = mconcat [
    if (o ^. #hiDpi)
      then M.word8 0x1b <> M.word8 0x69
      else M.word8 0x1b <> M.word8 0x68
  , M.word8 0x1b <> (o ^. #printDensity . L.to d)
  ]

  where d Light  = M.char8 'c'
        d Medium = M.char8 'd'
        d Normal = M.char8 'e'
        d Dark   = M.char8 'g'

tapeSpecific :: Options (Compile Tape) -> G.Image G.VS G.X G.Bit -> M.Builder
tapeSpecific o _ = mconcat [
    M.word8 0x1b <> M.word8 0x43 <> (o ^. #tapeType . L.to t)
  ]

  where t BlackOnWhite            = M.word8  0
        t BlackOnBlue             = M.word8  1
        t BlackOnRed              = M.word8  2
        t BlackOnSilver           = M.word8  3
        t BlackOnYellow           = M.word8  4
        t BlackOnGold             = M.word8  5
        t BlackOnGreen            = M.word8  6
        t BlackOnFluorescentGreen = M.word8  7
        t BlackOnFluorescentRed   = M.word8  8
        t WhiteOnClear            = M.word8  9
        t WhiteOnBlack            = M.word8 10
        t BlueOnWhite             = M.word8 11
        t RedOnWhite              = M.word8 12

common :: CommonCompileOptions -> G.Image G.VS G.X G.Bit -> M.Builder
common o img = mconcat $ [
    M.word8 0x1b <> M.word8 0x4c <> (o ^. #length . L.to l)
  , M.word8 0x1b <> M.word8 0x44 <> (o ^. #width . L.to w)
  ] ++ pixels

  where
    l (Length x) = M.word16BE x
    l Continuous = M.word16BE 0xffff

    w (Width x) = M.word8 (fromIntegral $ (x + 7) `shiftR` 3)

    pixels :: M.Buildable s => [M.BuilderFor s]
    pixels = flip map [0.. G.rows img - 1] $ \r ->
      M.word8 0x16 <> encodeRow r

    encodeRow :: Int -> M.Builder
    encodeRow r = mconcat $ map (M.word8 . encode8 r) [0,8.. G.cols img-1]

    encode8 :: Int -> Int -> Word8
    encode8 r c = foldr1 (.|.)
      [ pixelAt r (c+i) `shiftL` (7-i) | i <- [0 .. 7] ]

    pixelAt :: Int -> Int -> Word8
    pixelAt r c = coerce @_ @Word8 $ G.defaultIndex G.off img (r,c)

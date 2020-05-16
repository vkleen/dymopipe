module Options.Default ( defaultCommonArgs
                       , defaultSendArgs
                       , defaultSendLabelArgs
                       , defaultSendTapeArgs
                       , defaultCompileArgs
                       , defaultCompileLabelArgs
                       , defaultCompileTapeArgs
                       , idVendorDYMO, idProductDYMOLabelWriter450DUO
                       ) where

import Data.Vinyl (Rec(..))
import qualified Data.Vinyl as V
import Data.Singletons.Prelude hiding (Last)
import {-# SOURCE #-} Options

idVendorDYMO :: Word16
idVendorDYMO = 0x0922

idProductDYMOLabelWriter450DUO :: Word16
idProductDYMOLabelWriter450DUO = 0x0023

defaultCommonArgs :: V.Rec Attr CommonOptionsF
defaultCommonArgs = V.rcast $
     #verbose =: False
  :& #vendorId =: idVendorDYMO
  :& #productId =: idProductDYMOLabelWriter450DUO
  :& #input =: Stdin
  :& V.RNil

defaultSendArgs :: V.Rec Attr CommonSendOptionsF
defaultSendArgs = V.rcast $
     #reset =: True
  :& V.RNil

defaultSendLabelArgs :: V.Rec Attr SendLabelOptionsF
defaultSendLabelArgs = V.rcast $
     #formFeed =: Long
  :& V.RNil

defaultSendTapeArgs :: V.Rec Attr SendTapeOptionsF
defaultSendTapeArgs = V.rcast $
     #cut =: True
  :& V.RNil

defaultCompileArgs :: V.Rec Attr CommonCompileOptionsF
defaultCompileArgs = V.rcast $
     #width =: 672
  :& #length =: Continuous
  :& V.RNil

defaultCompileLabelArgs :: V.Rec Attr LabelCompileOptionsF
defaultCompileLabelArgs = V.rcast $
     #hiDpi =: False
  :& #printDensity =: Normal
  :& V.RNil

defaultCompileTapeArgs :: V.Rec Attr ('["width"] ++ TapeCompileOptionsF)
defaultCompileTapeArgs = V.rcast $
     #tapeType =: BlackOnWhite
  :& #width =: 80
  :& V.RNil



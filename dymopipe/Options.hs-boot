{-# language StrictData #-}
module Options ( CommonOptionsF, CommonSendOptionsF
               , SendLabelOptionsF, SendTapeOptionsF
               , CommonCompileOptionsF, TapeCompileOptionsF, LabelCompileOptionsF
               , Attr, FormFeed(..), TapeType(..), Density(..), Length(..), Width(..), Input(..)
               , (=:)
               ) where

import GHC.TypeLits

type family ElF (f :: Symbol) :: Type where
  ElF "verbose" = Bool
  ElF "reset" = Bool
  ElF "vendorId" = Word16
  ElF "productId" = Word16
  ElF "width" = Width
  ElF "length" = Length
  ElF "cut" = Bool
  ElF "hiDpi" = Bool
  ElF "printDensity" = Density
  ElF "tapeType" = TapeType
  ElF "formFeed" = FormFeed
  ElF "input" = Input

newtype Attr f = Attr { _unAttr :: ElF f }

type CommonOptionsF = '[ "verbose", "vendorId", "productId", "input" ]
type CommonSendOptionsF = '[ "reset" ]
type SendLabelOptionsF = '[ "formFeed" ]
type SendTapeOptionsF = '[ "cut" ]
type CommonCompileOptionsF = '[ "width", "length" ]
type LabelCompileOptionsF = '[ "hiDpi", "printDensity" ]
type TapeCompileOptionsF = '[ "tapeType" ]

data FieldLabel (f :: Symbol)
instance (s ~ f) => IsLabel s (FieldLabel f)
(=:) :: FieldLabel f -> ElF f -> Attr f

data Width = FromImage | Width Word16

data FormFeed = Short | Long | None

data TapeType = BlackOnWhite
              | BlackOnBlue
              | BlackOnRed
              | BlackOnSilver
              | BlackOnYellow
              | BlackOnGold
              | BlackOnGreen
              | BlackOnFluorescentGreen
              | BlackOnFluorescentRed
              | WhiteOnClear
              | WhiteOnBlack
              | BlueOnWhite
              | RedOnWhite

data Density = Light | Medium | Normal | Dark

data Length = Continuous | LFromImage | Length Word16

data Input = Path ByteString | Stdin

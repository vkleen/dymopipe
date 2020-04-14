{-# language UndecidableInstances #-}

module Options.ReadEnum ( ReadEnum(..)
                , WithOverrides(..)
                , LispCasePretty(..)
                , lispCase
                ) where

import Options.Applicative hiding (command)
import Control.Lens (ix)
import Control.Lens.Operators
import GHC.Generics
import GHC.TypeLits hiding (Mod)
import Data.Type.Map
import Data.Char

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), empty)

lispCase :: String -> String
lispCase = dropWhile (== '-') . (>>= lower) . dropWhile (== '_')
  where lower c | isUpper c = ['-', toLower c]
                | otherwise = [c]

class ReadEnum a where
  readEnum :: ReadM a
  readEnumFailure :: String -> ReadM a

  default readEnum :: (Generic a, GReadEnum '[] (Rep a)) => ReadM a
  readEnum = to <$> greadEnum @'[]

  default readEnumFailure :: (GReadEnum '[] (Rep a)) => String -> ReadM a
  readEnumFailure = greadEnumFailure @'[] @(Rep a)

class GReadEnum (ovr :: [Mapping Symbol Symbol]) f where
  genumPossibilities :: NonEmpty String
  gpretty :: f p -> Doc
  greadEnum :: ReadM (f p)
  greadEnumFailure :: String -> ReadM g

class GApplyOverride (ovr :: [Mapping Symbol Symbol]) (n :: Symbol) where
  gapplyOverride :: String

type family ProcessLookup n r where
  ProcessLookup n 'Nothing = n
  ProcessLookup n ('Just n') = n'

instance KnownSymbol (ProcessLookup n (ovr `Lookup` n)) => GApplyOverride ovr n where
  gapplyOverride = symbolVal (Proxy @(ProcessLookup n (ovr `Lookup` n)))

instance (KnownSymbol n, KnownSymbol (ProcessLookup n (ovr `Lookup` n))) => GReadEnum ovr (C1 ('MetaCons n f r) U1) where
  genumPossibilities = pure . lispCase $ gapplyOverride @ovr @n
  gpretty (M1 U1) = text . lispCase $ gapplyOverride @ovr @n

  greadEnum = str >>= (const empty & ix name .~ pure (M1 U1))
    where name = fromString . head $ genumPossibilities @ovr @(C1 ('MetaCons n f r) U1)
  greadEnumFailure _ = empty

instance (GReadEnum ovr f, GReadEnum ovr g) => GReadEnum ovr (f :+: g) where
  genumPossibilities = genumPossibilities @ovr @f <> genumPossibilities @ovr @g
  gpretty (L1 x) = gpretty @ovr x
  gpretty (R1 x) = gpretty @ovr x
  greadEnum = (L1 <$> greadEnum @ovr @f) <|> (R1 <$> greadEnum @ovr @g)
  greadEnumFailure _ = empty

instance GReadEnum ovr f => GReadEnum ovr (D1 d f) where
  genumPossibilities = genumPossibilities @ovr @f
  gpretty (M1 x) = gpretty @ovr x
  greadEnum = M1 <$> greadEnum @ovr @f
    <|> (str >>= (const empty & ix "?" .~ greadEnumFailure @ovr @(D1 d f) "Possible values are: "))
  greadEnumFailure n = readerAbort . InfoMsg . showWidth 80 $ text n </>
    (align . fillSep . punctuate comma . toList . fmap text $ (genumPossibilities @ovr @f))
    where
      showWidth w = flip displayS "" . renderSmart 1 w

newtype WithOverrides (a :: Type) (ovr :: [Mapping Symbol Symbol]) = WithOverrides { forgetOverrides :: a }

instance (Generic a, GReadEnum ovr (Rep a)) => ReadEnum (a `WithOverrides` ovr) where
  readEnum = WithOverrides . to <$> greadEnum @ovr
  readEnumFailure = greadEnumFailure @ovr @(Rep a)

instance (Generic a, GReadEnum ovr (Rep a)) => Pretty (a `WithOverrides` ovr) where
  pretty = gpretty @ovr . from . forgetOverrides

newtype LispCasePretty a = LispCasePretty a
  deriving Show via a
instance Show a => Pretty (LispCasePretty a) where
  pretty = text . lispCase . show

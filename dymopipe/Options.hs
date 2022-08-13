{-# language StrictData #-}
{-# language TemplateHaskell #-}
{-# language UndecidableInstances #-}
{-# language FunctionalDependencies #-}
{-# language StandaloneKindSignatures #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors
                -Wno-unused-top-binds
                -Wno-redundant-constraints
                -Wno-orphans
                -Wno-name-shadowing
#-}

module Options ( getOptions
               , validateOptions
               , Options
               , SomeOptions
               , SomeCompileOptions, SomeSendOptions
               , OptionsF
               , CommonOptionsF, CommonSendOptionsF
               , SendLabelOptionsF, SendTapeOptionsF
               , CommonCompileOptionsF, TapeCompileOptionsF, LabelCompileOptionsF
               , Target(..)
               , Command(..)
               , Density(..)
               , TapeType(..)
               , Width(..)
               , Length(..)
               , FormFeed(..)
               , Input(..)
               , STarget(..), SCommand(..)
               , Attr(..), PrettyAttr(..)
               , PrettyHexWord16(..), LispCasePretty(..)
               , (=:)
               ) where

import Prelude hiding (length, show, getArgs)
import Data.Maybe (fromJust)
import Data.Vinyl (Rec(..))
import qualified Data.Vinyl.TypeLevel as V
import qualified Data.Vinyl.Functor as V
import qualified Data.Vinyl.ARec as V
import qualified Data.Vinyl as V
import Prelude.Singletons hiding (Last, Length)
import Data.Singletons.Sigma
import Data.Singletons.TH (genSingletons, genDefunSymbols, showSingInstances)
import Data.Type.Set hiding (Proxy)
import qualified Control.Lens as L
import GHC.Show
import GHC.TypeLits
import Control.Lens.Operators ((&~), (.=), (^.))
import Data.Generics.Labels ()
import Data.Generics.Product.Fields
import Options.Applicative hiding (command)
import qualified Options.Applicative as O
import qualified Data.Attoparsec.Text as A
import Data.Typeable (typeOf)
import Data.Monoid.Generic
import Control.Applicative.Lift
import Data.Char
import Options.ReadEnum
import Text.Printf
import Data.Foldable (foldr1)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), empty, bool, width)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Text.PrettyPrint.ANSI.Leijen.Internal as P
import System.Exit (ExitCode(..))
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn)

import Options.Default

data Target = Label | Tape
  deriving stock (Show, Generic)
  deriving anyclass ReadEnum

data Command = Send Target
             | Compile Target
  deriving stock (Show, Generic)
genSingletons [ ''Command, ''Target ]
showSingInstances [ ''Command, ''Target ]

data Density = Light | Medium | Normal | Dark
  deriving stock (Show, Generic)
  deriving anyclass ReadEnum
  deriving Pretty via LispCasePretty Density

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
  deriving stock (Show, Generic)
  deriving anyclass ReadEnum
  deriving Pretty via LispCasePretty TapeType

data FormFeed = Short | Long | None
  deriving stock (Show, Generic)
  deriving anyclass ReadEnum
  deriving Pretty via LispCasePretty FormFeed

data Input = Path ByteString | Stdin
  deriving (Show, Generic)
  deriving Pretty via LispCasePretty Input

instance IsString Input where
  fromString "-" = Stdin
  fromString x = Path $ fromString x

data Width = FromImage | Width Word16
  deriving stock (Show)
  --deriving (Enum, Num, Ord, Eq, Real, Integral) via Word16

data Length = Continuous | LFromImage | Length Word16
  deriving stock (Show)

--instance Bounded Width where
--  minBound = Width 1
--  maxBound = Width 672

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
L.makeLenses ''Attr
instance (KnownSymbol f, Show (ElF f)) => Show (Attr f) where
  show (Attr x) = symbolVal @f Proxy ++ ": " ++ show x

type CommonOptionsF = '[ "verbose", "vendorId", "productId", "input" ]
type CommonSendOptionsF = '[ "reset" ]
type SendLabelOptionsF = '[ "formFeed" ]
type SendTapeOptionsF = '[ "cut" ]
type CommonCompileOptionsF = '[ "width", "length" ]
type LabelCompileOptionsF = '[ "hiDpi", "printDensity" ]
type TapeCompileOptionsF = '[ "tapeType" ]

type family OptionsF (cmd :: Command) :: [ Symbol ] where
  OptionsF (Send Label) = CommonOptionsF ++ CommonSendOptionsF ++ SendLabelOptionsF
  OptionsF (Send Tape) = CommonOptionsF ++ CommonSendOptionsF ++ SendTapeOptionsF
  OptionsF (Compile Label) = CommonOptionsF ++ CommonCompileOptionsF ++ LabelCompileOptionsF
  OptionsF (Compile Tape) = CommonOptionsF ++ CommonCompileOptionsF ++ TapeCompileOptionsF

type Options (cmd :: Command) = V.ARec Attr (OptionsF cmd)
genDefunSymbols [ ''Options ]

data FieldLabel (f :: Symbol) = FieldLabel
instance (s ~ f) => IsLabel s (FieldLabel f) where
  fromLabel = FieldLabel

(=:) :: FieldLabel f -> ElF f -> Attr f
_ =: x = Attr x

instance {-# OVERLAPPING #-} forall f name names a b.
     ( Functor f
     , HasField' name (V.ARec Attr names) a
     , a ~ ElF name, b ~ a
     )
  => IsLabel name ((a -> f b) -> (V.ARec Attr names -> f (V.ARec Attr names))) where
  fromLabel = field' @name

instance {-# OVERLAPPING #-} forall fs f t i.
     ( i ~ V.RIndex f fs
     , V.RecElem V.ARec f f fs fs i
     , t ~ ElF f
     )
  => HasField' f (V.ARec Attr fs) t where
  field' = V.rlens @f . unAttr

infixl +>
(+>) :: forall fs gs hs a.
        ( V.RMap fs, V.RMap gs, V.RMap hs
        , V.RecApplicative hs, V.RApply hs
        , hs V.≅ (fs `Union` gs)
        , fs V.⊆ hs, gs V.⊆ hs
        )
     => V.Rec a fs -> V.Rec a gs -> V.Rec a hs
x +> y = fromJust $ V.rtraverse (getLast . V.getCompose) z'
  where x', y', z' :: V.Rec (Last V.:. a) hs
        x' = V.rmap coerce $ V.rdowncast @hs x
        y' = V.rmap coerce $ V.rdowncast @hs y
        z' = V.rzipWith (<>) x' y'

type OptionsSend t = Options (Send t)
type OptionsCompile t = Options (Compile t)
genDefunSymbols [ ''OptionsSend, ''OptionsCompile ]

type SomeOptions = Sigma Command OptionsSym0
type SomeSendOptions = Sigma Target OptionsSendSym0
type SomeCompileOptions = Sigma Target OptionsCompileSym0

instance {-# OVERLAPPING #-} forall f name a b.
     ( Functor f
     , HasField' name SomeOptions a
     , a ~ ElF name, b ~ a
     )
  => IsLabel name ((a -> f b) -> (SomeOptions -> f SomeOptions)) where
  fromLabel = field' @name

instance {-# OVERLAPPING #-} forall f name a b.
     ( Functor f
     , HasField' name (SomeCompileOptions) a
     , a ~ ElF name, b ~ a
     )
  => IsLabel name ((a -> f b) -> (SomeCompileOptions -> f SomeCompileOptions)) where
  fromLabel = field' @name

instance {-# OVERLAPPING #-} forall f name a b.
     ( Functor f
     , HasField' name (SomeSendOptions) a
     , a ~ ElF name, b ~ a
     )
  => IsLabel name ((a -> f b) -> (SomeSendOptions -> f SomeSendOptions)) where
  fromLabel = field' @name

instance {-# OVERLAPPING #-} forall f t i.
         ( i ~ V.RIndex f (CommonOptionsF ++ CommonCompileOptionsF)
         , V.RecElem V.ARec f f (CommonOptionsF ++ CommonCompileOptionsF) (CommonOptionsF ++ CommonCompileOptionsF) i
         , t ~ ElF f
         )
  => HasField' f SomeCompileOptions t where
  field' = L.lens get set
    where
      get (SLabel :&: o) = getField @f (V.rcast @(CommonOptionsF ++ CommonCompileOptionsF) o)
      get (STape :&: o) = getField @f (V.rcast @(CommonOptionsF ++ CommonCompileOptionsF) o)

      set (SLabel :&: o) x =
        SLabel :&: V.rreplace (setField @f x $ V.rcast @(CommonOptionsF ++ CommonCompileOptionsF) o) o
      set (STape :&: o) x =
        STape :&: V.rreplace (setField @f x $ V.rcast @(CommonOptionsF ++ CommonCompileOptionsF) o) o

instance {-# OVERLAPPING #-} forall f t i.
         ( i ~ V.RIndex f (CommonOptionsF ++ CommonSendOptionsF)
         , V.RecElem V.ARec f f (CommonOptionsF ++ CommonSendOptionsF) (CommonOptionsF ++ CommonSendOptionsF) i
         , t ~ ElF f
         )
  => HasField' f SomeSendOptions t where
  field' = L.lens get set
    where
      get (SLabel :&: o) = getField @f (V.rcast @(CommonOptionsF ++ CommonSendOptionsF) o)
      get (STape :&: o) = getField @f (V.rcast @(CommonOptionsF ++ CommonSendOptionsF) o)

      set (SLabel :&: o) x =
        SLabel :&: V.rreplace (setField @f x $ V.rcast @(CommonOptionsF ++ CommonSendOptionsF) o) o
      set (STape :&: o) x =
        STape :&: V.rreplace (setField @f x $ V.rcast @(CommonOptionsF ++ CommonSendOptionsF) o) o

instance {-# OVERLAPPING #-} forall f t i.
         ( i ~ V.RIndex f CommonOptionsF
         , V.RecElem V.ARec f f CommonOptionsF CommonOptionsF i
         , t ~ ElF f
         )
  => HasField' f SomeOptions t where
  field' = L.lens get set
    where
      get (SSend SLabel :&: o) = getField @f (V.rcast @CommonOptionsF o)
      get (SSend STape :&: o) = getField @f (V.rcast @CommonOptionsF o)
      get (SCompile SLabel :&: o) = getField @f (V.rcast @CommonOptionsF o)
      get (SCompile STape :&: o) = getField @f (V.rcast @CommonOptionsF o)

      set (SSend SLabel :&: o) x =
        SSend SLabel :&: V.rreplace (setField @f x $ V.rcast @CommonOptionsF o) o
      set (SSend STape :&: o) x =
        SSend STape :&: V.rreplace (setField @f x $ V.rcast @CommonOptionsF o) o
      set (SCompile SLabel :&: o) x =
        SCompile SLabel :&: V.rreplace (setField @f x $ V.rcast @CommonOptionsF o) o
      set (SCompile STape :&: o) x =
        SCompile STape :&: V.rreplace (setField @f x $ V.rcast @CommonOptionsF o) o

instance Pretty SomeOptions where
  pretty (SSend SLabel :&: o) = text "command: send(label)" <$$$> pretty o
  pretty (SSend STape :&: o) = text "command: send(tape)" <$$$> pretty o
  pretty (SCompile SLabel :&: o) = text "command: compile(label)" <$$$> pretty o
  pretty (SCompile STape :&: o) = text "command: compile(tape)" <$$$> pretty o

instance Pretty SomeSendOptions where
  pretty (SLabel :&: o) = text "command: send(label)" <$$$> pretty o
  pretty (STape :&: o) = text "command: send(tape)" <$$$> pretty o

instance Pretty SomeCompileOptions where
  pretty (SLabel :&: o) = text "command: compile(label)" <$$$> pretty o
  pretty (STape :&: o) = text "command: compile(tape)" <$$$> pretty o

(<$$$>) :: Doc -> Doc -> Doc
P.Empty <$$$> y = y
x <$$$> P.Empty = x
x <$$$> y = x P.<$> y

instance Pretty Width where
  pretty FromImage = "from-image"
  pretty (Width x) = text (show x) <> "px"

instance Pretty Length where
  pretty Continuous = "continuous"
  pretty LFromImage = "from-image"
  pretty (Length n) = text (show n)

newtype PrettyHexWord16 = PrettyHexWord16 Word16
instance Pretty PrettyHexWord16 where
  pretty (PrettyHexWord16 x) = text $ printf "0x%04x" x

class ReifyPretty f g names where
  reifyPretty :: V.Rec f names -> V.Rec (V.Lift (,) (V.Dict Pretty) (V.Const String) `V.Compose` g) names
instance ReifyPretty f g '[] where
  reifyPretty RNil = RNil
instance forall f g x xs.
            (KnownSymbol x, Pretty (g x), Coercible (f x) (g x), ReifyPretty f g xs)
         => ReifyPretty f g (x : xs) where
  reifyPretty (v :& vs) = V.Compose (V.Lift (V.Dict (coerce v), V.Const (symbolVal (Proxy @x)))) :& reifyPretty vs

type family PrettyElFByType t where
  PrettyElFByType Word16 = PrettyHexWord16
  PrettyElFByType Width = Width
  PrettyElFByType Length = Length
  PrettyElFByType t = LispCasePretty t

type family PrettyElF x where
  PrettyElF x = PrettyElFByType (ElF x)

newtype PrettyAttr f = PrettyAttr { _unPrettyAttr :: PrettyElF f }
deriving via PrettyElF f instance (Pretty (PrettyElF f)) => Pretty (PrettyAttr f)

instance ( V.RecApplicative names, V.RPureConstrained (V.IndexableField names) names
         , V.RMap names, ReifyPretty Attr PrettyAttr names, V.RecordToList names)
  => Pretty (V.ARec Attr names) where
  pretty xs = pretty (V.fromARec xs)

instance ( V.RecApplicative names, V.RPureConstrained (V.IndexableField names) names
         , V.RMap names, ReifyPretty Attr PrettyAttr names, V.RecordToList names)
  => Pretty (V.Rec Attr names) where
  pretty xs =
      foldr1 (<$$$>)
    . V.recordToList
    . V.rmap (\(V.Compose (V.Lift (V.Dict x, V.Const n))) ->
        V.Const (text (lispCase n) <> ": " <> pretty x))
    $ reifyPretty @Attr @PrettyAttr xs


type AllOptionsF =  CommonOptionsF
                 ++ CommonSendOptionsF
                 ++ SendLabelOptionsF
                 ++ SendTapeOptionsF
                 ++ CommonCompileOptionsF
                 ++ LabelCompileOptionsF
                 ++ TapeCompileOptionsF

newtype LAttr f = LAttr { _unLAttr :: Last (ElF f) }
  deriving (Semigroup, Monoid) via Last (ElF f)
L.makeLenses ''LAttr
instance (KnownSymbol f, Show (ElF f)) => Show (LAttr f) where
  show (LAttr x) = symbolVal @f Proxy ++ ": " ++ show x

instance {-# OVERLAPPING #-} forall f name names a b.
     ( Functor f
     , HasField' name (V.Rec LAttr names) a
     , a ~ Last (ElF name), b ~ a
     )
  => IsLabel name ((a -> f b) -> (V.Rec LAttr names -> f (V.Rec LAttr names))) where
  fromLabel = field' @name

instance {-# OVERLAPPING #-} forall fs f t i.
     ( i ~ V.RIndex f fs
     , V.RecElem V.Rec f f fs fs i
     , t ~ Last (ElF f)
     )
  => HasField' f (V.Rec LAttr fs) t where
  field' = V.rlens @f . unLAttr

type RawRec = V.Rec LAttr AllOptionsF
data RawOptions = RawOptions (Last Command) RawRec
  deriving stock (Generic, Show)
  deriving Semigroup via GenericSemigroup RawOptions
  deriving Monoid via GenericMonoid RawOptions

customExecParser' :: ParserPrefs -> ParserInfo a -> IO a
customExecParser' pprefs pinfo =   execParserPure pprefs pinfo <$> getArgs
                               >>= handleParseResult'
  where handleParseResult' :: ParserResult a -> IO a
        handleParseResult' (Success a) = pure a
        handleParseResult' (Failure f) = do
          progn <- getProgName
          let (msg, exit) = renderFailure f progn
          case exit of
            ExitSuccess -> putStrLn msg
            _           -> hPutStrLn stderr msg
          exitWith exit
        handleParseResult' (CompletionInvoked c) = do
          progn <- getProgName
          msg <- execCompletion c progn
          putStr msg
          exitSuccess

getOptions :: IO RawOptions
getOptions =
  customExecParser' p $ info (optionsParser <**> helper)
    (  fullDesc
    <> progDesc "Send printing commands to a DYMO LabelWriter 450 DUO"
    <> footerDoc (Just f)
    )
  where p = prefs showHelpOnError
        f = "The default command is <label> with options:" P.<$>
            P.indent 2 (pretty defaultLabelArgs)

        defaultLabelArgs :: V.Rec Attr (OptionsF (Send Label))
        defaultLabelArgs = defaultCommonArgs V.<+> defaultSendArgs V.<+> defaultSendLabelArgs

optionsParser :: Parser RawOptions
optionsParser = do
  common <- commonParser
  sub <- subcommandParser
  input <- lastOption $ argument str
                                 (  metavar "INPUT"
                                 <> help "Input file or '-' for stdin"
                                 )
  pure $ common <> sub <> RawOptions mempty (mempty &~ do
    #input .= input
    )

rBounded :: forall a. (Integral a, Bounded a, Typeable a) => String -> ReadM a
rBounded errorFmt = auto >>= f
  where f i | i < lower = fail msg
            | i > upper = fail msg
            | otherwise = pure . fromInteger $ i
        lower = toInteger (minBound :: a)
        upper = toInteger (maxBound :: a)
        msg = map toUpper (show (typeOf (error "placeholder" :: a))) <>
              " must be within the range [" <> printf errorFmt lower <> " .. " <> printf errorFmt upper <> "]"

lastOption :: Parser a -> Parser (Last a)
lastOption p = Last <$> optional p

onOff :: String -> O.Mod FlagFields Bool -> Parser Bool
onOff name p = flag' True (long name <> p) <|> flag' False (long ("no-" <> name) <> internal)

commonParser :: Parser RawOptions
commonParser = RawOptions mempty <$> do
  verbose   <- lastOption $ onOff   "verbose"
                                    (  short 'v'
                                    <> help "Verbose mode"
                                    )
  vendorId  <- lastOption $ option  (rBounded @Word16 "0x%04x")
                                    (  long "vendor"
                                    <> metavar "WORD16"
                                    <> help ("USB Vendor ID")
                                    )
  productId <- lastOption $ option  (rBounded @Word16 "0x%04x")
                                    (  long "product"
                                    <> metavar "WORD16"
                                    <> help ("USB Product ID")
                                    )
  pure $ mempty @RawRec &~ do
    #verbose .= verbose
    #vendorId .= vendorId
    #productId .= productId

subcommandParser :: Parser RawOptions
subcommandParser = subparser (mconcat commands)

command' :: Monoid a => String -> String -> Maybe Doc -> Parser a -> Parser a -> O.Mod CommandFields a
command' label description f c p =
  O.command label (info (p' <**> helper) (progDesc description <> footerDoc f))
  where p' = (<>) <$> c <*> p

commands :: [O.Mod CommandFields RawOptions]
commands = [ command' "label" "Send commands to the label endpoint" Nothing
                      commonParser pSendLabel
           , command' "tape" "Send commands to the tape endpoint" Nothing
                      commonParser pSendTape
           , command' "compile" "Compile an image into LabelWriter commands" (Just pCompileDefaults)
                      commonCompile pCompile
           ]
 where pCompileDefaults = "The <compile> command defaults to the <label> target with arguments:" P.<$>
                          P.indent 2 (pretty defaultLabelArgs) <> P.hardline P.<$>
                          "The <compile> command with <tape> target has default arguments:" P.<$>
                          P.indent 2 (pretty defaultTapeArgs)

       defaultLabelArgs :: V.Rec Attr (CommonCompileOptionsF ++ LabelCompileOptionsF)
       defaultLabelArgs = defaultCompileArgs +> defaultCompileLabelArgs

       defaultTapeArgs :: V.Rec Attr (CommonCompileOptionsF ++ TapeCompileOptionsF)
       defaultTapeArgs = defaultCompileArgs +> defaultCompileTapeArgs

       commonCompile = (<>) <$> commonParser <*> pure (RawOptions (pure $ Compile Label) mempty)

pCommonSend :: Parser RawRec
pCommonSend = do
  reset    <- lastOption $  onOff "reset"
                                  (  short 'r'
                                  <> help "Reset the printer before sending commands"
                                  )
  pure $ mempty @RawRec &~ do
    #reset .= reset

pSendLabel :: Parser RawOptions
pSendLabel = do
  common <- pCommonSend
  formFeed <- lastOption $ option (   readEnum @FormFeed
                                  <|> readEnumFailure "Form feed options:"
                                  )
                                  (  long "form-feed"
                                  <> short 'f'
                                  <> metavar "FORMFEED"
                                  <> help "Insert a <short> or <long> form feed after printing"
                                  )
  pure $ RawOptions (pure $ Send Label) (common &~ do
    #formFeed .= formFeed
    )

pSendTape :: Parser RawOptions
pSendTape = do
  common <- pCommonSend
  cut <- lastOption $ onOff "cut"
                            (  short 'c'
                            <> help "Cut the tape after printing"
                            )
  pure $ RawOptions (pure $ Send Tape) (common &~ do
    #cut .= cut
    )

lastOption' :: Coercible a b => Parser a -> Parser (Last b)
lastOption' p = coerce <$> optional p

pLength :: A.Parser Length
pLength =     (Length <$> A.decimal)
          <|> (A.char 'c' *> pure Continuous)
          <|> (A.char 'i' *> pure LFromImage)

rLength :: ReadM Length
rLength = str >>= \s ->
    case A.parseOnly (pLength <* A.skipSpace <* A.endOfInput) s of
      Left _ -> fail "Length must be an integer or 'c' for continuous printing or 'i' for the length of the input"
      Right x -> pure x

pWidth :: A.Parser Width
pWidth =     (Width <$> A.decimal)
          <|> (A.char 'i' *> pure FromImage)

rWidth :: ReadM Width
rWidth = str >>= \s ->
    case A.parseOnly (pWidth <* A.skipSpace <* A.endOfInput) s of
      Left _ -> fail "Width must be an integer or 'i' for the width of the input"
      Right x -> f x
  where f (Width i) | i < lower = fail msg
                    | i > upper = fail msg
                    | otherwise = pure . Width $ i
        f FromImage = pure FromImage
        lower, upper :: Word16
        lower = 1
        upper = 672
        msg = "Width must be within the range [" <> printf "%d" lower <> " .. " <> printf "%d" upper <> "]"

pCompile :: Parser RawOptions
pCompile = do
  target       <- lastOption' $ option (   readEnum @Target
                                       <|> readEnumFailure "Available targets:"
                                       )
                                       (  long "target"
                                       <> short 't'
                                       <> metavar "TARGET"
                                       <> help "Compile commands suitable for <label> or <tape>"
                                       )
  hiDpi        <- lastOption $ onOff   "hi-dpi"
                                       (  short 'g'
                                       <> help "Enable 300dpi x 600dpi mode; only available for the <label> target"
                                       )
  printDensity <- lastOption  $ option (   readEnum @Density
                                       <|> readEnumFailure "Available densities:"
                                       )
                                       (  long "density"
                                       <> metavar "DENSITY"
                                       <> help "Set printer density; pass '?' for a list of available densities"
                                       )
  tapeType     <- lastOption  $ option (   readEnum @TapeType
                                       <|> readEnumFailure "Available tape types:"
                                       )
                                       (  long "tape-type"
                                       <> metavar "TAPE-TYPE"
                                       <> help "Set tape type; pass '?' for a list of available tape types"
                                       )
  width        <- lastOption  $ option rWidth
                                       (  long "width"
                                       <> short 'w'
                                       <> metavar "INT"
                                       <> help "Label width in pixels or 'i' for the width of the input"
                                       )
  length       <- lastOption  $ option rLength
                                       (  long "length"
                                       <> short 'l'
                                       <> metavar "INT"
                                       <> help "Label length in pixels or 'c' for continuous printing or 'i' for the length of the input"
                                       )
  pure $ RawOptions (Compile <$> target) (mempty @RawRec &~ do
    #hiDpi .= hiDpi
    #printDensity .= printDensity
    #tapeType .= tapeType
    #width .= width
    #length .= length
    )

makeDefault :: ( V.RMap fs, fs V.⊆ AllOptionsF )
            => V.Rec Attr fs -> RawRec
makeDefault d = V.rmap coerce $ V.rdowncast d

validateOptions :: IsString s => RawOptions -> Errors [s] SomeOptions
validateOptions (RawOptions (Last Nothing) _) = failure ["missing command"]
validateOptions (RawOptions (Last (Just (Compile Label))) o) =
  (sing @(Compile Label) :&:) . V.toARec <$> validateCompileLabelOptions o
validateOptions (RawOptions (Last (Just (Compile Tape))) o) =
  (sing @(Compile Tape) :&:) . V.toARec <$> validateCompileTapeOptions o
validateOptions (RawOptions (Last (Just (Send Label))) o) =
  (sing @(Send Label) :&:) . V.toARec <$> validateSendLabelOptions o
validateOptions (RawOptions (Last (Just (Send Tape))) o) =
  (sing @(Send Tape) :&:) . V.toARec <$> validateSendTapeOptions o

toError ::  s -> Last a -> Errors [s] a
toError msg (Last x) = maybe (failure [msg]) pure x

assertAbsent :: s -> Last a -> Errors [s] ()
assertAbsent _ (Last Nothing) = pure ()
assertAbsent msg (Last _) = failure [msg]

validateCommonOptions :: IsString s => RawRec -> Errors [s] (V.Rec Attr CommonOptionsF)
validateCommonOptions o =
  do verbose <- toError "missing verbose flag" (o ^. #verbose)
     vendorId <- toError "missing vendorId" (o ^. #vendorId)
     productId <- toError "missing productId" (o ^. #productId)
     input <- toError "missing input" (o ^. #input)
     pure $ V.rcast $
          #verbose =: verbose
       :& #vendorId =: vendorId
       :& #productId =: productId
       :& #input =: input
       :& V.RNil

validateCommonSendOptions :: IsString s => RawRec -> Errors [s] (V.Rec Attr CommonSendOptionsF)
validateCommonSendOptions o =
  do reset <- toError "missing reset flag" (o ^. #reset)
     pure $ V.rcast $
          #reset =: reset
       :& V.RNil

validateSendLabelOptions :: IsString s => RawRec -> Errors [s] (V.Rec Attr (OptionsF (Send Label)))
validateSendLabelOptions o' =
  do common <- validateCommonOptions o
     commonSend <- validateCommonSendOptions o
     formFeed <- toError "missing form feed" (o ^. #formFeed)
     pure $ common V.<+> commonSend V.<+> (V.rcast $
          #formFeed =: formFeed
       :& V.RNil
       )
  where
    o =  makeDefault defaultCommonArgs
      <> makeDefault defaultSendArgs
      <> makeDefault defaultSendLabelArgs
      <> o'

validateSendTapeOptions :: IsString s => RawRec -> Errors [s] (V.Rec Attr (OptionsF (Send Tape)))
validateSendTapeOptions o' =
  do common <- validateCommonOptions o
     commonSend <- validateCommonSendOptions o
     cut <- toError "missing cut" (o ^. #cut)
     pure $ common V.<+> commonSend V.<+> (V.rcast $
          #cut =: cut
       :& V.RNil
       )
  where
    o =  makeDefault defaultCommonArgs
      <> makeDefault defaultSendArgs
      <> makeDefault defaultSendTapeArgs
      <> o'

validateCommonCompileOptions :: IsString s
                             => RawRec -> Errors [s] (V.Rec Attr CommonCompileOptionsF)
validateCommonCompileOptions o =
  do width <- toError "missing width" (o ^. #width)
     length <- toError "missing length" (o ^. #length)
     pure $ V.rcast $
          #width =: width
       :& #length =: length
       :& V.RNil

validateCompileLabelOptions :: IsString s
                            => RawRec -> Errors [s] (V.Rec Attr (OptionsF (Compile Label)))
validateCompileLabelOptions o' =
  do assertAbsent "Tape type is incompatible with the label printer. Use density." (o ^. #tapeType)
     common <- validateCommonOptions o
     commonCompile <- validateCommonCompileOptions o
     hiDpi <- toError "missing hiDpi" (o ^. #hiDpi)
     printDensity <- toError "missing printDensity" (o ^. #printDensity)
     pure $ common V.<+> commonCompile V.<+> (V.rcast $
          #hiDpi =: hiDpi
       :& #printDensity =: printDensity
       :& V.RNil
       )
  where o =  makeDefault defaultCommonArgs
          <> makeDefault defaultCompileArgs
          <> makeDefault defaultCompileLabelArgs
          <> o'

validateCompileTapeOptions :: IsString s
                           => RawRec -> Errors [s] (V.Rec Attr (OptionsF (Compile Tape)))
validateCompileTapeOptions o' =
  do assertAbsent "Graphics mode is incompatible with the tape printer." (o ^. #hiDpi)
     assertAbsent "Density is incompatible with the tape printer. Use tape type." (o ^. #printDensity)
     common <- validateCommonOptions o
     commonCompile <- validateCommonCompileOptions o
     tapeType <- toError "missing tapeType" (o ^. #tapeType)
     pure $ common V.<+> commonCompile V.<+> (V.rcast $
          #tapeType =: tapeType
       :& V.RNil
       )
  where o =  makeDefault defaultCommonArgs
          <> makeDefault defaultCompileArgs
          <> makeDefault defaultCompileTapeArgs
          <> o'

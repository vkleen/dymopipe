{-# language StrictData #-}
module Options ( getOptions
               ) where

import Prelude hiding (length)
--import Options.Applicative hiding (command)
--import qualified Options.Applicative as O
--import qualified Data.Attoparsec.Text as A
--import Data.Typeable (typeOf)
--import Data.Monoid.Generic
--import Control.Applicative.Lift
--import Data.Type.Map
--import Data.Char
--import Options.ReadEnum
--import Text.Printf
--import Data.Foldable (foldr1)
--import Text.PrettyPrint.ANSI.Leijen hiding ((<$>), empty, bool, width)
--import qualified Text.PrettyPrint.ANSI.Leijen as P
--import qualified Text.PrettyPrint.ANSI.Leijen.Internal as P
--import System.Exit (ExitCode(..))
--import System.Environment (getArgs, getProgName)
--import System.IO (hPutStrLn)

--idVendorDYMO :: Word16
--idVendorDYMO = 0x0922
--
--idProductDYMOLabelWriter450DUO :: Word16
--idProductDYMOLabelWriter450DUO = 0x0023
--
--helper' :: Parser (a -> a)
--helper' = abortOption ShowHelpText
--                      (  long "help"
--                      <> short '?'
--                      <> help "Show this help text"
--                      <> hidden
--                      )
--
--customExecParser' :: ParserPrefs -> ParserInfo a -> IO a
--customExecParser' pprefs pinfo =   execParserPure pprefs pinfo <$> getArgs
--                               >>= handleParseResult'
--  where handleParseResult' :: ParserResult a -> IO a
--        handleParseResult' (Success a) = pure a
--        handleParseResult' (Failure f) = do
--          progn <- getProgName
--          let (msg, exit) = renderFailure f progn
--          case exit of
--            ExitSuccess -> putStrLn msg
--            _           -> hPutStrLn stderr msg
--          exitWith exit
--        handleParseResult' (CompletionInvoked c) = do
--          progn <- getProgName
--          msg <- execCompletion c progn
--          putStr msg
--          exitSuccess

getOptions :: IO ()
getOptions = error "not implemented"

--getOptions :: IO PartialOptions
--getOptions = customExecParser' p $ info (optionsParser <**> helper')
--  (  fullDesc
--  <> progDesc "Send printing commands to a DYMO LabelWriter 450 DUO"
--  <> footerDoc (Just f)
--  )
--  where p = prefs showHelpOnError
--        f = "The default options are as follows:" P.<$> pretty defaultOptions
--
--prettyLast :: Pretty a => Last a -> String -> Doc
--prettyLast (Last Nothing) _  = mempty
--prettyLast (Last (Just a)) n = text n <> ": " <> pretty a
--
--(<$$$>) :: Doc -> Doc -> Doc
--P.Empty <$$$> y = y
--x <$$$> P.Empty = x
--x <$$$> y = x P.<$> y
--
--newtype Width = Width Int
--  deriving stock (Show)
--  deriving (Enum, Num, Ord, Eq, Real, Integral) via Int
--
--instance Pretty Width where
--  pretty (Width x) = text (show x) <> "px"
--
--instance Bounded Width where
--  minBound = Width 1
--  maxBound = Width 672
--
--data Length = Continuous | Length Int
--  deriving stock (Show)
--
--instance Pretty Length where
--  pretty Continuous = "continuous"
--  pretty (Length n) = show n
--
--data Unwrapped (x :: k)
--
--type family (w :: k -> Type) ::: (x :: k) :: Type where
--  Unwrapped ::: x = x
--  w ::: x = w x
--
--type family (w :: k -> Type) ::? (x :: k) :: Type where
--  Unwrapped ::? x = ()
--  w ::? x = w x
--
--newtype PrettyHexWord16 = PrettyHexWord16 Word16
--instance Pretty PrettyHexWord16 where
--  pretty (PrettyHexWord16 x) = text $ printf "0x%04x" x
--
--instance Pretty (CommonOptions Last) where
--  pretty CommonOptions{..} = foldr1 (<$$$>) [ p (coerce @_ @(LispCasePretty Bool) <$> verbose) "verbose"
--                                            , p (coerce @_ @(LispCasePretty Bool) <$> reset) "reset"
--                                            , p (coerce @_ @PrettyHexWord16 <$> vendorId) "vendor-id"
--                                            , p (coerce @_ @PrettyHexWord16 <$> productId) "product-id"
--                                            ]
--    where p = prettyLast
--
--data Target = LabelTarget | TapeTarget
--  deriving stock (Generic, Show)
--  deriving Pretty via RelabeledTarget
--type RelabeledTarget = Target `WithOverrides`
--  '[ "LabelTarget" ':-> "Label"
--   , "TapeTarget" ':-> "Tape"
--   ]
--
--data Density = Light | Medium | Normal | Dark
--  deriving stock (Generic, Show)
--  deriving anyclass ReadEnum
--  deriving Pretty via LispCasePretty Density
--
--data TapeType = BlackOnWhite
--              | BlackOnBlue
--              | BlackOnRed
--              | BlackOnSilver
--              | BlackOnYellow
--              | BlackOnGold
--              | BlackOnGreen
--              | BlackOnFluorescentGreen
--              | BlackOnFluorescentRed
--              | WhiteOnClear
--              | WhiteOnBlack
--              | BlueOnWhite
--              | RedOnWhite
--  deriving stock (Generic, Show)
--  deriving anyclass ReadEnum
--  deriving Pretty via LispCasePretty TapeType
--
--data Command w =
--    Label
--  | Tape
--  | Compile (CompileArgs w)
--  deriving stock (Generic)
--deriving stock instance Show (Command Unwrapped)
--deriving stock instance Show (Command Last)
--
--instance Semigroup (Command Last) where
--  _ <> Label = Label
--  _ <> Tape = Tape
--  Compile a <> Compile a' = Compile (a <> a')
--  _ <> Compile a = Compile a
--
--data CommonCompileArgs w = CommonCompileArgs { width :: w ::: Width
--                                             , length :: w ::: Length
--                                             , cutAfter :: w :::  Bool
--                                             }
--  deriving stock (Generic)
--
--data CompileArgs w = CompileLabel { hiDpi :: w ::: Bool
--                                  , printDensity :: w ::: Density
--                                  , common :: CommonCompileArgs w
--                                  }
--                   | CompileTape { tapeType :: w ::: TapeType
--                                 , common :: CommonCompileArgs w
--                                 }
--  deriving stock (Generic)
--
--type family WrapMaybe (w :: k -> Type) (x :: k) :: Type where
--  WrapMaybe Unwrapped x = x
--  WrapMaybe _ x = Maybe x
--
--data CommonOptions w = CommonOptions { verbose :: w ::: Bool
--                                     , reset :: w ::: Bool
--                                     , vendorId :: w ::: Word16
--                                     , productId :: w ::: Word16
--                                     }
--  deriving stock (Generic)
--
--data Options w  = Options { common :: CommonOptions w
--                          , command :: Command w
--                          }
--  deriving stock (Generic)
--
--deriving via GenericSemigroup (CommonCompileArgs Last) instance Semigroup (CommonCompileArgs Last)
--deriving stock instance Show (CommonCompileArgs Unwrapped)
--deriving stock instance Show (CommonCompileArgs Last)
--
--deriving via GenericSemigroup (CompileArgs Last) instance Semigroup (CompileArgs Last)
--deriving stock instance Show (CompileArgs Unwrapped)
--deriving stock instance Show (CompileArgs Last)
--
--deriving via GenericSemigroup (CommonOptions Last) instance Semigroup (CommonOptions Last)
--deriving stock instance Show (CommonOptions Unwrapped)
--deriving stock instance Show (CommonOptions Last)
--
--deriving via GenericSemigroup (Options Last) instance Semigroup (Options Last)
--deriving stock instance Show (Options Unwrapped)
--deriving stock instance Show (Options Last)
--
--instance Pretty (Command Last) where
--  pretty Label = "label"
--  pretty Tape = "tape"
--  pretty (Compile a) = "compile using" P.<$> indent 2 (pretty a)
--
--instance Pretty (Options Last) where
--  pretty Options{..} = pretty common <$$$> prettyLast (Last command) "command"
--
--instance Pretty (CompileArgs Last) where
--  pretty CompileArgs{..} = foldr1 (<$$$>) [ p target "target"
--                                          , p (coerce @_ @(LispCasePretty Bool) <$> hiDpi) "hi-dpi"
--                                          , p printDensity "density"
--                                          , p tapeType "tape-type"
--                                          , p width "width"
--                                          , p length "length"
--                                          , p (coerce @_ @(LispCasePretty Bool) <$> cutAfter) "cut-after"
--                                          ]
--    where p = prettyLast
--
--type PartialOptions = Options Last
--type Options' = Options Unwrapped
--
--optionsParser :: Parser PartialOptions
--optionsParser = do
--  common <- commonParser
--  command <- subcommandParser
--  pure Options {..}
--
--lastOption :: Parser a -> Parser (Last a)
--lastOption p = Last <$> optional p
--
--lastOption' :: Coercible a b => Parser a -> Parser (Last b)
--lastOption' p = coerce <$> optional p
--
--commonParser :: Parser (CommonOptions Last)
--commonParser = do
--  verbose   <- lastOption $ switch (  long "verbose"
--                                   <> short 'v'
--                                   <> help "Verbose mode"
--                                   )
--  reset     <- lastOption $ switch (  long "reset"
--                                   <> short 'r'
--                                   <> help "Reset the printer before sending commands"
--                                   )
--  vendorId  <- lastOption $ option rBounded
--                                   (  long "vendor"
--                                   <> metavar "WORD16"
--                                   <> help ("USB Vendor ID")
--                                   )
--  productId <- lastOption $ option rBounded
--                                   (  long "product"
--                                   <> metavar "WORD16"
--                                   <> help ("USB Product ID")
--                                   )
--  pure CommonOptions {..}
--
--subcommandParser :: Parser (Maybe (Command Last))
--subcommandParser = optional $ subparser (mconcat commands)
--
--command' :: String -> String -> Maybe Doc -> Parser a -> Mod CommandFields a
--command' label description f p = O.command label (info (p <**> helper') (progDesc description <> footerDoc f))
--
--commands :: [Mod CommandFields (Command Last)]
--commands = [ command' "label" "Send commands to the label endpoint" Nothing (pure Label)
--           , command' "tape" "Send commands to the tape endpoint" Nothing (pure Tape)
--           , command' "compile" "Compile an image into LabelWriter commands" (Just pCompileDefaults) pCompile
--           ]
--  where pCompileDefaults = "The defaults for compile are as follows:" P.<$>
--                           pretty defaultCompileArgs
--
--pCompile :: Parser (Command Last)
--pCompile = do
--  target       <- lastOption' $ option (   readEnum @RelabeledTarget
--                                       <|> readEnumFailure "Available targets:"
--                                       )
--                                       (  long "target"
--                                       <> short 't'
--                                       <> metavar "TARGET"
--                                       <> help "Compile commands suitable for <label> or <tape>"
--                                       )
--  hiDpi        <- lastOption  $ switch (  long "hi-dpi"
--                                       <> short 'g'
--                                       <> help "Enable 300dpi x 600dpi mode; only available for the <label> target"
--                                       )
--  printDensity <- lastOption  $ option (   readEnum
--                                       <|> readEnumFailure "Available densities:"
--                                       )
--                                       (  long "density"
--                                       <> metavar "DENSITY"
--                                       <> help "Set printer density; pass '?' for a list of available densities"
--                                       )
--  tapeType     <- lastOption  $ option (   readEnum
--                                       <|> readEnumFailure "Available tape types:"
--                                       )
--                                       (  long "tape-type"
--                                       <> metavar "TAPE-TYPE"
--                                       <> help "Set tape type; pass '?' for a list of available tape types"
--                                       )
--  width        <- lastOption  $ option rBounded
--                                       (  long "width"
--                                       <> short 'w'
--                                       <> metavar "INT"
--                                       <> help "Label width in pixels"
--                                       )
--  length       <- lastOption  $ option rLength
--                                       (  long "length"
--                                       <> short 'l'
--                                       <> metavar "INT"
--                                       <> help "Label length in pixels or 'c' for continuous printing"
--                                       )
--  cutAfter     <- lastOption  $ switch (  long "cut-after"
--                                       <> short 'c'
--                                       <> help "Insert a form feed/cut tape command at the end"
--                                       )
--  pure $ Compile CompileArgs {..}
--
--rBounded :: forall a. (Integral a, Bounded a, Typeable a) => ReadM a
--rBounded = auto >>= f
--  where f i | i < lower = fail msg
--            | i > upper = fail msg
--            | otherwise = pure . fromInteger $ i
--        lower = toInteger (minBound :: a)
--        upper = toInteger (maxBound :: a)
--        msg = map toUpper (show (typeOf (error "placeholder" :: a))) <>
--              " must be within the range [" <> show lower <> " .. " <> show upper <> "]"
--
--pLength :: A.Parser Length
--pLength =     (Length <$> A.decimal)
--          <|> (A.char 'c' *> pure Continuous)
--
--rLength :: ReadM Length
--rLength = str >>= \s ->
--    case A.parseOnly (pLength <* A.skipSpace <* A.endOfInput) s of
--      Left _ -> fail "Length must be an integer or 'c' for continuous printing"
--      Right x -> pure x
--
--defaultOptions :: PartialOptions
--defaultOptions =
--  Options { common = CommonOptions { verbose = pure False
--                                   , reset = pure False
--                                   , vendorId = pure idVendorDYMO
--                                   , productId = pure idProductDYMOLabelWriter450DUO
--                                   }
--          , command = Just Label
--          }
--
--defaultCompileArgs :: CompileArgs Last
--defaultCompileArgs =
--  CompileArgs { target = pure LabelTarget
--              , hiDpi = pure False
--              , printDensity = pure Normal
--              , tapeType = pure BlackOnWhite
--              , width = pure 672
--              , length = pure Continuous
--              , cutAfter = pure True
--              }
--
--toError ::  s -> Last a -> Errors [s] a
--toError msg (Last x) = maybe (failure [msg]) pure x
--
--validateOptions :: IsString s => PartialOptions -> Errors [s] Options'
--validateOptions Options {..} = do
--  common <- validateCommonOptions common
--  command <- case command of
--    Just cmd -> validateCommand cmd
--    Nothing -> failure ["missing command"]
--  pure Options {..}
--
--validateCommonOptions :: IsString s => CommonOptions Last -> Errors [s] (CommonOptions Unwrapped)
--validateCommonOptions CommonOptions {..} = do
--  verbose <- toError "missing verbose flag" verbose
--  reset <- toError "missing reset flag" reset
--  vendorId <- toError "missing vendorId" vendorId
--  productId <- toError "missing productId" productId
--  pure CommonOptions {..}
--
--validateCommand :: IsString s => Command Last -> Errors [s] (Command Unwrapped)
--validateCommand Label = pure Label
--validateCommand Tape = pure Tape
--validateCommand (Compile args) = (Compile <$>) $ processCompileArgs args & \case
--  Just (LabelTarget, CompileArgs{..}) -> do hiDpi <- toError "missing hiDpi" hiDpi
--                                            printDensity <- toError "missing printDensity" printDensity
--                                            tapeType <- _
--  Just (TapeTarget, CompileArgs{..}) -> _
--  Nothing -> failure ["missing target for compile"]
--
--  where
--    processCompileArgs :: CompileArgs Last -> Maybe (Target, CompileArgs Last)
--    processCompileArgs a@CompileArgs{..} = case coerce target of
--      Just t -> Just (t, a)
--      Nothing -> Nothing
--
--
--  -- target <- toError "missing compile target" target
--  -- hiDpi <- case target of
--  --   LabelTarget -> toError "missing hiDpi" hiDpi
--  --   TapeTarget -> failure ["Graphics mode is incompatible with the tape printer"]
--  -- printDensity <- case target of
--  --   LabelTarget -> toError "missing printDensity" printDensity
--  --   TapeTarget -> failure ["Density is incompatible with the tape printer. Use tape type."]
--  -- tapeType <- case target of
--  --   LabelTarget -> failure ["Tape type is incompatible with the label printer. Use density."]
--  --   TapeTarget -> toError "missing tapeType" tapeType
--  -- dimensions <- toError "missing dimensions" dimensions
--  -- cutAfter <- toError "missing cutError" cutAfter
--  -- pure $ Compile CompileArgs {..}
{-# language BlockArguments #-}
{-# language MultiWayIf #-}
{-# language UndecidableInstances #-}

module Main (main) where

import Prelude hiding (readFile)

import Data.Generics.Labels()
import Text.PrettyPrint.ANSI.Leijen (hPutDoc)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import Control.Applicative.Lift (runErrors)
import Data.Singletons.Sigma
import Data.Generics.Labels ()
import qualified Control.Lens as L
import Control.Lens.Operators ((^.))
import qualified Data.ByteString.RawFilePath as BS
import qualified Data.ByteString.Char8 as BS8

import qualified Mason.Builder as M

import qualified Graphics.Image as G
import qualified Graphics.Image.Processing.Binary as G

import qualified System.Posix.Directory.Traversals as U
import System.IO (hPutStrLn)

import Options
import Compile
import Send

opts :: IO SomeOptions
opts = do
  rawOpts <- getOptions
  either (prettyErrors >=> const exitFailure) pure . runErrors $
    validateOptions rawOpts

  where prettyErrors es = hPutDoc stderr $
          "Command line options are invalid:" P.<$> P.indent 2 (
            P.vcat $ ("* " <>) <$> es
          )

canonicalizeInputPath :: SomeOptions -> IO SomeOptions
canonicalizeInputPath = #input \case
  Stdin  -> pure Stdin
  Path i -> Path <$> U.realpath i

doCompile :: SomeCompileOptions -> IO ()
doCompile o = do
  rgbImg :: G.Image G.VS G.RGB G.Word8 <- either (hPutStrLn stderr >=> const exitFailure) pure
    =<< G.decode G.PNG <$> case (o ^. #input) of
      Stdin  -> BS8.hGetContents stdin
      Path p -> BS.readFile p

  let img :: G.Image G.VS G.X G.Bit = G.thresholdWith (G.PixelRGB (<128) (<128) (<128)) rgbImg

  if G.rows img /= o ^. #width . L.to fromIntegral
    then hPutStrLn stderr "Rescaling is not implemented yet (width)" >> exitFailure
    else pure ()

  case o ^. #length of
    Continuous -> pure ()
    Length l   -> if G.cols img > fromIntegral l
                    then hPutStrLn stderr "Rescaling is not implemented yet (length)" >> exitFailure
                    else pure ()
  let img' = G.transpose . G.flipH $ img
  M.hPutBuilder stdout $ compile o img'

  -- let img = G.makeImage (l, w) \(r, c) -> if
  --       | r `rem` w <= c && c <= (r `rem` w) + 40 -> 1
  --       | otherwise                               -> 0

  --     w = o ^. #width . L.to fromIntegral
  --     l = case o ^. #length of
  --       Continuous -> o ^. #width . L.to fromIntegral
  --       Length l'  -> fromIntegral l'

  -- M.hPutBuilder stdout $ compile o img

doSend :: SomeSendOptions -> IO ()
doSend o = do
  inp <- case (o ^. #input) of
    Stdin -> BS8.hGetContents stdin
    Path p -> BS.readFile p
  send o inp

main :: IO ()
main = do
  opts >>= canonicalizeInputPath >>= \o -> case (coerce o) of
    SSend SLabel    :&: o' -> doSend    (SLabel :&: o')
    SSend STape     :&: o' -> doSend    (STape  :&: o')
    SCompile SLabel :&: o' -> doCompile (SLabel :&: o')
    SCompile STape  :&: o' -> doCompile (STape  :&: o')

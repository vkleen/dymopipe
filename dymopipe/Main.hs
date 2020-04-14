module Main (main) where

import Prelude hiding (readFile)

--import Control.Exception (mask_)
--import Control.Concurrent.MVar (mkWeakMVar)

import Data.Generics.Labels()
--import qualified Control.Lens as L
--import Control.Lens.Operators

--import Data.Monoid.Generic

--import qualified Data.Vector as V
--import qualified System.USB as USB

--import System.Posix.Env.ByteString (getArgs)
--import Data.ByteString.RawFilePath (readFile, getContents)

--import Data.Hex.Quote
--import Hexdump

import Options

main :: IO ()
main = do
  print =<< getOptions
  -- cmdLineOptions <- testOptions
  -- print cmdLineOptions

  -- ctx <- USB.newCtx
  -- dev <- waitForDYMO ctx

  -- config <- USB.getConfigDesc dev 0

  -- let Just ep = config ^? labelEndpointOut
  -- getArgs >>= \case
  --     [ "-" ] -> getContents
  --     [ file ] -> readFile file
  --     _ -> pure testCommands
  --   >>= printLabel 0 ep dev

--firstOutEndpoint :: L.Traversal' (V.Vector USB.InterfaceDesc) USB.EndpointAddress
--firstOutEndpoint =   L.ix 0
--                   . #interfaceEndpoints
--                   . L.traversed
--                   . #endpointAddress
--                   . L.filteredBy (#transferDirection . L.only USB.Out)
--
--labelEndpointOut :: L.Traversal' USB.ConfigDesc USB.EndpointAddress
--labelEndpointOut = #configInterfaces . L.ix 0 . firstOutEndpoint
--
--tapeEndpointOut :: L.Traversal' USB.ConfigDesc USB.EndpointAddress
--tapeEndpointOut = #configInterfaces . L.ix 1 . firstOutEndpoint
--
--printLabel :: USB.InterfaceNumber -> USB.EndpointAddress -> USB.Device -> ByteString -> IO ()
--printLabel i ep dev cmds =
--  USB.withDeviceHandle dev $ \hdl -> USB.withDetachedKernelDriver hdl i $ USB.withClaimedInterface hdl i $ do
--    transfer <- USB.newWriteTransfer USB.BulkTransfer
--                                     hdl
--                                     ep
--                                     cmds
--                                     USB.noTimeout
--    putTextLn $ "Trying to write the following to endpoint " <> show (ep ^. #endpointNumber) <> " on interface " <> show i
--    putStr . prettyHex $ cmds
--    USB.performWriteTransfer transfer >>= putTextLn.show
--
--testCommands :: ByteString
--testCommands =
--     stimes 84 [hex|1b|] -- Ensure synchronization per the documentation
--  <> [hex| 1B 40       -- Reset
--           1B 4C ff ff -- Enable continuous feed
--           1B 68       -- Enable Text Mode
--           1B 65       -- Set Print Density
--     |]
--  <> stimes 300 (  [hex|16|] <> stimes 42 [hex|00|] <> stimes 42 [hex|ff|]
--                <> [hex|16|] <> stimes 42 [hex|ff|] <> stimes 42 [hex|00|]
--                )
--  <> [hex| 1B 47 -- Short Form feed
--     |]
--  <> stimes 150 (  [hex|16|] <> stimes 42 [hex|00|] <> stimes 42 [hex|ff|]
--                )
--  <> stimes 150 (  [hex|16|] <> stimes 42 [hex|ff|] <> stimes 42 [hex|00|]
--                )
--  <> [hex| 1B 45 -- Form feed
--     |]
--  <> stimes 84 [hex|1b|] -- Ensure synchronization per the documentation
--  <> [hex| 1B 40       -- Reset
--           1B 4C ff ff -- Enable continuous feed
--           1B 69       -- Enable Graphics Mode
--           1B 65       -- Set Print Density
--     |]
--  <> stimes 300 (  [hex|16|] <> stimes 42 [hex|00|] <> stimes 42 [hex|ff|]
--                <> [hex|16|] <> stimes 42 [hex|ff|] <> stimes 42 [hex|00|]
--                )
--  <> [hex| 1B 47 -- Short Form feed
--     |]
--  <> stimes 150 (  [hex|16|] <> stimes 42 [hex|00|] <> stimes 42 [hex|ff|]
--                )
--  <> stimes 150 (  [hex|16|] <> stimes 42 [hex|ff|] <> stimes 42 [hex|00|]
--                )
--  <> [hex| 1B 45 -- Form feed
--     |]
--
--waitForDYMO :: USB.Ctx -> IO USB.Device
--waitForDYMO ctx = do
--  mv <- newEmptyMVar
--  mask_ $ do
--    h <- USB.registerHotplugCallback
--      ctx
--      USB.deviceArrived
--      USB.enumerate
--      Nothing Nothing
--      -- (Just idVendorDYMO)
--      -- (Just idProductDYMOLabelWriter450DUO)
--      Nothing
--      (\dev _ -> tryPutMVar mv dev $> USB.DeregisterThisCallback)
--    void $ mkWeakMVar mv $ USB.deregisterHotplugCallback h
--  dev <- takeMVar mv
--  pure dev

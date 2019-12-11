{-# language OverloadedStrings #-}
{-# language OverloadedLabels #-}
{-# language QuasiQuotes #-}
{-# language AllowAmbiguousTypes       #-}
{-# language DataKinds                 #-}
{-# language DeriveGeneric             #-}
{-# language DuplicateRecordFields     #-}
{-# language FlexibleContexts          #-}
{-# language NoMonomorphismRestriction #-}
{-# language TypeApplications          #-}
module Main (main) where

import Control.Exception (mask_)
import Control.Concurrent.MVar (mkWeakMVar)

import Data.Generics.Labels()
import qualified Control.Lens as L
import Control.Lens.Operators

import qualified Data.Vector as V
import qualified System.USB as USB

import Data.Hex.Quote
import Hexdump

idVendorDYMO :: Word16
idVendorDYMO = 0x0922

idProductDYMOLabelWriter450DUO :: Word16
idProductDYMOLabelWriter450DUO = 0x0023

main :: IO ()
main = do
  ctx <- USB.newCtx
  dev <- if ctx `USB.hasCapability` USB.HasHotplug
         then waitForDYMO ctx
         else findDYMO ctx

  config <- USB.getConfigDesc dev 0

  let Just ep = config ^? labelEndpointOut
  printTestLabel 0 ep dev

firstOutEndpoint :: L.Traversal' (V.Vector USB.InterfaceDesc) USB.EndpointAddress
firstOutEndpoint =   L.element 0
                   . #interfaceEndpoints
                   . L.traversed
                   . #endpointAddress
                   . L.filteredBy ( #transferDirection . L.only USB.Out )

labelEndpointOut :: L.Traversal' USB.ConfigDesc USB.EndpointAddress
labelEndpointOut = #configInterfaces . L.element 0 . firstOutEndpoint

tapeEndpointOut :: L.Traversal' USB.ConfigDesc USB.EndpointAddress
tapeEndpointOut = #configInterfaces . L.element 1 . firstOutEndpoint

printTestLabel :: USB.InterfaceNumber -> USB.EndpointAddress -> USB.Device -> IO ()
printTestLabel i ep dev =
  USB.withDeviceHandle dev $ \hdl -> USB.withDetachedKernelDriver hdl i $ USB.withClaimedInterface hdl i $ do
    transfer <- USB.newWriteTransfer USB.BulkTransfer
                                     hdl
                                     ep
                                     testCommands
                                     USB.noTimeout
    putTextLn $ "Trying to write the following to endpoint " <> show (ep ^. #endpointNumber) <> " on interface " <> show i
    putStr . prettyHex $ testCommands
    USB.performWriteTransfer transfer >>= putTextLn.show

testCommands :: ByteString
testCommands =
     stimes 84 [hex|1b|] -- Ensure synchronization per the documentation
  <> [hex| 1B 40       -- Reset
           1B 4C ff ff -- Enable continuous feed
           1B 68       -- Enable Text Mode
           1B 65       -- Set Print Density
     |]
  <> stimes 300 (  [hex|16|] <> stimes 42 [hex|00|] <> stimes 42 [hex|ff|]
                <> [hex|16|] <> stimes 42 [hex|ff|] <> stimes 42 [hex|00|]
                )
  <> [hex| 1B 47 -- Short Form feed
     |]
  <> stimes 150 (  [hex|16|] <> stimes 42 [hex|00|] <> stimes 42 [hex|ff|]
                )
  <> stimes 150 (  [hex|16|] <> stimes 42 [hex|ff|] <> stimes 42 [hex|00|]
                )
  <> [hex| 1B 45 -- Form feed
     |]
  <> stimes 84 [hex|1b|] -- Ensure synchronization per the documentation
  <> [hex| 1B 40       -- Reset
           1B 4C ff ff -- Enable continuous feed
           1B 69       -- Enable Graphics Mode
           1B 65       -- Set Print Density
     |]
  <> stimes 300 (  [hex|16|] <> stimes 42 [hex|00|] <> stimes 42 [hex|ff|]
                <> [hex|16|] <> stimes 42 [hex|ff|] <> stimes 42 [hex|00|]
                )
  <> [hex| 1B 47 -- Short Form feed
     |]
  <> stimes 150 (  [hex|16|] <> stimes 42 [hex|00|] <> stimes 42 [hex|ff|]
                )
  <> stimes 150 (  [hex|16|] <> stimes 42 [hex|ff|] <> stimes 42 [hex|00|]
                )
  <> [hex| 1B 45 -- Form feed
     |]



waitForDYMO :: USB.Ctx -> IO USB.Device
waitForDYMO ctx = do
  mv <- newEmptyMVar
  mask_ $ do
    h <- USB.registerHotplugCallback
      ctx
      USB.deviceArrived
      USB.enumerate
      (Just idVendorDYMO)
      (Just idProductDYMOLabelWriter450DUO)
      Nothing
      (\dev _ -> tryPutMVar mv dev $> USB.DeregisterThisCallback)
    void $ mkWeakMVar mv $ USB.deregisterHotplugCallback h
  dev <- takeMVar mv
  pure dev

findDYMO :: USB.Ctx -> IO USB.Device
findDYMO ctx = do
  devs <- V.toList <$> USB.getDevices ctx
  deviceDescs <- traverse USB.getDeviceDesc devs
  case fst <$> find (match.snd) (zip devs deviceDescs) of
    Nothing -> error "No DYMO LabelWriter 450 DUO found."
    Just dev -> pure dev

  where match devDesc =    USB.deviceVendorId devDesc == idVendorDYMO
                        && USB.deviceProductId devDesc == idProductDYMOLabelWriter450DUO

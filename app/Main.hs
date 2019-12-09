{-# language OverloadedStrings #-}
module Main (main) where

import Control.Exception (mask_)
import Control.Concurrent.MVar (mkWeakMVar)

import qualified Data.Vector as V
import qualified System.USB as USB

main :: IO ()
main = do
  ctx <- USB.newCtx
  USB.setDebug ctx USB.PrintDebug

  dev <- if ctx `USB.hasCapability` USB.HasHotplug
         then findDYMO ctx
         else findDYMO ctx

  putTextLn "Found the DYMO thingy:"
  traverse_ putTextLn (deviceInfo dev)

waitForDYMO :: USB.Ctx -> IO USB.Device
waitForDYMO ctx = do
  mv <- newEmptyMVar
  mask_ $ do
    h <- USB.registerHotplugCallback
      ctx
      USB.deviceArrived
      USB.enumerate
      Nothing
      Nothing
      Nothing
      (\dev _ -> tryPutMVar mv dev $> USB.DeregisterThisCallback)
    void $ mkWeakMVar mv $ USB.deregisterHotplugCallback h
  dev <- takeMVar mv
  pure dev

findDYMO :: USB.Ctx -> IO USB.Device
findDYMO ctx = do
  devs <- V.toList <$> USB.getDevices ctx
  deviceDescs <- traverse USB.getDeviceDesc devs
  traverse_ (putTextLn.show) deviceDescs
  error "I don't know which device to look for"

deviceInfo :: USB.Device -> [Text]
deviceInfo dev = [ "deviceSpeed:   " <> (maybe "-" show $ USB.deviceSpeed dev)
                 , "busNumber:     " <> (show $ USB.busNumber dev)
                 , "portNumber:    " <> (show $ USB.portNumber dev)
                 , "portNumbers:   " <> (maybe "-" (show . V.toList) $ USB.portNumbers dev 7)
                 , "deviceAddress: " <> (show $ USB.deviceAddress dev)
                 ]

module Send (send) where

import Control.Exception (mask_)
import Control.Concurrent.MVar (mkWeakMVar)

import qualified Data.Vector as V
import qualified System.USB as USB
import qualified Control.Lens as L
import Control.Lens.Operators ((^.), (^?))
import Data.Singletons.Prelude
import Data.Singletons.Sigma
import System.IO (hPutStrLn)
import qualified Mason.Builder as M

import Options

send :: SomeSendOptions -> ByteString -> IO ()
send o@(s :&: _) inp = do
  ctx <- USB.newCtx
  dev <- waitForDYMO o ctx
  config <- USB.getConfigDesc dev 0

  ep <- case config ^? (endpoint $ fromSing s) of
    Just ep -> pure ep
    Nothing -> hPutStrLn stderr "Couldn't find USB endpoint" >> exitFailure

  let d = prologue <> M.byteString inp <> epilogue o
  writeToUSB ep dev $ M.toStrictByteString d

  where endpoint Tape  = tapeEndpointOut
        endpoint Label = labelEndpointOut

        prologue =  stimes 84 (M.word8 0x1b)
                 <> if o ^. #reset
                    then M.word8 0x1b <> M.word8 0x40
                    else mempty

firstOutEndpoint :: L.Traversal' (V.Vector USB.InterfaceDesc) USB.EndpointAddress
firstOutEndpoint =   L.ix 0
                   . #interfaceEndpoints
                   . L.traversed
                   . #endpointAddress
                   . L.filteredBy (#transferDirection . L.only USB.Out)

labelEndpointOut :: L.Traversal' USB.ConfigDesc USB.EndpointAddress
labelEndpointOut = #configInterfaces . L.ix 0 . firstOutEndpoint

tapeEndpointOut :: L.Traversal' USB.ConfigDesc USB.EndpointAddress
tapeEndpointOut = #configInterfaces . L.ix 1 . firstOutEndpoint

writeToUSB :: USB.EndpointAddress -> USB.Device -> ByteString -> IO ()
writeToUSB ep dev d =
  USB.withDeviceHandle dev $ \hdl -> USB.withDetachedKernelDriver hdl 0 $ USB.withClaimedInterface hdl 0 $ do
    transfer <- USB.newWriteTransfer USB.BulkTransfer
                                     hdl
                                     ep
                                     d
                                     USB.noTimeout
    void $ USB.performWriteTransfer transfer

epilogue :: SomeSendOptions -> M.Builder
epilogue (SLabel :&: o) = case o ^. #formFeed of
  Short -> M.word8 0x1b <> M.char8 'G'
  Long -> M.word8 0x1b <> M.char8 'E'
  None -> mempty

epilogue (STape :&: o) = if o ^. #cut
                         then M.word8 0x1b <> M.char8 'E'
                         else mempty

waitForDYMO :: SomeSendOptions -> USB.Ctx -> IO USB.Device
waitForDYMO o ctx = do
  mv <- newEmptyMVar
  mask_ $ do
    h <- USB.registerHotplugCallback
      ctx
      USB.deviceArrived
      USB.enumerate
      (o ^. #vendorId . L.to Just) (o ^. #productId . L.to Just)
      Nothing
      (\dev _ -> tryPutMVar mv dev $> USB.DeregisterThisCallback)
    void $ mkWeakMVar mv $ USB.deregisterHotplugCallback h
  dev <- takeMVar mv
  pure dev

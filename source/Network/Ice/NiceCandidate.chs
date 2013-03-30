{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
module Network.Ice.NiceCandidate where

import Network.Socket
import Network.Socket.Internal (peekSockAddr, withSockAddr)
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Control.Applicative
import Control.Monad
import Foreign.C
import Data.Maybe (fromJust)

#include <nice/agent.h>
#include <marshal.h>

{#context lib="libnice" prefix="nice" #}

{# enum NiceCandidateType as NiceCandidateType {underscoreToCase}
    with prefix="NICE_CANDIDATE_TYPE"
    deriving (Eq, Show, Read, Bounded) #}

{# enum NiceCandidateTransport {underscoreToCase}
    deriving (Eq, Show, Read, Bounded) #}

type NiceAddress = SockAddr
type TurnServer = Ptr ()

data NiceCandidate = NiceCandidate
  { candidateType :: NiceCandidateType
  , candidateTransport :: NiceCandidateTransport
  , address :: SockAddr
  , baseAddress :: Maybe SockAddr
  , priority :: Integer
  , streamId :: Int
  , componentId :: Int
  , foundation :: String
  , username :: Maybe String
  , password :: Maybe String
  , turn :: TurnServer
  , sockPtr :: Ptr ()
  } deriving Show

enum :: (Monad m, Integral i, Enum e)  => m i -> m e
enum = liftM $ toEnum . fromIntegral

mbPeekCString p = do
    mbstr <- if p == nullPtr
                 then return Nothing
                 else Just `fmap` peekCString p
    free p
    return mbstr

mbNewCString Nothing = return nullPtr
mbNewCString (Just x) = newCString x

candidateBaseAddress p = undefined

withSockAddr' sa f = withSockAddr sa (\p _ -> f $ castPtr p)

type SizeT = {# type size_t #}

foreign import ccall safe "memset"
    memset :: Ptr () -> CInt -> SizeT -> IO (Ptr ())

withCandidate :: Storable s => s -> (Ptr () -> IO b) -> IO b
withCandidate c f = alloca $ \p -> do
  memset (castPtr p) 0 {#sizeof NiceCandidate #}
  poke p c
  f $ castPtr p

{# fun copy_to_sockaddr_check as ^
   { id `Ptr ()'
   , id `Ptr ()'
   }
   -> `CInt' id #}

getNiceAddress na = allocaBytes {# sizeof sa_storage #} $ \p -> do
        nnul <- copyToSockaddrCheck na p
        if nnul > 0
            then Just <$> peekSockAddr (castPtr p)
            else return Nothing


{# fun nice_address_set_from_sockaddr as ^
   { id `Ptr ()'
   , withSockAddr'* `SockAddr'
   }
   -> `()' #}

instance Storable NiceCandidate where
    sizeOf _ = {# sizeof NiceCandidateType #}
    alignment _ = {# alignof NiceCandidateType #}
    peek p = NiceCandidate
               <$> enum ({# get NiceCandidate->type      #} p)
               <*> enum ({# get NiceCandidate->transport #} p)
               <*> (fmap fromJust . getNiceAddress =<<
                      {# ptrto NiceCandidate->addr #} p)
               <*> (getNiceAddress =<<
                      {# ptrto NiceCandidate->base_addr #} p)
               <*> (fromIntegral <$> {# get NiceCandidate->priority #} p)
               <*> (fromIntegral <$> {# get NiceCandidate->stream_id #} p)
               <*> (fromIntegral <$> {# get NiceCandidate->component_id #} p)
               <*> (peekCString . castPtr
                       =<< {# ptrto NiceCandidate->foundation #} p)
               <*> (mbPeekCString =<< {# get NiceCandidate->username #} p)
               <*> (mbPeekCString =<< {# get NiceCandidate->password #} p)
               <*> {# get NiceCandidate->turn #} p
               <*> {# get NiceCandidate->sockptr #} p
    poke p NiceCandidate{..} = do
        {# set NiceCandidate->type #} p . fromIntegral . fromEnum $ candidateType
        {# set NiceCandidate->transport #} p . fromIntegral . fromEnum
            $ candidateTransport
        addrP <- castPtr `fmap` {# ptrto NiceCandidate->addr #} p
        niceAddressSetFromSockaddr addrP  address
        baseAddrP <- castPtr `fmap` {# ptrto NiceCandidate->base_addr #} p
        case baseAddress of
            Nothing -> return ()
            Just ba -> niceAddressSetFromSockaddr baseAddrP ba
        {# set NiceCandidate->priority #} p $ fromIntegral priority
        {# set NiceCandidate->stream_id #} p $ fromIntegral streamId
        {# set NiceCandidate->component_id #} p $ fromIntegral componentId
        withCString foundation $ \pa ->
            {#call set_foundation #} (castPtr p) (castPtr pa)
        {# set NiceCandidate->username #} p =<< mbNewCString username
        {# set NiceCandidate->password #} p =<< mbNewCString password
        {# set NiceCandidate->turn #} p turn
        {# set NiceCandidate->sockptr #} p sockPtr

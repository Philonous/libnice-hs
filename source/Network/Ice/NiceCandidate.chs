{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Ice.NiceCandidate where

import Network.Socket
import Network.Socket.Internal (peekSockAddr, withSockAddr)
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Control.Applicative
import Control.Monad
import Foreign.C

#include <nice/agent.h>
#include <marshal.h>

{#context lib="libnice" prefix="nice" #}

{# enum NiceCandidateType as NiceCandidateType {underscoreToCase}
    deriving (Eq, Show, Read, Bounded) #}

{# enum NiceCandidateTransport {underscoreToCase}
    deriving (Eq, Show, Read, Bounded) #}

type NiceAddress = SockAddr
type TurnServer = Ptr ()

data NiceCandidate = NiceCandidate
  { candidateType :: NiceCandidateType
  , candidateTransport :: NiceCandidateTransport
  , address :: SockAddr
  , baseAddress :: SockAddr
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

instance Storable NiceCandidate where
    sizeOf _ = {# sizeof NiceCandidateType #}
    alignment _ = {# alignof NiceCandidateType #}
    peek p = NiceCandidate
               <$> enum ({# get NiceCandidate->type      #} p)
               <*> enum ({# get NiceCandidate->transport #} p)
               <*> (peekSockAddr . castPtr
                     =<< {# call get_candidate_addr #} (castPtr p))
               <*> (peekSockAddr . castPtr
                     =<< {# call get_candidate_base_addr #} (castPtr p))
               <*> (fromIntegral <$> {# get NiceCandidate->priority #} p)
               <*> (fromIntegral <$> {# get NiceCandidate->stream_id #} p)
               <*> (fromIntegral <$> {# get NiceCandidate->component_id #} p)
               <*> (peekCString . castPtr $ p `plusPtr` 76)
               <*> (mbPeekCString =<< {# get NiceCandidate->username #} p)
               <*> (mbPeekCString =<< {# get NiceCandidate->password #} p)
               <*> {# get NiceCandidate->turn #} p
               <*> {# get NiceCandidate->sockptr #} p
    poke p NiceCandidate{..} = do
        {# set NiceCandidate->type #} p . fromIntegral . fromEnum $ candidateType
        {# set NiceCandidate->transport #} p . fromIntegral . fromEnum
            $ candidateTransport
        withSockAddr address $ \pa _ ->
            {# call set_candidate_addr #} (castPtr p) (castPtr pa)
        withSockAddr baseAddress $ \pa _ ->
            {# call set_candidate_base_addr #} (castPtr p) (castPtr pa)
        {# set NiceCandidate->priority #} p $ fromIntegral priority
        {# set NiceCandidate->stream_id #} p $ fromIntegral streamId
        {# set NiceCandidate->component_id #} p $ fromIntegral componentId
        withCString foundation $ \pa ->
            {#call set_foundation #} (castPtr p) (castPtr pa)
        {# set NiceCandidate->username #} p =<< mbNewCString username
        {# set NiceCandidate->password #} p =<< mbNewCString password
        {# set NiceCandidate->turn #} p turn
        {# set NiceCandidate->sockptr #} p sockPtr

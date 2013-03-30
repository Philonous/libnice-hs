{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_HADDOCK hide #-}
module Network.Ice.NiceAgent where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString, packCStringLen)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Word
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Control.Concurrent
import Control.Monad

import System.Glib.GObject
import System.Glib.Properties
import System.Glib.Attributes
import System.Glib.GList
import System.Glib.MainLoop

import Network.Ice.Signals
import Network.Ice.NiceCandidate
import Network.Ice.Utils

#include <nice/agent.h>
#include "marshal.h"

{#context lib="libnice" prefix="nice_agent" #}

{# fun nice_debug_enable as ^
    {`Bool'
    }
    -> `()' #}

{#enum NiceCompatibility as Compatibility {underscoreToCase}
   with prefix="nice_compatibility" deriving (Eq, Show, Read) #}

type RecvFun = Ptr () -> CUInt -> CUInt -> CUInt -> Ptr CChar -> Ptr () -> IO ()

data NiceAgent = NiceAgent {unNiceAgent  :: ForeignPtr NiceAgent } deriving Eq



withNiceAgent (NiceAgent na ) f = withForeignPtr na $ f . castPtr

instance GObjectClass NiceAgent where
    toGObject = GObject . castForeignPtr . unNiceAgent
    unsafeCastGObject = NiceAgent . castForeignPtr . unGObject

niceAgentNew :: Compatibility -> MainContext -> IO NiceAgent
niceAgentNew compat ctx = withForeignPtr (fromMainContext ctx) $ \p ->
                          constructNewGObject
                        (NiceAgent, objectUnref)
                        (castPtr <$> {# call nice_agent_new #}
                                 (castPtr p)
                                 (fromIntegral $ fromEnum compat))

-- nice_agent_add_local_address

{# fun add_stream as ^
    {withNiceAgent* `NiceAgent', `Int'} -> `Int' #}

{# fun remove_stream as ^
    {withNiceAgent* `NiceAgent', `Int'} -> `()' #}

-- nice_agent_set_relay_info

-- | You HAVE to call 'attachReceive' before running this, otherwise stun messages
-- can't be received
{# fun gather_candidates as ^
    {withNiceAgent* `NiceAgent', `Int'} -> `Bool' #}

peekGString x = do
  strp <- peek x
  str <- peekCString strp
  {#call g_free #} $ castPtr strp
  return str

{# fun set_remote_credentials as ^
    { withNiceAgent* `NiceAgent', `Int', `String', `String'} -> `Bool' #}

{# fun get_local_credentials as ^
    { withNiceAgent* `NiceAgent'
    , `Int'
    , alloca- `String' peekGString*
    , alloca- `String' peekGString* }
    -> `Bool' #}

gsListify xs f = do
  ptrs <- mapM new xs
  withGSList ptrs f

{# fun set_remote_candidates as ^
   { withNiceAgent* `NiceAgent'
   , `Int'
   , `Int'
   ,  gsListify* `[NiceCandidate]'
   }
   -> `Bool' #}

unGsListify x = do
  ptrs <- fromGSList x
  mapM peek ptrs

{# fun get_remote_candidates as ^
  { withNiceAgent* `NiceAgent'
  , `Int'
  , `Int'
  }
  -> `[NiceCandidate]' unGsListify* #}

unGsListify' x = do
  ptrs <- fromGSList x
  mapM (\p ->do
             x <- peek p
             -- free p -- TODO: Why does this crash?
             return x
       )  ptrs

{# fun get_local_candidates as ^
  { withNiceAgent* `NiceAgent'
  , `Int'
  , `Int'
  }
  -> `[NiceCandidate]' unGsListify'* #}

useAsCStringLen' bs f = unsafeUseAsCStringLen bs
                           (f . \(x,y) -> (fromIntegral y,x))

{# fun nice_agent_send as send
  { withNiceAgent* `NiceAgent'
  , `Int'
  , `Int'
  , useAsCStringLen'* `ByteString'&
  }
  -> `Int' #}

dataP = ($ nullPtr)

foreign import ccall "wrapper"
    mkRecvFun :: RecvFun -> IO (FunPtr RecvFun)

withMainContext ctx f  = withForeignPtr (fromMainContext ctx) $ f . castPtr

{# fun attach_recv as niceAgentAttachRecv'
  { withNiceAgent* `NiceAgent'
  , `Int'
  , `Int'
  , withMainContext* `MainContext'
  , id `FunPtr RecvFun'
  , dataP- `Ptr ()'
  }
  -> `Bool' #}

attachReceive agent sid cid ctx f = do
  let f' _agent _sid _cid len buf _userData = do
              bs <- packCStringLen (buf, fromIntegral len)
              f bs
  fp <- mkRecvFun f'
  niceAgentAttachRecv' agent sid cid ctx fp

{# fun set_selected_pair as ^
   { withNiceAgent* `NiceAgent'
   , `Int'
   , `Int'
   , `String'
   , `String'
   }
   -> `Bool' #}

{# fun set_selected_remote_candidate as ^
   { withNiceAgent* `NiceAgent'
   , `Int'
   , `Int'
   , withCandidate* `NiceCandidate'
   }
   -> `Bool' #}

{# fun set_stream_tos as ^
  { withNiceAgent* `NiceAgent'
  , `Int'
  , `Int'
  }
  -> `()' #}

{# fun set_software as ^
  { withNiceAgent* `NiceAgent'
  , `String'
  }
  -> `()' #}

{# fun restart as ^
  { withNiceAgent* `NiceAgent'
  }
  -> `Bool' #}

-- Signals

candidateGatheringDone :: Signal NiceAgent (Word -> IO ())
candidateGatheringDone = Signal (connect_WORD__NONE "candidate-gathering-done")

componentStateChanged :: Signal NiceAgent (Word -> Word -> Word -> IO ())
componentStateChanged = Signal (connect_WORD_WORD_WORD__NONE
                                  "component-state-changed")

initialBindingrequestReceived ::Signal NiceAgent (Word -> IO ())
initialBindingrequestReceived  = Signal (connect_WORD__NONE "initial-binding-request-received")

newCandidate :: Signal NiceAgent (Word -> Word -> String -> IO ())
newCandidate = Signal (connect_WORD_WORD_STRING__NONE
                                      "new-candidate")

newRemotecandidate :: Signal NiceAgent (Word -> Word -> String -> IO ())
newRemotecandidate = Signal (connect_WORD_WORD_STRING__NONE
                                      "new-remote-candidate")
newSelectedPair :: Signal NiceAgent (Word -> Word -> String -> String -> IO ())
newSelectedPair = Signal (connect_WORD_WORD_STRING_STRING__NONE
                                      "new-selected-pair")

reliableTransportWritable :: Signal NiceAgent (Word -> Word -> IO ())
reliableTransportWritable = Signal (connect_WORD_WORD__NONE
                                       "reliable-transport-writable")

-- Properties

compatibility :: Attr NiceAgent Int
compatibility = newAttrFromUIntProperty "compatibility"

controllingMode :: Attr NiceAgent Bool
controllingMode = newAttrFromBoolProperty "controlling-mode"

fullMode :: Attr NiceAgent Bool
fullMode = newAttrFromBoolProperty "full-mode"

maxConnectivityChecks :: Attr NiceAgent Int
maxConnectivityChecks = newAttrFromUIntProperty "max-connectivity-checks"

proxyIp         = newAttrFromStringProperty "proxy-ip"
proxyPassword   = newAttrFromStringProperty "proxy-password"
proxyPort       = newAttrFromUIntProperty "proxy-port"
proxyType       = newAttrFromUIntProperty "proxy-type"
proxyUsername   = newAttrFromStringProperty "proxy-username"
reliable        = newAttrFromBoolProperty "reliable"
stunPacingtimer = newAttrFromUIntProperty "stun-pacing-timer"
stunServer      = newAttrFromStringProperty "stun-server"
stunServerPort  = newAttrFromUIntProperty "stun-server-port"
upnp            = newAttrFromBoolProperty "upnp"
upnpTimeout     = newAttrFromUIntProperty "upnp-timeout"


-- mainContext "main-context" :: ReadAttr NiceAgent (Ptr ())

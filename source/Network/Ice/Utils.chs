{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Ice.Utils where

import System.Glib.MainLoop
import System.IO.Unsafe
import Foreign.Ptr
import Foreign.ForeignPtr
import Unsafe.Coerce

#include <glib.h>

{# context lib="glib" prefix ="g" #}

type MainContextPtr = ForeignPtr MainContext

foreign import ccall unsafe "&g_main_context_unref"
    mainContextFinalizer :: FunPtr (Ptr MainContext -> IO ())

fromMainLoop :: MainLoop -> ForeignPtr MainLoop
fromMainLoop = unsafeCoerce

toMainLoop :: ForeignPtr MainLoop -> MainLoop
toMainLoop = unsafeCoerce

fromMainContext :: MainContext -> ForeignPtr MainContext
fromMainContext = unsafeCoerce

toMainContext :: ForeignPtr MainContext -> MainContext
toMainContext = unsafeCoerce


-- | Gets a 'MainLoop's context.
mainLoopGetContext :: MainLoop
                   -> MainContext
mainLoopGetContext loop = toMainContext . unsafePerformIO $
                          withForeignPtr (fromMainLoop loop) $ \p ->
                          {# call main_loop_get_context #} (castPtr p)  >>=
                          newForeignPtr mainContextFinalizer . castPtr

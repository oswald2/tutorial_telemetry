module GUI.Colors
where 


import           GI.Gdk.Structs.RGBA
import           RIO
import           System.IO.Unsafe


{-# NOINLINE yellow #-}
yellow :: RGBA
yellow = unsafePerformIO $ do
    col <- newZeroRGBA
    void $ rGBAParse col "#ffff35"
    return col

{-# NOINLINE black #-}
black :: RGBA
black = unsafePerformIO $ do
    col <- newZeroRGBA
    void $ rGBAParse col "#000000"
    return col

{-# NOINLINE red #-}
red :: RGBA
red = unsafePerformIO $ do
    col <- newZeroRGBA
    void $ rGBAParse col "#ff0000"
    return col

{-# NOINLINE green #-}
green :: RGBA
green = unsafePerformIO $ do
    col <- newZeroRGBA
    res <- rGBAParse col "#00bb00"
    unless res $ error "Could not parse green!"
    return col


{-# NOINLINE white #-}
white :: RGBA
white = unsafePerformIO $ do
    col <- newZeroRGBA
    void $ rGBAParse col "#ffffff"
    return col

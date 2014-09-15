module Displayer
    (
        display
    )
    where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Audio as SDL
import Graphics.UI.SDL.CPUInfo as SDL
import Graphics.UI.SDL.Color as SDL
import Graphics.UI.SDL.Events as SDL
import Graphics.UI.SDL.General as SDL
import Graphics.UI.SDL.Joystick as SDL
import Graphics.UI.SDL.Keysym as SDL
import Graphics.UI.SDL.RWOps as SDL
import Graphics.UI.SDL.Rect as SDL
import Graphics.UI.SDL.Time as SDL
import Graphics.UI.SDL.Types as SDL
import Graphics.UI.SDL.Utilities as SDL
import Graphics.UI.SDL.Version as SDL
import Graphics.UI.SDL.Video as SDL
import Graphics.UI.SDL.WindowManagement as SDL

import Shortnames
import Util




-- | If Nothing is ginen, Create a window, and display an image. Else, save the
-- as a bmp file to the given location.
-- It executes the following actions in this order :
--
-- * It creates a empty windows,
--
-- * It calls a subaction, @display'@ which blits the image data on the screen
-- buffer
--
-- * It loops waiting for the exit event
--
-- * It quits SDL
--
-- That function is perticuliary unperformant and may take up to a few seconds
-- to display an image. The lack of performance comes from the use of 1x1
-- SDL_Rect to fill a pixel. Unfortunatly the sdl have no function to deal with
-- pixel per pixel drawing, and in haskell the buffer are unmodifiable by hand.
display :: Util.Color c =>
    Maybe String -- ^ Possibly a file path to save the image, otherwise the image will be displayed using sdl
    -> Raster2D c -- ^ An image to display, in the raytracer output format
    -> IO ()   -- ^ interaction with SDL surface, events and windows is always non-deterministic
display m_filename image = do
    SDL.init [SDL.InitVideo]
    case m_filename of
        Nothing -> do
            screen <- SDL.setVideoMode w h 32 [SDL.HWSurface]
            setCaption "INGI2325-Brack Nicolas" "INGI2325-Brack Nicolas"
            display' True image screen
            loop
        Just filename -> do
            surface <- SDL.createRGBSurface [] w h 32 0 0 0 0 
            display' False image surface
            hasSavedFile <- SDL.saveBMP surface filename 
            if hasSavedFile
            then
                putStrLn (">> Saving to "++show filename)
            else
                putStrLn (">> Couldn't save to "++show filename)

    SDL.quit
    where
            w = i_ $ imgWidth image; h = i_ $ imgHeight image

            loop = SDL.waitEventBlocking >>= handleEvent
            handleEvent Quit = return ()
            handleEvent _ = loop

            
-- | For each pixel contained in Raster2D, convert its color to RGB value, then
-- blit the pixel at its position, using 'blitPixel'.
display':: Util.Color c => 
    Bool             -- ^ Whether we should update the surface or not
    -> Raster2D c
    -> SDL.Surface
    -> IO ()
display' doflip (Raster2D((w,h), list)) screen = do
    background <- SDL.mapRGB (SDL.surfaceGetPixelFormat screen) 0 0 255
    fillRect screen Nothing background
    mapM_ (blitPixel doflip screen) inverted_list
    where inverted_list = map (\(Util.Pixel (x,y) color) -> Util.Pixel (x,h-y-1) color) list
            -- SDL use a reverted y axis 

    
-- | blit a pixel on a given surface, thanks to another 1x1 pixel.
-- It's perticuliary slow and unoptimized code, but it works and displaying an
-- image is not the aim of this project.
blitPixel doflip surface (Util.Pixel (x,y) color) = 
    let
        rgb = toRGB color
        sdlColor color =  SDL.mapRGB (SDL.surfaceGetPixelFormat surface) (red rgb) (green rgb) (blue rgb)
    in do
        pixel <- sdlColor color
        SDL.fillRect surface (Just (pixelAt x y)) pixel
        if doflip then
            SDL.flip surface
        else
            return () -- force ghc to compile ...


pixelAt x y = SDL.Rect (i_ x) (i_ y) 1 1
red   (r,_,_) = i_ $ r
green (_,g,_) = i_ $ g
blue  (_,_,b) = i_ $ b


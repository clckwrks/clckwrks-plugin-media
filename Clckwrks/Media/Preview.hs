module Clckwrks.Media.Preview where

import Control.Applicative ((<$>))
import Control.Monad (unless)
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import Clckwrks (query)
import Clckwrks.IOThread (ioRequest)
import Clckwrks.Media.Acid (GetMediumById(..))
import Clckwrks.Media.Types (Medium(..), MediumId(..), MediumKind(..), PreviewSize(..))

import Graphics.GD (imageSize, loadGifFile, loadJpegFile, loadPngFile, resizeImage, saveJpegFile, savePngFile, withImage)
import qualified Graphics.GD as GD
import System.Directory (doesFileExist)
import System.FilePath ((</>), splitExtension)

jpgThumbnail :: PreviewSize -> FilePath -> FilePath -> IO ()
jpgThumbnail size inPath outPath =
    withImage (loadJpegFile inPath) $ \image ->
        do (w,h) <- imageSize image
           let (w', h') =
                 if w > h
                 then (maxWidth, (h * maxWidth) `div` w)
                 else ((w * maxHeight) `div` h, maxHeight)
           image' <- resizeImage w' h' image
           saveJpegFile 60 outPath image'
    where
      maxWidth  = case size of
                    Tall   -> 200
                    Grande -> 300
                    Venti  -> 400
      maxHeight = maxWidth

pngThumbnail :: PreviewSize -> FilePath -> FilePath -> IO ()
pngThumbnail size inPath outPath =
    withImage (loadPngFile inPath) $ \image ->
        do (w,h) <- imageSize image
           let (w', h') =
                 if w > h
                 then (maxWidth, (h * maxWidth) `div` w)
                 else ((w * maxHeight) `div` h, maxHeight)
           image' <- resizeImage w' h' image
           savePngFile outPath image'
    where
      maxWidth  = case size of
                    Tall   -> 200
                    Grande -> 300
                    Venti  -> 400
      maxHeight = maxWidth

gifThumbnail :: PreviewSize -> FilePath -> FilePath -> IO ()
gifThumbnail size inPath outPath =
    withImage (loadGifFile inPath) $ \image ->
        do (w,h) <- imageSize image
           let (w', h') =
                 if w > h
                 then (maxWidth, (h * maxWidth) `div` w)
                 else ((w * maxHeight) `div` h, maxHeight)
           image' <- resizeImage w' h' image
           savePngFile outPath image'
    where
      maxWidth  = case size of
                    Tall   -> 200
                    Grande -> 300
                    Venti  -> 400
      maxHeight = maxWidth

mkThumbnail :: MediumKind -> PreviewSize -> FilePath -> FilePath -> IO ()
mkThumbnail JPEG = jpgThumbnail
mkThumbnail PNG  = pngThumbnail
mkThumbnail GIF  = gifThumbnail

applyTransforms :: FilePath -> FilePath -> (Medium, PreviewSize) -> IO FilePath
applyTransforms storageDir cacheDir (medium, size) =
    let origFP      = storageDir </> (mediumPath medium)
        (name, ext) = splitExtension (mediumPath medium)
        thumbFP     = cacheDir </> name ++ "_" ++ (show size) ++ ext
    in do e <- doesFileExist thumbFP
          unless e (mkThumbnail (mediumKind medium) size origFP thumbFP)
          return thumbFP

{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

{- cabal:
build-depends:
    base,
    containers,
    directory,
    extra,
    filepath,
    HPDF,
    hsexif,
    mtl,
    pretty-simple,
    transformers,
-}
{- project:
if os(windows)
    constraints:
        hsexif -iconv
-}
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Foldable hiding (maximum)
import Data.Foldable1
import Data.Function
import Data.Functor
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Traversable
import Data.Tuple.Extra
import Graphics.HsExif
import Graphics.PDF
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import Prelude hiding (maximum, unzip)

main :: IO ()
main = do
    (outPath, imgs) <-
        either (\s -> putStrLn s >> exitFailure) pure =<< runExceptT do
            (inDir, outPath) <-
                liftIO getArgs >>= \case
                    [d, o] -> pure (d, o)
                    _ -> throwError "expected two arguments - input image directory and output PDF path"
            inPaths <-
                maybe (throwError "no files") (pure . NE.sort) . NE.nonEmpty . map (inDir </>)
                    =<< liftIO (listDirectory inDir)
            (outPath,) <$> for inPaths \path -> do
                exif <- either (throwError . ("couldn't read file: " <>)) pure =<< liftIO (parseFileExif path)
                (,)
                    <$> (either (throwError . ("couldn't read metadata: " <>)) pure =<< liftIO (readJpegFile path))
                    <*> maybe
                        (throwError "metadata error")
                        pure
                        ( (,)
                            <$> ( (,)
                                    <$> (Map.lookup exifImageWidth exif >>= \case ExifNumber n -> Just n; _ -> Nothing)
                                    <*> (Map.lookup exifImageHeight exif >>= \case ExifNumber n -> Just n; _ -> Nothing)
                                )
                            <*> ( getOrientation exif <&> \case
                                    Normal -> (Nothing, False)
                                    Mirror -> (Nothing, True)
                                    Rotation a -> (Just a, False)
                                    MirrorRotation a -> (Just a, True)
                                )
                        )
    let allDimensions =
            imgs <&> \(_, (size, (r, _))) -> applyWhen (r `notElem` [Nothing, Just HundredAndEighty]) swap size
        pageSize = both fromIntegral . bimap maximum maximum $ unzip allDimensions
        rect = uncurry (PDFRect 0 0) pageSize
        docInfo = standardDocInfo{compressed = False}
    runPdf outPath docInfo rect $ for_ imgs \(imageFile, (_, (rotation, mirror))) -> do
        pageRef <- addPage Nothing
        jpgRef <- createPDFJpeg imageFile
        drawWithPage pageRef do
            applyMatrix $
                translate
                    let (w, h) = pageSize
                     in case rotation of
                            Nothing -> 0 :+ 0
                            Just Ninety -> w :+ h
                            Just HundredAndEighty -> w :+ 0
                            Just MinusNinety -> 0 :+ h
            when mirror do
                applyMatrix $ translate $ fst pageSize :+ 0
                applyMatrix $ scale -1 1
            applyMatrix $ rotate $ Degree case rotation of
                Nothing -> 0
                Just Ninety -> 90
                Just HundredAndEighty -> 180
                Just MinusNinety -> 270
            drawXObject jpgRef

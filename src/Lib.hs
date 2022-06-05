module Lib
  ( someFunc,
  )
where

import Codec.Picture
import Control.Monad
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Binary.Get
import Data.ByteString
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Word
import Happstack.Server
import Happstack.Server.Monads
import Prelude hiding (readFile)
import Codec.Picture.Extra
import System.Directory
import System.FilePath

theConf :: Conf
theConf =
  Conf
    { port = 7890,
      validator = Nothing,
      logAccess = Just logMAccess,
      timeout = 30,
      threadGroup = Nothing
    }

myPolicy :: BodyPolicy
myPolicy = defaultBodyPolicy "/tmp/" (1024 * 1024 * 128) (1024 * 1024) 1024

fTo8 :: Float -> Word8
fTo8 = round . (255 *) . min 1 . max 0

-- getPixelF :: Get PixelRGBA8
-- getPixelF = do
--   rf <- getFloatle
--   gf <- getFloatle
--   bf <- getFloatle
--   af <- getFloatle
--   return $ PixelRGBA8 (fTo8 rf) (fTo8 gf) (fTo8 bf) (fTo8 af)

getPixel :: Get Word8 -> Get PixelRGBA8
getPixel getC = do
  r <- getC
  g <- getC
  b <- getC
  a <- getC
  return $ PixelRGBA8 r g b a

folder :: Get Word8 -> Int -> ByteString -> Int -> Int -> (ByteString, PixelRGBA8)
folder getC pixelByteLen bs _ _ =
  let (head, tail) = Data.ByteString.splitAt pixelByteLen bs
   in (tail, runGet (getPixel getC) (fromStrict head))

userContentsDirName = "user-contents"

handlePNG :: FilePath -> Int -> Int -> String -> IO ()
handlePNG tmpFilePath width height filename = do
  b <- readFile tmpFilePath
  let byteLen = Data.ByteString.length b
  print "length is"
  print byteLen
  let pixelByteLen = byteLen `div` width `div` height
  let getComponent = if pixelByteLen == 4 then getWord8 else fTo8 <$> getFloatle
  let (_, img) = generateFoldImage (folder getComponent pixelByteLen) b width height
  savePngImage (userContentsDirName </> filename) (ImageRGBA8 (flipVertically img))

--   generateFoldImage (acc -> Int -> Int -> (acc, a)) acc Int Int
--   generateFoldImage folder b Int Int
--   let e = decodeImage b
--   print "Length is "
--   print
--   print
--     ( case e of
--         Left s -> s
--         Right di -> "Success"
--     )

handlers :: ServerPart Response
handlers =
  do
    addHeaderM "Access-Control-Allow-Origin" "*"
    decodeBody myPolicy
    msum
      [ dir "png" $ do
          method POST
          widthStr <- look "width"
          heightStr <- look "height"
          (tmpFilePath, fileName, fileType) <- lookFile "pixels"
          liftIO $ handlePNG tmpFilePath (read widthStr) (read heightStr) fileName
          ok $ do toResponse "You did a GET request on /foo\n",
        dir "hello" $ do
          ok $ toResponse "hello"
      ]

someFunc :: IO ()
someFunc = do
  createDirectoryIfMissing True userContentsDirName
  simpleHTTP theConf $ handlers
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
fTo8 = round . (8 *)

getPixel :: Get PixelRGBA8
getPixel = do
  rf <- getFloatbe
  gf <- getFloatbe
  bf <- getFloatbe
  af <- getFloatbe
  return $ PixelRGBA8 (fTo8 rf) (fTo8 gf) (fTo8 bf) (fTo8 af)

folder :: ByteString -> Int -> Int -> (ByteString, PixelRGBA8)
folder bs _ _ =
  let (head, tail) = Data.ByteString.splitAt 16 bs
   in (tail, runGet getPixel (fromStrict head))

handlePNG :: FilePath -> Int -> Int -> String -> IO ()
handlePNG tmpFilePath width height filename = do
  b <- readFile tmpFilePath
  print "length is"
  print $ Data.ByteString.length b
  let (_, img) = generateFoldImage folder b width height
  savePngImage filename (ImageRGBA8 img)

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
someFunc =
  simpleHTTP theConf $ handlers
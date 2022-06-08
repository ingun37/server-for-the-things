module UnzipStream (
    extractFirstEntryFromZipURL
) where

import System.FilePath.Posix (joinPath, takeFileName)
import Network.URI (parseURI, uriPath)
import Codec.Archive.Zip (ZipArchive, getEntries, getEntry, withArchive)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import Data.Map (toList)
import Optics
import Network.Http.Client (get)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)
import System.IO.Streams.File (withFileAsOutput)
import System.IO.Streams (connect)
import Control.Monad (forM, join)

takeFileNameFromURI = fmap (takeFileName . uriPath) . parseURI

getFirstEntry = do
  entryMap <- getEntries
  let firstEntry = toList entryMap ^? ix 0 % _1
  traverse getEntry firstEntry

bbb url zipName = do
  withSystemTempDirectory
    zipName
    ( \tmpDir -> do
        get
          (encodeUtf8 (pack url))
          ( \p zipIn -> do
              let zipPath = joinPath [tmpDir, "the.zip"]
              print zipPath
              withFileAsOutput zipPath (connect zipIn)
              withArchive zipPath getFirstEntry
          )
    )

extractFirstEntryFromZipURL url =
  let zipName = takeFileNameFromURI url
   in join <$> forM zipName (bbb url)
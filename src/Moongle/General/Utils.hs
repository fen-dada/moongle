module Moongle.General.Utils
  ( untarGz,
    unZip,
  )
where

import Codec.Archive.Tar qualified as Tar
import Codec.Archive.Zip
import Codec.Compression.GZip qualified as GZip
import Data.ByteString.Lazy qualified as BL

untarGz :: BL.ByteString -> Either Tar.FormatError [(FilePath, BL.ByteString)]
untarGz raw =
  let entries = Tar.read (GZip.decompress raw)
      step ::
        Tar.Entry ->
        Either Tar.FormatError [(FilePath, BL.ByteString)] ->
        Either Tar.FormatError [(FilePath, BL.ByteString)]
      step e acc =
        acc >>= \xs ->
          case Tar.entryContent e of
            Tar.NormalFile payload _ ->
              Right ((Tar.entryPath e, payload) : xs)
            _ -> Right xs
   in Tar.foldEntries step (Right []) Left entries

unZip :: BL.ByteString -> Either String [(FilePath, BL.ByteString)]
unZip raw = do
  archive <- toArchiveOrFail raw
  let files =
        [ (path, fromEntry entry)
        | entry <- zEntries archive, -- all entries
          let path = eRelativePath entry, -- POSIX relative path
          not (null path), -- skip null path
          last path /= '/', -- skip directories
          not (isEntrySymbolicLink entry) -- skip symlinks
        ]
  pure files

{-# LANGUAGE Arrows #-}
module Moongle.DB where

import Opaleye qualified as O
import Data.Text (Text)
import Effectful.Opaleye qualified as EO
import Effectful

data DefSummary = DefSummary
  { pkgOwner   :: Text
  , pkgName    :: Text
  , pkgVersion :: Text
  , modUser    :: Text
  , modName    :: Text
  , funName    :: Text
  , prettySig  :: Text
  } deriving (Show, Eq)

-- toTsQuery :: O.Field O.SqlText -> O.Field O.SqlText
-- toTsQuery = O.unsafeSqlFunction "to_tsquery"
--
-- tsMatch :: O.Field O.SqlText -> O.Field O.SqlText -> O.Field O.SqlBool
-- tsMatch = O.unsafeSqlBinOp "@@"
--
-- selectByTsQuery :: (EO.Opaleye :> es) => Text -> Eff es [DefSummary]
-- selectByTsQuery tsq = runSelect $ proc () -> do
--   row <- O.selectTable defsTable -< ()
--   O.restrict -< tsMatch (tokens row) (toTsQuery (O.toFields tsq))
--   returnA -< DefSummary
--     { pkgOwner   = O.fromFields (pkgOwner row)
--     , pkgName    = O.fromFields (pkgName row)
--     , pkgVersion = O.fromFields (pkgVersion row)
--     , modUser    = O.fromFields (modUser row)
--     , modName    = O.fromFields (modName row)
--     , funName    = O.fromFields (funName row)
--     , prettySig  = O.fromFields (prettySig row)
--     }

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Moongle.DB.Index where

import Data.Text (Text)
import Data.Text qualified as T
import Language.Moonbit.Mbti.Pretty (renderDecl)
import Language.Moonbit.Mbti.Syntax
import Moongle.DB.Types
import Moongle.Registry
import Moongle.TypeSearch (lexemesForFnDecl, toTsVectorLiteral)

mbtiToDefRows :: PackageId -> MbtiFile -> [DefRow]
mbtiToDefRows PackageId {..} (MbtiFile mp _ decls) =
  let mpath = Just (toTPath mp)
      fns = collectFns decls
   in map (rowOf mpath) fns
  where
    rowOf mpath fn@(FnDecl' sig _ knd) =
      let toks = lexemesForFnDecl mpath fn
          tsv = toTsVectorLiteral toks
          (a, ha, mr) = deriveFlags sig
       in DefRow
            { pkgVersion = version,
              username = T.pack (mpUserName mp),
              mod = T.pack (mpModuleName mp),
              pkgPath = map T.pack (mpPackagePath mp),
              funName = T.pack (Language.Moonbit.Mbti.Syntax.funName sig),
              prettySig = renderDecl $ FnDecl fn,
              visibility = visOf fn,
              kind = kindOf knd,
              tokensLex = toks,
              tokensTSV = tsv,
              arity = a,
              hasAsync = ha,
              mayRaise = mr,
              srcFile = Nothing,
              srcLine = Nothing,
              srcCol = Nothing,
              versionTag = version
            }

    collectFns :: [Decl] -> [FnDecl']
    collectFns = concatMap \case
      FnDecl f -> [f]
      TraitDecl _ _ _ methods -> methods
      _ -> []

    deriveFlags :: FnSig -> (Int, Bool, Bool)
    deriveFlags (FnSig _ ps _ _ effs) =
      (length ps, any isAsync effs, any isRaise effs)
      where
        isAsync EffAsync = True
        isAsync _ = False
        isRaise (EffException NoAraise) = False
        isRaise (EffException _) = True
        isRaise _ = False

    visOf :: FnDecl' -> Text
    visOf (FnDecl' (FnSig {}) _ _) = "pub" -- TODO: Visibility
    kindOf :: FnKind -> Text
    kindOf FreeFn = "free"
    kindOf (Method _) = "method"
    kindOf (TraitMethod _ b) = if b then "trait-default" else "trait"

    toTPath :: ModulePath -> TPath
    toTPath (ModulePath u m p) = TPath (p ++ [u]) m

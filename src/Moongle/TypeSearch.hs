{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Moongle.TypeSearch
  ( lexemesForFnDecl,
    lexemesForQuery,
    lexemesForFiles,
  )
where

import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Language.Moonbit.Mbti.Syntax
import Moongle.Query.Syntax

-- >>> lexemesForQuery (TyQuery (TypeQuery {queryTyParams = [], queryParams = [AnonParam False (TName Nothing (TCon "Int" []))], queryReturnTy = TName Nothing (TCon "String" []), queryEff = [EffException NoAraise]}))
-- ["mn,1,Int.","mn,1,String.","mn,r,String."]

-- >>> lexemesForQuery (TyQuery (TypeQuery {queryTyParams = [(TCon "K" [],[CTrait (TTrait Nothing "Compare")]),(TCon "V" [],[])], queryParams = [AnonParam False (TName Nothing (TCon "K" [])),AnonParam False (TName Nothing (TCon "V" []))], queryReturnTy = TName Nothing (TCon "Int" []), queryEff = [EffAsync,EffException (Araise (TName Nothing (TCon "E" [])))]}))
-- ["attr,async.","e,E.","mn,1,Int.","v,1,1","v,2,1","mn,r,Int."]

lexemesForFnDecl :: Maybe TPath -> FnDecl' -> [Text]
lexemesForFnDecl mPath decl =
  let FnSig {..} = fnSig decl
      varNames = tyParamNames funTyParams
      mnKs = mentionKTokens varNames (map paramType funParams) funReturnType
      mnR = mentionRTokens varNames funReturnType
      attrs = effTokens funEff
      sym = [symToken mPath (TCon funName [])]
   in sym ++ attrs ++ mnKs ++ mnR

lexemesForQuery :: Query -> [Text]
lexemesForQuery (NmQuery (NameQuery mp nm)) =
  [symToken mp nm]
lexemesForQuery (TyQuery (TypeQuery typs ps ret effs)) =
  let varNames = tyParamNames typs
      mnKs = mentionKTokens varNames (map paramType ps) ret
      mnR = mentionRTokens varNames ret
      attrs = effTokens effs
   in attrs ++ mnKs ++ mnR

-- ==== lexeme gen ====

mentionKTokens :: S.Set Name -> [Type] -> Type -> [Text]
mentionKTokens varNames params ret =
  let occur = typeOccurs varNames (params ++ [ret])
      mn =
        concatMap
          (\(k, n) -> [token "mn" (T.pack (show i)) (k <> ".") | i <- [1 .. n]])
          [(rp, n) | (Left rp, n) <- M.toList occur]
      vv =
        concatMap
          (\(vid, n) -> [T.intercalate "," ["v", T.pack (show i), T.pack (show vid)] | i <- [1 .. n]])
          [(vid, n) | (Right vid, n) <- M.toList occur]
   in mn ++ vv

-- r
mentionRTokens :: S.Set Name -> Type -> [Text]
mentionRTokens varNames ty =
  let ks = concreteNames varNames ty
      vs = varIds varNames ty
   in [token "mn" "r" (k <> ".") | k <- ks]
        ++ [T.intercalate "," ["v", "r", T.pack (show vid)] | vid <- vs]

-- effects
effTokens :: [Effect] -> [Text]
effTokens effs =
  let asyncTok = ["attr,async." | any isAsync effs]
      raiseTok = case mapMaybe raiseOf effs of
        [] -> []
        (r : _) -> case r of
          NoAraise -> []
          Araise ty -> ["e," <> revPathOfType ty <> "."]
          AraisePoly -> ["e,?"]
   in asyncTok ++ raiseTok
  where
    isAsync EffAsync = True
    isAsync _ = False
    raiseOf (EffException e) = Just e
    raiseOf _ = Nothing

-- name query: sym,<revpath-of-fn>.
symToken :: Maybe TPath -> TCon -> Text
symToken mp (TCon nm _) =
  case mp of
    Nothing -> "sym," <> tconOnly nm <> "."
    Just path -> "sym," <> revPath (Just path) (T.pack nm) <> "."

-- ==== normalise ====

-- extract tyvar
tyParamNames :: [(TCon, a)] -> S.Set Name
tyParamNames = S.fromList . map (\(TCon n _, _) -> n)

paramType :: FnParam -> Type
paramType (AnonParam _ t) = t
paramType (NamedParam _ _ t _ _) = t

-- Left revPathText for concrete tynamesl Right varId for tyvars
typeOccurs :: S.Set Name -> [Type] -> M.Map (Either Text Int) Int
typeOccurs varNames tys =
  let ks = concatMap (map Left . concreteNames varNames) tys
      vs = concatMap (map Right . varIds varNames) tys
   in M.fromListWith (+) [(k, 1) | k <- ks ++ vs]

-- collect rev paths
concreteNames :: S.Set Name -> Type -> [Text]
concreteNames varNames = go
  where
    go (TName mpath (TCon n args))
      | isVar n = concatMap go args
      | otherwise = revPath mpath (T.pack n) : concatMap go args
    go (TFun ps r _) = concatMap go ps ++ go r
    go (TTuple xs) = concatMap go xs
    go (TDynTrait _) = [] -- TODO: encode trait
    isVar n = n `S.member` varNames

-- collect normalised vars (noc asc + name)
varIds :: S.Set Name -> Type -> [Int]
varIds varNames ty =
  let vs = collectVarNames ty
      occ = M.fromListWith (+) [(v, 1 :: Int) | v <- vs]
      order =
        map fst $
          sortOn (\(v, cnt) -> (cnt :: Int, v)) $
            M.toList occ
      canon = M.fromList (zip order [1 ..])
   in mapMaybe (`M.lookup` canon) vs
  where
    collectVarNames = go
    go (TName _ (TCon n args))
      | n `S.member` varNames = n : concatMap go args
      | otherwise = concatMap go args
    go (TFun ps r _) = concatMap go ps ++ go r
    go (TTuple xs) = concatMap go xs
    go (TDynTrait _) = []

-- ==== rendering ====

-- reverse path: Con.Module.User
revPath :: Maybe TPath -> Text -> Text
revPath mp con =
  case mp of
    Nothing -> con
    Just (TPath pkg modn) ->
      let segs = map T.pack pkg ++ [T.pack modn, con]
       in T.intercalate "." (reverse segs) -- NOTE: we do not add trailing dot here

revPathOfType :: Type -> Text
revPathOfType (TName mp (TCon n _)) = revPath mp (T.pack n)
revPathOfType _ = "Unknown" -- IMPOSSIBLE

tconOnly :: Name -> Text
tconOnly = T.pack

token :: Text -> Text -> Text -> Text
token kind occ nameWithDot = T.intercalate "," [kind, occ, nameWithDot]

lexemesForFiles :: [MbtiFile] -> [[Text]]
lexemesForFiles = concatMap fileLexemes
  where
    fileLexemes :: MbtiFile -> [[Text]]
    fileLexemes (MbtiFile mp _ decls) =
      let mpath = Just (toTPath mp)
          fns = collectFns decls
       in map (lexemesForFnDecl mpath) fns

-- Collects:
--   * FnDecl f
--   * Method in Trait: TraitDecl ... [FnDecl']
--   TODO: add other sources
collectFns :: [Decl] -> [FnDecl']
collectFns = concatMap go
  where
    go :: Decl -> [FnDecl']
    go (FnDecl f) = [f]
    go (TraitDecl _ _ _ methods) = methods
    go _ = []

toTPath :: ModulePath -> TPath
toTPath ModulePath {..} =
  TPath (mpPackagePath ++ [mpUserName]) mpModuleName

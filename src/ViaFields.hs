{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module ViaFields (plugin) where

import qualified Data.Generics as G
#if __GLASGOW_HASKELL__ >= 902
import Data.Void
#endif

import GHC.Hs
#if __GLASGOW_HASKELL__ >= 902
import GHC.LanguageExtensions.Type
#endif
#if __GLASGOW_HASKELL__ < 902
import GHC.Parser.Annotation
#endif
import GHC.Plugins
import GHC.Types.Name.Occurrence as NS
#if __GLASGOW_HASKELL__ >= 902
import GHC.Types.SourceText
#endif

--------------------------------------------------------------------------------

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = \_opts _summary -> pure . desugarMod
  , pluginRecompile = purePlugin
#if __GLASGOW_HASKELL__ >= 902
  , driverPlugin = \_opts env -> pure $ env
    { hsc_dflags = xopt_set (hsc_dflags env) PatternSynonyms }
#endif
  }

--------------------------------------------------------------------------------

desugarMod :: HsParsedModule -> HsParsedModule
desugarMod hpm@(HsParsedModule { hpm_module = L l hm@(HsModule
    { hsmodImports = imports
    , hsmodDecls = decls
    }) }) =
  let decls' = uncurry (++) $ G.everywhereM (G.mkM desugarData) decls
  in hpm { hpm_module = L l $ hm
   { hsmodImports = extraImports ++ imports
   , hsmodDecls = decls'
   } }

desugarData :: TyClDecl GhcPs -> ([LHsDecl GhcPs], TyClDecl GhcPs)
desugarData decl@DataDecl{ tcdLName = tc, tcdTyVars = tvs } =
  let ((decls, pats), decl') = G.everywhereM (G.mkM $ desugarCon tc tvs) decl
      complete = if null decls then [] else [makeComplete pats]
  in (complete ++ decls, decl')
desugarData decl = ([], decl)

desugarCon
  :: LIdP GhcPs -> LHsQTyVars GhcPs
  -> ConDecl GhcPs -> (([LHsDecl GhcPs], [LIdP GhcPs]), ConDecl GhcPs)
desugarCon tc tvs con =
  let (args, con')
        =   G.everywhereM (G.mkM desugarArg)
        =<< G.everywhereM (G.mkM desugarField) con
      anyVia = any ca_via args
      con'' = if anyVia then mangleFields $ modifyConNames mangle con' else con'
      names = conNames con
      pats = if anyVia then makePats tc tvs names (conPatArgs con) args else []
  in ((pats, names), con'')

desugarField :: ConDeclField GhcPs -> ([ConArg], ConDeclField GhcPs)
desugarField field@(ConDeclField { cd_fld_names = names, cd_fld_type = ty }) =
  let mkArgs ty' via = ConArg ty' via <$ names in
  case splitVia ty of
    Nothing -> (mkArgs ty False, field)
    Just (ty', tyVia) ->
      ( mkArgs ty' True
      , field { cd_fld_type = tyVia }
      )

desugarArg
  :: HsScaled GhcPs (LBangType GhcPs)
  -> ([ConArg], HsScaled GhcPs (LBangType GhcPs))
desugarArg (HsScaled arr ty) =
  case splitVia ty of
    Nothing -> ([ConArg ty False], HsScaled arr ty)
    Just (ty', tyVia) -> ([ConArg ty' True], HsScaled arr tyVia)

--------------------------------------------------------------------------------

makeComplete :: [LIdP GhcPs] -> LHsDecl GhcPs
makeComplete pats = gen . SigD nef $
  CompleteMatchSig eanu NoSourceText (genL pats) Nothing

#if __GLASGOW_HASKELL__ < 902
type HsConDetails' = HsConDetails
type HsPatSynDetails' pass =
  HsConDetails' (LIdP pass) [RecordPatSynField (LIdP pass)]
#else
type HsConDetails' = HsConDetails Void
type HsPatSynDetails' pass = HsConDetails' (LIdP pass) [RecordPatSynField pass]
#endif

#if __GLASGOW_HASKELL__ < 902
pattern PrefixCon' :: [arg] -> HsConDetails arg rec
pattern PrefixCon' args = PrefixCon args
#else
pattern PrefixCon' :: [arg] -> HsConDetails tyarg arg rec
pattern PrefixCon' args <- PrefixCon _ args
  where PrefixCon' args = PrefixCon [] args
#endif

{-# COMPLETE PrefixCon', RecCon, InfixCon #-}

makePats
  :: LIdP GhcPs -> LHsQTyVars GhcPs
  -> [LIdP GhcPs] -> HsPatSynDetails' GhcPs -> [ConArg] -> [LHsDecl GhcPs]
makePats tc tvs cons dets args
  = ( gen . SigD nef . PatSynSig eanu cons
#if __GLASGOW_HASKELL__ < 902
    . HsIB nef
#else
    . gen . HsSig nef (HsOuterImplicit nef)
#endif
    . gen $ makePatType tc tvs args )
  : map (\con -> makePat con dets args) cons

makePatType :: LIdP GhcPs -> LHsQTyVars GhcPs -> [ConArg] -> HsType GhcPs
makePatType tc
  = foldr
     (\a -> HsFunTy eanu (HsUnrestrictedArrow NormalSyntax) (ca_type a)
          . gen)
  . foldl
      (\f -> HsAppTy nef (gen f) . gen
           . HsTyVar eanu NotPromoted
           . bndrVar . unLoc)
      (HsTyVar eanu NotPromoted tc)
  . hsq_explicit

makePat :: LIdP GhcPs -> HsPatSynDetails' GhcPs -> [ConArg] -> LHsDecl GhcPs
makePat con dets args
  = gen . ValD nef . PatSynBind nef
  $ PSB eanu con dets
    (gen . ConPat eanu (mangle con) . PrefixCon' $ zipWith
      (\(ConArg { ca_via = via }) var -> gen $
        let varPat = VarPat nef var in
        if via
        then ConPat eanu coerced $ PrefixCon' [gen varPat]
        else varPat) args vars)
    ImplicitBidirectional

--------------------------------------------------------------------------------

data ConArg = ConArg
  { ca_type :: LBangType GhcPs
  , ca_via :: Bool
  }

conNames :: ConDecl GhcPs -> [LIdP GhcPs]
conNames (ConDeclGADT { con_names = names }) = names
conNames (ConDeclH98 { con_name = name }) = [name]

modifyConNames :: (LIdP GhcPs -> LIdP GhcPs) -> ConDecl GhcPs -> ConDecl GhcPs
modifyConNames f con@(ConDeclGADT { con_names = names }) =
  con { con_names = map f names }
modifyConNames f con@(ConDeclH98 { con_name = name }) =
  con { con_name = f name }

#if __GLASGOW_HASKELL__ < 902
type HsConDeclH98Details pass = HsConDeclDetails pass
#else
gadtToH98 :: HsConDeclGADTDetails GhcPs -> HsConDeclH98Details GhcPs
gadtToH98 (PrefixConGADT args) = PrefixCon [] args
gadtToH98 (RecConGADT fields) = RecCon fields
#endif

conPatArgs' :: HsConDeclH98Details GhcPs -> HsPatSynDetails' GhcPs
conPatArgs' (PrefixCon' args) = PrefixCon' $ zipWith const vars args
conPatArgs' (RecCon (L _ fields)) = RecCon
  $ zipWith (RecordPatSynField . nameField . unLoc)
    (concatMap (cd_fld_names . unLoc) fields) vars
  where
#if __GLASGOW_HASKELL__ < 902
    nameField = rdrNameFieldOcc
#else
    nameField = id
#endif
conPatArgs' (InfixCon _ _) = InfixCon (vars!!0) (vars!!1)

conPatArgs :: ConDecl GhcPs -> HsPatSynDetails' GhcPs
#if __GLASGOW_HASKELL__ < 902
conPatArgs = conPatArgs' . con_args
#else
conPatArgs (ConDeclGADT { con_g_args = args }) = conPatArgs' $ gadtToH98 args
conPatArgs (ConDeclH98 { con_args = args }) = conPatArgs' args
#endif

vars :: [LIdP GhcPs]
vars = gen . mkUnqual NS.varName . mangleFS . fsLit . show <$> [(0::Int)..]

mangle :: G.Data a => a -> a
mangle = G.everywhere (G.mkT mangleOcc)

mangleFields :: G.Data a => a -> a
mangleFields = G.everywhere (G.mkT (mangle @(FieldOcc GhcPs)))

-- NB: This is needed because OccName's Data instance makes it opaque
mangleOcc :: OccName -> OccName
mangleOcc occ = mkOccNameFS (occNameSpace occ) . mangleFS $ occNameFS occ

mangleFS :: FastString -> FastString
mangleFS = ("_via-fields$" Prelude.<>)

bndrVar :: HsTyVarBndr flag GhcPs -> LIdP GhcPs
bndrVar (UserTyVar _ _ var) = var
bndrVar (KindedTyVar _ _ var _) = var

--------------------------------------------------------------------------------

mkImport :: ModuleName -> LImportDecl GhcPs
mkImport modName = gen $ ImportDecl
  { ideclExt = eanu
  , ideclSourceSrc = NoSourceText
  , ideclName = genL modName
  , ideclPkgQual = Nothing
  , ideclSource = NotBoot
  , ideclSafe = False
  , ideclQualified = QualifiedPre
  , ideclImplicit = True
  , ideclAs = Nothing
  , ideclHiding = Nothing
  }

extraImports :: [LImportDecl GhcPs]
extraImports = [mkImport util]

util :: ModuleName
util = mkModuleName "ViaFields.Util"

coerced :: LIdP GhcPs
coerced = gen $ mkQual dataName ("ViaFields.Util", "Coerced")

viaFS :: FastString
viaFS = "via"

isVia :: LHsType GhcPs -> Bool
isVia (L _ (HsTyVar _ NotPromoted (L _ (Unqual via)))) = occNameFS via == viaFS
isVia _ = False

splitVia :: LHsType GhcPs -> Maybe (LHsType GhcPs, LHsType GhcPs)
splitVia ty
  | let (f, xs) = splitHsAppTys ty
  , (lov, _via:g:rov) <- break isVia xs
  = Just (hsAppTys f lov, hsAppTys g rov)
splitVia _ = Nothing

splitHsAppTys :: LHsType GhcPs -> (LHsType GhcPs, [LHsType GhcPs])
splitHsAppTys (L _ (HsAppTy _ f t)) = fmap (++ [t]) $ splitHsAppTys f
splitHsAppTys (L _ (HsParTy _ t)) = splitHsAppTys t
splitHsAppTys ty = (ty, [])

hsAppTys :: LHsType GhcPs -> [LHsType GhcPs] -> LHsType GhcPs
hsAppTys f = gen . HsParTy eanu . foldl app f

app :: LHsType GhcPs -> LHsType GhcPs -> LHsType GhcPs
app f x = gen $ HsAppTy nef f x

--------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ < 902
gen :: a -> Located a
gen = genL
#else
gen :: a -> GenLocated (SrcSpanAnn' (EpAnn ann)) a
gen = L $ SrcSpanAnn EpAnnNotUsed genSrcSpan
#endif

genL :: a -> Located a
genL = L genSrcSpan

genSrcSpan :: SrcSpan
genSrcSpan = mkGeneralSrcSpan "<generated by via-fields>"

--------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ < 902
eanu :: NoExtField
eanu = noExtField
#else
eanu :: EpAnn a
eanu = EpAnnNotUsed
#endif

nef :: NoExtField
nef = noExtField

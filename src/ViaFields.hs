{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ViaFields (plugin) where

import qualified Data.Generics as G

import GHC.Hs
import GHC.LanguageExtensions.Type
import GHC.Plugins
import GHC.Types.Name.Occurrence as NS
import GHC.Types.SourceText

--------------------------------------------------------------------------------

plugin :: Plugin
plugin = defaultPlugin
  { driverPlugin = \_opts -> pure . addExts
  , parsedResultAction = \_opts _summary -> pure . desugarMod
  , pluginRecompile = purePlugin
  }

addExts :: HscEnv -> HscEnv
addExts env@(HscEnv { hsc_dflags = dflags }) =
  env { hsc_dflags = xopt_set dflags PatternSynonyms }

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
makeComplete pats = gen . SigD noExtField $
  CompleteMatchSig EpAnnNotUsed NoSourceText (genL pats) Nothing

makePats
  :: LIdP GhcPs -> LHsQTyVars GhcPs
  -> [LIdP GhcPs] -> HsPatSynDetails GhcPs -> [ConArg] -> [LHsDecl GhcPs]
makePats tc tvs cons dets args
  = ( gen . SigD noExtField . PatSynSig EpAnnNotUsed cons
    . gen . HsSig noExtField (HsOuterImplicit noExtField)
    . gen $ makePatType tc tvs args )
  : map (\con -> makePat con dets args) cons

makePatType :: LIdP GhcPs -> LHsQTyVars GhcPs -> [ConArg] -> HsType GhcPs
makePatType tc
  = foldr
     (\a -> HsFunTy EpAnnNotUsed (HsUnrestrictedArrow NormalSyntax) (ca_type a)
          . gen)
  . foldl
      (\f -> HsAppTy noExtField (gen f) . gen
           . HsTyVar EpAnnNotUsed NotPromoted
           . bndrVar . unLoc)
      (HsTyVar EpAnnNotUsed NotPromoted tc)
  . hsq_explicit

makePat :: LIdP GhcPs -> HsPatSynDetails GhcPs -> [ConArg] -> LHsDecl GhcPs
makePat con dets args
  = gen . ValD noExtField . PatSynBind noExtField
  $ PSB EpAnnNotUsed con dets
    (gen . ConPat EpAnnNotUsed (mangle con) . PrefixCon [] $ zipWith
      (\(ConArg { ca_via = via }) var -> gen $
        let varPat = VarPat noExtField var in
        if via
        then ConPat EpAnnNotUsed coerced $ PrefixCon [] [gen varPat]
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

gadtToH98 :: HsConDeclGADTDetails GhcPs -> HsConDeclH98Details GhcPs
gadtToH98 (PrefixConGADT args) = PrefixCon [] args
gadtToH98 (RecConGADT fields) = RecCon fields

conPatArgs' :: HsConDeclH98Details GhcPs -> HsPatSynDetails GhcPs
conPatArgs' (PrefixCon _ args) = PrefixCon [] $ zipWith const vars args
conPatArgs' (RecCon (L _ fields)) = RecCon $
  zipWith (RecordPatSynField . unLoc)
  (concatMap (cd_fld_names . unLoc) fields) vars
conPatArgs' (InfixCon _ _) = InfixCon (vars!!0) (vars!!1)

conPatArgs :: ConDecl GhcPs -> HsPatSynDetails GhcPs
conPatArgs (ConDeclGADT { con_g_args = args }) = conPatArgs' $ gadtToH98 args
conPatArgs (ConDeclH98 { con_args = args }) = conPatArgs' args

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
  { ideclExt = EpAnnNotUsed
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
hsAppTys f = gen . HsParTy EpAnnNotUsed . foldl app f

app :: LHsType GhcPs -> LHsType GhcPs -> LHsType GhcPs
app f x = gen $ HsAppTy noExtField f x

--------------------------------------------------------------------------------

gen :: a -> GenLocated (SrcSpanAnn' (EpAnn ann)) a
gen = L genSrcSpanAnn

genL :: a -> Located a
genL = L genSrcSpan

genSrcSpanAnn :: SrcSpanAnn' (EpAnn ann)
genSrcSpanAnn = SrcSpanAnn EpAnnNotUsed genSrcSpan

genSrcSpan :: SrcSpan
genSrcSpan = mkGeneralSrcSpan "<generated by via-fields>"

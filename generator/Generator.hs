{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Generator where

import Prelude hiding (const, mod)

import Data.Char (isLower, toUpper)
import Data.List (find, intercalate)
import Data.Maybe (isJust, maybeToList)
import Language.Haskell.Exts.SrcLoc (noLoc)
import qualified Language.Haskell.Exts.Syntax as S

import Types

typeDecl :: Identifier -> DefinitionType -> S.Decl
typeDecl ident d = S.TypeDecl noLoc (S.Ident ident) [] (toType d)

dataOrRec :: Identifier -> [Identifier] -> [S.ConDecl] -> S.Decl
dataOrRec ident derivs cds = S.DataDecl noLoc
                                        S.DataType
                                        context
                                        (S.Ident ident)
                                        tyVarBind
                                        (map qualConDecl cds)
                                        (map mkDeriving derivs)
  where
    tyVarBind = []
    context   = []
    qualConDecl  = S.QualConDecl noLoc tyVarBind' context'
      where tyVarBind'    = []
            context'      = []
    mkDeriving d = (S.UnQual . S.Ident $ d, types)
      where types         = []

recDecl :: Identifier -> [Identifier] -> [Field] -> S.Decl
recDecl ident derivs fields = dataOrRec ident derivs [conDecl]
  where
    conDecl           = S.RecDecl (S.Ident ident) (map mkField fields)
    mkField Field{..} = ( [S.Ident ('_':_fieldName)]
                        , S.UnBangedTy (toType _fieldType)
                        )

dataDecl :: Identifier -> [Identifier] -> [FieldType] -> S.Decl
dataDecl ident derivs ftypes = dataOrRec ident derivs [conDecl]
  where
    conDecl = S.ConDecl (S.Ident ident) types
    types   = map (S.UnBangedTy . toType) ftypes

mapType :: String -> FieldType -> FieldType -> S.Type
mapType m k v = S.TyApp (S.TyApp (toType m) (toType k)) (toType v)

setType :: String -> FieldType -> S.Type
setType s a = S.TyApp (toType s) (toType a)

listType :: FieldType -> S.Type
listType = S.TyList . toType

class Typeable a where
    toType :: a -> S.Type

instance Typeable a => Typeable (Maybe a) where
    toType Nothing  = toType "()"
    toType (Just x) = toType x

instance Typeable FieldType where
    toType (ContainerType x) = toType x
    toType (BaseType x)      = toType x
    toType (Identifier x)    = toType x

instance Typeable ContainerType where
    toType (MapType _ (k, v)) = mapType "HashMap" k v
    toType (SetType _ t)      = setType "HashSet" t
    toType (ListType t _)     = listType t

instance Typeable DefinitionType where
    toType (Left a)  = toType a
    toType (Right b) = toType b

instance Typeable String where
    toType t = if isLower . head . mod $ t
               then S.TyVar . S.Ident . mod $ t
               else S.TyCon . S.UnQual . S.Ident . mod $ t
      where mod "binary" = "ByteString"
            mod "bool"   = "Bool"
            mod "byte"   = "Char"
            mod "double" = "Double"
            mod "i16"    = "Int16"
            mod "i32"    = "Int32"
            mod "i64"    = "Int64"
            mod "string" = "String"
            mod t'       = t'

toExp :: ConstValue -> S.Exp
toExp (ConstLiteral s)        = S.Lit . S.String $ s
toExp (ConstIdentifier s)     = S.Var . S.UnQual . S.Ident $ s
toExp (ConstNumber (Left i))  = S.Lit . S.Int $ i
toExp (ConstNumber (Right d)) = S.Lit . S.Frac . toRational $ d
toExp (ConstList cs)          = S.List . map toExp $ cs
toExp (ConstMap ps)           = S.List . map tuplize $ ps
  where tuplize (a, b) = S.Tuple S.Boxed (map toExp [a,b])

patBind :: Identifier -> S.Exp -> S.Decl
patBind ident exp = S.PatBind noLoc
                                (S.PVar . S.Ident $ ident)
                                Nothing
                                (S.UnGuardedRhs exp)
                                (S.BDecls bindingGroup)
  where bindingGroup = []

typeSig :: Identifier -> S.Type -> S.Decl
typeSig ident = S.TypeSig noLoc [S.Ident ident]

classDecl :: Identifier -> Maybe Parent -> [Function] -> S.Decl
classDecl ident p fs = S.ClassDecl noLoc
                                   context
                                   (S.Ident ident)
                                   [S.UnkindedVar (S.Ident "a")]
                                   funDep
                                   (map (S.ClsDecl . sig) fs)
  where
    classA           = (`S.ClassA` [S.TyVar . S.Ident $ "a"]) . S.UnQual . S.Ident
    context          = map classA (maybeToList p)
    funDep           = []
    sig Function{..} = let types = (map getType _fnFields) ++ [(toType _fnType)]
                        in typeSig _fnName (foldr1 S.TyFun types)
      where getType Field{..} = toType _fieldType

class Generator a where
    gen :: a -> [S.Decl]

instance Generator Definition where
    gen (Typedef t ident) = [typeDecl ident t]
    gen (Const t ident val) = [typeSig ident (toType t), patBind ident (toExp val)]
    gen (Struct ident fields) =
        [ recDecl ident ["Show"] fields
        , instDecl "Binary" ident []
        ]
    gen (Enum ident maps) =
        [ dataDecl ident ["Enum", "Show"] $ map (Identifier . fst) maps
        , instDecl "Binary" ident []
        ]
    gen (Exception ident fields) =
        [ recDecl ident ["Show"] fields
        , instDecl "Binary" ident []
        ]
    gen (Service _ _ funcs) = concatMap funcDecl funcs
    gen _ = []

funcDecl :: Function -> [S.Decl]
funcDecl Function{..} =
    [ dataDecl ident ["Show"] (types _fnFields)
    , instDecl "Binary" ident [ S.InsDecl $ patBind "get" get
                              , S.InsDecl $ funBind "put" [putLhs] put
                              ]
    , instDecl "HasName" ident [S.InsDecl $ funBind "nameOf" [S.PWildCard] nameOf]
    ]
  where
    con = S.Con    . S.UnQual . S.Ident
    op  = S.QVarOp . S.UnQual . S.Symbol
    var = S.Var    . S.UnQual . S.Ident
    get = S.InfixApp (S.InfixApp (con "Echo") (op ".") (var "third"))
                     (op "<$>") (var "getField")
    put = S.InfixApp (S.App (S.Var (qName "struct"))
                            (S.Paren (S.App (S.Var . qName $ "nameOf") (S.Var . qName $ "x"))))
         (op "$")
         (S.App (S.Var . qName $ "putField")
                 (S.Tuple S.Boxed [ S.Lit (S.Int 1)
                                  , S.Lit (S.String "message")
                                  , S.Var (qName "message")
                                  ]))
    putLhs = S.PAsPat (S.Ident "x") (S.PRec (qName _fnName) [S.PFieldWildcard])
    ident = capitalize _fnName
    types = map (\Field{..} -> _fieldType)
    nameOf = S.Lit . S.String $ _fnName

funBind ident lhs exp = S.FunBind [ S.Match noLoc
                                            (S.Ident ident)
                                            lhs
                                            Nothing
                                            (S.UnGuardedRhs exp)
                                            (S.BDecls [])
                        ]

qName :: Identifier -> S.QName
qName = S.UnQual . S.Ident

instDecl :: Identifier -> Identifier -> [S.InstDecl] -> S.Decl
instDecl c i ds = S.InstDecl noLoc [] (qName c) [toType i] ds

capitalize :: String -> String
capitalize [] = []
capitalize (a:bs) = toUpper a:bs

importDecl :: String -> Maybe String -> (Bool,[S.ImportSpec]) -> S.ImportDecl
importDecl name alias spec =
    S.ImportDecl noLoc
                 (S.ModuleName name)
                 (isJust alias)
                 False
                 Nothing
                 (fmap S.ModuleName alias)
                 (mkSpec spec)
  where
    mkSpec (_,[]) = Nothing
    mkSpec s      = Just s

specifying :: [String] -> (Bool, [S.ImportSpec])
specifying s = (False, map (S.IAbs . S.Ident) s)

generate :: Document -> S.Module
generate (Document heads defs) =
    S.Module noLoc
             (S.ModuleName ns)
             [ S.LanguagePragma noLoc [S.Ident "OverloadedStrings"]
             , S.LanguagePragma noLoc [S.Ident "RecordWildCards"]
             , S.LanguagePragma noLoc [S.Ident "TypeSynonymInstances"]
             ]
             Nothing
             Nothing
             [ importDecl "Data.Binary" Nothing (specifying ["Binary"])
             , importDecl "Thrift" Nothing (specifying [])
             , importDecl "Vintage.Protocol.Binary" Nothing (specifying [])
             ]
             (concatMap gen defs)
  where
    ns      = intercalate "." (maybeToList nsDef ++ ["GenTypes"])
    nsDef   = fmap (\(Namespace _ n) -> n) (find isNs heads)
      where isNs (Namespace "hs" _) = True
            isNs _ = False

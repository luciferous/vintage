{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Generator where

import Prelude hiding (const, mod)

import Data.Char (isLower)
import Data.Maybe (maybeToList)
import Language.Haskell.Exts.Pretty (prettyPrint)
import Language.Haskell.Exts.SrcLoc (noLoc)
import qualified Language.Haskell.Exts.Syntax as S

import Types

typeDecl :: Identifier -> DefinitionType -> S.Decl
typeDecl ident d = S.TypeDecl noLoc (S.Ident ident) [] (toType d)

recDecl :: Identifier -> [Field] -> S.Decl
recDecl ident fields = dataDeclHelper ident deriv [qualConDecl]
  where
    deriv       = []
    qualConDecl = S.QualConDecl noLoc
                                tyVarBind'
                                context'
                                conDecl
      where
        tyVarBind'        = []
        context'          = []
        conDecl           = S.RecDecl (S.Ident ident) (map mkField fields)
        mkField Field{..} = ( [S.Ident ("_" ++ _fieldName)]
                            , S.UnBangedTy (toType _fieldType)
                            )

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

patBind :: Identifier -> ConstValue -> S.Decl
patBind ident val = S.PatBind noLoc
                                (S.PVar . S.Ident $ ident)
                                Nothing
                                (S.UnGuardedRhs . toExp $ val)
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

dataDeclHelper :: Identifier -> [S.Deriving] -> [S.QualConDecl] -> S.Decl
dataDeclHelper ident deriv qcds = S.DataDecl noLoc
                                       S.DataType
                                       context
                                       (S.Ident ident)
                                       tyVarBind
                                       qcds
                                       deriv
  where
    context   = []
    tyVarBind = []

enumDecl :: Identifier -> [(Identifier, Maybe Integer)] -> S.Decl
enumDecl ident = dataDeclHelper ident [deriv] . map (qualConDecl . fst)
  where
    deriv       = (S.UnQual . S.Ident $ "Enum", [])
    qualConDecl = (S.QualConDecl noLoc tyVarBind context) . conDecl
      where tyVarBind = []
            context   = []
            conDecl   = (`S.ConDecl` []) . S.Ident

class Generator a where
    gen :: a -> [S.Decl]

instance Generator Definition where
    gen (Typedef t ident) = [typeDecl ident t]
    gen (Struct ident fields) = [recDecl ident fields]
    gen (Const t ident val) = [typeSig ident (toType t), patBind ident val]
    gen (Service ident parent funcs) = [classDecl ident parent funcs]
    gen (Enum ident maps) = [enumDecl ident maps]
    gen (Exception ident fields) = [recDecl ident fields]

    gen x = [typeDecl "unknown:" (Left . show $ x)]

generate :: Document -> String
generate (Document _ defs) = prettyPrint mod
  where mod  = S.Module noLoc
                        (S.ModuleName "Types")
                        []
                        Nothing
                        Nothing
                        []
                        ((concatMap gen) . (filter predicate) $ defs)
        --predicate (Typedef _ _)   = True
        --predicate (Struct _ _)    = True
        --predicate (Const _ _ _)   = True
        --predicate (Service _ _ _) = True
        --predicate (Enum _ _)      = False
        --predicate (Exception _ _) = True
        predicate _ = True

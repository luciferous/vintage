module Types where

type Identifier = String

type Scope = String

data Document   = Document [Header] [Definition]
                  deriving Show

data Header     = Include String
                | CppInclude String
                | Namespace Scope Identifier
                  deriving Show

data Definition = Const FieldType Identifier ConstValue
                | Typedef DefinitionType Identifier
                | Enum Identifier [(Identifier, Maybe Integer)]
                | Senum Identifier [String]
                | Struct Identifier [Field]
                | Exception Identifier [Field]
                | Service Identifier (Maybe Parent) [Function]
                  deriving Show

type Parent     = Identifier

data Field      = Field { _fieldId    :: Maybe Integer
                        , _fieldReq   :: Maybe Bool
                        , _fieldType  :: FieldType
                        , _fieldName  :: Identifier
                        , _fieldValue :: Maybe ConstValue
                        } deriving Show

data Function   = Function { _fnOneway :: Maybe Bool
                           , _fnType   :: Maybe FieldType
                           , _fnName   :: Identifier
                           , _fnFields :: [Field]
                           , _fnThrows :: [Field]
                           } deriving Show

data FieldType = Identifier Identifier
               | BaseType BaseType
               | ContainerType ContainerType
                 deriving Show

type DefinitionType = Either BaseType ContainerType

type BaseType = String

data ConstValue = ConstNumber (Either Integer Double)
                | ConstLiteral String
                | ConstIdentifier Identifier
                | ConstList [ConstValue]
                | ConstMap [(ConstValue, ConstValue)]
                  deriving Show

type CppType = String

data ContainerType = MapType (Maybe CppType) (FieldType, FieldType)
                   | SetType (Maybe CppType) FieldType
                   | ListType FieldType (Maybe CppType)
                     deriving Show

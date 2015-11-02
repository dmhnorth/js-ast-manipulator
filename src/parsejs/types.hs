{-# LANGUAGE DeriveGeneric #-}

module ParseJS.Types where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Text (pack, Text)
import Data.Maybe
import Data.Foldable
import GHC.Generics


data Element = Literal Text deriving (Show, Generic)
data Identifier = Identifier Text deriving (Show, Generic)
data Property = Property Argument Argument deriving (Show, Generic)
data Argument = ElementArgument Element
              | IdentifierArgument Identifier
              | ExpressionArgument Expression
              | UnknownArgument Text
              deriving (Show, Generic)
data Expression = CallExpression Argument [Argument]
                | NewExpression Argument [Argument]
                | ArrayExpression [Argument]
                | MemberExpression Argument Identifier
                | AssignmentExpression Expression Expression
                | ThisExpression
                | FunctionExpression [Identifier] Statement
                | ObjectExpression [Property]
                | UnknownExpression Text
                deriving (Show, Generic)
data Statement = ExpressionStatement Expression
               | BlockStatement [Statement]
               | ReturnStatement Expression
               | UnknownStatement Text
               deriving (Show, Generic)
data Program = Program [Statement] deriving (Show, Generic)


selectArgument :: Object -> Parser Argument
selectArgument x = (IdentifierArgument <$> selectIdentifier x)
  <|> (ElementArgument <$> selectElement x)
  <|> (ExpressionArgument <$> selectExpression x)
  <|> (UnknownArgument <$> x .: pack "type")

selectProperty :: Object -> Parser Property
selectProperty x = Property <$> x .: pack "key" <*> x .: pack "value"

selectExpression :: Object -> Parser Expression
selectExpression x = key >>= flip selectExpression' x
  where key = x .: pack "type"

makeCall x = CallExpression <$> x .: pack "callee" <*> x .: pack "arguments"
makeNew x = NewExpression <$> x .: pack "callee" <*> x .: pack "arguments"
makeArray x = ArrayExpression <$> x .: pack "elements"
makeMember x = MemberExpression <$> x .: pack "object" <*> x .: pack "property"
makeThis x = return ThisExpression
makeAssignment x = AssignmentExpression <$> x .: pack "left" <*> x .: pack "right"
makeFunction x = FunctionExpression <$> x .: pack "params" <*> x .: pack "body"
makeObject x = ObjectExpression <$> x .: pack "properties"
makeUnknown x = UnknownExpression <$> x .: pack "type"

selectExpression' :: String -> Object -> Parser Expression
selectExpression' "CallExpression" = makeCall
selectExpression' "NewExpression" = makeNew
selectExpression' "MemberExpression" = makeMember
selectExpression' "ThisExpression" = makeThis
selectExpression' "AssignmentExpression" = makeAssignment
selectExpression' "FunctionExpression" = makeFunction
selectExpression' "ObjectExpression" = makeObject
selectExpression' _ = makeUnknown

makeExpressionStatement x = ExpressionStatement <$> x .: pack "expression"
makeBlockStatement x = BlockStatement <$> x .: pack "body"
makeReturnStatement x = ReturnStatement <$> x .: pack "argument"
unknownStatement x = UnknownStatement <$> x .: pack "type"

selectStatement :: Object -> Parser Statement
selectStatement x = makeExpressionStatement x
  <|> makeBlockStatement x
  <|> makeReturnStatement x
  <|> unknownStatement x

selectElement :: Object -> Parser Element
selectElement x = Literal <$> x .: pack "raw"

selectIdentifier :: Object -> Parser Identifier
selectIdentifier x = Identifier <$> x .: pack "name"

instance FromJSON Argument where
  parseJSON (Object x) = selectArgument x
  parseJSON _ = fail "expected Object"

instance ToJSON Argument where
  toJSON = genericToJSON defaultOptions

instance FromJSON Property where
  parseJSON (Object x) = selectProperty x
  parseJSON _ = fail "expected Object"

instance ToJSON Property where
  toJSON = genericToJSON defaultOptions

instance FromJSON Element where
  parseJSON (Object x) = selectElement x
  parseJSON _ = fail "Expected Object"

instance ToJSON Element where
  toJSON = genericToJSON defaultOptions

instance FromJSON Identifier where
  parseJSON (Object x) = selectIdentifier x
  parseJSON _ = fail "Expected Object"

instance ToJSON Identifier where
  toJSON = genericToJSON defaultOptions

instance FromJSON Statement where
  parseJSON (Object x) = selectStatement x
  parseJSON _ = fail "Expected Object"

instance ToJSON Statement where
  toJSON = genericToJSON defaultOptions

instance FromJSON Expression where
  parseJSON (Object x) = selectExpression x
  parseJSON x = fail $ show x

instance ToJSON Expression where
  toJSON = genericToJSON defaultOptions

instance FromJSON Program where
  parseJSON (Object x) = Program <$> x .: pack "body"
  parseJSON _ = fail "Expected Object"

instance ToJSON Program where
  toJSON = genericToJSON defaultOptions

findUnknowns :: Program -> [Text]
findUnknowns (Program s) = concatMap funcStatement s

funcStatement :: Statement -> [Text]
funcStatement (ExpressionStatement expression) = funcExpression expression
funcStatement (BlockStatement statements) = concatMap funcStatement statements
funcStatement (ReturnStatement expression) = funcExpression expression
funcStatement (UnknownStatement text) = [text]

funcExpression :: Expression -> [Text]
funcExpression (CallExpression arg args) = funcArguments (arg : args)
funcExpression (NewExpression arg args) = funcArguments (arg : args)
funcExpression (ArrayExpression args) = funcArguments args
funcExpression (MemberExpression arg ident) = []
funcExpression (AssignmentExpression expr1 expr2) = concatMap funcExpression [expr1, expr2]
funcExpression (ThisExpression) = []
funcExpression (FunctionExpression idents statement) = []
funcExpression (ObjectExpression props) = []
funcExpression (UnknownExpression text) = [text]

funcArguments :: [Argument] -> [Text]
funcArguments _ = []



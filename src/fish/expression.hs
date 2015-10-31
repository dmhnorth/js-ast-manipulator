{-# LANGUAGE DeriveGeneric #-}

module Fish.Expression (
  selectExpression
) where

import qualified Data.ByteString.Lazy as B
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import GHC.Generics

data Expression = CallExpression Identifier [Argument]
                | ArrayExpression [Argument]
                | FunctionExpression [Identifier] Statement
                | UnknownExpression Text
                deriving (Show, Generic)

makeCall x = CallExpression <$> x .: pack "callee" <*> x .: pack "arguments"
makeArray x = ArrayExpression <$> x .: pack "elements"
makeFunction x = FunctionExpression <$> x .: pack "params" <*> x .: pack "body"
makeUnknown x = UnknownExpression <$> x .: pack "type"

selectExpression :: Object -> Parser Expression
selectExpression x = makeCall x
  <|> makeArray x
  <|> makeFunction x
  <|> makeUnknown x


instance FromJSON Expression where
  parseJSON (Object x) = selectExpression x
  parseJSON x = fail $ show x

instance ToJSON Expression where
  toJSON = genericToJSON defaultOptions

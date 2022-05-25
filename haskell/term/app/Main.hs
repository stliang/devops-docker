{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Yaml as Aeson.Yaml
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp)

data Goal = 
  Goal { goalName :: T.Text
       , status :: T.Text
       , priority :: Int
       , value :: Int
       , funFactor :: Int
       , cost :: Int
       , goals :: [Goal]
         } deriving Show

-- We expect a JSON object, so we fail at any non-Object value.
instance FromJSON Goal where
    parseJSON (Object v) = Goal <$> v .: "goalName" <*> v .: "status"  <*> v .: "priority"  <*> v .: "value"  <*> v .: "funFactor"  <*> v .: "cost"  <*> v .: "goals" 
    parseJSON _ = empty

instance ToJSON Goal where
    toJSON (Goal goalName status priority value funFactor cost goals) = object ["goalName" .= goalName, "status" .= status, "priority" .= priority, "value" .= value, "funFactor" .= funFactor, "cost" .= cost, "goals" .= goals]

-- | Type of each JSON entry in record syntax.
data Person =
  Person { firstName  :: !T.Text
         , lastName   :: !T.Text
         , age        :: Int
         , likesPizza :: Bool
           } deriving (Show,Generic)

-- Instances to convert our type to/from JSON.

instance FromJSON Person

-- | Location of the local copy, in case you have it,
--   of the JSON file.
jsonFile :: FilePath
jsonFile = "pizza.json"

-- | Location of the local copy, in case you have it,
--   of the JSON file.
jsonGoal :: FilePath
jsonGoal = "goal.json"

-- | URL that points to the remote JSON file, in case
--   you have it.
jsonURL :: String
jsonURL = "http://daniel-diaz.github.io/misc/pizza.json"

-- Move the right brace (}) from one comment to another
-- to switch from local to remote.

{--}
-- Read the local copy of the JSON file.
getJSON :: IO LBS.ByteString
getJSON = LBS.readFile jsonGoal
--}

{--
-- Read the local copy of the JSON file.
getJSON :: IO LBS.ByteString
getJSON = LBS.readFile jsonFile
--}

{--
-- Read the remote copy of the JSON file.
getJSON :: IO LBS.ByteString
getJSON = simpleHttp jsonURL
--}

lsbToText :: LBS.ByteString -> T.Text
lsbToText = E.decodeUtf8 . LBS.toStrict

-- jsonToText :: Value -> T.Text
-- jsonToText = lsbToText . encodePretty

-- prettyPrint :: Value -> IO ()
-- prettyPrint = TIO.putStrLn . jsonToText

main :: IO ()
main = do
 -- Get JSON data and decode it
 -- d <- (eitherDecode <$> getJSON) :: IO (Either String [Person])
 d <- (eitherDecode <$> getJSON) :: IO (Either String Goal)
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 case d of
  Left err -> putStrLn err
  Right ps -> TIO.putStrLn $ lsbToText $ Aeson.Yaml.encode ps
  -- Right ps -> TIO.putStrLn $ lsbToText $ encodePretty ps

module Library where

import Data.Aeson (FromJSON (..))
import qualified Data.Aeson as JSON
import Data.Char ( toLower )
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS (newTlsManager)
import RIO
import Prelude (print)

data EDT = EDT 
  { edtTitle :: !Text
  , edtDescription :: !Text
  , edtStart :: !Text
  , edtEnd :: !Text
  }
  deriving (Eq, Show , Generic)

-- instance ToJSON EDT where
--   toJSON = JSON.genericToJSON $ jsonOptions "edt" 

instance FromJSON EDT where
  parseJSON  = JSON.genericParseJSON $ jsonOptions "edt"

-- instance FromJSON EDT where
--   parseJSON  = JSON.withObject "edt" $ \o -> do
--     titleString <- o .: "title"
--     descriptionString <- o .: "description"
--     startString <- o .: "start"
--     endString <- o .: "end"
--     let edtTitle = read titleString
--         edtDescription = read descriptionString
--         edtStart = read startString
--         edtEnd = read endString
--     pure EDT {edtTitle, edtDescription, edtStart, edtEnd}

getEDTContent :: IO LByteString 
getEDTContent = do
  manager <- newTlsManager 
  request <- HTTP.parseRequest "https://sos-salle.polytech-lille.fr/wsADE_events.php?promo=3YMHMH-120&start=2022-04-18&end=2022-04-25"
  HTTP.responseBody <$> HTTP.httpLbs request manager

getEDT :: IO (Either String [EDT])
getEDT = do
  manager <- newTlsManager 
  request <- HTTP.parseRequest "https://sos-salle.polytech-lille.fr/wsADE_events.php?promo=3YMHMH-120&start=2022-04-18&end=2022-04-25"
  (HTTP.responseBody >>> JSON.eitherDecode) <$> HTTP.httpLbs request manager

runMain :: IO ()
runMain = do
  -- get Json and decode it
  maybeData <- getEDT
  case maybeData of
    -- Right edts -> do
    --   let titles = map edtTitle edts
    --   print titles
    Right edt -> print edt
    Left e -> error e

jsonOptions :: String -> JSON.Options
jsonOptions prefix = 
  let prefixLength = length prefix
      lowercaseFirstCharacter (c : rest) = toLower c : rest
      lowercaseFirstCharacter [] = []
    in JSON.defaultOptions {JSON.fieldLabelModifier = drop prefixLength >>> lowercaseFirstCharacter}
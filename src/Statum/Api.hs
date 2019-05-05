module Statum.Api
    ( Widget(..)
    , Response(..)
    , Request
    , prepareRequest
    , sendRequest
    ) where



import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Dhall
import qualified GHC.Generics as GHC
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Types.Method as Method
import qualified Network.HTTP.Types.Status as Status
import qualified Statum.Widget.Meter as Meter
import qualified Statum.Widget.Number as Number


type BaseUrl = String

type Path = String

type WidgetId = String


data Widget
    = MeterWidget { widgetId :: String, meter :: Meter.Meter }
    | NumberWidget { widgetId :: String, number :: Number.Number }
    deriving (Show, GHC.Generic)


instance Dhall.Interpret Widget


newtype Request = Request Client.Request
    deriving (Show)


data Response = Response
    { responseBody :: LBS.ByteString
    , responseStatus :: Status.Status
    }
    deriving (Show, Eq)



prepareRequest :: BaseUrl -> Widget -> Request
prepareRequest baseUrl widget =
    case widget of
        MeterWidget widgetId body ->
            "/api/widgets/meter/" ++ widgetId
                & buildRequest body baseUrl


buildRequest :: Aeson.ToJSON body => body -> BaseUrl -> Path -> Request
buildRequest body baseUrl path =
    (Client.parseRequest_ $ baseUrl ++ path)
        { Client.method = "POST"
        , Client.requestBody = Client.RequestBodyLBS (Aeson.encode body)
        }
        & Request


sendRequest :: Client.Manager -> Request -> IO Response
sendRequest manager (Request req) = do
    res <- Client.httpLbs req manager
    return $ Response
        { responseBody = Client.responseBody res
        , responseStatus = Client.responseStatus res
        }

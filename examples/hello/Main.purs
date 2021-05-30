module Payload.Examples.Hello.Main where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Payload.Server (defaultOpts)
import Payload.Server as Payload
import Payload.Spec (Spec(Spec), GET)

type Message = 
  { id :: Int
  , text :: String }

spec :: Spec {
  routes :: {
    getMessages :: GET "/users/<id>/messages?limit=<limit>" {
      params :: { id :: Int },
      query :: { limit :: Int },
      response :: Array Message
    }
  },
  guards :: {}
}
spec = Spec

getMessages :: { params :: { id :: Int }, query :: { limit :: Int } } -> ReaderT String Aff (Array Message)
getMessages {params: {id}, query: {limit}} = do
  env <- ask
  pure [{ id: 1, text: "Hey " <> show id}, { id: 2, text: "Limit " <> show limit }, { id: 3, text: env }]

handlers ::
    { getMessages ::
        { params :: { id :: Int }
        , query :: { limit :: Int }
        } -> ReaderT String Aff (Array { id :: Int, text :: String })
    }
handlers = { getMessages }

runM :: ReaderT String Aff ~> Aff
runM = flip runReaderT "env"

main :: Effect Unit
main = launchAff_ $ Payload.startGuarded' runM defaultOpts spec { handlers, guards: {} }

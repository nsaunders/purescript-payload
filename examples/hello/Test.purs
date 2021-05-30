module Payload.Examples.Hello.Test where

import Prelude

import Control.Monad.Reader (runReaderT)
import Payload.Examples.Hello.Main (handlers, spec)
import Payload.Test.Helpers (respMatches, withServer')
import Payload.Test.Helpers as Helpers
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = do
  let withApi = withServer' (flip runReaderT "env") spec { handlers, guards: {} }
  suite "Hello world" do
    let { get } = Helpers.request "http://localhost:3000"
    test "GET /user/12/messages?limit=2 succeeds" $ withApi do
      res <- get "/users/1/messages?limit=2"
      respMatches
        { status: 200
        , body: "[{\"text\":\"Hey 1\",\"id\":1},{\"text\":\"Limit 2\",\"id\":2}]" }
        res
    test "GET /user/12/messages?limit=2.5 fails with 400 Bad Request" $ withApi do
      res <- get "/users/12/messages?limit=2.5"
      Assert.equal 400 res.status

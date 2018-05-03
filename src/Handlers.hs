module Handlers where

import           Servant

import           Api
import qualified App     as App

serveApp :: App.ConnectionPool -> App.Config -> Application
serveApp pool conf = serve api $ handler pool conf


handler :: App.ConnectionPool -> App.Config -> Server API
handler pool conf = App.run pool conf $
                       return NoContent

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.State
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.SqliteSimple
import Snap.Snaplet.Session

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _db   :: Snaplet Sqlite
    , _auth :: Snaplet (AuthManager App)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasSqlite (Handler b App) where
  getSqliteState = with db get

------------------------------------------------------------------------------
type AppHandler = Handler App App


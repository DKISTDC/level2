module App.Page.Tasks where

import App.Colors
import App.Route
import App.Style qualified as Style
import App.View.DataRow (cell, hd)
import App.View.DataRow qualified as View
import App.View.Layout
import NSO.Prelude
import Web.Atomic.CSS
import Web.Hyperbole


page
  :: (Hyperbole :> es)
  => Page es '[TasksTable]
page = do
  -- login <- loginUrl
  appLayout Dashboard $ do
    col ~ pad 20 . gap 20 $ do
      col ~ Style.card . gap 15 $ do
        el ~ Style.cardHeader Primary $ "Active Tasks"
        hyper TasksTable $ tasksTable []


data TasksTable = TasksTable
  deriving (Generic, ViewId)


instance HyperView TasksTable es where
  data Action TasksTable = Refresh
    deriving (Generic, ViewAction)


  update Refresh = do
    pure "WOOT"


tasksTable :: [String] -> View TasksTable ()
tasksTable ss = do
  table ss ~ View.table $ do
    tcol (hd "Id") $ \s -> cell $ text . cs $ s

module App.Colors where

import NSO.Prelude
import Web.UI

data AppColor
  = Green
  | GreenLight
  | GrayLight
  deriving (Show)

instance ToColor AppColor where
  colorValue Green = HexColor "080"
  colorValue GreenLight = HexColor "0F0"
  colorValue GrayLight = HexColor "DDD"

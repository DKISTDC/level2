module App.Colors where

import NSO.Prelude
import Web.UI

data AppColor
  = White
  | Light
  | GrayLight
  | GrayDark
  | Dark
  | Success
  | SuccessDark
  | Error
  | ErrorDark
  | Warning
  | Primary
  | PrimaryLight
  | Secondary
  | SecondaryLight
  deriving (Show)

instance ToColor AppColor where
  colorValue White = "#FFF"
  colorValue Light = "#F8F8F8"
  colorValue GrayLight = "#E3E5E9"
  colorValue GrayDark = "#666666"
  colorValue Dark = "#2E3842" -- "#232C41"
  colorValue Primary = "#2C74BB"
  colorValue PrimaryLight = "#3281cf"
  colorValue Secondary = "#5CADDB"
  colorValue SecondaryLight = "#BBD6FB"
  -- colorValue Success = "67C837"
  colorValue Success = "#D5E6DE"
  colorValue SuccessDark = "#71AA8F"
  colorValue Error = "#F3D8DA"
  colorValue ErrorDark = "#d95d66"
  colorValue Warning = "#FDF3D1"

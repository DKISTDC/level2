module App.Colors where

import NSO.Prelude
import Web.View


data AppColor
  = White
  | Black
  | Light
  | Primary
  | Secondary
  | Info
  | Success
  | Warning
  | Danger
  | Gray
  deriving (Show)


data Weight
  = Normal AppColor
  | Lighter AppColor
  | Darker AppColor
  deriving (Show)


-- Generate semantic theme: https://huemint.com/bootstrap-basic/ ("Normal" values came from here)
-- Generate weights for various colors: https://colors.eva.design (I used 300, 500 and 700)
hex :: Weight -> HexColor
hex (Lighter White) = "#FFF"
hex (Normal White) = "#FFF"
hex (Darker White) = "#FFF"
hex (Lighter Black) = "#0e0d11"
hex (Normal Black) = "#0e0d11"
hex (Darker Black) = "#0e0d11"
hex (Lighter Light) = "#F2F7FD"
hex (Normal Light) = "#d3dceb"
hex (Darker Light) = "#d3dceb"
hex (Lighter Primary) = "#6D9BD3"
hex (Normal Primary) = "#4171b7"
hex (Darker Primary) = "#204083"
hex (Lighter Secondary) = "#9D999C"
hex (Normal Secondary) = "#5D5A5C"
hex (Darker Secondary) = "#4F414C"
hex (Lighter Info) = "#E86D91"
hex (Normal Info) = "#DA407C"
hex (Darker Info) = "#BB2E73"
hex (Lighter Success) = "#43C478"
hex (Normal Success) = "#149e5a"
hex (Darker Success) = "#0E8758"
hex (Lighter Warning) = "#ECDC4B"
hex (Normal Warning) = "#e1c915"
hex (Darker Warning) = "#C1AA0F"
hex (Lighter Danger) = "#F55C44"
hex (Normal Danger) = "#ef1509"
hex (Darker Danger) = "#CD060C"
hex (Lighter Gray) = "#E8E8E9"
hex (Normal Gray) = "#D2D2D3"
hex (Darker Gray) = "#9999B5"


-- hex (Lighter Danger) = "#fdd9d7"

light :: AppColor -> Weight
light = Lighter


dark :: AppColor -> Weight
dark = Darker


class Contrast a where
  contrast :: a -> HexColor


instance Contrast AppColor where
  contrast White = colorValue Black
  contrast Black = colorValue White
  contrast Light = colorValue Black
  contrast Primary = colorValue White
  contrast Secondary = colorValue White
  contrast Info = colorValue White
  contrast Success = colorValue White
  contrast Warning = colorValue White
  contrast Danger = colorValue White
  contrast Gray = colorValue White


instance Contrast Weight where
  contrast (Normal c) = contrast c
  contrast (Lighter c) = contrast c
  contrast (Darker c) = contrast c


instance ToColor AppColor where
  colorValue c = hex (Normal c)


instance ToColor Weight where
  colorValue = hex
  colorName (Lighter c) = colorName c <> "-lgt"
  colorName (Normal c) = colorName c
  colorName (Darker c) = colorName c <> "-drk"

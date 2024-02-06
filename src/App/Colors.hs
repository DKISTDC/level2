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
  = Normal
  | Lighter
  | Darker


-- Generate semantic theme: https://huemint.com/bootstrap-basic/ ("Normal" values came from here)
-- Get weights for various colors: https://colors.eva.design (I used 300, 500 and 700)
weight :: AppColor -> Weight -> HexColor
weight White _ = "#FFF"
weight Black _ = "#0e0d11"
weight Light Lighter = "#F2F7FD"
weight Light _ = "#d3dceb"
weight Primary Lighter = "#6D9BD3"
weight Primary Normal = "#4171b7"
weight Primary Darker = "#204083"
weight Secondary Lighter = "#9D999C"
weight Secondary Normal = "#5D5A5C"
weight Secondary Darker = "#4F414C"
weight Info Lighter = "#E86D91"
weight Info Normal = "#DA407C"
weight Info Darker = "#BB2E73"
weight Success Lighter = "#43C478"
weight Success Normal = "#149e5a"
weight Success Darker = "#0E8758"
weight Warning Lighter = "#ECDC4B"
weight Warning Normal = "#e1c915"
weight Warning Darker = "#C1AA0F"
weight Danger Lighter = "#F55C44"
weight Danger Normal = "#ef1509"
weight Danger Darker = "#CD060C"
weight Gray Lighter = "#E8E8E9"
weight Gray Normal = "#D2D2D3"
weight Gray Darker = "#9999B5"


light :: AppColor -> HexColor
light c = weight c Lighter


dark :: AppColor -> HexColor
dark c = weight c Darker


contrast :: AppColor -> HexColor
contrast White = colorValue Black
contrast Black = colorValue White
contrast Light = colorValue Black
contrast Primary = colorValue White
contrast Secondary = colorValue White
contrast Info = colorValue White
contrast Success = colorValue White
contrast Warning = colorValue White
contrast Danger = colorValue White
contrast Gray = colorValue Black


instance ToColor AppColor where
  colorValue c = weight c Normal

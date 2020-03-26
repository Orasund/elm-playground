module Material.Widget exposing (Widget)

import Svg exposing (Svg)


type Media
    = ImageMedia String


type ButtonEmphasis
    = Low
    | Default -- can be removed
    | High


type TextButton msg
    = TextButton
        { emphasis : ButtonEmphasis
        , label : String
        , enabled : Bool
        , onClick : msg
        , icon : Maybe (Svg Never)
        }


type IconButton msg
    = IconButton
        { enabled : Bool
        , onClick : msg
        , icon : Svg Never
        }


type ToggleButton msg state
    = ToggleButton
        { options :
            List
                ( state
                , { enabled : Bool
                  , onClick : state -> msg
                  , icon : Svg Never
                  }
                )
        , selected : Maybe state
        }


type FAButton msg
    = FAButton
        { onClick : msg
        , icon : Svg Never
        , label : Maybe String
        }


type SpeedDial msg
    = SpeedDial
        { options : List (FAButton msg)
        , icon : Svg Never
        , label : Maybe String
        }


type CardHeader
    = CardHeader
        { title : String
        , subtitle : Maybe String
        , thumbnail : Maybe String
        }


type Card msg
    = Card
        { header : Maybe CardHeader
        , media : Maybe Media
        , text : Maybe String
        , textButtons : List (TextButton msg)
        }


type Widget msg state
    = TextButtonWidget (TextButton msg)
    | IconButtonWidget (IconButton msg)
    | ToggleButtonWidget (ToggleButton msg state)
    | FAButtonWidget (FAButton msg)
    | SpeedDialWidget (FAButton msg) -- can be removed
    | CardWidget (Card msg)

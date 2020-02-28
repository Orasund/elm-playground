module View.FilterSelect exposing (Model,Msg(..),init,update,viewInput, viewOptions)

import Element exposing (Attribute, Element)
import Element.Input as Input exposing (Placeholder )
import Set exposing (Set)

type alias Model =
    { raw : String
    , selected : Maybe String
    , options : Set String
    }

type Msg =
    ChangedRaw String
    | Selected (Maybe String)


init : Set String -> Model
init options =
    { raw = ""
    , selected = Nothing
    , options = options
    }

update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangedRaw string ->
            { model
            | raw = string
            }
        Selected maybe ->
            { model 
            | selected = maybe
            }
                |> case maybe of
                    Just string -> 
                        (\m -> { m | raw = string })
                    Nothing -> 
                        identity

viewInput : List (Attribute msg) -> Model 
    -> { msgMapper : Msg -> msg
       , placeholder : Maybe (Placeholder msg)
       , label : String
       }
       -> Element msg
viewInput attributes model {msgMapper,placeholder,label}=
    Input.text attributes
        { onChange = ChangedRaw >> msgMapper
        , text = model.raw
        , placeholder = placeholder
        , label = Input.labelHidden label
        }


viewOptions : Model -> List String
viewOptions { raw, options } =
    if raw == "" then
        []
    else
        options
        |> Set.filter (String.toUpper >> String.contains (raw |> String.toUpper))
        |> Set.toList
module View.FilterSelect exposing (Model,Msg,init,update,viewInput, viewOptions)

import Element exposing (Attribute, Element)
import Element.Input as Input
import Set exposing (Set)

type Model =
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
    , options = Set String
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
            | selected = Just string
            }
                |> case maybe of
                    Just string -> 
                        (\m -> { m | raw = string })
                    Nothing 
                        identity

viewInput : List (Attribute msg) -> Model 
    -> { msgMapper : Msg -> msg
       , placeholder : Maybe (Placeholder msg)
       , label : String
       }
       -> Element msg
viewInput attributes model =
    Input.text attributes
        { onChange = ChangedRaw
        , text = model.raw
        , placeholder = model.placeholder
        , label = Input.hiddenLabel model.label
        }


viewOptions : Model -> List String
viewOptions { selected, options } =
    options
        |> Set.filter
            (String.toUpper >> String.contains (selected |> String.toUpper))
        |> Set.toList
module View.ValidatedInput exposing (Model)

type alias Model err a =
    Model
        { raw : String
        , value : a
        , err : Maybe err
        , validator : String -> Error err a
        , toString : a -> String
        }

getRaw : Model err a -> String
getRaw (Model {raw}) =
    raw

getValue : Model err a -> a
getValue (Model {value}) =
    value

getError : Maybe err
getError (Model {err}) =
    err

type Msg
    = ChangedRaw String
    | LostFocus

init : { value : a, validator : String -> Error err a, toString : a -> String } -> Model a
init (Model { validator,toString,value }) =
    { raw = value |> toString
    , value = value
    , err = Nothing
    , validator = validator
    , toString = toString
    }

update : Msg -> Model -> Model
update msg (Model model) =
    case msg of
        ChangedRaw string ->
            Model
            { model
            | raw = string
            , err = Nothing
            }
        LostFocus ->
            case model.validator model.raw of
                Ok value ->
                    Model
                    { model
                    | value = value
                    , err = Nothing
                    }
                Err err ->
                    Model
                    { model
                    , raw = value |> model.toString
                    | err = Just err
                    }

view : List (Attribute msg) -> Model 
    -> { msgMapper : Msg -> msg
       , placeholder : Maybe (Placeholder msg)
       , label : String
       }
       -> Element msg
view attributes (Model model) {msgMapper,placeholder,label} =
    Input.text (attributes ++ [Events.onLoseFocus <| msgMapper <| LostFocus])
        { onChange = ChangedRaw
        , text = model.raw
        , placeholder = placeholder
        , label = Input.hiddenLabel label
        }
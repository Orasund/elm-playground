module Example.FlippableCard exposing (..)

import Game.Card
import Html exposing (Html)
import Html.Events
import View.Component


type alias Model =
    { isFlipped : Bool }


type Msg
    = Flip


init : Model
init =
    { isFlipped = False }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Flip ->
            { model | isFlipped = not model.isFlipped }


view : Model -> Html Msg
view model =
    Game.Card.flippable [ Html.Events.onClick Flip ]
        { front = \transform attrs -> View.Component.defaultCard ([ Game.Card.transform [ transform ] ] ++ attrs)
        , back = \transform attrs -> View.Component.defaultBack ([ Game.Card.transform [ transform ] ] ++ attrs)
        , faceUp = model.isFlipped
        }

module FactoryCity.State.Ready exposing (Model, Msg, init, update, view)

import Action
import Bag exposing (Bag)
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import FactoryCity.Data.Item exposing (Item(..))
import FactoryCity.State.Playing as PlayingState
import FactoryCity.View.Shop as Shop
import FactoryCity.View.Text as Text
import Framework.Button as Button
import Framework.Card as Card
import Framework.Grid as Grid
import Framework.Heading as Heading
import Random exposing (Seed)
import Time exposing (Month(..))



----------------------
-- Model
----------------------


type alias State =
    Bag String


type alias Model =
    ( State, Seed )


type Msg
    = ClickedStart Item


type alias Action =
    Action.Action Model Msg PlayingState.TransitionData Never



----------------------
-- Init
----------------------


initialState : Bag String -> State
initialState =
    identity


init : Bag String -> Seed -> ( Model, Cmd Msg )
init shop seed =
    ( ( initialState shop, seed ), Cmd.none )



----------------------
-- Update
----------------------
{- monthToInt : Month -> Int
   monthToInt month =
       case month of
           Jan ->
               1

           Feb ->
               2

           Mar ->
               3

           Apr ->
               4

           May ->
               5

           Jun ->
               6

           Jul ->
               7

           Aug ->
               8

           Sep ->
               9

           Oct ->
               10

           Nov ->
               11

           Dec ->
               12
-}


update : Msg -> Model -> Action
update msg ( shop, seed ) =
    case msg of
        ClickedStart item ->
            Action.transitioning
                { seed = seed
                , shop = shop
                , source = item
                }



----------------------
-- View
----------------------


viewMode : msg -> { title : String, desc : String } -> Element msg
viewMode msg { title, desc } =
    Input.button
        (Button.simple
            ++ Card.large
            ++ [ Font.family
                    [ Font.sansSerif ]
               , Element.centerX
               , Element.centerY
               , Font.color <| Element.rgb255 0 0 0
               ]
        )
    <|
        { onPress = Just msg
        , label =
            Element.column
                Grid.spaceEvenly
            <|
                [ Element.paragraph
                    (Heading.h2 ++ [ Element.centerX ])
                  <|
                    List.singleton <|
                        Element.text title
                , Element.paragraph [] <|
                    List.singleton <|
                        Element.text desc
                ]
        }


view :
    Float
    -> (Msg -> msg)
    -> Model
    -> ( Maybe ( Element msg, Element msg ), List (Element msg) )
view scale msgMapper ( shop, _ ) =
    ( Nothing
    , List.singleton <|
        Element.wrappedRow (Grid.simple ++ [ Element.height <| Element.fill ])
            [ Element.column (Grid.simple ++ [ Element.width <| Element.fill ])
                [ Element.row
                    (Grid.simple
                        ++ [ Element.width <| Element.shrink
                           , Element.centerY
                           ]
                    )
                    [ Text.view (round <| scale * 150) "🏭"
                    , Element.column
                        [ Font.size <| floor <| scale * 80
                        , Element.centerX
                        , Font.center
                        ]
                      <|
                        [ Element.text "Factory"
                        , Element.text "City"
                        ]
                    ]
                , Shop.view
                    { shop = shop
                    , buyMsg = Nothing
                    , sellMsg = Nothing
                    , money = 0
                    , deck = Bag.empty
                    }
                ]
            , Element.column
                (Grid.simple
                    ++ [ Element.centerY
                       , Element.centerX
                       , Element.width <| Element.fill
                       ]
                )
              <|
                [ viewMode
                    (msgMapper <| ClickedStart <| Wood)
                    { title = "Wood cutter"
                    , desc = "You get wood for free"
                    }
                , viewMode
                    (msgMapper <| ClickedStart <| Stone)
                    { title = "Miner"
                    , desc = "You get stone for free"
                    }
                ]
            ]
    )

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


type alias Model =
    { shop : Bag String
    , seed : Seed
    }


type Msg
    = ClickedStart Item


type alias Action =
    Action.Action Model Msg PlayingState.TransitionData Never



----------------------
-- Init
----------------------


init : Model -> ( Model, Cmd Msg )
init model =
    ( model, Cmd.none )



----------------------
-- Update
----------------------


update : Msg -> Model -> Action
update msg { shop, seed } =
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
        (Card.large
            ++ Button.simple
            ++ [ Font.family [ Font.sansSerif ]
               , Element.centerX
               , Element.centerY
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
view scale msgMapper { shop } =
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
                    [ Text.colored (round <| scale * 150) "🏭"
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
                    (msgMapper <| ClickedStart <| Scrap)
                    { title = "Scrap Dealer"
                    , desc = "You get scrap for free"
                    }
                , viewMode
                    (msgMapper <| ClickedStart <| Stone)
                    { title = "Miner"
                    , desc = "You get stone for free"
                    }
                , viewMode
                    (msgMapper <| ClickedStart <| Wood)
                    { title = "Wood cutter"
                    , desc = "You get wood for free"
                    }
                ]
            ]
    )

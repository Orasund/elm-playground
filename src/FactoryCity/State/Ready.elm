module FactoryCity.State.Ready exposing (Model, Msg, init, update, view)

import Action
import Element exposing (Element)
import Element.Font as Font
import Element.Input as Input
import FactoryCity.Data.Game as Game exposing (Game)
import FactoryCity.State.Playing as PlayingState
import FactoryCity.View.Game as GameView
import FactoryCity.View.Header as HeaderView
import FactoryCity.View.Text as Text
import Framework.Button as Button
import Framework.Card as Card
import Framework.Grid as Grid
import Framework.Heading as Heading
import Random exposing (Generator, Seed)
import Task
import Time exposing (Month(..))



----------------------
-- Model
----------------------


type alias State =
    Game


type alias Model =
    ( State, Seed )


type Msg
    = NormalModeSelected


type alias Action =
    Action.Action Model Msg PlayingState.TransitionData Never



----------------------
-- Init
----------------------


initialState : State
initialState =
    Game.init


init : Seed -> ( Model, Cmd Msg )
init seed =
    ( ( initialState, seed ), Cmd.none )



----------------------
-- Update
----------------------


monthToInt : Month -> Int
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


update : Msg -> Model -> Action
update msg (( game, seed ) as model) =
    case msg of
        NormalModeSelected ->
            Action.transitioning
                { game = game
                , seed = seed
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
    -> msg
    -> (Msg -> msg)
    -> Model
    -> ( Maybe { isWon : Bool, shade : List (Element msg) }, List (Element msg) )
view scale restartMsg msgMapper ( game, _ ) =
    ( Just
        { isWon = False
        , shade =
            [ Element.wrappedRow (Grid.simple ++ [ Element.height <| Element.fill ])
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
                        , Font.color <| Element.rgb255 255 255 255
                        , Font.center
                        ]
                      <|
                        [ Element.text "Factory"
                        , Element.text "City"
                        ]
                    ]
                , Element.column (Grid.simple ++ [ Element.centerY ]) <|
                    [ viewMode
                        (msgMapper <| NormalModeSelected)
                        { title = "Normal"
                        , desc = "Random cards, one life. If you loose you can blame RNG."
                        }
                    ]
                ]
            ]
        }
    , [ HeaderView.view scale restartMsg game.score
      , GameView.viewHome scale game
      ]
    )

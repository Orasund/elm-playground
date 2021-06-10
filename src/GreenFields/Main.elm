module GreenFields.Main exposing (main)

import Browser
import Color as C
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import GreenFields.Data.Building as Building
import GreenFields.Data.Game as Game exposing (Game)
import GreenFields.View.Card as Card
import GreenFields.View.Color as Color
import GreenFields.View.Tile as Tile
import Html exposing (Html)
import Task
import Time exposing (Posix)


type alias Model =
    { game : Game
    , center : ( Int, Int )
    , selected : Maybe ( Int, Int )
    , timestamp : Posix
    }


type Msg
    = Restart
    | Select ( Int, Int )
    | Restore ( Int, Int )
    | GotTime Posix


init : () -> ( Model, Cmd Msg )
init () =
    ( { game = Game.empty
      , center = ( 0, 0 )
      , selected = Nothing
      , timestamp = Time.millisToPosix 0
      }
    , Task.perform GotTime Time.now
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTime timestamp ->
            ( { model | timestamp = timestamp }, Cmd.none )

        Select pos ->
            ( { model | selected = Just pos }, Cmd.none )

        Restore pos ->
            ( { model
                | game =
                    model.game
                        |> Game.restore
                            { position = pos
                            , timestamp = model.timestamp
                            }
              }
            , Cmd.none
            )

        Restart ->
            init ()


view : Model -> Html Msg
view model =
    let
        size =
            20

        ( centerX, centerY ) =
            model.center
    in
    [ List.range (centerY - size // 2) (centerY + size // 2)
        |> List.map
            (\y ->
                List.range (centerX - size // 2) (centerX + size // 2)
                    |> List.map
                        (\x ->
                            Tile.view
                                { game = model.game
                                , pos = ( x, y )
                                , onClick = Select ( x, y )
                                , timestamp = model.timestamp
                                }
                        )
                    |> Element.row [ Element.spacing 2 ]
            )
        |> Element.column
            [ Element.spacing 2
            , Element.alignRight
            , Element.centerY
            ]
        |> Element.el
            [ Element.width <| Element.fill
            , Element.height <| Element.fill
            ]
    , Card.view
        { selected = model.selected
        , game = model.game
        , onRestore =
            model.selected
                |> Maybe.map Restore
                |> Maybe.withDefault Restart
        }
        |> Element.el
            [ Element.width <| Element.fill
            , Element.height <| Element.fill
            ]
    ]
        |> Element.row
            [ Element.width <| Element.fill
            , Element.centerY
            , Element.spacing 2
            ]
        |> Element.layout
            [ Color.green
                |> C.toRgba
                |> Element.fromRgb
                |> Background.color
            , Font.size 14
            , Font.family [ Font.monospace ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

module GameJam exposing (main)

import Color
import Game
import GameJam.Data exposing (boardSize, initialHealth, initialPlayer, screenWidth, spriteSize)
import GameJam.Data.Behaviour as Behaviour
import GameJam.Data.Board as Board
import GameJam.Data.Game as DataGame exposing (Game)
import GameJam.Data.Square as Square exposing (Square(..))
import GameJam.View as View
import GameJam.View.Health as Health
import GameJam.View.Square as Square
import Grid
import Grid.Direction exposing (Direction(..))
import Grid.Position as Position
import PixelEngine exposing (Area, Input(..))
import PixelEngine.Image as Image
import PixelEngine.Tile as Tile
import Random exposing (Generator)
import Game.Gui as Gui exposing (Gui)

type alias Model =
    { game : Game
    , won : Bool
    }


type Msg
    = Move Direction


init : Int -> Generator ( Model, Cmd msg )
init level =
    Board.generator level
        |> Random.map
            (\board ->
                ( { game =
                        { board = board
                        , health = initialHealth
                        , player = initialPlayer
                        , super = False
                        , level = level
                        }
                  , won = False
                  }
                , Cmd.none
                )
            )


update : Msg -> Model -> Generator (Maybe ( Model, Cmd Msg ))
update msg ({ game, won } as model) =
    let
        { player, health, level } =
            game
    in
    case msg of
        Move dir ->
            if won then
                init (level + 1)
                    |> Random.map Just

            else if health <= 0 then
                init level
                    |> Random.map Just

            else
                let
                    newGame : Game
                    newGame =
                        DataGame.update
                            { game
                                | player =
                                    player
                                        |> Position.move 1 dir
                                        |> (\p ->
                                                if
                                                    game.board
                                                        |> Grid.get p
                                                        |> (\ms ->
                                                                (ms /= Just Wall)
                                                                    && (ms /= Just LookedDoor)
                                                           )
                                                then
                                                    p

                                                else
                                                    player
                                           )
                            }
                in
                ( { model
                    | game = newGame
                    , won =
                        newGame.board
                            |> Grid.filter
                                (\_ s -> Behaviour.removeToWin level |> List.member s)
                            |> Grid.isEmpty
                  }
                , Cmd.none
                )
                    |> Just
                    |> Random.constant



{------------------------
   VIEW
------------------------}


view : Model -> Gui
view { game, won } =
    let
        { health, board, player, super, level } =
            game
    in
    (board
            |> Grid.insert player
                (if super then
                    ActivePlayer

                 else
                    Player
                )
            |> Grid.toList
            |> List.map (\( pos, square ) -> ( pos, square |> Square.view ))
        )
    |> Gui.create
        {gui=
                PixelEngine.colorBackground <|
                Color.rgb255 68 36 52
            ,grid =
                PixelEngine.colorBackground <|
                if won then
                    Color.rgb255 218 212 94
                    --yellow

                else if health <= 0 then
                    Color.rgb255 208 70 72
                    --red

                else
                    Color.rgb255 20 12 28
            }
        View.tileset
    |> Gui.withHeader
        ( ( 0
          , Image.fromTextWithSpacing -3 ("Lv." ++ String.fromInt level) <|
                Tile.tileset
                    { source = "Expire8x8.png"
                    , spriteWidth = 8
                    , spriteHeight = 8
                    }
          )
        |> List.singleton
        )

    |> Gui.withFooter
        []
        (Health.view health)
        []
        


{------------------------
   CONTROLS
------------------------}


controls : Input -> Maybe Msg
controls input =
    case input of
        InputUp ->
            Just <| Move Up

        InputDown ->
            Just <| Move Down

        InputLeft ->
            Just <| Move Left

        InputRight ->
            Just <| Move Right

        _ ->
            Nothing



{------------------------
   CONFIGURATION
------------------------}


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Game.Game Model Msg
main =
    Game.define
        { init = init 1
        , update = update
        , subscriptions = subscriptions
        , view = view
        , controls = controls
        , imgSize = spriteSize
        , title = "One Switch"
        }

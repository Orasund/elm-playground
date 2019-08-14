module GameJam.Page.Running exposing (Model, Msg(..), init, update, view)

import Action
import Color
import GameJam.Data exposing (boardSize, initialHealth, initialPlayer, spriteSize)
import GameJam.Data.Behaviour as Behaviour
import GameJam.Data.Board as Board
import GameJam.Data.Game as Game exposing (Game)
import GameJam.Data.Square as Square exposing (Square(..))
import GameJam.View as View
import GameJam.View.Health as Health
import GameJam.View.Square as Square
import Grid
import Grid.Direction exposing (Direction)
import Grid.Position as Position
import PixelEngine exposing (Area)
import PixelEngine.Image as Image
import PixelEngine.Tile as Tile
import Random exposing (Seed)


type alias Model =
    { game : Game
    , won : Bool
    , seed : Seed
    }


type Msg
    = Move Direction


type alias Action =
    Action.Action Model Msg Never ()


init : ( Seed, Int ) -> Model
init ( s, level ) =
    s
        |> Random.step (Board.generator level)
        |> (\( board, seed ) ->
                { game =
                    { board = board
                    , health = initialHealth
                    , player = initialPlayer
                    , super = False
                    , level = level
                    }
                , seed = seed
                , won = False
                }
           )


update : Msg -> Model -> Action
update msg ({ game, seed, won } as model) =
    let
        { player, health, level } =
            game
    in
    case msg of
        Move dir ->
            if won then
                Action.updating
                    ( init ( seed, level + 1 ), Cmd.none )

            else if health <= 0 then
                Action.updating
                    ( init ( seed, level ), Cmd.none )

            else
                let
                    newGame : Game
                    newGame =
                        Game.update
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
                Action.updating
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


view : Model -> List (Area Msg)
view { game, won } =
    let
        { health, board, player, super, level } =
            game
    in
    [ PixelEngine.tiledArea
        { rows = boardSize
        , tileset = View.tileset
        , background =
            PixelEngine.colorBackground <|
                if won then
                    Color.rgb255 218 212 94
                    --yellow

                else if health <= 0 then
                    Color.rgb255 208 70 72
                    --red

                else
                    Color.rgb255 20 12 28

        --black
        }
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
    , PixelEngine.imageArea
        { height = toFloat <| spriteSize * 1
        , background =
            PixelEngine.colorBackground <|
                Color.rgb255 68 36 52

        --gray
        }
      <|
        [ ( ( 0, 0 )
          , Image.fromTextWithSpacing -3 ("Lv." ++ String.fromInt level) <|
                Tile.tileset
                    { source = "Expire8x8.png"
                    , spriteWidth = 8
                    , spriteHeight = 8
                    }
          )
        ]
            ++ Health.view health
    ]

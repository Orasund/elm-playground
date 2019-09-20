module GameJam exposing (main)

import Color
import GJumper exposing (GameData, View)
import GameJam.Data exposing (initialHealth, initialPlayer, screenWidth, spriteSize)
import GameJam.Data.Behaviour as Behaviour
import GameJam.Data.Board as Board
import GameJam.Data.Game as DataGame exposing (Game)
import GameJam.Data.Square as Square exposing (Square(..))
import GameJam.View as View
import GameJam.View.Square as Square
import Grid
import Grid.Direction exposing (Direction(..))
import Grid.Position exposing (Position)
import PixelEngine exposing (Input(..))
import PixelEngine.Image as Image
import PixelEngine.Tile as Tile
import Random exposing (Generator)


type alias Model =
    GameData Square Game


init : Int -> Generator Model
init level =
    Board.generator level
        |> Random.map
            (\board ->
                { data =
                    { health = initialHealth
                    , super = False
                    , level = level
                    , won = False
                    }
                , grid = board
                , player = initialPlayer
                }
            )


isValid : Position -> Model -> Bool
isValid p { grid } =
    grid
        |> Grid.get p
        |> (\ms ->
                (ms /= Just Wall)
                    && (ms /= Just LookedDoor)
           )
        |> not


tick : Model -> Generator (Maybe Model)
tick game =
    let
        { health, level, won } =
            game.data

        ({ data, grid } as newGame) =
            DataGame.update game
    in
    if won then
        init (level + 1)
            |> Random.map Just

    else if health <= 0 then
        init level
            |> Random.map Just

    else
        { newGame
            | data =
                { data
                    | won =
                        grid
                            |> Grid.filter
                                (\_ s -> Behaviour.removeToWin level |> List.member s)
                            |> Grid.isEmpty
                }
        }
            |> Just
            |> Random.constant



{------------------------
   VIEW
------------------------}


view : Game -> View Square
view { health, won, super, level } =
    GJumper.view
        { player =
            if super then
                ActivePlayer |> Square.view

            else
                Player |> Square.view
        , square = Square.view
        }
        View.tileset
        (PixelEngine.colorBackground <|
            if won then
                Color.rgb255 218 212 94
                --yellow

            else if health <= 0 then
                Color.rgb255 208 70 72
                --red

            else
                Color.rgb255 20 12 28
        )
        |> GJumper.withGui
            (GJumper.header
                (( 0
                 , Image.fromTextWithSpacing -3 ("Lv." ++ String.fromInt level) <|
                    Tile.tileset
                        { source = "Expire8x8.png"
                        , spriteWidth = 8
                        , spriteHeight = 8
                        }
                 )
                    |> List.singleton
                )
            )
            (GJumper.footer
                []
                (View.tileset
                    |> Image.fromTile (Square.view Health)
                    |> List.repeat health
                    |> List.indexedMap
                        (\i image ->
                            ( ((screenWidth - (toFloat <| health * spriteSize)) / 2)
                                + (toFloat <| i * spriteSize)
                            , image
                            )
                        )
                )
                []
            )
            (PixelEngine.colorBackground <|
                Color.rgb255 68 36 52
            )


main : GJumper.Game Square Game
main =
    GJumper.define
        { init = init 1
        , isValid = isValid
        , tick = tick
        , view = view
        , imgSize = spriteSize
        , title = "One Switch"
        }

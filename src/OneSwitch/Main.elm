module OneSwitch exposing (main)

import Color
import GJumper exposing (GameData, View,Status(..))
import OneSwitch.Data exposing (initialHealth, initialPlayer, screenWidth, spriteSize)
import OneSwitch.Data.Behaviour as Behaviour
import OneSwitch.Data.Board as Board
import OneSwitch.Data.Game as DataGame exposing (Game)
import OneSwitch.Data.Square as Square exposing (Square(..))
import OneSwitch.View as View
import OneSwitch.View.Square as Square
import Grid
import Grid.Position exposing (Position)
import Grid.Direction exposing (Direction(..))
import PixelEngine exposing (Input(..))
import PixelEngine.Image as Image
import PixelEngine.Tile as Tile
import Random exposing (Generator)


type alias Model =
    GameData Square Game


init : Maybe Game
          -> Generator
                 { columns : Int
                 , data : Game
                 , distribution :
                       Game
                       -> ( ( Float, Maybe Square )
                          , List ( Float, Maybe Square )
                          )
                 , fixed : Game -> List ( Int, Square )
                 , level : Game -> List (List (Maybe Square))
                 , player : Position
                 , rows : Int
                 }
init maybeGame =
    let
        level = maybeGame
                |> Maybe.map .level
                |> Maybe.withDefault 1
    in
    {columns = 16
                 , data = { health = initialHealth
                    , super = False
                    , level = level
                    , won = False
                    }
                 , distribution = .level >> Board.distribution
                 , fixed = .level >> Board.fixed
                 , level = .level >> Board.level
                 , player = initialPlayer
                 , rows = 16
    }
    |> Random.constant


isSolid : Square -> Bool
isSolid square =
    case square of
        Wall ->
            True

        LookedDoor ->
            True

        Enemy ->
            True

        _ ->
            False


tick : Model -> Generator (Model, Status )
tick game =
    let
        { level } =
            game.data

        ({ data, grid } as newGame) =
            DataGame.update game
        


        won : Bool
        won =
            grid
                            |> Grid.filter
                                (\_ s -> Behaviour.removeToWin level |> List.member s)
                            |> Grid.isEmpty
    in
    ({ newGame
        | data = { data | won = won, level = if won then level+1 else level }
         }
        , if won then
            Won
        else if  data.health <= 0  then
            Lost
        else
            Ongoing
        )
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
            (PixelEngine.colorBackground <| Color.rgb255 68 36 52)


main : GJumper.Game Square Game
main =
    GJumper.define
        { init = init
        , isSolid = isSolid
        , tick = tick
        , view = view
        , imgSize = spriteSize
        , title = "One Switch"
        }

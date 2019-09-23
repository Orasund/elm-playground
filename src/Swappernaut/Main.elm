module Swappernaut exposing (main)

import Color
import GJumper exposing (GameData, View)
import Grid exposing (Grid)
import Grid.Direction exposing (Direction(..))
import Grid.Position exposing (Position)
import PixelEngine exposing (Input(..))
import PixelEngine.Image as Image
import PixelEngine.Tile as Tile
import Random exposing (Generator)
import Swappernaut.Board as Board
import Swappernaut.Square as Square exposing (Square(..))


type alias Game =
    { level : Int
    , status : Status
    , x : Int
    }


type Status
    = Running
    | Won
    | Lost


type alias Model =
    GameData Square Game


init : Int -> Generator Model
init level =
    Random.map2
        (\board x ->
            { data =
                { level = level
                , status = Running
                , x = x
                }
            , grid =
                board
                    |> Grid.remove ( x, 14 )
                    |> applyPosition ( x, 14 )
            , player =
                ( x, 14 )
            }
        )
        (Board.generator level)
        (Random.int 2 13)


isSolid : Square -> Bool
isSolid square =
    case square of
        Wall _ ->
            True

        _ ->
            False


swap : Int -> Int -> Int
swap around x =
    if x == (around - 1 |> modBy 16) then
        around + 1 |> modBy 16

    else if x == (around + 1 |> modBy 16) then
        around - 1 |> modBy 16

    else
        x


applyPosition : Position -> Grid Square -> Grid Square
applyPosition ( x, y ) =
    {--Grid.map
        (\(posX,posY) ->
            Maybe.map
                (\square ->
                    if (x == posX) 
                        || (posX - 1 |> modBy 16 |> (==) x) 
                        || (posX + 1 |> modBy 16 |> (==) x)
                    then
                        case square of
                            Wall _ -> Wall True
                            Bumper _ -> Bumper True
                            _ -> square
                    else
                        case square of
                            Wall _ -> Wall False
                            Bumper _ -> Bumper False
                            _ -> square
                )
        )--}
    identity


tick : Model -> Generator (Maybe Model)
tick ({ player } as game) =
    let
        data =
            game.data
                |> (\a -> { a | x = player |> Tuple.first })

        { level, status } =
            data

        square =
            game.grid |> Grid.get player

        ( playerX, playerY ) =
            player

        oldX =
            game.data.x

        gridGenerator : Generator (Grid Square)
        gridGenerator =
            Random.map2
                (\x y -> ( x, y ))
                (Random.int 0 15)
                (Random.int 1 14)
                |> Random.map
                    (\newBumperPos ->
                        if data.x /= oldX then
                            game.grid
                                |> Grid.toList
                                |> List.map
                                    (\( ( x, y ), elem ) ->
                                        ( if y == playerY then
                                            ( x |> swap oldX
                                            , y
                                            )

                                          else
                                            ( x, y )
                                        , elem
                                        )
                                    )
                                |> Grid.fromList
                                    { rows = 16
                                    , columns = 16
                                    }
                                |> Grid.update newBumperPos
                                    (Maybe.withDefault
                                        (Bumper False)
                                        >> Just
                                    )

                        else
                            game.grid
                    )
    in
    if status /= Running then
        init 1
            |> Random.map Just

    else
        (case square of
            Just Goal ->
                { game | data = { data | status = Won }, grid = game.grid |> Grid.remove ( playerX, 15 ) }
                    |> Random.constant

            Just (Bumper _) ->
                gridGenerator
                    |> Random.map
                        (\grid ->
                            let
                                newGrid : Grid Square
                                newGrid =
                                    grid
                                        |> Grid.remove player
                                        |> Grid.toList
                                        |> List.map
                                            (\( ( x, y ), elem ) ->
                                                ( ( x
                                                  , if y == (player |> Tuple.second) then
                                                        14

                                                    else if y == 14 then
                                                        player |> Tuple.second

                                                    else
                                                        y
                                                  )
                                                , elem
                                                )
                                            )
                                        |> Grid.fromList
                                            { rows = 16
                                            , columns = 16
                                            }

                                newPlayer =
                                    player |> (\( x, y ) -> ( x, 14 ))
                            in
                            { game
                                | data =
                                    { data
                                        | status =
                                            if
                                                [ newGrid |> Grid.get ( playerX + 1, 14 )
                                                , newGrid |> Grid.get ( playerX - 1, 14 )
                                                , newGrid |> Grid.get ( playerX, 13 )
                                                ]
                                                    |> List.all
                                                        (\x ->
                                                            case x of
                                                                Just (Wall _) ->
                                                                    True

                                                                _ ->
                                                                    False
                                                        )
                                            then
                                                Lost

                                            else
                                                Running
                                    }
                                , grid = newGrid |> applyPosition newPlayer
                                , player = newPlayer
                            }
                        )

            _ ->
                let
                    newPlayer =
                        if data.x /= oldX then
                            player |> (\( x, y ) -> ( x |> swap oldX, y ))

                        else
                            player
                in
                gridGenerator
                    |> Random.map
                        (\grid ->
                            { game
                                | grid = grid |> applyPosition newPlayer
                                , player = newPlayer
                                , data =
                                    { data
                                        | x = data.x |> swap oldX
                                        , level = game.data.level + 1
                                    }
                            }
                        )
        )
            |> Random.map Just



{------------------------
   VIEW
------------------------}


view : Game -> View Square
view { status, level } =
    GJumper.view
        { player = Tile.fromPosition ( 0, 1 ) |> Tile.movable "player"
        , square = Square.view
        }
        (Tile.tileset
            { source = "tileset.png"
            , spriteWidth = 16
            , spriteHeight = 16
            }
        )
        (PixelEngine.colorBackground <|
            case status of
                Won ->
                    Color.rgb255 109 194 202

                Lost ->
                    Color.rgb255 20 12 28

                --Color.rgb255 48 52 109
                Running ->
                    Color.rgb255 20 12 28
        )
        |> GJumper.withGui
            (GJumper.header
                (( 0
                 , Image.fromTextWithSpacing -1 ("turn " ++ String.fromInt level) <|
                    Tile.tileset
                        { source = "Expire8x8.png"
                        , spriteWidth = 16
                        , spriteHeight = 16
                        }
                 )
                    |> List.singleton
                )
            )
            (GJumper.footer
                []
                []
                []
            )
            (PixelEngine.colorBackground <| Color.rgb255 20 12 28)


main : GJumper.Game Square Game
main =
    GJumper.define
        { init = init 1
        , isSolid = isSolid
        , tick = tick
        , view = view
        , imgSize = 16
        , title = "Swappernaut"
        }

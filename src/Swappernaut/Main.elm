module Swappernaut exposing (main)

import Color
import GJumper exposing (GameData, Status(..), View)
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
    , won : Bool
    , x : Int
    }


type alias Model =
    GameData Square Game


init :
    Maybe Game
    ->
        Generator
            { columns : Int
            , data : Game
            , distribution :
                Game
                -> ( ( Float, Maybe Square ), List ( Float, Maybe Square ) )
            , fixed : Game -> List ( Int, Square )
            , level : Game -> List (List (Maybe Square))
            , player : Position
            , rows : Int
            }
init game =
    let
        lv : Int
        lv =
            game
                |> Maybe.map .level
                |> Maybe.withDefault 1
    in
    Random.map
        (\x ->
            { data =
                { level = lv
                , won = False
                , x = x
                }
            , player =
                ( x, 14 )
            , columns = 16
            , rows = 23
            , fixed = always []
            , distribution =
                always
                    ( ( 9, Nothing )
                    , [ ( 6, Just (Wall False) ), ( 6, Just (Bumper False) ) ]
                    )
            , level = .level >> Board.level
            }
        )
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


tick : Model -> Generator ( Model, Status )
tick ({ player } as game) =
    let
        data =
            game.data
                |> (\a -> { a | x = player |> Tuple.first })

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
                                    { rows = 23
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
    case square of
        Just Goal ->
            ( { game
                | data = { data | won = True }
              }
            , Won
            )
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
                                        { rows = 23
                                        , columns = 16
                                        }

                            newPlayer =
                                player |> (\( x, _ ) -> ( x, 14 ))
                        in
                        ( { game
                            | data = data
                            , grid = newGrid
                            , player = newPlayer
                          }
                        , if
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
                            Ongoing
                        )
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
                        ( { game
                            | grid = grid
                            , player = newPlayer
                            , data =
                                { data
                                    | x = data.x |> swap oldX
                                    , level = game.data.level + 1
                                }
                          }
                        , Ongoing
                        )
                    )



{------------------------
   VIEW
------------------------}


view : Game -> View Square
view { won, level } =
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
            if won then
                Color.rgb255 109 194 202

            else
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
        { init = init
        , isSolid = isSolid
        , tick = tick
        , view = view
        , imgSize = 16
        , title = "Swappernaut"
        }

module GameJam.Data.Game exposing (Game, update, updateBehaviour)

import GameJam.Data.Behaviour as Behaviour
import GameJam.Data.Board as Board exposing (Board)
import GameJam.Data.Square as Square exposing (Square(..))
import Grid
import Grid.Direction exposing (Direction)
import Grid.Position as Position exposing (Position)


type alias Game =
    { player : Position
    , health : Int
    , super : Bool
    , board : Board
    , level : Int
    }


updateBehaviour : Position -> Square -> Game -> Game
updateBehaviour pos square ({ player, health, board, super } as game) =
    let
        defaultCase : Game
        defaultCase =
            game
    in
    case square of
        Enemy ->
            let
                newPos : Position
                newPos =
                    if player == pos then
                        pos

                    else
                        pos
                            |> Position.move 1
                                (pos
                                    |> Position.coordsTo player
                                    |> Position.toDirection
                                )
            in
            case board |> Grid.get newPos of
                Nothing ->
                    if player == newPos && not super then
                        { game
                            | board =
                                board
                                    |> Grid.insert newPos Enemy
                                    |> Grid.remove pos
                            , player =
                                player
                                    |> Position.move 1
                                        (pos |> Position.coordsTo player |> Position.toDirection)
                            , health = health - 1
                        }

                    else
                        { game
                            | board =
                                board
                                    |> Grid.insert newPos Enemy
                                    |> Grid.remove pos
                        }

                _ ->
                    defaultCase

        Health ->
            if player == pos then
                { game
                    | health = health + 1
                }

            else
                defaultCase

        Lava ->
            if player == pos then
                { game
                    | health = health - 1
                }

            else
                defaultCase

        PowerUp ->
            if player == pos then
                { game
                    | super = True
                }

            else
                defaultCase

        PowerDown ->
            if player == pos then
                { game
                    | super = False
                }

            else
                defaultCase

        Swap ->
            if player == pos then
                { game | super = not super }

            else
                defaultCase

        _ ->
            defaultCase


update : Game -> Game
update ({ player, super, level } as game) =
    let
        activatedBoard : Board
        activatedBoard =
            game.board
                |> (game.board
                        |> Grid.get player
                        |> Maybe.map (Behaviour.activate level)
                        |> Maybe.withDefault identity
                   )

        updatedGame : Game
        updatedGame =
            activatedBoard
                |> Grid.toList
                |> List.foldl
                    (\( pos, square ) ->
                        updateBehaviour pos square
                    )
                    { game | board = activatedBoard }

        consumedGame : Game
        consumedGame =
            { updatedGame
                | board =
                    updatedGame.board
                        |> (if
                                updatedGame.board
                                    |> Grid.get player
                                    |> Maybe.map
                                        (\s -> Behaviour.consumable level super |> List.member s)
                                    |> Maybe.withDefault False
                            then
                                Grid.remove player

                            else
                                identity
                           )
            }
    in
    consumedGame

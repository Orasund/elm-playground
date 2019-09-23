module OneSwitch.Data.Game exposing (Game, update, updateBehaviour)

import GJumper exposing (GameData)
import OneSwitch.Data.Behaviour as Behaviour
import OneSwitch.Data.Board
import OneSwitch.Data.Square exposing (Square(..))
import Grid exposing (Grid)
import Grid.Position as Position exposing (Position)


type alias Game =
    { health : Int
    , super : Bool
    , level : Int
    , won : Bool
    }


updateBehaviour : Position -> Square -> GameData Square Game -> GameData Square Game
updateBehaviour pos square ({ data, grid, player } as game) =
    let
        { health, super } =
            data

        board =
            grid

        defaultCase : GameData Square Game
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
                        { player =
                            player
                                |> Position.move 1
                                    (pos |> Position.coordsTo player |> Position.toDirection)
                        , grid =
                            board
                                |> Grid.insert newPos Enemy
                                |> Grid.remove pos
                        , data = { data | health = health - 1 }
                        }

                    else
                        { game
                            | grid =
                                board
                                    |> Grid.insert newPos Enemy
                                    |> Grid.remove pos
                        }

                _ ->
                    defaultCase

        Health ->
            if player == pos then
                { game | data = { data | health = health + 1 } }

            else
                defaultCase

        Lava ->
            if player == pos then
                { game | data = { data | health = health - 1 } }

            else
                defaultCase

        PowerUp ->
            if player == pos then
                { game | data = { data | super = True } }

            else
                defaultCase

        PowerDown ->
            if player == pos then
                { game | data = { data | super = False } }

            else
                defaultCase

        Swap ->
            if player == pos then
                { game | data = { data | super = not super } }

            else
                defaultCase

        _ ->
            defaultCase


update : GameData Square Game -> GameData Square Game
update ({ data, grid, player } as game) =
    let
        { super, level } =
            data

        board =
            grid

        activatedBoard : Grid Square
        activatedBoard =
            board
                |> (board
                        |> Grid.get player
                        |> Maybe.map (Behaviour.activate level)
                        |> Maybe.withDefault identity
                   )

        updatedGame : GameData Square Game
        updatedGame =
            activatedBoard
                |> Grid.toList
                |> List.foldl
                    (\( pos, square ) ->
                        updateBehaviour pos square
                    )
                    { game | grid = activatedBoard }

        consumedGame : GameData Square Game
        consumedGame =
            { updatedGame
                | grid =
                    updatedGame.grid
                        |> (if
                                updatedGame.grid
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

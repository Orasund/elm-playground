module GreenFields.Data.Database exposing (deleteTile, firestore, getBoard, getTile, insertTile)

import Dict exposing (Dict)
import Firestore exposing (Document, Error(..), Firestore)
import Firestore.Codec as Codec
import Firestore.Config
import Firestore.Options.List
import Firestore.Query as Query exposing (Direction(..))
import GreenFields.Data.Tile as Tile exposing (Tile)
import Task exposing (Task)


distanceBetweenTowns : Int
distanceBetweenTowns =
    32


firestore : Firestore
firestore =
    Firestore.Config.new
        { apiKey = "AIzaSyAHNxt048Q4BFwbt_ehv4t4rxydqdc0QNc"
        , project = "elm-games"
        }
        |> Firestore.init


positionToString : ( Int, Int ) -> String
positionToString ( x, y ) =
    "(" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")"


getTile : ( Int, Int ) -> Task Error (Maybe Tile)
getTile ( x, y ) =
    firestore
        |> Firestore.root
        |> Firestore.collection "green-fields"
        |> Firestore.document "game"
        |> Firestore.subCollection "board"
        |> Firestore.document (positionToString ( x, y ))
        |> Firestore.get (Codec.asDecoder Tile.codec)
        |> Task.map (.fields >> Just)
        |> Task.onError
            (\error ->
                case error of
                    Response { code } ->
                        case code of
                            404 ->
                                --Not Found
                                Task.succeed Nothing

                            _ ->
                                Task.fail error

                    _ ->
                        Task.fail error
            )


insertTile : Tile -> Task Error Tile
insertTile tile =
    firestore
        |> Firestore.root
        |> Firestore.collection "green-fields"
        |> Firestore.document "game"
        |> Firestore.subCollection "board"
        |> Firestore.document (positionToString ( tile.x, tile.y ))
        |> Firestore.upsert (Codec.asDecoder Tile.codec) (tile |> Codec.asEncoder Tile.codec)
        |> Task.map .fields


deleteTile : ( Int, Int ) -> Task Error ()
deleteTile pos =
    firestore
        |> Firestore.root
        |> Firestore.collection "green-fields"
        |> Firestore.document "game"
        |> Firestore.subCollection "board"
        |> Firestore.document (positionToString pos)
        |> Firestore.delete


getBoard : { radius : Int, position : ( Int, Int ) } -> Task Error (Dict ( Int, Int ) Tile)
getBoard args =
    let
        ( posX, posY ) =
            args.position
    in
    firestore
        |> Firestore.root
        |> Firestore.collection "green-fields"
        |> Firestore.document "game"
        |> Firestore.runQuery (Codec.asDecoder Tile.codec)
            (Query.new
                |> Query.collection "board"
                --|> Query.orderBy "timestamp" Descending
                |> Query.where_
                    (Query.compositeFilter Query.And
                        (Query.fieldFilter "x" Query.GreaterThanOrEqual (Query.int (posX - distanceBetweenTowns)))
                        [ Query.fieldFilter "x" Query.LessThanOrEqual (Query.int (posX + distanceBetweenTowns)) ]
                    )
            )
        |> Task.map
            (List.map
                (.document
                    >> .fields
                    >> (\tile ->
                            ( ( tile.x, tile.y ), tile )
                       )
                )
                >> Dict.fromList
            )



{--List.range (posY - args.radius) (posY + args.radius)
        |> List.concatMap
            (\y ->
                List.range (posX - args.radius) (posX + args.radius)
                    |> List.map
                        (\x ->
                            getTile ( x, y )
                        )
            )
        |> List.foldl
            (\task outTask ->
                outTask
                    |> Task.andThen
                        (\list ->
                            task
                                |> Task.map
                                    (\tile ->
                                        case tile of
                                            Just t ->
                                                ( ( t.x, t.y ), t ) :: list

                                            Nothing ->
                                                list
                                    )
                        )
            )
            (Task.succeed [])
        |> Task.map Dict.fromList--}
{----}
{--firestore
        |> Firestore.root
        |> Firestore.collection "green-fields"
        |> Firestore.document "game"
        |> Firestore.subCollection "board"
        |> Firestore.list (Codec.asDecoder Tile.codec)
            (Firestore.Options.List.default
                |> Firestore.Options.List.orderBy
            )
        |> Task.map
            (.documents
                >> List.map
                    (.fields
                        >> (\tile ->
                                ( ( tile.x, tile.y ), tile )
                           )
                    )
                >> Dict.fromList
            )
    --}
{--
--}

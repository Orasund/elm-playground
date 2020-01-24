module FactoryCity.Data.RemoteShop exposing (insert, json, remove, sync)

import Bag exposing (Bag)
import FactoryCity.Data.CellType as CellType exposing (ContainerSort(..), Item(..))
import FactoryCity.String as String
import Http
import Jsonstore exposing (Json)
import Task exposing (Task)


type alias RemoteShop =
    { chips : Maybe Int
    , iron : Maybe Int
    , scrap : Maybe Int
    , stone : Maybe Int
    , wood : Maybe Int
    , chipboard : Maybe Int
    }


toBag : RemoteShop -> Bag String
toBag { chips, iron, scrap, stone, wood, chipboard } =
    [ ( Chips, chips )
    , ( Iron, iron )
    , ( Scrap, scrap )
    , ( Stone, stone )
    , ( Wood, wood )
    , ( Chipboard, chipboard )
    ]
        |> List.map
            (Tuple.mapBoth CellType.itemToString
                (Maybe.withDefault 0)
            )
        |> Bag.fromList


json : Json RemoteShop
json =
    Jsonstore.object RemoteShop
        |> Jsonstore.withMaybe (Chips |> CellType.itemToString) Jsonstore.int .chips
        |> Jsonstore.withMaybe (Iron |> CellType.itemToString) Jsonstore.int .iron
        |> Jsonstore.withMaybe (Scrap |> CellType.itemToString) Jsonstore.int .scrap
        |> Jsonstore.withMaybe (Stone |> CellType.itemToString) Jsonstore.int .stone
        |> Jsonstore.withMaybe (Wood |> CellType.itemToString) Jsonstore.int .wood
        |> Jsonstore.withMaybe (Chipboard |> CellType.itemToString) Jsonstore.int .chipboard
        |> Jsonstore.toJson


sync : Task Http.Error (Bag String)
sync =
    json
        |> Jsonstore.decode
        |> Jsonstore.get String.url
        |> Task.map (Maybe.map toBag >> Maybe.withDefault Bag.empty)


remove : Item -> Task Http.Error ()
remove item =
    Jsonstore.update
        { url = String.url ++ "/" ++ (item |> CellType.itemToString)
        , decoder = Jsonstore.int |> Jsonstore.decode
        , value =
            Maybe.andThen
                (\n ->
                    if n == 1 then
                        Nothing

                    else
                        Just <| Jsonstore.encode Jsonstore.int <| n - 1
                )
        }


insert : Item -> Task Http.Error ()
insert item =
    Jsonstore.update
        { url = String.url ++ "/" ++ (item |> CellType.itemToString)
        , decoder = Jsonstore.int |> Jsonstore.decode
        , value =
            Maybe.map ((+) 1)
                >> Maybe.withDefault 1
                >> Jsonstore.encode Jsonstore.int
                >> Just
        }

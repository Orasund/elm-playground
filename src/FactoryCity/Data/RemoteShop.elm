module FactoryCity.Data.RemoteShop exposing (insert, json, remove, sync)

import Bag exposing (Bag)
import FactoryCity.Data.CellType exposing (ContainerSort(..))
import FactoryCity.Data.Item as Item exposing (Item(..))
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
            (Tuple.mapBoth Item.itemToString
                (Maybe.withDefault 0)
            )
        |> Bag.fromList


json : Json RemoteShop
json =
    Jsonstore.object RemoteShop
        |> Jsonstore.withMaybe (Chips |> Item.itemToString) Jsonstore.int .chips
        |> Jsonstore.withMaybe (Iron |> Item.itemToString) Jsonstore.int .iron
        |> Jsonstore.withMaybe (Scrap |> Item.itemToString) Jsonstore.int .scrap
        |> Jsonstore.withMaybe (Stone |> Item.itemToString) Jsonstore.int .stone
        |> Jsonstore.withMaybe (Wood |> Item.itemToString) Jsonstore.int .wood
        |> Jsonstore.withMaybe (Chipboard |> Item.itemToString) Jsonstore.int .chipboard
        |> Jsonstore.toJson


sync : Task Http.Error (Bag String)
sync =
    json
        |> Jsonstore.decode
        |> Jsonstore.get String.url
        |> Task.map (Maybe.map toBag >> Maybe.withDefault Bag.empty)


remove : Item -> Int -> Task Http.Error ()
remove item amount =
    Jsonstore.update
        { url = String.url ++ "/" ++ (item |> Item.itemToString)
        , decoder = Jsonstore.int |> Jsonstore.decode
        , value =
            Maybe.andThen
                (\n ->
                    if n <= amount then
                        Nothing

                    else
                        Just <| Jsonstore.encode Jsonstore.int <| n - amount
                )
        }


insert : Item -> Int -> Task Http.Error ()
insert item amount =
    Jsonstore.update
        { url = String.url ++ "/" ++ (item |> Item.itemToString)
        , decoder = Jsonstore.int |> Jsonstore.decode
        , value =
            Maybe.map ((+) amount)
                >> Maybe.withDefault amount
                >> Jsonstore.encode Jsonstore.int
                >> Just
        }

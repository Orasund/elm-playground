module FactoryCity.Data.RemoteShop exposing (default, insert, remove, sync)

import Bag exposing (Bag)
import FactoryCity.Data as Data
import FactoryCity.Data.CellType exposing (ContainerSort(..))
import FactoryCity.Data.Item as Item exposing (Item(..))
import FactoryCity.String as String
import Firestore exposing (Error(..))
import Firestore.Decode as D exposing (Decoder)
import Firestore.Encode as E exposing (Encoder)
import Http
import Json.Encode
import Jsonstore exposing (Json)
import Task exposing (Task)


type alias RemoteShop =
    { chips : Int
    , iron : Int
    , scrap : Int
    , stone : Int
    , wood : Int
    , chipboard : Int
    }


default : Bag String
default =
    { chips = 500
    , iron = 500
    , scrap = 500
    , stone = 500
    , wood = 500
    , chipboard = 500
    }
        |> toBag


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
            (Tuple.mapFirst Item.itemToString)
        |> Bag.fromList


decoder : Decoder RemoteShop
decoder =
    D.document RemoteShop
        |> D.optional (Chips |> Item.itemToString) D.int 0
        |> D.optional (Iron |> Item.itemToString) D.int 0
        |> D.optional (Scrap |> Item.itemToString) D.int 0
        |> D.optional (Stone |> Item.itemToString) D.int 0
        |> D.optional (Wood |> Item.itemToString) D.int 0
        |> D.optional (Chipboard |> Item.itemToString) D.int 0


encoder : RemoteShop -> Encoder
encoder shop =
    [ ( Chips, .chips )
    , ( Iron, .iron )
    , ( Scrap, .scrap )
    , ( Stone, .stone )
    , ( Wood, .wood )
    , ( Chipboard, .chipboard )
    ]
        |> List.filterMap
            (\( item, fun ) ->
                shop
                    |> fun
                    |> (\amount ->
                            Just ( item |> Item.itemToString, E.int amount )
                       )
            )
        |> E.document


mapTask :
    Task Firestore.Error (Firestore.Document RemoteShop)
    -> Task Http.Error (Bag String)
mapTask =
    Task.map (.fields >> toBag)
        >> Task.onError
            (\err ->
                case err of
                    Http_ httpError ->
                        httpError
                            |> Task.fail

                    Response response ->
                        let
                            _ =
                                response
                                    |> Debug.log "Response"
                        in
                        default
                            |> Task.succeed
            )


sync : Task Http.Error (Bag String)
sync =
    Data.firestore
        |> Firestore.get decoder
        |> mapTask


remove : Item -> Int -> Task Http.Error (Bag String)
remove item amount =
    Data.firestore
        |> Firestore.get decoder
        |> Task.andThen
            (\{ fields } ->
                Data.firestore
                    |> Firestore.patch decoder
                        ((case item of
                            Chips ->
                                { fields | chips = fields.chips - amount |> max 0 }

                            Iron ->
                                { fields | iron = fields.iron - amount |> max 0 }

                            Scrap ->
                                { fields | scrap = fields.scrap - amount |> max 0 }

                            Stone ->
                                { fields | stone = fields.stone - amount |> max 0 }

                            Wood ->
                                { fields | wood = fields.wood - amount |> max 0 }

                            Chipboard ->
                                { fields | chipboard = fields.chipboard - amount |> max 0 }
                         )
                            |> encoder
                        )
            )
        |> mapTask



{--Jsonstore.update
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
        }--}


insert : Item -> Int -> Task Http.Error (Bag String)
insert item amount =
    Data.firestore
        |> Firestore.get decoder
        |> Task.andThen
            (\{ fields } ->
                Data.firestore
                    |> Firestore.patch decoder
                        ((case item of
                            Chips ->
                                { fields | chips = fields.chips + amount }

                            Iron ->
                                { fields | iron = fields.iron + amount }

                            Scrap ->
                                { fields | scrap = fields.scrap + amount }

                            Stone ->
                                { fields | stone = fields.stone + amount }

                            Wood ->
                                { fields | wood = fields.wood + amount }

                            Chipboard ->
                                { fields | chipboard = fields.chipboard + amount }
                         )
                            |> encoder
                        )
            )
        |> mapTask



{--Jsonstore.update
        { url = String.url ++ "/" ++ (item |> Item.itemToString)
        , decoder = Jsonstore.int |> Jsonstore.decode
        , value =
            Maybe.map ((+) amount)
                >> Maybe.withDefault amount
                >> Jsonstore.encode Jsonstore.int
                >> Just
        }--}

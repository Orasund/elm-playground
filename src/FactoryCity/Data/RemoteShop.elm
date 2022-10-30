module FactoryCity.Data.RemoteShop exposing (default)

--,insert, remove, sync)

import Bag exposing (Bag)
import FactoryCity.Data.CellType exposing (ContainerSort(..))
import FactoryCity.Data.Item as Item exposing (Item(..))
import Firestore exposing (Error(..))


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



{--codec : Codec RemoteShop
codec =
    Codec.document RemoteShop
        |> Codec.optional (Chips |> Item.itemToString) .chips Codec.int 0
        |> Codec.optional (Iron |> Item.itemToString) .iron Codec.int 0
        |> Codec.optional (Scrap |> Item.itemToString) .scrap Codec.int 0
        |> Codec.optional (Stone |> Item.itemToString) .stone Codec.int 0
        |> Codec.optional (Wood |> Item.itemToString) .wood Codec.int 0
        |> Codec.optional (Chipboard |> Item.itemToString) .chipboard Codec.int 0
        |> Codec.build


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
                        in
                        default
                            |> Task.succeed
            )



sync : Task Http.Error (Bag String)
sync =
    Data.firestore
        |> Firestore.get (codec |> Codec.asDecoder)
        |> mapTask--}
{--remove : Item -> Int -> Task Http.Error (Bag String)
remove item amount =
    Data.firestore
        |> Firestore.get (codec |> Codec.asDecoder)
        |> Task.andThen
            (\{ fields } ->
                Data.firestore
                    |> Firestore.patch (codec |> Codec.asDecoder)
                        { updateFields =
                            [ ( item |> Item.itemToString
                              , fields
                                    |> (case item of
                                            Chips ->
                                                .chips

                                            Iron ->
                                                .iron

                                            Scrap ->
                                                .scrap

                                            Stone ->
                                                .stone

                                            Wood ->
                                                .wood

                                            Chipboard ->
                                                .chipboard
                                       )
                                    |> (+) -amount
                                    |> max 0
                                    |> E.int
                              )
                            ]
                        , deleteFields = []
                        }
            )
        |> mapTask--}
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
{--insert : Item -> Int -> Task Http.Error (Bag String)
insert item amount =
    Data.firestore
        |> Firestore.get (codec |> Codec.asDecoder)
        |> Task.andThen
            (\{ fields } ->
                Data.firestore
                    |> Firestore.patch (codec |> Codec.asDecoder)
                        { updateFields =
                            [ ( item |> Item.itemToString
                              , fields
                                    |> (case item of
                                            Chips ->
                                                .chips

                                            Iron ->
                                                .iron

                                            Scrap ->
                                                .scrap

                                            Stone ->
                                                .stone

                                            Wood ->
                                                .wood

                                            Chipboard ->
                                                .chipboard
                                       )
                                    |> (+) amount
                                    |> E.int
                              )
                            ]
                        , deleteFields = []
                        }
            )
        |> mapTask
--}
{--Jsonstore.update
        { url = String.url ++ "/" ++ (item |> Item.itemToString)
        , decoder = Jsonstore.int |> Jsonstore.decode
        , value =
            Maybe.map ((+) amount)
                >> Maybe.withDefault amount
                >> Jsonstore.encode Jsonstore.int
                >> Just
        }--}

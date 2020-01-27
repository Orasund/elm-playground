module FactoryCity.Data.Item exposing (..)


type Item
    = Wood
    | Stone
    | Iron
    | Scrap
    | Chips
    | Chipboard


itemList : List Item
itemList =
    [ Wood, Stone, Iron, Scrap, Chips, Chipboard ]


color : Item -> ( Int, Int, Int )
color item =
    case item of
        Wood ->
            ( 255, 194, 170 )

        Stone ->
            ( 117, 175, 150 )

        Iron ->
            ( 102, 153, 153 )

        Scrap ->
            ( 255, 170, 170 )

        Chips ->
            ( 255, 255, 170 )

        Chipboard ->
            ( 255, 226, 170 )

burnable : List Item
burnable =
    [ Wood, Chips, Chipboard ]


smeltable : List ( Item, Item )
smeltable =
    [ ( Stone, Iron )
    , ( Chipboard, Scrap )
    ]


shreddable : List ( Item, Item )
shreddable =
    [ ( Stone, Scrap )
    , ( Wood, Chips )
    , ( Iron, Scrap )
    , ( Scrap, Chips)
    ]


pressable : List ( Item, Item )
pressable =
    [ ( Scrap, Stone )
    , ( Chips, Chipboard )
    ]

itemToString : Item -> String
itemToString item =
    case item of
        Wood ->
            "Wood"

        Stone ->
            "Stone"

        Iron ->
            "Iron"

        Scrap ->
            "Scrap"

        Chips ->
            "Chips"

        Chipboard ->
            "Chipboard"


stringToItem : String -> Maybe Item
stringToItem string =
    case string of
        "Wood" ->
            Just Wood

        "Stone" ->
            Just Stone

        "Iron" ->
            Just Iron

        "Scrap" ->
            Just Scrap

        "Chips" ->
            Just Chips

        "Chipboard" ->
            Just Chipboard

        _ ->
            Nothing
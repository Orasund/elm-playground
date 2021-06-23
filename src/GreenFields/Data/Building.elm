module GreenFields.Data.Building exposing (Building(..), buildingCost, color, effects, fromString, generate, getSymbol, listTier0, listTier1, produces, restoringCost, toString, upgradedProduction, upgrades)

import Array
import Bag exposing (Bag)
import Color as C exposing (Color)
import GreenFields.Data.Effect exposing (Effect(..))
import GreenFields.Data.Resource as Resource exposing (Resource(..))
import GreenFields.View.Color as Color
import List.Extra as List
import Random exposing (Generator)
import Set exposing (Set)


type Building
    = Query
    | Farm
    | Forest
    | Warehouse
    | Street
    | Field
    | TreeNursery
    | Mine
    | Lumberjack
    | Inn
    | Weaving
    | Carpentry
    | Mason
    | Market


listTier0 : List Building
listTier0 =
    [ Query, Farm, Forest ]


listTier1 : List Building
listTier1 =
    [ Warehouse, Street, Field, TreeNursery ]


toString : Building -> String
toString building =
    case building of
        Query ->
            "Query"

        Farm ->
            "Farm"

        Forest ->
            "Forest"

        Warehouse ->
            "Warehouse"

        Street ->
            "Street"

        Field ->
            "Field"

        TreeNursery ->
            "Tree Nursery"

        Mine ->
            "Mine"

        Lumberjack ->
            "Lumberjack"

        Inn ->
            "Inn"

        Weaving ->
            "Weaving"

        Carpentry ->
            "Carpentry"

        Mason ->
            "Mason"

        Market ->
            "Market"


fromString : String -> Building
fromString string =
    case string of
        "Query" ->
            Query

        "Farm" ->
            Farm

        "Forest" ->
            Forest

        "Warehouse" ->
            Warehouse

        "Street" ->
            Street

        "Field" ->
            Field

        "Tree Nursery" ->
            TreeNursery

        "Mine" ->
            Mine

        "Lumberjack" ->
            Lumberjack

        "Inn" ->
            Inn

        "Weaving" ->
            Weaving

        "Carpentry" ->
            Carpentry

        "Mason" ->
            Mason

        "Market" ->
            Market

        _ ->
            Query


restoringCost : Building -> Bag String
restoringCost building =
    (case building of
        Query ->
            [ ( Wood, 1 ) ]

        Farm ->
            [ ( Wood, 1 ) ]

        Forest ->
            [ ( Wood, 1 ) ]

        Warehouse ->
            [ ( Wood, 2 ) ]

        Street ->
            []

        Field ->
            [ ( Sheep, 1 ) ]

        TreeNursery ->
            [ ( Wood, 1 ) ]

        Mine ->
            [ ( Wood, 1 ) ]

        Lumberjack ->
            [ ( Wood, 2 ) ]

        Inn ->
            [ ( Wood, 2 ), ( Silver, 1 ) ]

        Weaving ->
            [ ( Sheep, 2 ) ]

        Carpentry ->
            [ ( Wood, 3 ) ]

        Mason ->
            [ ( Wood, 1 ) ]

        Market ->
            [ ( Silver, 2 ) ]
    )
        |> List.map (Tuple.mapFirst Resource.toString)
        |> Bag.fromList


produces : Building -> Bag String
produces building =
    (case building of
        Query ->
            [ ( Stone, 3 ) ]

        Farm ->
            [ ( Sheep, 3 ) ]

        Forest ->
            [ ( Wood, 2 ) ]

        Warehouse ->
            []

        Street ->
            []

        Field ->
            [ ( Sheep, 2 ) ]

        TreeNursery ->
            [ ( Wood, 2 ) ]

        Mine ->
            [ ( Stone, 4 ) ]

        Lumberjack ->
            [ ( Wood, 4 ) ]

        Inn ->
            []

        Weaving ->
            [ ( Silver, 1 ) ]

        Carpentry ->
            [ ( Wood, 4 ), ( Silver, 1 ) ]

        Mason ->
            [ ( Stone, 3 ), ( Silver, 2 ) ]

        Market ->
            [ ( Sheep, 1 ), ( Stone, 1 ), ( Wood, 1 ) ]
    )
        |> List.map (Tuple.mapFirst Resource.toString)
        |> Bag.fromList


buildingCost : Building -> Bag String
buildingCost building =
    (case building of
        Query ->
            []

        Farm ->
            []

        Forest ->
            []

        Warehouse ->
            [ ( Wood, 2 ) ]

        Street ->
            [ ( Stone, 2 ) ]

        Field ->
            [ ( Sheep, 2 ), ( Wood, 1 ) ]

        TreeNursery ->
            [ ( Wood, 4 ) ]

        Mine ->
            [ ( Stone, 2 ) ]

        Lumberjack ->
            [ ( Wood, 2 ) ]

        Inn ->
            [ ( Silver, 1 ) ]

        Weaving ->
            [ ( Sheep, 2 ) ]

        Carpentry ->
            [ ( Wood, 6 ) ]

        Mason ->
            [ ( Stone, 4 ) ]

        Market ->
            [ ( Silver, 2 ) ]
    )
        |> List.map (Tuple.mapFirst Resource.toString)
        |> Bag.fromList


upgrades : Building -> List Building
upgrades building =
    case building of
        Query ->
            [ Mine ]

        Forest ->
            [ Lumberjack ]

        TreeNursery ->
            [ Lumberjack ]

        Field ->
            [ Market ]

        Warehouse ->
            [ Inn, Weaving, Market ]

        Lumberjack ->
            [ Carpentry ]

        Mine ->
            [ Mason ]

        _ ->
            []


effects : Building -> List Effect
effects building =
    case building of
        Inn ->
            [ IncreaseInventorySpace 8 ]

        _ ->
            []


color : Building -> Color
color building =
    case building of
        Field ->
            Color.lightGreen

        Street ->
            Color.lightGray

        TreeNursery ->
            Color.lightRed

        _ ->
            Color.white



--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------


upgradedProduction : { old : Building, new : Building } -> Bag String
upgradedProduction args =
    args.old
        |> produces
        |> Bag.foldl
            (\string int ->
                Bag.remove int string
            )
            (args.new |> produces)


generate : Generator Building
generate =
    case listTier0 of
        head :: tail ->
            Random.uniform head tail

        [] ->
            --dead branch
            Random.constant Query


getSymbol : Building -> String
getSymbol building =
    let
        array =
            building
                |> toString
                |> String.toList
                |> Array.fromList

        arr2 =
            array |> Array.filter (\char -> (char /= 'a') && (char /= 'e') && (char /= 'i') && (char /= 'o') && (char /= 'u'))
    in
    [ array |> Array.get 0
    , arr2 |> Array.get ((arr2 |> Array.length) // 2)
    , array |> Array.get (array |> Array.length |> (+) -1)
    ]
        |> List.filterMap identity
        |> String.fromList

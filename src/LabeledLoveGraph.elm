module LabeledLoveGraph exposing (NodeLabel, labeledLoveGraph)

import Graph exposing (Edge, Node)
import LoveGraph exposing (Connection(..), Gender(..), LoveGraph, Person, contactPerson, femalePerson, malePerson)


type alias NodeLabel =
    { name : String
    , gender : Gender
    }


femaleName : Int -> String
femaleName id =
    case id % 5 of
        0 ->
            "Flora"

        1 ->
            "Florentine"

        2 ->
            "Franziska"

        3 ->
            "Frieda"

        _ ->
            "Freya"


maleName : Int -> String
maleName id =
    case id % 10 of
        0 ->
            "Manfried"

        1 ->
            "Manuel"

        2 ->
            "Marcellus"

        3 ->
            "Mario"

        4 ->
            "Markus"

        5 ->
            "Martin"

        6 ->
            "Matthäus"

        7 ->
            "Maximilian"

        8 ->
            "Michael"

        _ ->
            "Moritz"


toNode : Person -> Node NodeLabel
toNode person =
    let
        name =
            case person.label of
                F ->
                    femaleName person.id

                _ ->
                    maleName person.id
    in
    Node person.id (NodeLabel name person.label)


labeledLoveGraph : LoveGraph -> Graph.Graph NodeLabel Connection
labeledLoveGraph graph =
    Graph.fromNodesAndEdges
        (graph |> Graph.nodes |> List.map toNode)
        (graph |> Graph.edges)

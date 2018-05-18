module SampleData exposing (sampleData)

import Graph exposing (Adjacency, Edge, Node, NodeId)
import IntDict
import LoveGraph exposing (Connection(..), Gender(..), LoveGraph, Person, Relation, contactPerson, femalePerson, malePerson)


type alias ConstructorLoveGraph =
    { graph : LoveGraph, id : NodeId, total : NodeId }


appendFemale : ConstructorLoveGraph -> ConstructorLoveGraph
appendFemale a =
    ConstructorLoveGraph
        (a.graph
            |> Graph.insert (Graph.NodeContext (femalePerson a.id) IntDict.empty IntDict.empty)
        )
        (a.id + 1)
        a.total


appendMale : Int -> ConstructorLoveGraph -> ConstructorLoveGraph
appendMale i a =
    let
        fold : NodeId -> LoveGraph -> LoveGraph
        fold id graph =
            graph
                |> Graph.insert
                    (Graph.NodeContext
                        (contactPerson id)
                        (IntDict.singleton a.id Friend)
                        IntDict.empty
                    )
    in
    ConstructorLoveGraph
        (List.range a.total (a.total + i - 1)
            |> List.foldl fold
                (a.graph
                    |> Graph.insert
                        (Graph.NodeContext (malePerson a.id) IntDict.empty IntDict.empty)
                )
        )
        (a.id + 1)
        (a.total + i)


loveGraph : List ( Gender, Int ) -> List Relation -> LoveGraph
loveGraph people relations =
    let
        addPerson : ( Gender, Int ) -> ConstructorLoveGraph -> ConstructorLoveGraph
        addPerson a graph =
            case a |> Tuple.first of
                F ->
                    graph |> appendFemale

                _ ->
                    graph |> appendMale (a |> Tuple.second)

        nodes : LoveGraph
        nodes =
            people
                |> List.foldl addPerson (ConstructorLoveGraph Graph.empty 0 (List.length people))
                |> .graph
    in
    Graph.fromNodesAndEdges (nodes |> Graph.nodes)
        (nodes
            |> Graph.edges
            |> List.append relations
        )


sampleData : LoveGraph
sampleData =
    loveGraph
        [   (F,0) --0
        ,   (F,0) --1
        , ( M, 0 ) --2
        , ( M, 1 ) --3
        , ( M, 2 ) --4
        , ( M, 3 ) --5
        , ( M, 4 ) --6
        ]
        [ Edge 2 0 Friend
        , Edge 2 1 Friend
        , Edge 3 0 Friend
        , Edge 3 1 Friend
        , Edge 4 1 Friend
        , Edge 5 1 Friend
        , Edge 6 1 Friend
        ]

    {-    [ ( F, 0 ) --0
        , ( F, 0 ) --1
        , ( F, 0 ) --2
        , ( F, 0 ) --3
        , ( F, 0 ) --4
        , ( M, 2 ) --5
        , ( M, 1 ) --6
        , ( M, 0 ) --7
        , ( M, 1 ) --8
        , ( M, 0 ) --9
        , ( M, 0 ) --10
        , ( M, 0 ) --11
        , ( M, 0 ) --12
        , ( M, 0 ) --13
        , ( M, 0 ) --14
        ]
        [ Edge 5 0 Friend
        , Edge 6 0 Friend
        , Edge 6 1 Friend
        , Edge 7 0 Friend
        , Edge 7 2 Friend
        , Edge 7 6 Friend
        , Edge 8 0 Friend
        , Edge 8 3 Friend
        , Edge 8 6 Friend
        , Edge 9 0 Friend
        , Edge 9 4 Friend
        , Edge 10 0 Friend
        , Edge 11 1 Friend
        , Edge 12 3 Friend
        , Edge 13 2 Friend
        , Edge 14 4 Friend
        ]-}

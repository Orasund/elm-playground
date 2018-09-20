module SampleData exposing (SampleData, map, sampleData, toList, update)

import Graph exposing (Edge, NodeId)
import IntDict
import LoveGraph exposing (Connection(..), Gender(..), LoveGraph, Relation, contactPerson, femalePerson, malePerson)


type alias ConstructorLoveGraph =
    { graph : LoveGraph
    , id : NodeId
    , total : NodeId
    }


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


type alias SampleData a =
    { introGraph : a
    , friendlyTriangle : a
    , twoCircles : a
    , hostileTriangle : a
    , loveSquare : a
    , nonTerminatingTiangle : a
    , theoremContact : a
    , testing : a
    , doubleOnOff : a
    }


toList : SampleData a -> List ( String, a )
toList data =
    [ ( "introGraph", data.introGraph )
    , ( "friendlyTriangle", data.friendlyTriangle )
    , ( "twoCircles", data.twoCircles )
    , ( "hostileTriangle", data.hostileTriangle )
    , ( "loveSquare", data.loveSquare )
    , ( "nonTerminatingTiangle", data.nonTerminatingTiangle )
    , ( "theoremContact", data.theoremContact )
    , ( "testing", data.testing )
    , ( "doubleOnOff", data.doubleOnOff )
    ]


map : (a -> b) -> SampleData a -> SampleData b
map fun data =
    { introGraph = fun data.introGraph
    , twoCircles = fun data.twoCircles
    , friendlyTriangle = fun data.friendlyTriangle
    , hostileTriangle = fun data.hostileTriangle
    , loveSquare = fun data.loveSquare
    , nonTerminatingTiangle = fun data.nonTerminatingTiangle
    , theoremContact = fun data.theoremContact
    , testing = fun data.testing
    , doubleOnOff = fun data.doubleOnOff
    }


update : String -> (a -> a) -> SampleData a -> SampleData a
update name fun data =
    case name of
        "introGraph" ->
            { data | introGraph = fun data.introGraph }

        "twoCircles" ->
            { data | twoCircles = fun data.twoCircles }

        "friendlyTriangle" ->
            { data | friendlyTriangle = fun data.friendlyTriangle }

        "hostileTriangle" ->
            { data | hostileTriangle = fun data.hostileTriangle }

        "loveSquare" ->
            { data | loveSquare = fun data.loveSquare }

        "nonTerminatingTiangle" ->
            { data | nonTerminatingTiangle = fun data.nonTerminatingTiangle }

        "theoremContact" ->
            { data | theoremContact = fun data.theoremContact }

        "testing" ->
            { data | testing = fun data.testing }

        "doubleOnOff" ->
            { data | doubleOnOff = fun data.doubleOnOff }

        _ ->
            data


sampleData : SampleData LoveGraph
sampleData =
    { testing =
        loveGraph
            [ ( F, 0 ) --0
            , ( F, 0 ) --1
            , ( F, 0 ) --2
            , ( M, 2 ) --3
            , ( M, 2 ) --4
            , ( M, 1 ) --5
            , ( M, 1 ) --6
            , ( M, 0 ) --7 --
            , ( M, 0 ) --8
            , ( M, 0 ) --9
            , ( M, 0 ) --10
            , ( M, 4 ) --11 --
            , ( M, 4 ) --12
            , ( M, 5 ) --13 --
            , ( M, 5 ) --14
            , ( M, 4 ) --15
            , ( M, 4 ) --16
            , ( M, 0 ) --17
            , ( M, 0 ) --18
            , ( M, 4 ) --19
            , ( M, 4 ) --20
            ]
            [ Edge 3 0 Friend
            , Edge 4 0 Friend
            , Edge 4 1 Friend
            , Edge 5 1 Friend
            , Edge 5 2 Friend
            , Edge 6 2 Friend
            , Edge 7 0 Friend
            , Edge 8 0 Friend
            , Edge 9 0 Friend
            , Edge 10 0 Friend
            , Edge 11 1 Friend
            , Edge 12 1 Friend
            , Edge 13 2 Friend
            , Edge 14 2 Friend
            , Edge 15 2 Friend
            , Edge 16 2 Friend
            , Edge 17 0 Friend
            , Edge 18 0 Friend
            , Edge 19 1 Friend
            , Edge 20 1 Friend
            ]
    , doubleOnOff =
        loveGraph
            [ ( F, 0 ) --0
            , ( F, 0 ) --1
            , ( M, 1 ) --2
            , ( M, 1 ) --3
            , ( M, 1 ) --4
            , ( M, 4 ) --5
            , ( M, 4 ) --6
            , ( M, 0 ) --7
            , ( M, 0 ) --8
            , ( M, 0 ) --9
            , ( M, 0 ) --10
            ]
            [ Edge 2 0 Friend
            , Edge 3 0 Friend
            , Edge 3 1 Friend
            , Edge 4 1 Friend
            , Edge 5 1 Friend
            , Edge 6 1 Friend
            , Edge 7 0 Friend
            , Edge 8 0 Friend
            , Edge 9 0 Friend
            , Edge 10 0 Friend
            ]
    , introGraph =
        loveGraph
            [ ( F, 0 ) --0
            , ( F, 0 ) --1
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
    , nonTerminatingTiangle =
        loveGraph
            [ ( F, 0 ) --0
            , ( M, 1 ) --1
            , ( M, 2 ) --2
            , ( M, 0 ) --3
            , ( M, 0 ) --4
            ]
            [ Edge 1 0 Friend
            , Edge 2 0 Friend
            , Edge 3 0 Friend
            , Edge 4 0 Friend
            ]
    , loveSquare =
        loveGraph
            [ ( F, 0 ) --0
            , ( F, 0 ) --1
            , ( M, 0 ) --2
            , ( M, 1 ) --3
            , ( M, 2 ) --4
            ]
            [ Edge 2 0 Friend
            , Edge 3 0 Friend
            , Edge 4 0 Friend
            , Edge 2 1 Friend
            , Edge 3 1 Friend
            ]
    , friendlyTriangle =
        loveGraph
            [ ( F, 0 ) --0
            , ( F, 0 ) --1
            , ( M, 0 ) --2
            , ( M, 1 ) --3
            ]
            [ Edge 2 0 Friend
            , Edge 2 1 Friend
            , Edge 3 0 Friend
            ]
    , hostileTriangle =
        loveGraph
            [ ( F, 0 ) --0
            , ( F, 0 ) --1
            , ( M, 0 ) --2
            , ( M, 1 ) --3
            ]
            [ Edge 2 1 Friend
            , Edge 3 0 Friend
            , Edge 3 1 Friend
            ]
    , theoremContact =
        loveGraph
            [ ( F, 0 ) --0
            , ( F, 0 ) --1
            , ( M, 0 ) --2
            , ( M, 1 ) --3
            , ( M, 2 ) --4
            ]
            [ Edge 2 0 Friend
            , Edge 2 1 Friend
            , Edge 4 1 Friend
            , Edge 3 0 Friend
            ]
    , twoCircles =
        loveGraph
            [ ( F, 0 ) --0
            , ( F, 0 ) --1
            , ( M, 0 ) --2
            , ( M, 1 ) --3
            , ( M, 0 ) --4
            , ( M, 2 ) --5
            , ( M, 2 ) --6
            ]
            [ Edge 2 0 Friend
            , Edge 2 1 Friend
            , Edge 3 0 Friend
            , Edge 3 1 Friend
            , Edge 4 1 Friend
            , Edge 5 1 Friend
            , Edge 6 1 Friend
            ]
    }

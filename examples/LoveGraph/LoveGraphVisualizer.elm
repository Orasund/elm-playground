module LoveGraphVisualizer exposing (Model, Msg, init, subscriptions, update, view)

import AnimationFrame exposing (times)
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (Html, button, program)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import LabeledLoveGraph exposing (NodeLabel, labeledLoveGraph)
import LoveGraph exposing (Connection(..), Gender(..), LoveGraph)
import LoveGraphAutomata exposing (EntityNode, Visualisation, updateState)
import Mouse exposing (Position)
import Svg exposing (Attribute, Svg, g, line, rect, svg, text, text_)
import Svg.Attributes as Attr exposing (..)
import Time exposing (Time)
import Visualization.Force as Force exposing (State, simulation)


screenWidth : Float
screenWidth =
    600


screenHeight : Float
screenHeight =
    300


type Msg
    = DragStart NodeId Position
    | DragAt Position
    | DragEnd Position
    | Tick Time
    | NewState
    | Reset


type alias Model =
    { drag : Maybe Drag
    , graph : Visualisation
    , simulation : Force.State NodeId
    , initialState : Visualisation
    }


type alias Drag =
    { start : Position
    , current : Position
    , index : NodeId
    }



{--miserablesGraph : Graph.Graph NodeLabel Connection
miserablesGraph =
    labeledLoveGraph sampleData--}


init : LoveGraph -> ( Model, Cmd Msg )
init data =
    let
        {--graph : Visualisation--}
        graph =
            Graph.mapContexts
                (\({ node } as ctx) ->
                    { ctx | node = { label = Force.entity node.id node.label, id = node.id } }
                )
                (labeledLoveGraph data)

        {--link : Edge a -> ( NodeId, NodeId )--}
        {--link { from, to } =
            ( from, to )--}

        link2 { from, to } =
            if data |> Graph.nodes |> List.length |> (>) 20 then
                { source = from
                , target = to
                , distance = 10 --1
                , strength = Just 0.1
                }
            else
                { source = from
                , target = to
                , distance = 20 --1
                , strength = Just 0.5
                }

        {--forces : List (Force.Force Int)--}
        forces =
            [ Force.customLinks 1 <| List.map link2 <| Graph.edges graph

            --Force.links <| List.map link <| Graph.edges graph
            , Force.manyBody <| List.map .id <| Graph.nodes graph
            , Force.center (screenWidth / 2) (screenHeight / 2)
            ]
    in
    Model Nothing graph (Force.simulation forces) graph ! [ Cmd.none ]
--}


updateNode : Position -> NodeContext EntityNode a -> NodeContext EntityNode a
updateNode pos nodeCtx =
    let
        nodeValue : EntityNode
        nodeValue =
            nodeCtx.node.label
    in
    updateContextWithValue nodeCtx { nodeValue | x = toFloat pos.x, y = toFloat pos.y }


updateContextWithValue : NodeContext EntityNode a -> EntityNode -> NodeContext EntityNode a
updateContextWithValue nodeCtx value =
    let
        node : Node EntityNode
        node =
            nodeCtx.node
    in
    { nodeCtx | node = { node | label = value } }


updateGraphWithList : Visualisation -> List EntityNode -> Visualisation
updateGraphWithList =
    let
        graphUpdater : EntityNode -> Maybe (NodeContext EntityNode Connection) -> Maybe (NodeContext EntityNode Connection)
        graphUpdater value =
            Maybe.map (\ctx -> updateContextWithValue ctx value)
    in
    List.foldr (\node graph -> Graph.update node.id (graphUpdater node) graph)


update : Msg -> Model -> Model
update msg ({ drag, graph, simulation, initialState } as model) =
    case msg of
        Tick t ->
            let
                ( newState, list ) =
                    Force.tick simulation <| List.map .label <| Graph.nodes graph
            in
            case drag of
                Nothing ->
                    { model | drag = drag, graph = updateGraphWithList graph list, simulation = newState }

                Just { current, index } ->
                    { model | drag = drag, graph = Graph.update index (Maybe.map (updateNode current)) (updateGraphWithList graph list), simulation = newState }

        DragStart index xy ->
            { model | drag = Just (Drag xy xy index) }

        DragAt xy ->
            case drag of
                Just { start, index } ->
                    { model
                        | drag = Just (Drag start xy index)
                        , graph = Graph.update index (Maybe.map (updateNode xy)) graph
                        , simulation = Force.reheat simulation
                    }

                Nothing ->
                    { model | drag = Nothing }

        DragEnd xy ->
            case drag of
                Just { start, index } ->
                    { model | drag = Nothing, graph = Graph.update index (Maybe.map (updateNode xy)) graph }

                Nothing ->
                    { model | drag = Nothing }

        NewState ->
            { model | graph = updateState graph, simulation = Force.reheat simulation }

        Reset ->
            { model | graph = initialState, simulation = Force.reheat simulation }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            -- This allows us to save resources, as if the simulation is done, there is no point in subscribing
            -- to the rAF.
            if Force.isCompleted model.simulation then
                Sub.none
            else
                AnimationFrame.times Tick

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd, AnimationFrame.times Tick ]


onMouseDown : NodeId -> Attribute Msg
onMouseDown index =
    on "mousedown" (Decode.map (DragStart index) Mouse.position)


linkElement : Visualisation -> Edge Connection -> Svg msg
linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 (NodeLabel "" F)) <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 (NodeLabel "" F)) <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
    case edge.label of
        Partner ->
            line
                [ strokeWidth "5"
                , stroke "#983352"
                , x1 (toString source.x)
                , y1 (toString source.y)
                , x2 (toString target.x)
                , y2 (toString target.y)
                ]
                []

        _ ->
            line
                [ strokeWidth "1"
                , stroke "#aaa"
                , x1 (toString source.x)
                , y1 (toString source.y)
                , x2 (toString target.x)
                , y2 (toString target.y)
                ]
                []


moodEmoji : Gender -> String
moodEmoji a =
    case a of
        F ->
            "👩"

        _ ->
            "👨"


defaultNode : Node EntityNode -> Svg Msg
defaultNode node =
    g []
        [ rect
            [ x (toString (node.label.x - 30))
            , y (toString (node.label.y - 30))
            , width "60"
            , height "60"
            , fill "#000"
            , fillOpacity "0"
            , stroke "transparent"
            , strokeWidth "7px"
            , onMouseDown node.id
            ]
            []
        , text_
            [ x (toString (node.label.x - 13))
            , y (toString (node.label.y + 7))
            , Attr.style "font-size:20px"
            ]
            [ text (moodEmoji node.label.value.gender) ]
        , text_
            [ x (toString (node.label.x - 2))
            , y (toString (node.label.y + 20))
            , Attr.style "font-size:8px;"
            ]
            [ text (toString node.id) ]

        -- [ text (toString node.id ++ ":" ++ node.label.value.name) ]
        ]


contactNode : Node EntityNode -> Svg Msg
contactNode node =
    text_
        [ x (toString (node.label.x - 6))
        , y (toString (node.label.y + 3))
        , Attr.style "font-size:10px"
        ]
        [ text (moodEmoji node.label.value.gender) ]


nodeElement : Node EntityNode -> Svg Msg
nodeElement node =
    node
        |> (case node.label.value.gender of
                C ->
                    contactNode

                _ ->
                    defaultNode
           )


canvas : Model -> Html Msg
canvas model =
    svg [ width (toString screenWidth ++ "px"), height (toString screenHeight ++ "px") ]
        [ g [ class "links" ] <| List.map (linkElement model.graph) <| Graph.edges model.graph
        , g [ class "nodes" ] <| List.map nodeElement <| Graph.nodes model.graph
        ]


view : Model -> Html Msg
view model =
    Html.div []
        [ canvas model
        , Html.p []
            [ ButtonGroup.buttonGroup []
                [ ButtonGroup.button [ Button.outlinePrimary, Button.onClick Reset ] [ text "Reset" ]
                , ButtonGroup.button [ Button.primary, Button.onClick NewState ] [ text "Weiter" ]
                ]
            ]
        ]



{---}

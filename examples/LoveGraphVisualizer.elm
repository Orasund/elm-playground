module Main exposing (..)

import AnimationFrame exposing (times)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (Html, button, program)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import LabeledLoveGraph exposing (NodeLabel, labeledLoveGraph)
import LoveGraph exposing (Connection(..), Gender(..))
import LoveGraphAutomata exposing (EntityNode, Visualisation, updateState)
import Mouse exposing (Position)
import SampleData exposing (sampleData)
import Svg exposing (Attribute, Svg, g, line, rect, svg, text, text_)
import Svg.Attributes as Attr exposing (..)
import Time exposing (Time)
import Visualization.Force as Force exposing (State, simulation)


screenWidth : Float
screenWidth =
    990


screenHeight : Float
screenHeight =
    504


type Msg
    = DragStart NodeId Position
    | DragAt Position
    | DragEnd Position
    | Tick Time
    | NewState


type alias Model =
    { drag : Maybe Drag
    , graph : Visualisation
    , simulation : Force.State NodeId
    }


type alias Drag =
    { start : Position
    , current : Position
    , index : NodeId
    }


miserablesGraph : Graph.Graph NodeLabel Connection
miserablesGraph =
    labeledLoveGraph sampleData


init : ( Model, Cmd Msg )
init =
    let
        {--graph : Visualisation--}
        graph =
            Graph.mapContexts
                (\({ node } as ctx) ->
                    { ctx | node = { label = Force.entity node.id node.label, id = node.id } }
                )
                miserablesGraph

        {--link : Edge a -> ( NodeId, NodeId )--}
        link { from, to } =
            ( from, to )
        link2 {from, to } = 
            {source=from,target=to, distance=1,strength= Just 0.05}

        {--forces : List (Force.Force Int)--}
        forces =
            [ Force.customLinks 1 <| List.map link2 <| Graph.edges graph
                --Force.links <| List.map link <| Graph.edges graph
            , Force.manyBody <| List.map .id <| Graph.nodes graph
            , Force.center (screenWidth / 2) (screenHeight / 2)
            ]
    in
    Model Nothing graph (Force.simulation forces) ! [ Cmd.none ]


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
update msg ({ drag, graph, simulation } as model) =
    case msg of
        Tick t ->
            let
                ( newState, list ) =
                    Force.tick simulation <| List.map .label <| Graph.nodes graph
            in
            case drag of
                Nothing ->
                    Model drag (updateGraphWithList graph list) newState

                Just { current, index } ->
                    Model drag (Graph.update index (Maybe.map (updateNode current)) (updateGraphWithList graph list)) newState

        DragStart index xy ->
            Model (Just (Drag xy xy index)) graph simulation

        DragAt xy ->
            case drag of
                Just { start, index } ->
                    Model (Just (Drag start xy index))
                        (Graph.update index (Maybe.map (updateNode xy)) graph)
                        (Force.reheat simulation)

                Nothing ->
                    Model Nothing graph simulation

        DragEnd xy ->
            case drag of
                Just { start, index } ->
                    Model Nothing (Graph.update index (Maybe.map (updateNode xy)) graph) simulation

                Nothing ->
                    Model Nothing graph simulation

        NewState ->
            Model drag (updateState graph) simulation


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
            "👱"


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
            [ text (toString node.id)]
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
        , button [ onClick (Tick 0) ] [ text "⏮️" ]
        , button [ onClick NewState ] [ text "▶️" ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }



{---}

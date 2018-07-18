module LoveGraphAutomata exposing (EntityNode, Visualisation, updateState)

import Graph exposing (Graph, NodeContext, NodeId)
import IntDict
import LabeledLoveGraph exposing (NodeLabel)
import LoveGraph exposing (Connection(..), Gender(..), Person, Relation)
import Visualization.Force as Force


type alias MaleId =
    NodeId


type alias FemaleId =
    NodeId


type alias PartnerProposal =
    { female : FemaleId
    , male : MaleId
    }


type alias EntityNode =
    Force.Entity NodeId { value : NodeLabel }


type alias Visualisation =
    Graph EntityNode Connection


updateConnection : MaleId -> FemaleId -> Connection -> Visualisation -> Visualisation
updateConnection from to connection graph =
    graph
        |> Graph.update
            from
            (Maybe.map
                (\{ node, incoming, outgoing } ->
                    NodeContext
                        node
                        incoming
                        (outgoing
                            |> IntDict.update to (always (Just connection))
                        )
                )
            )


removeConnection : MaleId -> MaleId -> Visualisation -> Visualisation
removeConnection from to graph =
    graph
        |> Graph.update
            (max from to)
            (Maybe.map
                (\{ node, incoming, outgoing } ->
                    NodeContext
                        node
                        incoming
                        (outgoing
                            |> IntDict.remove (min from to)
                        )
                )
            )


addConnection : MaleId -> MaleId -> Visualisation -> Visualisation
addConnection from to graph =
    graph
        |> Graph.update
            (max from to)
            (Maybe.map
                (\{ node, incoming, outgoing } ->
                    NodeContext
                        node
                        incoming
                        (outgoing
                            |> IntDict.insert (min from to) Friend
                        )
                )
            )


updateState : Visualisation -> Visualisation
updateState g =
    let
        hasRelationWith : NodeId -> Relation -> Maybe NodeId
        hasRelationWith a relation =
            if relation.from == a then
                Just relation.to
            else if relation.to == a then
                Just relation.from
            else
                Nothing

        connectionsOf : Visualisation -> NodeId -> Int
        connectionsOf graph a =
            graph |> Graph.edges |> List.filterMap (hasRelationWith a) |> List.map (\_ -> 1) |> List.sum

        isFemale : Person -> Maybe FemaleId
        isFemale a =
            if a.label == F then
                Just a.id
            else
                Nothing

        females : List FemaleId
        females =
            g
                |> Graph.nodes
                |> List.map (\a -> Graph.Node a.id a.label.value.gender)
                |> List.filterMap isFemale

        maxFemaleConnections : Int
        maxFemaleConnections =
            females
                |> List.map (connectionsOf g)
                |> List.maximum
                |> Maybe.withDefault 0

        isRelationship : Relation -> Bool
        isRelationship a =
            a.label == Partner

        partnerOf : Visualisation -> NodeId -> Maybe MaleId
        partnerOf graph a =
            graph
                |> Graph.edges
                |> List.filter isRelationship
                |> List.filterMap (hasRelationWith a)
                |> List.head

        isSingle : Visualisation -> NodeId -> Bool
        isSingle graph a =
            partnerOf graph a == Nothing

        ifBetterPartnerFor : Visualisation -> FemaleId -> MaleId -> Bool
        ifBetterPartnerFor graph a b =
            case partnerOf graph a of
                Just c ->
                    connectionsOf graph b
                        |> (<) (connectionsOf graph c)

                _ ->
                    True

        candidatePrioritysOf : Visualisation -> FemaleId -> List (List MaleId)
        candidatePrioritysOf graph female =
            graph
                |> Graph.edges
                |> List.filterMap (hasRelationWith female)
                |> List.filter (isSingle graph)
                |> List.filter (ifBetterPartnerFor graph female)
                |> List.sortBy (connectionsOf graph)
                |> List.foldl
                    (\elem list ->
                        case list |> List.head of
                            Just sublist ->
                                case sublist |> List.head of
                                    Just max ->
                                        if connectionsOf graph max == connectionsOf graph elem then
                                            (elem :: sublist) :: Maybe.withDefault [] (List.tail list)
                                        else
                                            [ elem ] :: list

                                    _ ->
                                        (elem :: sublist) :: Maybe.withDefault [] (List.tail list)

                            _ ->
                                [ elem ] :: list
                    )
                    []

        candidatesFor : Visualisation -> FemaleId -> Int -> List MaleId
        candidatesFor graph female nr =
            candidatePrioritysOf graph female
                |> List.drop nr
                |> List.head
                |> Maybe.withDefault []

        newPartnerFor : Visualisation -> FemaleId -> Int -> Maybe MaleId
        newPartnerFor graph a nr =
            candidatesFor graph a nr
                |> (\list ->
                        case List.length list of
                            1 ->
                                List.head list

                            _ ->
                                Nothing
                   )

        partnerProposals : Visualisation -> Int -> List PartnerProposal
        partnerProposals graph nr =
            females
                |> List.filterMap
                    (\female ->
                        newPartnerFor graph female nr
                            |> Maybe.map (PartnerProposal female)
                    )

        isUniqueProposal : Visualisation -> Int -> PartnerProposal -> Bool
        isUniqueProposal graph nr a =
            partnerProposals graph nr
                |> List.all (\b -> b.male /= a.male || b.female == a.female)

        applyPartnerProposal : PartnerProposal -> Visualisation -> Visualisation
        applyPartnerProposal { male, female } graph =
            graph
                |> (case partnerOf graph female of
                        Just id ->
                            updateConnection id female Friend

                        _ ->
                            identity
                   )
                |> updateConnection male female Partner

        updateMaleConnections : List PartnerProposal -> Visualisation -> Visualisation
        updateMaleConnections newPartners graph =
            newPartners
                |> List.foldl
                    (\{ male, female } v1 ->
                        v1
                            |> Graph.edges
                            |> List.filterMap (hasRelationWith female)
                            |> List.filter (isSingle graph)
                            |> (\males ->
                                    males
                                        |> List.foldl
                                            (\a v2 ->
                                                males
                                                    |> List.filter ((<) a)
                                                    |> List.foldl
                                                        (\b v3 -> v3 |> addConnection a b)
                                                        (v2 |> removeConnection male a)
                                            )
                                            v1
                               )
                    )
                    graph
    in
    List.range 0 (maxFemaleConnections - 1)
        |> List.foldl
            (\nr { graph, f, proposals } ->
                f
                    |> List.partition (\a -> newPartnerFor graph a nr /= Nothing)
                    |> Tuple.mapFirst
                        (List.filterMap
                            (\female ->
                                newPartnerFor graph female nr
                                    |> Maybe.map (PartnerProposal female)
                            )
                        )
                    |> (\( first, second ) ->
                            List.partition (isUniqueProposal graph nr) first
                                |> Tuple.mapSecond (List.map .female >> List.append second)
                       )
                    |> (\( first, second ) ->
                            { graph =
                                first
                                    |> List.foldl applyPartnerProposal graph
                            , f = second
                            , proposals = proposals |> List.append first
                            }
                       )
            )
            { graph = g, f = females, proposals = [] }
        |> (\{ graph, proposals } -> graph |> updateMaleConnections proposals)

module Presentation exposing (main)

import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Grid as Grid
import Bootstrap.Tab as Tab
import Bootstrap.Text as Text
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import LoveGraphVisualizer
import SampleData exposing (SampleData, sampleData)


type alias Model =
    { visualizer : SampleData LoveGraphVisualizer.Model
    , tabState : Tab.State
    }


type Msg
    = Visualizer String LoveGraphVisualizer.Msg
    | TabMsg Tab.State


init : ( Model, Cmd Msg )
init =
    let
        visualizer : SampleData LoveGraphVisualizer.Model
        visualizer =
            sampleData
                |> SampleData.map
                    (\a ->
                        LoveGraphVisualizer.init a
                            |> Tuple.first
                    )

        tabState : Tab.State
        tabState =
            Tab.initialState
    in
    { visualizer = visualizer
    , tabState = tabState
    }
        ! [ Cmd.none ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ visualizer, tabState } as model) =
    case msg of
        Visualizer name m ->
            { model | visualizer = visualizer |> SampleData.update name (\a -> LoveGraphVisualizer.update m a) } ! [ Cmd.none ]

        TabMsg state ->
            ( { model | tabState = state }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions { visualizer, tabState } =
    visualizer
        |> SampleData.toList
        |> List.map
            (\( name, elem ) ->
                elem
                    |> LoveGraphVisualizer.subscriptions
                    |> Sub.map (Visualizer name)
            )
        |> Sub.batch


view : Model -> Html Msg
view { visualizer, tabState } =
    let
        titlePage : List (Html Msg) -> List (Html Msg) -> Html Msg
        titlePage title body =
            Card.config
                [ Card.outlineLight
                , Card.attrs [ Spacing.mb3 ]
                , Card.align Text.alignXsCenter
                ]
                |> Card.headerH1 [] title
                |> Card.block [ Block.textColor Text.primary ]
                    [ Block.quote []
                        body
                    ]
                |> Card.view

        slide : List (Html Msg) -> List (Html Msg) -> Html Msg
        slide title body =
            Card.config
                [ Card.outlineLight
                , Card.attrs [ Spacing.mb3 ]
                ]
                |> Card.headerH2 [] title
                |> Card.block [ Block.textColor Text.primary ]
                    [ Block.quote []
                        body
                    ]
                |> Card.view

        tab : List (Html Msg) -> Tab.Config Msg
        tab list =
            Tab.config TabMsg
                |> Tab.items
                    (list
                        |> List.indexedMap
                            (\id elem ->
                                Tab.item
                                    { id = "tabItem" ++ toString id
                                    , link = Tab.link [] [ Html.text (toString id) ]
                                    , pane =
                                        Tab.pane [ Spacing.mt3 ]
                                            [ elem ]
                                    }
                            )
                    )

        drawVisualizer : String -> LoveGraphVisualizer.Model -> Html Msg
        drawVisualizer name vis =
            Html.div [ class "text-center" ]
                [ LoveGraphVisualizer.view vis |> Html.map (Visualizer name)
                ]
    in
    Grid.container []
        -- Responsive fixed width container
        [ CDN.stylesheet -- Inlined Bootstrap CSS for use with reactor
        , Grid.row []
            [ Grid.col []
                [ tab
                    [ titlePage [ Html.text "Der Payrische Liebes Graphen Automat" ]
                        [ Html.p [] [ Html.text "Vortrag von Lucas Payr" ]
                        , drawVisualizer "introGraph" visualizer.introGraph
                        , Html.p [] [ Html.text "Einführung in die Liebes Graphen Theorie" ]
                        ]
                    , slide [ Html.text "Grundlagen" ]
                        [ Html.b [] [ Html.text "Liebes Graph" ]
                        , Html.ul []
                            [ Html.li [] [ Html.text "Beschriftete Knoten : 👩 und 👨" ]
                            , Html.li []
                                [ Html.text "Beschriftete Kanten : "
                                , Html.span
                                    [ style
                                        [ ( "color", "#aaa" )
                                        ]
                                    ]
                                    [ Html.text "Freundschaft" ]
                                , Html.text " und "
                                , Html.span
                                    [ style
                                        [ ( "color", "#983352" )
                                        , ( "font-weight", "bold" )
                                        ]
                                    ]
                                    [ Html.text "Beziehung" ]
                                ]
                            ]
                        , Html.b [] [ Html.text "Freundeskreis" ]
                        , Html.ul []
                            [ Html.li []
                                [ Html.text "Freundeskreis: "
                                , Html.code [] [ Html.text "B: ∃f≈👩: ∀a∈B:a≈👨∧edge(f,a) ∧ ∀b∈B:a≠b⇒b≈👨∧edge(a,b)" ]
                                ]
                            , Html.li []
                                [ Html.text "Erweiterter Freundeskreis (von f): "
                                , Html.code [] [ Html.text "B: ∃f≈👩: B = {a|a≈👨∧edge(f,a)}" ]
                                ]
                            ]
                        , Html.b [] [ Html.text "Spezielle Formen" ]
                        , Html.ul []
                            [ Html.li []
                                [ Html.text "Grundform"
                                , Html.ul []
                                    [ Html.li [] [ Html.text "Keine Freundeskreise" ]
                                    , Html.li [] [ Html.text "Keine Beziehungen" ]
                                    ]
                                ]
                            , Html.li []
                                [ Html.text "Stabile Normalform"
                                , Html.ul []
                                    [ Html.li [] [ Html.text "Normalform bezüglich Automat" ]
                                    ]
                                ]
                            ]
                        , Html.b [] [ Html.text "Wert" ]
                        , Html.br [] []
                        , Html.text "sei m≈👨."
                        , Html.ul []
                            [ Html.li []
                                [ Html.text "Wert (von m): "
                                , Html.code [] [ Html.text "|edges(m)|" ]
                                ]
                            , Html.li []
                                [ Html.text "Grundwert (von m): "
                                , Html.br [] []
                                , Html.code [] [ Html.text "| Menge aller Kanten mit Männern von m in der Grundform des Graphen |"]
                                , Html.br [] []
                                , Html.b [] [ Html.text "Vereinbarung: Grundwert ist eindeutig ⇒ Einfache Liebes Graphen" ]
                                , Html.br [] []
                                , Html.p [] [Html.text "Vermutung: hilft der Frau beim Entscheiden"]
                                ]
                            ]
                        ]
                    , slide [ Html.text "Modellierung" ]
                        [ Html.b [] [ Html.text "Beziehung" ]
                        , Html.ul []
                            [ Html.li [] [ Html.text "Ein Mann und eine Frau bilden eine Beziehung." ]
                            , Html.li [] [ Html.text "Männer machen Antrag, Frauen wählen." ]
                            , Html.li [] [ Html.text "Nur Frauen können eine Beziehung beenden." ]
                            , Html.li [] [ Html.text "Frauen beenden Beziehungen nur für bessere Partner." ]
                            , Html.li [] [ Html.text "Frauen können nur schwer Entscheidungen fällen." ]
                            , Html.li [] [ Html.text "Frauen behindern andere Frauen falls nötig." ]
                            ]
                        , Html.b [] [ Html.text "Freundschaften" ]
                        , Html.ul []
                            [ Html.li [] [ Html.text "Frauen können nicht befreundet sein." ]
                            , Html.li [] [ Html.text "Männer: Der Rivale meines Rivalen ist mein Freund." ]
                            , Html.li [] [ Html.text "Männer: keine Freundschaft mit Rivalen" ]
                            , Html.li [] [ Html.text "Männer haben nur Rivalen wenn sie Single sind." ]
                            , Html.li [] [ Html.text "Es existieren Freunde auserhalt von Freundschaften "
                                , Html.code [] [ Html.text "⇒ Grundwert"]
                                
                            ]
                            , Html.li [] [ Html.text "Männer: Umso mehr Freundschaften umso attraktiver." ]
                            ]
                        , Html.b [] [ Html.text "Freundeskreise" ]
                        , Html.ul []
                            [ Html.li [] [ Html.text "(Saunaclub-Annahme) Jeder Freundeskreis beinhaltet genau eine Frau" ]
                            , Html.li [] [ Html.text "(Julian-Stockinger Satz) die Mitglieder eines Freundeskreise variieren." ]
                            ]
                        ]
                    , slide [ Html.text "Friedliche Dreiecksbeziehung (1 2 0)" ]
                        [ drawVisualizer "friendlyTriangle" visualizer.friendlyTriangle
                        , Html.i [] [Html.text "Harry Potter: 0-Hermine, 1-Ginny, 2-Harry, 3-Ron"]
                        ]
                    , slide [ Html.text "Feindliche Dreiecksbeziehung (1 3 0)"]
                         [ drawVisualizer "hostileTriangle" visualizer.hostileTriangle
                         , Html.i [] [Html.text "How I Met Your Mother: 0-Frau des Tages, 1-Robin, 2-Barney, 3-Ted"]
                         ]
                    , slide [ Html.text "Friedliche Vierecksbeziehung (0 2 1 3)" ]
                        [ drawVisualizer "loveSquare" visualizer.loveSquare 
                        , Html.i [] [Html.text "Game of Thrones: 0-Cersei Lannester, 1-Catelyn Tully, 2-Robert Baratheon, 3-Ned Stark, 4-Jaime Lannester"]
                        ]
                    , slide [ Html.text "Satz der Isomorphie" ]
                        [ Html.b [] [ Html.text "Satz" ]
                        , Html.p [class "text-center"] [Html.text "Jeder Liebes Graph lässt sich als Einfacher Liebes Graph darstellen"]
                        , Html.b [] [ Html.text "Beweis" ]
                        , drawVisualizer "theoremContact" visualizer.theoremContact
                        , Html.b [] [ Html.text "Absofort: Grundwert ist nicht eindeutig ⇒ (normale) Liebes Graphen" ]
                        ]
                    , slide [ Html.text "Feindliche Vierecksbeziehung (0 2 1 3)" ] [ drawVisualizer "twoCircles" visualizer.twoCircles ]
                    , slide [ Html.text "Kleinste On-Off-Beziehung" ] [ drawVisualizer "nonTerminatingTiangle" visualizer.nonTerminatingTiangle ]
                    , slide [ Html.text "Doppelte On-Off-Beziehung" ] [ drawVisualizer "doubleOnOff" visualizer.doubleOnOff ]
                    , slide [ Html.text "Dreifache On-Off-Beziehung" ] [ drawVisualizer "testing" visualizer.testing ]
                    , slide [ Html.text "Fazit" ]
                        [ Html.b [] [ Html.text "Algorithmus zur Erstellung n-Facher On-Off-Beziehungen" ]
                        , Html.ul []
                            [ Html.li [] [ Html.text "Zyklen lassen sich mit Algorithmus nicht erstellen." ]
                            , Html.li [] [ Html.text "Existenz von Zyklen unbekannt → weitere Nachforschungen notwendig." ]
                            , Html.li []
                                [ Html.text "weitere Abstraktionmöglichkeiten (Einführung von Schwarm)."
                                , Html.br [] []
                                , Html.text " → Erweiterter Payrischer Automat."
                                ]
                            ]
                        , Html.h4 [] [ Html.text "Danke fürs Zuhören" ]
                        ]
                    ]
                    |> Tab.view tabState
                ]
            ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

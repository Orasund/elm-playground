module View.Overlay exposing (..)

import Action exposing (Action(..))
import Artefact exposing (Artefact)
import Config
import Html exposing (Html)
import Layout
import View.Artefact


foundArtefact : { onCloseOverlay : Action -> msg, artefacts : List Artefact } -> Artefact -> Html msg
foundArtefact args artefact =
    [ "You have found the artefact" |> Layout.text []
    , View.Artefact.noButtons artefact
    , if List.length args.artefacts < Config.maxArtefacts then
        Layout.textButton []
            { label = "Take"
            , onPress =
                AddArtefactAnd artefact EndMove
                    |> args.onCloseOverlay
                    |> Just
            }

      else
        [ args.artefacts
            |> List.map
                (\oldArtefact ->
                    Layout.textButton []
                        { label = "Discard " ++ Artefact.name oldArtefact
                        , onPress =
                            RemoveArtefactAnd oldArtefact
                                (AddArtefactAnd artefact
                                    EndMove
                                )
                                |> args.onCloseOverlay
                                |> Just
                        }
                )
            |> Layout.column [ Layout.gap 8 ]
        , Layout.textButton []
            { label = "Discard " ++ Artefact.name artefact
            , onPress =
                EndMove
                    |> args.onCloseOverlay
                    |> Just
            }
        ]
            |> Layout.column [ Layout.gap 8 ]
    ]
        |> Layout.column [ Layout.gap 8 ]

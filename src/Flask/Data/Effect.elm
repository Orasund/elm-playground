module Flask.Data.Effect exposing (Effect(..), simplify, toTextField)

import Flask.Data.Element as Element exposing (Element)


type Effect
    = Add (List Element)
    | Choose
    | Draw Int
    | Remove Int
    | Discard Int
    | Reboot
    | Plant


simplify : List Effect -> List Effect
simplify list =
    let
        { add, choose, draw, remove, discard, reboot, plant } =
            list
                |> List.foldl
                    (\effect out ->
                        case effect of
                            Add l ->
                                { out | add = l ++ out.add }

                            Choose ->
                                { out | choose = out.choose + 1 }

                            Draw n ->
                                { out | draw = out.draw + n }

                            Remove n ->
                                { out | remove = out.remove + n }

                            Discard n ->
                                { out | discard = out.discard + n }

                            Reboot ->
                                { out | reboot = True }

                            Plant ->
                                { out | plant = True }
                    )
                    { add = [], choose = 0, draw = 0, remove = 0, discard = 0, reboot = False, plant = False }
    in
    List.concat
        [ if plant then
            [ Plant ]

          else
            []
        , if add == [] then
            []

          else
            [ Add add ]
        , if draw == 0 then
            []

          else
            [ Draw draw ]
        , Choose |> List.repeat choose
        , if remove == 0 then
            []

          else
            [ Remove remove ]
        , if discard == 0 then
            []

          else
            [ Discard discard ]
        , if reboot then
            [ Reboot ]

          else
            []
        ]


toTextField : Effect -> { title : String, desc : String }
toTextField effect =
    case effect of
        Add list ->
            list
                |> List.map Element.toString
                |> String.concat
                |> (\elems ->
                        { title = "+" ++ elems
                        , desc = "Füge " ++ elems ++ " zu deinen Countern hinzu. Am Ende des Zuges dürfen die Counter maximal 6 anzeigen, alle weiteren Ressourcen verfallen."
                        }
                   )

        Choose ->
            { title = "+2 Resources of one kind"
            , desc = "Füge entweder 💰💰 , 💥💥 , 📘📘 oder 💚💚 zu deinem Counter hinzu"
            }

        Draw n ->
            { title = "Draw " ++ String.fromInt n ++ " Card"
            , desc =
                "Ziehe "
                    ++ (if n <= 1 then
                            "eine Karte"

                        else
                            String.fromInt n ++ " Karten"
                       )
                    ++ " des Nachzieh- "
                    ++ (if n <= 1 then
                            "oder"

                        else
                            "und/oder"
                       )
                    ++ " Ablagestapels."
            }

        Remove n ->
            { title =
                "Action: -"
                    ++ String.fromInt n
                    ++ (if n <= 1 then
                            " Resource"

                        else
                            " Resources"
                       )
            , desc =
                "Ein Gegner verliert "
                    ++ (if n <= 1 then
                            "ein Ressource"

                        else
                            String.fromInt n ++ " Ressourcen"
                       )
                    ++ " der eigenen Wahl. Der Gegner hat das Spiel verloren sobald dieser am Ende des eigenen oder gegnerischen Zuges keine Ressourcen mehr besitzt."
            }

        Discard n ->
            { title = "Action: -" ++ String.fromInt n ++ " Cards"
            , desc =
                "Ein Gegner legt "
                    ++ (if n <= 1 then
                            "eine Handkarte oder Pflanze"

                        else
                            String.fromInt n ++ " Handkarten und/oder Pflanzen"
                       )
                    ++ " der eigenen Wahl auf den Ablagestape. Der Gegner hat das Spiel verloren sobald am Anfang des eigenen Zuges keine Karte mehr auf den Nachziehstapel liegt."
            }

        Reboot ->
            { title = "Reboot"
            , desc =
                "Alle bisher in deinem Zug gespielten Karten kommen wieder zurück auf die Hand. Auch diese."
            }

        Plant ->
            { title = "Plant"
            , desc =
                "Diese Karte wird verdeckt gespielt und bleibt am Spielfeld liegen bis sie aktiviert wird. Alle weiteren in diesem Zug gespielten Karten werden bezahlt und für eine spätere Aktivierung unter diese Karte gelegt. Die Karte darf jederzeit aktiviert werden. Anschließend werden alle darunter liegen Karten aktiviert."
            }

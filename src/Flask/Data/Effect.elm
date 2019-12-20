module Flask.Data.Effect exposing (Effect(..), toTextField)

import Flask.Data.Element as Element exposing (Element)


type Effect
    = Add (List Element)
    | Choose
    | Draw Int
    | Remove Int
    | Discard Int
    | Reboot
    | Plant


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
            { title = "+(💰💰|💥💥| 📘📘)"
            , desc = "Füge entweder 💰💰 oder 💥💥 oder 📘📘 zu deinem Counter hinzu"
            }

        Draw n ->
            { title = "Draw " ++ String.fromInt n ++ " Card"
            , desc =
                "Ziehe die "
                    ++ (if n <= 1 then
                            "oberste Karte"

                        else
                            String.fromInt n ++ " obersten Karten"
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
                    ++ ("❔"
                            |> List.repeat n
                            |> String.concat
                       )
            , desc =
                "Ein Gegner verliert "
                    ++ (if n <= 1 then
                            "ein Ressource"

                        else
                            String.fromInt n ++ " Ressourcen"
                       )
                    ++ " der eigenen Wahl. Der Gegner hat das Spiel verloren sobald dieser am Anfang des eigenen Zuges keine Ressourcen mehr besitzt."
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
                "Diese Karte bleibt am Spielfeld liegen bis sie aktiviert wird. Alle weiteren in diesem Zug gespielten Karten werden bezahlt und für spätere Aktivierung unter diese Karte gelegt. Die Karte kann vor oder während deinem Zug aktiviert werden. Anschließend werden alle darunter liegen Karten aktiviert."
            }

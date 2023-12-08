module Message exposing (..)

import Card exposing (Card(..))
import Location exposing (Location(..))


type Message
    = Info String
    | Played Card
    | At Location


interact : Card -> Location -> ( List Message, Location )
interact card location =
    case location of
        TownSquare ->
            case card of
                Door ->
                    ( [ Info "Du klopfst bei einem der Häuser an und trittst hindruch."
                      ]
                    , OldLady
                    )

                Death ->
                    ( [ Info "Ein Schrei ertöhnt und eine Gestalt eillt davon. Du folgst ihr." ]
                    , Cemetery
                    )

                Letter ->
                    ( [ Info "Die Tür eines der Häuser offnet sich und eine Hand deutet dir Einzutreten." ]
                    , OldLady
                    )

                Raven ->
                    ( [ Info "Plötzlich kreischen eine Schahr Raben auf und fliegen in eine Richtung. Du folgst ihnen." ]
                    , Cemetery
                    )

        OldLady ->
            case card of
                Door ->
                    ( [ Info "Du eilst zur Ausgangstüre und rennst hinaus." ]
                    , TownSquare
                    )

                Death ->
                    ( [ Info "Aus dem Keller ertöhnt ein qualvoller Schrei." ]
                    , OldLady
                    )

                Letter ->
                    ( [ Info "\"Verschwinden sie von hier.\" flüstert die Dame dir ins Ohr." ]
                    , TownSquare
                    )

                Raven ->
                    ( [ Info "Ein Rabe landet vor dem Fenster und sieht dich durchtringend an" ]
                    , OldLady
                    )

        Cemetery ->
            case card of
                Door ->
                    ( [ Info "Mit eilligen Schritten läufst du zurück zum Dorfzentrum." ]
                    , TownSquare
                    )

                Death ->
                    ( [ Info "Du steht vor einem frisch geschaufelten Grab mit deinem Namen drauf." ]
                    , Cemetery
                    )

                Letter ->
                    ( [ Info "Die Friedhofskapelle zieht dich magisch an." ]
                    , Cemetery
                    )

                Raven ->
                    ( [ Info "Die Raben kreischen laut während sie über dir Kreise ziehen." ]
                    , Cemetery
                    )

        Crypt ->
            case card of
                Door ->
                    ( [ Info "Du rennst eine Treppe hinauf." ]
                    , OldLady
                    )


toString : Message -> String
toString message =
    case message of
        Info string ->
            string

        Played card ->
            "[\""
                ++ Card.name card
                ++ "\" wurde gespielt"
                ++ "]"

        At location ->
            "Du befindest dich " ++ Location.name location ++ "."

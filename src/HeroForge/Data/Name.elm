module Katakomben.Data.Name exposing (generate)

import Random exposing (Generator)


generate : Int -> Generator String
generate quality =
    case quality of
        _ ->
            Random.int 0 1
                |> Random.map ((==) 0)
                |> Random.andThen
                    (\isMale ->
                        Random.map3
                            (\j v n ->
                                j ++ " " ++ v ++ " " ++ n
                            )
                            (job quality isMale)
                            (vorname isMale)
                            (nachname quality)
                    )


vorname : Bool -> Generator String
vorname isMale =
    Random.uniform "Luca" <|
        if isMale then
            [ "Günter"
            , "Hans"
            , "Karl"
            , "Heinz"
            , "Werner"
            , "Gerhard"
            , "Horst"
            , "Helmut"
            , "Walter"
            , "Kurt"
            ]

        else
            [ "Ursula"
            , "Helga"
            , "Gisela"
            , "Inge"
            , "Gerda"
            , "Ingrid"
            , "Ingeborg"
            , "Ilse"
            , "Edith"
            , "Hildegard"
            ]


nachname : Int -> Generator String
nachname quality =
    case quality of
        _ ->
            Random.uniform "Müller" <|
                [ "Schmidt"
                , "Schneider"
                , "Fischer"
                , "Weber"
                , "Meyer"
                , "Wagner"
                , "Becker"
                , "Schulz"
                , "Hoffmann"
                ]


job : Int -> Bool -> Generator String
job quality isMale =
    case quality of
        _ ->
            Random.uniform ""
                [ if isMale then
                    "Diener"

                  else
                    "Dienerin"
                , "Gehilfe"
                , "Page"
                , if isMale then
                    "Stallknecht"

                  else
                    "Magd"
                ]

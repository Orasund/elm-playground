module Chapter.DangerousColors exposing (..)

import Data.Chapter exposing (Chapter)
import Data.Page


chapter : Chapter
chapter =
    let
        intro =
            """Es gibt Farben die sich nicht im Regenbogen wiederfinden. 
 Um diese Farben mit dem menschlichen Auge wahrnehmen zu 
 können, hilft kein Elfenglas und auch keine 
 babylonische Meditation. 
 Einzig externe Hilfsmittel können dem Auge die Kraft 
 verleihen um das unsichtbare zu sehen.
"""
    in
    { title = "Gefährliche Farben"
    , pages =
        [ intro
            |> Data.Page.fromString
            |> Data.Page.withQuiz
                { question = "Die restliche Seite ist leer."
                , answers = [ "Umblättern", "Deinen Verstand auf die leere Seite konzentrieren." ]
                , correctAnswer = 2
                }
        , """Das durchschnittliche Auge vermag dieser Operation für 10 Minuten stand zu halten,
 bevor es erblindet.
 Werden die Augen jedoch ganz durch externe Objektive 
 ersetzt, so vermag ein Mensch für ein ganzes Monat die 
 "echte Welt" erblicken zu können, ehe sein Verstand unter der 
 Flut der Wahrnehmungen in sich zusammenfällt.

 ![Abbildung 1: Ein Apparat welcher scheinbar eine 
 dunklen Substanz in ein abgetrenntes und ausgehöhltes 
 Auge pumpt, in dem eine kleine Kerze brennt. 
 Neben der Substanz ist ein Symbol abgebildet, 
 das wage an einen Oktopus erinnert.](/abb1.jpg) 
""" |> Data.Page.fromString
        ]
    , closingText =
        intro
            ++ "\n\n"
            ++ "> Das restliche Kapitel ist voller leerer Seiten."
    }
        |> Data.Chapter.new

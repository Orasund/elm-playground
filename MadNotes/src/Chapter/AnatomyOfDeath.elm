module Chapter.AnatomyOfDeath exposing (chapter)

import Data.Chapter exposing (Chapter)
import Data.Page


chapter : Chapter
chapter =
    let
        imageOfChild =
            """![Abbildung 1: Ein Kind welches zur Hälfte tot und zur 
 Hälfte lebendig zu seien scheint. Die tote Hälfte besitzt 
 unnatürliche Fühler, die scheinbar vom gesamten 
 Körper ausgehend sich über die halbe Seite erstrecken.
 Es lächelt während es mit der rechten Hand nach seinem
 Herz greift.](/abb1.jpg)"""
    in
    { title = "Anatomie des Todes"
    , pages =
        [ """
Um die Zusammenhänge zwischen Leben und Tod besser zu 
 verstehen müssen wir zuerst die Essenz des Todes 
 studieren. 
 Was ist der Tod? Ist es ein Zustand? Ein Prozess? 
 Eine Pforte? Ein Ort? Ein Wesen?

Ich habe den Tod mehre Tausend Male gesehen. 
 Mehre Hundert Male habe ich mich selbst im eben jenen 
 Zustand befunden. 
 Mehrere Dutzend Male habe ich den Tod rückgängig gemacht.
 Und ein einziges Mal konnte ich den Tod in seinen 
 Einzelteilen studieren. 
 Dieses Buch handelt vom Tod, genauer gesagt von dessen 
 Tanz, dessen Musik und dessen Klang."""
            ++ "\n\n"
            ++ imageOfChild
            |> Data.Page.fromString
            |> Data.Page.withQuiz
                { question = "Der Blick des Kindes schaut dich direkt an. Er durchdringt dich und blickt dir tief in die Seele."
                , answers = [ "Blick abwenden", "Blick erwidern" ]
                , correctAnswer = 1
                }
        , """Ich habe den Tod mit meinen eigenen Augen gesehen 
 und ich kann Ihnen versichern, es handelt sich hierbei 
 um nichts weiter als eine Illusion. 
 Um ein wissenschaftliches Phänomen, dass, sobald man 
 die Wahrheit kennt,sich als weit einfacher herausstellt
 als das Vermessen des Niels oder den Bau der Pyramide 
 von Nyarlathotep.

Der Tod ist lediglich eine Veränderung der Schwingung um 
 das unsichtbare sichtbar und das sichtbare unsichtbar 
 zu machen. 
 Um sich dem Tod zu nähern muss die eigene Frequenz auf 
 richtige Weise moduliert werden.

![Abbildung 2: Ein Chemischer Prozess, der musikalische Elemente beinhaltet und in Moll
 geschrieben ist.
 Eine Doppelhelix verläuft wie ein Notenblatt quer über 
 die Darstellung, wobei die Noten einer Regelmäßigkeit 
 folgen die gänzlich überirdisch scheinen.](/abb2.jpg)    
"""
            |> Data.Page.fromString
        ]
    , closingText =
        "> Jedes mal wenn du dieses Kapitel aufschlägst, bleiben deine Augen auf dem Bild des Kindes haften. Der Blick hat sich auf ewig in dein Gedächtnis eingebrannt."
            ++ "\n\n"
            ++ imageOfChild
    }
        |> Data.Chapter.new

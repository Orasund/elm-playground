module Chapter.Darkness exposing (..)

import Data.Chapter exposing (Chapter)
import Data.Page


chapter : Chapter
chapter =
    let
        blackImage =
            """![Abbildung 1: Ein komplett in schwarz gehülltes Bild. Bei längerem Hinsehen sind Formen zu erkennen. 
Harte Kanten, die Konturen in das sonst pechschwarze Bild bringen.
Doch diese Konturen könnten auch einfach nur zufällige Striche sein, die absolut nichts auszusagen haben.](abb1.jpg)"""
    in
    { title = "Finsternis"
    , pages =
        [ """
Die Finsternis scheint ein Ort mit eigenen Gesetzen zu sein.
Hier gibt es keine Himmelrichtungen, keine Geräusche, kein Gewicht.
Die Kälte mag zu Beginn erschreckend wirken, es ist jedoch nur ein Schutz vor ihnen.
Denn sie leben von der Wärme. 
Warme Objekte ziehen sie magisch an und ihr endloser Hunger scheint grenzenlos zu sein.

Es wird empfohlen eine Hand voll warmer Kadaver mit zu nehmen und 
sie in regelmäßigen Abständen um sich herum zu verteilen.
Dies verschafft einem etwas mehr Zeit, wenn auch nicht viel.

Ich war niemals mehr als ein paar Atemzüge in der Finsternis.
Meist veranlasste meine Neugierde meine Zeit bis auf die letzte Sekunde hinauszuzögern."""
            ++ "\n\n"
            ++ blackImage
            |> Data.Page.fromString
            |> Data.Page.withQuiz
                { question = "Umso länger du in das schwarze Chaos blickst, umso mehr kommt dir vor, als würdest du darin etwas erkennen"
                , answers = [ "Ein Auge", "Eine Spinne", "Ein Tentakel", "Ein Rabe" ]
                , correctAnswer = -1
                }
        , "" |> Data.Page.fromString
        ]
    , closingText =
        """
> Während du das Bild anschaust wird dir ganz schwindelig. Allmählich verschwimmen die Konturen und die Schwärze breitet sich in deinem Kopf aus. Vielleicht hast du für heute genug gelesen. Ein Spaziergang an der frischen Luft wäre jetzt wohl genau das Richtige."""
            ++ "\n\n"
            ++ blackImage
    }
        |> Data.Chapter.new

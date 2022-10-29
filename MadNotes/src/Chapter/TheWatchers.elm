module Chapter.TheWatchers exposing (chapter)

import Data.Chapter exposing (Chapter)
import Data.Page


chapter : Chapter
chapter =
    { title = "Die Beobachter"
    , pages =
        [ """
Auf meinen Reisen auf die andere Seite habe ich immer das Gefühl, als würde ich beobachtet werden. 
 Manchmal ist mir, als folgt mir ein Schatten, immer gerade so, 
 dass ich ihn aus dem Augenwinkel erkennen, jedoch nie ganz zu orten vermag.
Der Schatten selbst scheint immer ein anderer zu sein, jedoch lassen sie sich aufgrund ihrer Eigenschaften 
 grundsätzlich in vier Kategorien einteilen.

Die runden Schatten verweilen meist stationär und verschmelzen mit der Umgebung, 
 wenn man sich ihnen nähert.
Ihr Erscheinen wird meist von einem kaum hörbarem Pfeifen angekündigt, welches in schrillen, 
 fast unerträglichen Tonhöhen sich dem allgemeinen Sirren anschließt.

Die langen und dünnen Schatten sind so dünn, dass man sie mit bloßen Auge kaum zu erkennen vermag.
Ihr Kopf erstreckt sich über das ganze Firmament bis an den gegenüberliegenden Horizon aus.
Sie sind schnell und überaus gerissen. Sie können mit kleinen Hautfetzen abgelenkt werden.

![Ein fraktal artiges Bild welches erschreckend echt wirkt. In dem Bild sind dünne schwarze Fäden, 
 die sich wie eine Decke über die Landschaft stülpen](abb2.jpg)
"""
            |> Data.Page.fromString
            |> Data.Page.withQuiz
                { question = "Die Landschaft kommt dir bekannt vor. Wo hast du sie schon einmal gesehen?"
                , answers = [ "In einem Traum", "Das ist nur eine Einbildung", "Aus meiner Kindheit" ]
                , correctAnswer = 2
                }
        , """
Der gelbe scheint der klügste zu sein, war er mir fast unentdeckt geblieben. 
 Ich konnte ihn einmal beobachten wie er mit einem der Ortsansässigen kommunizierte.
 Wobei die Sprache selbst nicht aus Lauten zu bestehen schien.

![Ein gelbes Quadrat, das quadrat ist perfekt symmetrisch und scheinbar wurde über ein schwarzes 
 Quadrat der gleichen Größe gezeichnet. 
 Daneben sind schwarze punkte in verschiedenen Größen und formen zu erkennen.](abb3.jpg)

Die vierte Kategorie ist die, die mich heute noch heimsucht. Sie ist die schlimmste. 
 Allumfassend frisst sie alles um sich herum auf. 
 Scheinbar aus Nichts bestehend breitet sie sich ca. im Tempo eines trabenden Pferdes aus.
 Nichts kann sie aufhalten.

> Ein großer schwarzer Tintenfleck bedeckt das restliche Blatt
""" |> Data.Page.fromString
        ]
    , closingText = """![Ein fraktal artiges Bild welches erschreckend echt wirkt. In dem Bild sind dünne schwarze Fäden, 
 die sich wie eine Decke über die Landschaft stülpen](abb2.jpg)
 
 > Umso länger du die Landschaft betrachtest, umso vertrauter kommt sie dir vor. Es ist alles so klar. Du kannst jedes Detail ganz klar erkennen. Und da sind auch diese dünnen Fäden die dich zu verfolgen scheinen. Du versuchst ihnen zu entkommen, doch schaffst es nicht. Du musst irgendwann die Augen geschlossen haben, denn mit einem Schrei öffnest du Sie schlagartig wieder. Das Buch ist dir aus der Hand gefallen und liegt nun zugeklappt auf den Boden.
 """
    }
        |> Data.Chapter.new

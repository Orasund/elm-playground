module Chapter.Introduction exposing (chapter)

import Data.Chapter exposing (Chapter)
import Data.Page


chapter : Chapter
chapter =
    { title = "Vorwort des Übersetzers"
    , pages =
        [ """
Wenn du dieses Manuskript in Händen hältst, 
 kann dies nur zweierlei Gründe haben.
 Entweder du bist aus purer Neugierde auf dieses Werk gestoßen 
 oder du gehörst zu den wenigen Unglücklichen, 
 welche vom Pfad der Vernunft und 
 des Verstandes abgekommen sind und 
 dieses Buch in der Hoffnung auf Antworten aufgespürt haben.

In beiden Fällen rate ich dir ab, 
 weiter in diesem Buch zu lesen.

Ich, **Artem Shillic, Professor für 
 Revidierte Geschichte an der 
 Miskatonic Universität in Arkham**,
 würde stattdessen dir herzlichst empfehlen,
 dieses Buch zur Seite zu legen und 
 einen langen erfrischenden Spaziergang zu tätigen.
 Ein klarer und funktionierender Verstand ist 
 bei weitem wichtiger als das Wissen,
 welches auf den folgenden Seiten niedergeschrieben ist.
"""
            |> Data.Page.fromString
        , """Ich scherze nicht.

Als ich 1902 das originale Werk zum ersten Male überflog,
 musste ich anschließend eine Woche lang mit 
 Ketten an mein Bett gefesselt in 
 der Irrenanstalt von Arkham verbringen. 
 Auch heute noch pflege ich auf Nacht dort zu schlafen nur um mich in Sicherheit zu wiegen.

Als wir 1903 ein Abbild der ersten paar Seiten anlegten,
 wurden in der darauffolgenden Woche die zwei zuständigen Doktoranden tod aufgefunden. 
 In ihrem Abschiedsbrief berichteten sie von Alpträume in denen sie in einem ewigen Wald von Kreaturen heimgesucht wurden.
 Der Tod schien ihnen der einzige Ausweg zu sein.

Ich hatte daher zuerst gezögert als ein unbekannter Großspender der Universität mich 1907 beauftragte eine möglichst detailgetreue Kopie des Manuskripts anzufertigen.

Über 24 Jahre war ich und mein Team mit dieser Aufgabe beschäftigt und das alles war nur möglich gewesen,
 da die Irrenanstalt von Arkham uns ein Team aus höchst professionellen Experten zur Verfügung stellte, 
 welches uns Zeile für Zeile bei der Arbeit begleitete und bei jeder kleinsten Unregelmäßigkeiten Eingriff.
 In dieser Spanne haben insgesamt 
 25 Postdocs und Doktoranden nie länger als 10 Minuten 
 am Stück daran gearbeitet.
 Zeitweise mussten wir einzelne Passagen überspringen und
 einige Kapitel sind nur teilweise übersetzt,
 da wir unsere eigene psychische Gesundheit dem 
 grausamen und entsetzten Wissen dieser Kapitel vorzogen.

Deswegen bitte ich Sie noch einmal:

LEGEN SIE DIESES BUCH ZUR SEITE 
UND GEHEN SIE AN DIE FRISCHE LUFT!

Keine Absicht, sei sie noch so nobel, 
ist Grund genug um auch nur Teile dieses Buches zu lesen.

Artem Shillic, 1932 Arkham
"""
            |> Data.Page.fromString

        {--, """
## Anmerkung zur Digitalen Version

* Das Werk entstand 1923 und fiel im zweiten Weltkrieg den Nazis in die Hände.
  Dort wurde es als "Entartete Kunst" deklariert und scheinbar öffentlich verbrannt.
* 2020 wurde eine deutsche Übersetzung in einer geheimen Bibliothek alter Bücher aus NS-Zeiten entdeckt.
* 2021 konnten die darin enthaltenen Randnotizen mit der von Heinrich Himmlers abgeglichen werden.
* Am 13. Juli 2022 wurde das Buch digitalisiert und online gestellt, 
  musste allerdings am selbigen Tag wieder vom Netz genommen werden, da das Bildmaterial gegen §431 IrgNA verstoß.
* Am 31. Oktober 2022 konnte das Werk stark gekürzt und ohne Bildmaterial hochgeladen werden.
"""
            |> Data.Page.fromString--}
        ]
    , closingText = "Du hast genug gelesen"
    }
        |> Data.Chapter.new

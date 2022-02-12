module LoveGraph exposing (Connection(..), Gender(..), LoveGraph, Person, Relation, contactPerson, femalePerson, malePerson)

import Graph exposing (Edge, Graph, Node)


type Gender
    = F
    | M
    | C


type alias Person =
    Node Gender


femalePerson : Int -> Person
femalePerson a =
    Node a F


malePerson : Int -> Person
malePerson a =
    Node a M


contactPerson : Int -> Person
contactPerson a =
    Node a C


type Connection
    = Friend
    | Partner


type alias Relation =
    Edge Connection


type alias LoveGraph =
    Graph Gender Connection



{- }  { persons : List Person
   , relations : List Relation
   }
-}

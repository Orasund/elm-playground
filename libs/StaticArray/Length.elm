module StaticArray.Length exposing (Length, minus1, minus10, minus2, minus20, minus4, minus5, minus8, one, plus1, plus10, plus2, plus20, plus4, plus5, plus8)

import StaticArray.Internal as Internal exposing (Length(..))
import StaticArray.Label exposing (EightPlus, FivePlus, FourPlus, One, OnePlus, TenPlus, TwentyPlus, TwoPlus)


type alias Length n =
    Internal.Length n


one : Length One
one =
    C 1


plus1 : Length n -> Length (OnePlus n)
plus1 (C n) =
    C (n + 1)


plus2 : Length n -> Length (TwoPlus n)
plus2 (C n) =
    C (n + 2)


plus4 : Length n -> Length (FourPlus n)
plus4 (C n) =
    C (n + 4)


plus8 : Length n -> Length (EightPlus n)
plus8 (C n) =
    C (n + 8)


plus5 : Length n -> Length (FivePlus n)
plus5 (C n) =
    C (n + 5)


plus10 : Length n -> Length (TenPlus n)
plus10 (C n) =
    C (n + 10)


plus20 : Length n -> Length (TwentyPlus n)
plus20 (C n) =
    C (n + 20)


minus1 : Length (OnePlus n) -> Length n
minus1 (C n) =
    C (n - 1)


minus2 : Length (TwoPlus n) -> Length n
minus2 (C n) =
    C (n - 2)


minus4 : Length (FourPlus n) -> Length n
minus4 (C n) =
    C (n - 4)


minus8 : Length (EightPlus n) -> Length n
minus8 (C n) =
    C (n - 8)


minus5 : Length (FivePlus n) -> Length n
minus5 (C n) =
    C (n - 5)


minus10 : Length (TenPlus n) -> Length n
minus10 (C n) =
    C (n - 10)


minus20 : Length (TwentyPlus n) -> Length n
minus20 (C n) =
    C (n - 20)

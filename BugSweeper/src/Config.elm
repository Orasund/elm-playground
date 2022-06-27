module Config exposing (bugAmount, gridSize, leafAmount, stoneAmount)


gridSize : Int
gridSize =
    6


leafAmount : Int
leafAmount =
    gridSize * gridSize // 3


stoneAmount : Int
stoneAmount =
    0


bugAmount : Int
bugAmount =
    2

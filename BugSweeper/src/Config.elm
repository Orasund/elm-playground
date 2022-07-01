module Config exposing (bugAmount, gridSize, leafAmount, stoneAmount)


gridSize : Int
gridSize =
    10


leafAmount : Int
leafAmount =
    gridSize


stoneAmount : Int
stoneAmount =
    gridSize ^ 2 // 4


bugAmount : Int
bugAmount =
    3

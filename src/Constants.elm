module Constants exposing (..)


tileSize : Float
tileSize =
    10


roadTileLength : Float
roadTileLength =
    10



-- Derived


villagerSize : Float
villagerSize =
    tileSize / 2


roadPixelWidth : Float
roadPixelWidth =
    roadTileLength * tileSize

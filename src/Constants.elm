module Constants exposing (..)


tileSize : Float
tileSize =
    10


roadTileLength : Float
roadTileLength =
    10


battlegroundTileLength : Float
battlegroundTileLength =
    50



-- Derived


villagerSize : Float
villagerSize =
    tileSize / 2


resourceSitePixelWidth : Float
resourceSitePixelWidth =
    tileSize * 10


roadPixelWidth : Float
roadPixelWidth =
    roadTileLength * tileSize + villagerSize


battlegroundPixelWidth : Float
battlegroundPixelWidth =
    battlegroundTileLength * tileSize

module Data.Shoupai exposing
    ( Shoupai, init
    , isMenqian, isLizhi
    , zimo, dapai
    )

{-|

@docs Shoupai, init
@docs isMenqian, isLizhi
@docs zimo, dapai

-}

import Data.Tile exposing (Tile)
import List.Extra


type alias Shoupai =
    { bingpai : List Tile -- 副露牌を含まない手牌の枚数
    , fulou : List String -- 副露面子
    , zimo : Maybe Tile -- ツモ牌
    , lizhi : Bool -- リーチしている時 True
    }


init : List Tile -> Shoupai
init qipai =
    { bingpai = qipai
    , fulou = []
    , zimo = Nothing
    , lizhi = False
    }


isMenqian : Shoupai -> Bool
isMenqian shoupai =
    List.isEmpty shoupai.fulou


isLizhi : Shoupai -> Bool
isLizhi shoupai =
    shoupai.lizhi


zimo : Tile -> Shoupai -> Shoupai
zimo tile shoupai =
    { shoupai
        | bingpai = shoupai.bingpai ++ [ tile ]
        , zimo = Just tile
    }


dapai : Tile -> Shoupai -> Shoupai
dapai tile shoupai =
    { shoupai
        | bingpai = List.Extra.remove tile shoupai.bingpai
        , zimo = Nothing
    }

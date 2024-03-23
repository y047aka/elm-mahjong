module Data.Shoupai exposing (Shoupai, init)

import Data.Tile exposing (Tile, isMan, isPin, isSou, isHonour)


type alias Shoupai =
    { -- 副露牌を含まない手牌の枚数
      bingpai :
        { man : List Tile -- 萬子（添字0は赤牌）
        , pin : List Tile -- 筒子（添字0は赤牌）
        , sou : List Tile -- 索子（添字0は赤牌）
        , honour : List Tile -- 字牌
        }
    , fulou : List String -- 副露面子
    , zimo : String -- ツモ牌
    , lizhi : Bool -- リーチしている時 True
    }


init : List Tile -> Shoupai
init qipai =
    { bingpai =
        { man = List.filter isMan qipai
        , pin = List.filter isPin qipai
        , sou = List.filter isSou qipai
        , honour = List.filter isHonour qipai
        }
    , fulou = []
    , zimo = ""
    , lizhi = False
    }

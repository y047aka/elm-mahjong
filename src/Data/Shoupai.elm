module Data.Shoupai exposing (dummy)


type alias Shoupai =
    { -- 副露牌を含まない手牌の枚数
      bingpai :
        { underscore : Int -- 伏せた牌（他者の手牌の時に使う）
        , m : List Int -- 萬子（添字0は赤牌）
        , p : List Int -- 筒子（添字0は赤牌）
        , s : List Int -- 索子（添字0は赤牌）
        , z : List Int -- 字牌
        }
    , fulou : List String -- 副露面子
    , zimo : String -- ツモ牌
    , lizhi : Bool -- リーチしている時 True
    }


dummy : Shoupai
dummy =
    { bingpai =
        { underscore = 0
        , m = [ 0, 1, 1, 1, 0, 0, 0, 0, 0, 0 ]
        , p = [ 0, 1, 1, 1, 0, 0, 0, 0, 0, 0 ]
        , s = [ 0, 1, 1, 1, 0, 0, 0, 0, 0, 0 ]
        , z = [ 0, 2, 0, 0, 0, 0, 0, 0 ]
        }
    , fulou = []
    , zimo = ""
    , lizhi = False
    }

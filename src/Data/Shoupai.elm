module Data.Shoupai exposing
    ( Shoupai, init
    , isMenqian, isLizhi
    , zimo, dapai, peng, chi, gang
    )

{-|

@docs Shoupai, init
@docs isMenqian, isLizhi
@docs zimo, dapai, peng, chi, gang

-}

import Data.Group exposing (Group(..))
import Data.Tile exposing (Tile)
import List.Extra


type alias Shoupai =
    { bingpai : List Tile -- 副露牌を含まない手牌の枚数
    , fulou : List Group -- 副露面子
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


peng : Tile -> Tile -> Tile -> Shoupai -> Shoupai
peng t1 t2 t3 shoupai =
    { shoupai
        | bingpai =
            shoupai.bingpai
                |> List.Extra.remove t2
                |> List.Extra.remove t3
        , fulou = shoupai.fulou ++ [ Triplet t1 t2 t3 ]
    }


chi : Tile -> Tile -> Tile -> Shoupai -> Shoupai
chi t1 t2 t3 shoupai =
    { shoupai
        | bingpai =
            shoupai.bingpai
                |> List.Extra.remove t2
                |> List.Extra.remove t3
        , fulou = shoupai.fulou ++ [ Run t1 t2 t3 ]
    }


gang : Tile -> Tile -> Tile -> Tile -> Shoupai -> Shoupai
gang t1 t2 t3 t4 shoupai =
    { shoupai
        | bingpai =
            shoupai.bingpai
                |> List.Extra.remove t2
                |> List.Extra.remove t3
                |> List.Extra.remove t4
        , fulou = shoupai.fulou ++ [ Gang t1 t2 t3 t4 ]
    }

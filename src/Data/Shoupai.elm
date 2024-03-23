module Data.Shoupai exposing
    ( Shoupai, init
    , zimo, dapai
    )

{-|

@docs Shoupai, init
@docs zimo, dapai

-}

import Data.Tile exposing (Tile(..), isHonour, isMan, isPin, isSou)
import List.Extra


type alias Shoupai =
    { -- 副露牌を含まない手牌の枚数
      bingpai :
        { man : List Tile -- 萬子（添字0は赤牌）
        , pin : List Tile -- 筒子（添字0は赤牌）
        , sou : List Tile -- 索子（添字0は赤牌）
        , honour : List Tile -- 字牌
        }
    , fulou : List String -- 副露面子
    , zimo : Maybe Tile -- ツモ牌
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
    , zimo = Nothing
    , lizhi = False
    }


zimo : Tile -> Shoupai -> Shoupai
zimo tile shoupai =
    let
        bingpai =
            shoupai.bingpai
    in
    { shoupai
        | bingpai =
            case tile of
                Man _ ->
                    { bingpai | man = bingpai.man ++ [ tile ] }

                Pin _ ->
                    { bingpai | pin = bingpai.pin ++ [ tile ] }

                Sou _ ->
                    { bingpai | sou = bingpai.sou ++ [ tile ] }

                Honour _ ->
                    { bingpai | honour = bingpai.honour ++ [ tile ] }
        , zimo = Just tile
    }


dapai : Tile -> Shoupai -> Shoupai
dapai tile shoupai =
    let
        bingpai =
            shoupai.bingpai
    in
    { shoupai
        | bingpai =
            case tile of
                Man _ ->
                    { bingpai | man = List.Extra.remove tile bingpai.man }

                Pin _ ->
                    { bingpai | pin = List.Extra.remove tile bingpai.pin }

                Sou _ ->
                    { bingpai | sou = List.Extra.remove tile bingpai.sou }

                Honour _ ->
                    { bingpai | honour = List.Extra.remove tile bingpai.honour }
        , zimo = Nothing
    }

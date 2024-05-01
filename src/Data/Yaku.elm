module Data.Yaku exposing
    ( Yaku
    , check
    , menzenTsumo, reach, ippatsu, yakuhai, pinfu, tanyao, ipeko, haitei, houtei, chankan, rinshanKaihou
    , doubleReach, renpuhai, toitoi, sananko, sanshokuDoukou, sankantsu, shousangen, honroutou, sanshokuDoujun, ittsu, chanta, chiitoitsu
    , ryanpeikou, honitsu, junchan
    , chinitsu
    , tenho, chiho, kokushiMusou, suanko, daisangen, ryuiso, tsuiso, shosushi, daishushi, chinroto, sukantsu, churenPoto
    , Hand(..)
    )

{-|

@docs Yaku
@docs check

@docs menzenTsumo, reach, ippatsu, yakuhai, pinfu, tanyao, ipeko, haitei, houtei, chankan, rinshanKaihou
@docs doubleReach, renpuhai, toitoi, sananko, sanshokuDoukou, sankantsu, shousangen, honroutou, sanshokuDoujun, ittsu, chanta, chiitoitsu
@docs ryanpeikou, honitsu, junchan
@docs chinitsu
@docs tenho, chiho, kokushiMusou, suanko, daisangen, ryuiso, tsuiso, shosushi, daishushi, chinroto, sukantsu, churenPoto

-}

import Data.Tile exposing (Tile(..))


type alias Yaku =
    { name : String
    , hanType : HanType
    , condition : { hand : Hand, isMenqian : Bool } -> Bool
    }


type HanType
    = One
    | Two
    | Two_ConsiderFulouPenalty
    | Three
    | Three_ConsiderFulouPenalty
    | Six_ConsiderFulouPenalty
    | Yakuman


countHan : { isMenqian : Bool } -> List HanType -> Int
countHan state hanTypes =
    if List.member Yakuman hanTypes then
        calcHan state Yakuman

    else
        List.map (calcHan state) hanTypes
            |> List.sum


calcHan : { isMenqian : Bool } -> HanType -> Int
calcHan { isMenqian } hanType =
    case ( hanType, isMenqian ) of
        ( One, _ ) ->
            1

        ( Two, _ ) ->
            2

        ( Two_ConsiderFulouPenalty, True ) ->
            2

        ( Two_ConsiderFulouPenalty, False ) ->
            1

        ( Three, _ ) ->
            3

        ( Three_ConsiderFulouPenalty, True ) ->
            3

        ( Three_ConsiderFulouPenalty, False ) ->
            2

        ( Six_ConsiderFulouPenalty, True ) ->
            6

        ( Six_ConsiderFulouPenalty, False ) ->
            5

        ( Yakuman, _ ) ->
            13


type Hand
    = Hand Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile Tile


check : Yaku -> { hand : Hand, isMenqian : Bool } -> Bool
check yaku state =
    yaku.condition state



-- 1飜


{-|

    import Data.Tile exposing (Tile(..))

    check menzenTsumo { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> True
    check menzenTsumo { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = False } --> False

-}
menzenTsumo : Yaku
menzenTsumo =
    { name = "門前清自摸和"
    , hanType = One
    , condition = \{ isMenqian } -> isMenqian
    }


{-|

    import Data.Tile exposing (Tile(..))

    check reach { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> True
    check reach { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = False } --> False

-}
reach : Yaku
reach =
    { name = "立直"
    , hanType = One
    , condition = \{ isMenqian } -> isMenqian
    }


{-|

    import Data.Tile exposing (Tile(..))

    check ippatsu { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> True
    check ippatsu { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = False } --> False

-}
ippatsu : Yaku
ippatsu =
    { name = "一発"
    , hanType = One
    , condition = \{ isMenqian } -> isMenqian
    }


{-|

    import Data.Tile exposing (Tile(..))

    check yakuhai { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> True
    check yakuhai { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = False } --> True

-}
yakuhai : Yaku
yakuhai =
    { name = "役牌"
    , hanType = One
    , condition = always True
    }


{-|

    import Data.Tile exposing (Tile(..))

    check pinfu { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> True
    check pinfu { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = False } --> False

-}
pinfu : Yaku
pinfu =
    { name = "平和"
    , hanType = One
    , condition = \{ isMenqian } -> isMenqian
    }


{-|

    import Data.Tile exposing (Tile(..))

    check tanyao { hand = Hand M2 M3 M4 M6 M7 M8 P2 P3 P4 P6 P7 P8 S2 S2, isMenqian = True } --> True
    check tanyao { hand = Hand M2 M3 M4 M6 M7 M8 P2 P3 P4 P6 P7 P8 S2 S2, isMenqian = False } --> True

    check tanyao { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> False

-}
tanyao : Yaku
tanyao =
    { name = "断么九"
    , hanType = One
    , condition = \{ hand } -> hand == Hand M2 M3 M4 M6 M7 M8 P2 P3 P4 P6 P7 P8 S2 S2
    }


{-|

    import Data.Tile exposing (Tile(..))

    check ipeko { hand = Hand M1 M2 M3 M1 M2 M3 S7 S8 S9 East East East White White, isMenqian = True } --> True
    check ipeko { hand = Hand M1 M2 M3 M1 M2 M3 S7 S8 S9 East East East White White, isMenqian = False } --> False

    check ipeko { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> False

-}
ipeko : Yaku
ipeko =
    { name = "一盃口"
    , hanType = One
    , condition = \{ hand, isMenqian } -> isMenqian && (hand == Hand M1 M2 M3 M1 M2 M3 S7 S8 S9 East East East White White)
    }


{-|

    import Data.Tile exposing (Tile(..))

    check haitei { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> True
    check haitei { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = False } --> True

-}
haitei : Yaku
haitei =
    { name = "海底摸月"
    , hanType = One
    , condition = always True
    }


{-|

    import Data.Tile exposing (Tile(..))

    check houtei { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> True
    check houtei { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = False } --> True

-}
houtei : Yaku
houtei =
    { name = "河底撈魚"
    , hanType = One
    , condition = always True
    }


{-|

    import Data.Tile exposing (Tile(..))

    check chankan { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> True
    check chankan { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = False } --> True

-}
chankan : Yaku
chankan =
    { name = "槍槓"
    , hanType = One
    , condition = always True
    }


{-|

    import Data.Tile exposing (Tile(..))

    check rinshanKaihou { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> True
    check rinshanKaihou { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = False } --> True

-}
rinshanKaihou : Yaku
rinshanKaihou =
    { name = "嶺上開花"
    , hanType = One
    , condition = always True
    }



-- 2飜


{-|

    import Data.Tile exposing (Tile(..))

    check doubleReach { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> True
    check doubleReach { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = False } --> False

-}
doubleReach : Yaku
doubleReach =
    { name = "ダブル立直"
    , hanType = Two
    , condition = \{ isMenqian } -> isMenqian
    }


{-|

    import Data.Tile exposing (Tile(..))

    check renpuhai { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> True
    check renpuhai { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = False } --> True

-}
renpuhai : Yaku
renpuhai =
    { name = "連風牌"
    , hanType = Two
    , condition = always True
    }


{-|

    import Data.Tile exposing (Tile(..))

    check toitoi { hand = Hand M1 M1 M1 P4 P4 P4 S7 S7 S7 East East East White White, isMenqian = True } --> True
    check toitoi { hand = Hand M1 M1 M1 P4 P4 P4 S7 S7 S7 East East East White White, isMenqian = False } --> True

    check toitoi { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> False

-}
toitoi : Yaku
toitoi =
    { name = "対々和"
    , hanType = Two
    , condition = \{ hand } -> hand == Hand M1 M1 M1 P4 P4 P4 S7 S7 S7 East East East White White
    }


{-|

    import Data.Tile exposing (Tile(..))

    check sananko { hand = Hand M1 M1 M1 P4 P4 P4 S7 S8 S9 East East East White White, isMenqian = True } --> True
    check sananko { hand = Hand M1 M1 M1 P4 P4 P4 S7 S8 S9 East East East White White, isMenqian = False } --> True

    check sananko { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> False

-}
sananko : Yaku
sananko =
    { name = "三暗刻"
    , hanType = Two
    , condition = \{ hand } -> hand == Hand M1 M1 M1 P4 P4 P4 S7 S8 S9 East East East White White
    }


{-|

    import Data.Tile exposing (Tile(..))

    check sanshokuDoukou { hand = Hand M1 M1 M1 P1 P1 P1 S1 S1 S1 East East East White White, isMenqian = True } --> True
    check sanshokuDoukou { hand = Hand M1 M1 M1 P1 P1 P1 S1 S1 S1 East East East White White, isMenqian = False } --> True

    check sanshokuDoukou { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = False } --> False

-}
sanshokuDoukou : Yaku
sanshokuDoukou =
    { name = "三色同刻"
    , hanType = Two
    , condition = \{ hand } -> hand == Hand M1 M1 M1 P1 P1 P1 S1 S1 S1 East East East White White
    }


{-|

    import Data.Tile exposing (Tile(..))

    check sankantsu { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> True
    check sankantsu { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = False } --> True

-}
sankantsu : Yaku
sankantsu =
    { name = "三槓子"
    , hanType = Two
    , condition = always True
    }


{-|

    import Data.Tile exposing (Tile(..))

    check shousangen { hand = Hand M1 M2 M3 East East East White White White Green Green Green Red Red, isMenqian = True } --> True
    check shousangen { hand = Hand M1 M2 M3 East East East White White White Green Green Green Red Red, isMenqian = False } --> True

    check shousangen { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> False

-}
shousangen : Yaku
shousangen =
    { name = "小三元"
    , hanType = Two
    , condition = \{ hand } -> hand == Hand M1 M2 M3 East East East White White White Green Green Green Red Red
    }


{-|

    import Data.Tile exposing (Tile(..))

    check honroutou { hand = Hand M1 M1 M1 P9 P9 P9 S1 S1 S1 East East East White White, isMenqian = True } --> True
    check honroutou { hand = Hand M1 M1 M1 P9 P9 P9 S1 S1 S1 East East East White White, isMenqian = False } --> True

    check honroutou { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> False

-}
honroutou : Yaku
honroutou =
    { name = "混老頭"
    , hanType = Two
    , condition = \{ hand } -> hand == Hand M1 M1 M1 P9 P9 P9 S1 S1 S1 East East East White White
    }


{-|

    import Data.Tile exposing (Tile(..))

    check sanshokuDoujun { hand = Hand M1 M2 M3 P1 P2 P3 S1 S2 S3 East East East White White, isMenqian = True } --> True
    check sanshokuDoujun { hand = Hand M1 M2 M3 P1 P2 P3 S1 S2 S3 East East East White White, isMenqian = False } --> True

    check sanshokuDoujun { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> False

-}
sanshokuDoujun : Yaku
sanshokuDoujun =
    { name = "三色同順"
    , hanType = Two_ConsiderFulouPenalty
    , condition = \{ hand } -> hand == Hand M1 M2 M3 P1 P2 P3 S1 S2 S3 East East East White White
    }


{-|

    import Data.Tile exposing (Tile(..))

    check ittsu { hand = Hand M1 M2 M3 M4 (M5 False) M6 M7 M8 M9 East East East White White, isMenqian = True } --> True
    check ittsu { hand = Hand M1 M2 M3 M4 (M5 False) M6 M7 M8 M9 East East East White White, isMenqian = False } --> True

    check ittsu { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> False

-}
ittsu : Yaku
ittsu =
    { name = "一気通貫"
    , hanType = Two_ConsiderFulouPenalty
    , condition = \{ hand } -> hand == Hand M1 M2 M3 M4 (M5 False) M6 M7 M8 M9 East East East White White
    }


{-|

    import Data.Tile exposing (Tile(..))

    check chanta { hand = Hand M1 M2 M3 P7 P8 P9 S1 S2 S3 East East East White White, isMenqian = True } --> True
    check chanta { hand = Hand M1 M2 M3 P7 P8 P9 S1 S2 S3 East East East White White, isMenqian = False } --> True

    check chanta { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> False

-}
chanta : Yaku
chanta =
    { name = "混全帯么九"
    , hanType = Two_ConsiderFulouPenalty
    , condition = \{ hand } -> hand == Hand M1 M2 M3 P7 P8 P9 S1 S2 S3 East East East White White
    }


{-|

    import Data.Tile exposing (Tile(..))

    check chiitoitsu { hand = Hand M1 M1 M7 M7 P2 P2 P9 P9 S2 S2 East East White White, isMenqian = True } --> True
    check chiitoitsu { hand = Hand M1 M1 M7 M7 P2 P2 P9 P9 S2 S2 East East White White, isMenqian = False } --> False

    check chiitoitsu { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> False

-}
chiitoitsu : Yaku
chiitoitsu =
    { name = "七対子"
    , hanType = Two
    , condition = \{ hand, isMenqian } -> isMenqian && (hand == Hand M1 M1 M7 M7 P2 P2 P9 P9 S2 S2 East East White White)
    }



-- 3飜


{-|

    import Data.Tile exposing (Tile(..))

    check ryanpeikou { hand = Hand M1 M2 M3 M1 M2 M3 S7 S8 S9 S7 S8 S9 White White, isMenqian = True } --> True
    check ryanpeikou { hand = Hand M1 M2 M3 M1 M2 M3 S7 S8 S9 S7 S8 S9 White White, isMenqian = False } --> False

    check ryanpeikou { hand = Hand M1 M2 M3 M1 M2 M3 S7 S8 S9 East East East White White, isMenqian = True } --> False

-}
ryanpeikou : Yaku
ryanpeikou =
    { name = "二盃口"
    , hanType = Three
    , condition = \{ hand, isMenqian } -> isMenqian && (hand == Hand M1 M2 M3 M1 M2 M3 S7 S8 S9 S7 S8 S9 White White)
    }


{-|

    import Data.Tile exposing (Tile(..))

    check honitsu { hand = Hand M1 M2 M3 M4 (M5 False) M6 M7 M8 M9 East East East White White, isMenqian = True } --> True
    check honitsu { hand = Hand M1 M2 M3 M4 (M5 False) M6 M7 M8 M9 East East East White White, isMenqian = False } --> True

    check honitsu { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> False

-}
honitsu : Yaku
honitsu =
    { name = "混一色"
    , hanType = Three_ConsiderFulouPenalty
    , condition = \{ hand } -> hand == Hand M1 M2 M3 M4 (M5 False) M6 M7 M8 M9 East East East White White
    }


{-|

    import Data.Tile exposing (Tile(..))

    check junchan { hand = Hand M1 M2 M3 P1 P1 P1 P7 P8 P9 S1 S2 S3 S9 S9, isMenqian = True } --> True
    check junchan { hand = Hand M1 M2 M3 P1 P1 P1 P7 P8 P9 S1 S2 S3 S9 S9, isMenqian = False } --> True

    check junchan { hand = Hand M1 M2 M3 P7 P8 P9 S1 S2 S3 East East East White White, isMenqian = True } --> False

-}
junchan : Yaku
junchan =
    { name = "純全帯么九"
    , hanType = Three_ConsiderFulouPenalty
    , condition = \{ hand } -> hand == Hand M1 M2 M3 P1 P1 P1 P7 P8 P9 S1 S2 S3 S9 S9
    }



-- 6飜


{-|

    import Data.Tile exposing (Tile(..))


    check chinitsu { hand = Hand M1 M2 M3 M1 M2 M3 (M5 False) M6 M7 (M5 False) M6 M7 M9 M9, isMenqian = True } --> True
    check chinitsu { hand = Hand M1 M2 M3 M1 M2 M3 (M5 False) M6 M7 (M5 False) M6 M7 M9 M9, isMenqian = False } --> True

    check chinitsu { hand = Hand M1 M2 M3 M4 (M5 False) M6 M7 M8 M9 East East East White White, isMenqian = True } --> False

-}
chinitsu : Yaku
chinitsu =
    { name = "清一色"
    , hanType = Six_ConsiderFulouPenalty
    , condition = \{ hand } -> hand == Hand M1 M2 M3 M1 M2 M3 (M5 False) M6 M7 (M5 False) M6 M7 M9 M9
    }



-- 役満


{-|

    import Data.Tile exposing (Tile(..))

    check tenho { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> True
    check tenho { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = False } --> False

-}
tenho : Yaku
tenho =
    { name = "天和"
    , hanType = Yakuman
    , condition = \{ isMenqian } -> isMenqian
    }


{-|

    import Data.Tile exposing (Tile(..))

    check chiho { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> True
    check chiho { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = False } --> False

-}
chiho : Yaku
chiho =
    { name = "地和"
    , hanType = Yakuman
    , condition = \{ isMenqian } -> isMenqian
    }


{-|

    import Data.Tile exposing (Tile(..))

    check kokushiMusou { hand = Hand M1 M1 M9 P1 P9 S1 S9 East South West North White Green Red, isMenqian = True } --> True
    check kokushiMusou { hand = Hand M1 M1 M9 P1 P9 S1 S9 East South West North White Green Red, isMenqian = False } --> False

    check kokushiMusou { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> False

-}
kokushiMusou : Yaku
kokushiMusou =
    { name = "国士無双"
    , hanType = Yakuman
    , condition = \{ hand, isMenqian } -> isMenqian && (hand == Hand M1 M1 M9 P1 P9 S1 S9 East South West North White Green Red)
    }


{-|

    import Data.Tile exposing (Tile(..))

    check suanko { hand = Hand M1 M1 M1 P4 P4 P4 S7 S7 S7 East East East White White, isMenqian = True } --> True
    check suanko { hand = Hand M1 M1 M1 P4 P4 P4 S7 S7 S7 East East East White White, isMenqian = False } --> False

    check suanko { hand = Hand M1 M1 M1 P4 P4 P4 S7 S8 S9 East East East White White, isMenqian = True } --> False

-}
suanko : Yaku
suanko =
    { name = "四暗刻"
    , hanType = Yakuman
    , condition = \{ hand, isMenqian } -> isMenqian && (hand == Hand M1 M1 M1 P4 P4 P4 S7 S7 S7 East East East White White)
    }


{-|

    import Data.Tile exposing (Tile(..))

    check daisangen { hand = Hand M1 M2 M3 East East White White White Green Green Green Red Red Red, isMenqian = True } --> True
    check daisangen { hand = Hand M1 M2 M3 East East White White White Green Green Green Red Red Red, isMenqian = False } --> True

    check daisangen { hand = Hand M1 M2 M3 East East East White White White Green Green Green Red Red, isMenqian = True } --> False

-}
daisangen : Yaku
daisangen =
    { name = "大三元"
    , hanType = Yakuman
    , condition = \{ hand } -> hand == Hand M1 M2 M3 East East White White White Green Green Green Red Red Red
    }


{-|

    import Data.Tile exposing (Tile(..))

    check ryuiso { hand = Hand S2 S2 S3 S3 S4 S4 S6 S6 S6 S8 S8 S8 Green Green, isMenqian = True } --> True
    check ryuiso { hand = Hand S2 S2 S3 S3 S4 S4 S6 S6 S6 S8 S8 S8 Green Green, isMenqian = False } --> True

    check ryuiso { hand = Hand S2 S2 S3 S3 S4 S4 S6 S6 S6 S9 S9 S9 Green Green, isMenqian = False } --> False

-}
ryuiso : Yaku
ryuiso =
    { name = "緑一色"
    , hanType = Yakuman
    , condition = \{ hand } -> hand == Hand S2 S2 S3 S3 S4 S4 S6 S6 S6 S8 S8 S8 Green Green
    }


{-|

    import Data.Tile exposing (Tile(..))

    check tsuiso { hand = Hand East East East South South South West West West White White White Red Red, isMenqian = True } --> True
    check tsuiso { hand = Hand East East East South South South West West West White White White Red Red, isMenqian = False } --> True

    check tsuiso { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> False

-}
tsuiso : Yaku
tsuiso =
    { name = "字一色"
    , hanType = Yakuman
    , condition = \{ hand } -> hand == Hand East East East South South South West West West White White White Red Red
    }


{-|

    import Data.Tile exposing (Tile(..))

    check shosushi { hand = Hand M1 M2 M3 East East East South South South West West West North North, isMenqian = True } --> True
    check shosushi { hand = Hand M1 M2 M3 East East East South South South West West West North North, isMenqian = False } --> True

    check shosushi { hand = Hand East East East South South South West West West White White White Red Red, isMenqian = True } --> False

-}
shosushi : Yaku
shosushi =
    { name = "小四喜"
    , hanType = Yakuman
    , condition = \{ hand } -> hand == Hand M1 M2 M3 East East East South South South West West West North North
    }


{-|

    import Data.Tile exposing (Tile(..))

    check daishushi { hand = Hand M1 M1 East East East South South South West West West North North North, isMenqian = True } --> True
    check daishushi { hand = Hand M1 M1 East East East South South South West West West North North North, isMenqian = False } --> True

    check daishushi { hand = Hand M1 M2 M3 East East East South South South West West West North North, isMenqian = True } --> False

-}
daishushi : Yaku
daishushi =
    { name = "大四喜"
    , hanType = Yakuman
    , condition = \{ hand } -> hand == Hand M1 M1 East East East South South South West West West North North North
    }


{-|

    import Data.Tile exposing (Tile(..))

    check chinroto { hand = Hand M1 M1 M1 M9 M9 M9 P1 P1 P1 P9 P9 P9 S1 S1, isMenqian = True } --> True
    check chinroto { hand = Hand M1 M1 M1 M9 M9 M9 P1 P1 P1 P9 P9 P9 S1 S1, isMenqian = False } --> True

    check chinroto { hand = Hand M1 M1 M1 P9 P9 P9 S1 S1 S1 East East East White White, isMenqian = True } --> False

-}
chinroto : Yaku
chinroto =
    { name = "清老頭"
    , hanType = Yakuman
    , condition = \{ hand } -> hand == Hand M1 M1 M1 M9 M9 M9 P1 P1 P1 P9 P9 P9 S1 S1
    }


{-|

    import Data.Tile exposing (Tile(..))

    check sukantsu { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> True
    check sukantsu { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = False } --> True

-}
sukantsu : Yaku
sukantsu =
    { name = "四槓子"
    , hanType = Yakuman
    , condition = always True
    }


{-|

    import Data.Tile exposing (Tile(..))

    check churenPoto { hand = Hand M1 M1 M1 M2 M3 M4 (M5 False) M6 M7 M8 M9 M9 M9 M2, isMenqian = True } --> True
    check churenPoto { hand = Hand M1 M1 M1 M2 M3 M4 (M5 False) M6 M7 M8 M9 M9 M9 M2, isMenqian = False } --> False

    check churenPoto { hand = Hand M1 M2 M3 P4 (P5 False) P6 S7 S8 S9 East East East White White, isMenqian = True } --> False

-}
churenPoto : Yaku
churenPoto =
    { name = "九蓮宝燈"
    , hanType = Yakuman
    , condition = \{ hand, isMenqian } -> isMenqian && (hand == Hand M1 M1 M1 M2 M3 M4 (M5 False) M6 M7 M8 M9 M9 M9 M2)
    }

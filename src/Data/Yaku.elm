module Data.Yaku exposing
    ( Yaku
    , menzenTsumo, reach, ippatsu, yakuhai, pinfu, tanyao, ipeko, haitei, houtei, chankan, rinshanKaihou
    , doubleReach, renpuhai, toitoi, sananko, sanshokuDoukou, sankantsu, shousangen, honroutou, sanshokuDoujun, ittsu, chanta, chiitoitsu
    , ryanpeikou, honitsu, junchan
    , chinitsu
    , tenho, chiho, kokushiMusou, suanko, daisangen, ryuiso, tsuiso, shosushi, daishushi, chinroto, sukantsu, churenPoto
    )

{-|

@docs Yaku
@docs menzenTsumo, reach, ippatsu, yakuhai, pinfu, tanyao, ipeko, haitei, houtei, chankan, rinshanKaihou
@docs doubleReach, renpuhai, toitoi, sananko, sanshokuDoukou, sankantsu, shousangen, honroutou, sanshokuDoujun, ittsu, chanta, chiitoitsu
@docs ryanpeikou, honitsu, junchan
@docs chinitsu
@docs tenho, chiho, kokushiMusou, suanko, daisangen, ryuiso, tsuiso, shosushi, daishushi, chinroto, sukantsu, churenPoto

-}


type alias Yaku =
    { name : String
    , hanType : HanType
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



-- 1飜


menzenTsumo : Yaku
menzenTsumo =
    { name = "門前清自摸和", hanType = One }


reach : Yaku
reach =
    { name = "立直", hanType = One }


ippatsu : Yaku
ippatsu =
    { name = "一発", hanType = One }


yakuhai : Yaku
yakuhai =
    { name = "役牌", hanType = One }


pinfu : Yaku
pinfu =
    { name = "平和", hanType = One }


tanyao : Yaku
tanyao =
    { name = "断么九", hanType = One }


ipeko : Yaku
ipeko =
    { name = "一盃口", hanType = One }


haitei : Yaku
haitei =
    { name = "海底摸月", hanType = One }


houtei : Yaku
houtei =
    { name = "河底撈魚", hanType = One }


chankan : Yaku
chankan =
    { name = "槍槓", hanType = One }


rinshanKaihou : Yaku
rinshanKaihou =
    { name = "嶺上開花", hanType = One }



-- 2飜


doubleReach : Yaku
doubleReach =
    { name = "ダブル立直", hanType = Two }


renpuhai : Yaku
renpuhai =
    { name = "連風牌", hanType = Two }


toitoi : Yaku
toitoi =
    { name = "対々和", hanType = Two }


sananko : Yaku
sananko =
    { name = "三暗刻", hanType = Two }


sanshokuDoukou : Yaku
sanshokuDoukou =
    { name = "三色同刻", hanType = Two }


sankantsu : Yaku
sankantsu =
    { name = "三槓子", hanType = Two }


shousangen : Yaku
shousangen =
    { name = "小三元", hanType = Two }


honroutou : Yaku
honroutou =
    { name = "混老頭", hanType = Two }


sanshokuDoujun : Yaku
sanshokuDoujun =
    { name = "三色同順", hanType = Two_ConsiderFulouPenalty }


ittsu : Yaku
ittsu =
    { name = "一気通貫", hanType = Two_ConsiderFulouPenalty }


chanta : Yaku
chanta =
    { name = "混全帯么九", hanType = Two_ConsiderFulouPenalty }


chiitoitsu : Yaku
chiitoitsu =
    { name = "七対子", hanType = Two }



-- 3飜


ryanpeikou : Yaku
ryanpeikou =
    { name = "二盃口", hanType = Three }


honitsu : Yaku
honitsu =
    { name = "混一色", hanType = Three_ConsiderFulouPenalty }


junchan : Yaku
junchan =
    { name = "純全帯么九", hanType = Three_ConsiderFulouPenalty }



-- 6飜


chinitsu : Yaku
chinitsu =
    { name = "清一色", hanType = Six_ConsiderFulouPenalty }



-- 役満


tenho : Yaku
tenho =
    { name = "天和", hanType = Yakuman }


chiho : Yaku
chiho =
    { name = "地和", hanType = Yakuman }


kokushiMusou : Yaku
kokushiMusou =
    { name = "国士無双", hanType = Yakuman }


suanko : Yaku
suanko =
    { name = "四暗刻", hanType = Yakuman }


daisangen : Yaku
daisangen =
    { name = "大三元", hanType = Yakuman }


ryuiso : Yaku
ryuiso =
    { name = "緑一色", hanType = Yakuman }


tsuiso : Yaku
tsuiso =
    { name = "字一色", hanType = Yakuman }


shosushi : Yaku
shosushi =
    { name = "小四喜", hanType = Yakuman }


daishushi : Yaku
daishushi =
    { name = "大四喜", hanType = Yakuman }


chinroto : Yaku
chinroto =
    { name = "清老頭", hanType = Yakuman }


sukantsu : Yaku
sukantsu =
    { name = "四槓子", hanType = Yakuman }


churenPoto : Yaku
churenPoto =
    { name = "九蓮宝燈", hanType = Yakuman }

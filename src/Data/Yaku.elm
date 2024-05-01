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
    { name : String }



-- 1飜


menzenTsumo : Yaku
menzenTsumo =
    { name = "門前清自摸和" }


reach : Yaku
reach =
    { name = "立直" }


ippatsu : Yaku
ippatsu =
    { name = "一発" }


yakuhai : Yaku
yakuhai =
    { name = "役牌" }


pinfu : Yaku
pinfu =
    { name = "平和" }


tanyao : Yaku
tanyao =
    { name = "断么九" }


ipeko : Yaku
ipeko =
    { name = "一盃口" }


haitei : Yaku
haitei =
    { name = "海底摸月" }


houtei : Yaku
houtei =
    { name = "河底撈魚" }


chankan : Yaku
chankan =
    { name = "槍槓" }


rinshanKaihou : Yaku
rinshanKaihou =
    { name = "嶺上開花" }



-- 2飜


doubleReach : Yaku
doubleReach =
    { name = "ダブル立直" }


renpuhai : Yaku
renpuhai =
    { name = "連風牌" }


toitoi : Yaku
toitoi =
    { name = "対々和" }


sananko : Yaku
sananko =
    { name = "三暗刻" }


sanshokuDoukou : Yaku
sanshokuDoukou =
    { name = "三色同刻" }


sankantsu : Yaku
sankantsu =
    { name = "三槓子" }


shousangen : Yaku
shousangen =
    { name = "小三元" }


honroutou : Yaku
honroutou =
    { name = "混老頭" }


sanshokuDoujun : Yaku
sanshokuDoujun =
    { name = "三色同順" }


ittsu : Yaku
ittsu =
    { name = "一気通貫" }


chanta : Yaku
chanta =
    { name = "混全帯么九" }


chiitoitsu : Yaku
chiitoitsu =
    { name = "七対子" }



-- 3飜


ryanpeikou : Yaku
ryanpeikou =
    { name = "二盃口" }


honitsu : Yaku
honitsu =
    { name = "混一色" }


junchan : Yaku
junchan =
    { name = "純全帯么九" }



-- 6飜


chinitsu : Yaku
chinitsu =
    { name = "清一色" }



-- 役満


tenho : Yaku
tenho =
    { name = "天和" }


chiho : Yaku
chiho =
    { name = "地和" }


kokushiMusou : Yaku
kokushiMusou =
    { name = "国士無双" }


suanko : Yaku
suanko =
    { name = "四暗刻" }


daisangen : Yaku
daisangen =
    { name = "大三元" }


ryuiso : Yaku
ryuiso =
    { name = "緑一色" }


tsuiso : Yaku
tsuiso =
    { name = "字一色" }


shosushi : Yaku
shosushi =
    { name = "小四喜" }


daishushi : Yaku
daishushi =
    { name = "大四喜" }


chinroto : Yaku
chinroto =
    { name = "清老頭" }


sukantsu : Yaku
sukantsu =
    { name = "四槓子" }


churenPoto : Yaku
churenPoto =
    { name = "九蓮宝燈" }

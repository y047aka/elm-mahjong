module Data.Yaku exposing
    ( Yaku
    , check
    , menzenTsumo, reach, ippatsu, yakuhai_SeatWind, yakuhai_RoundWind, yakuhai_Sangen, pinfu, tanyao, ipeko, haitei, houtei, chankan, rinshanKaihou
    , doubleReach, toitoi, sananko, sanshokuDoukou, sankantsu, shousangen, honroutou, sanshokuDoujun, ittsu, chanta, chiitoitsu
    , ryanpeikou, honitsu, junchan
    , chinitsu
    , tenho, chiho, kokushiMusou, suanko, daisangen, ryuiso, tsuiso, shosushi, daishushi, chinroto, sukantsu, churenPoto
    , Situation(..)
    )

{-|

@docs Yaku
@docs check

@docs menzenTsumo, reach, ippatsu, yakuhai_SeatWind, yakuhai_RoundWind, yakuhai_Sangen, pinfu, tanyao, ipeko, haitei, houtei, chankan, rinshanKaihou
@docs doubleReach, toitoi, sananko, sanshokuDoukou, sankantsu, shousangen, honroutou, sanshokuDoujun, ittsu, chanta, chiitoitsu
@docs ryanpeikou, honitsu, junchan
@docs chinitsu
@docs tenho, chiho, kokushiMusou, suanko, daisangen, ryuiso, tsuiso, shosushi, daishushi, chinroto, sukantsu, churenPoto

-}

import Data.Group as Group exposing (Group(..))
import Data.Tile as Tile exposing (Tile(..))
import List.Extra exposing (gatherEquals)


type alias Yaku =
    { name : String
    , hanType : HanType
    , requirement : HandState -> Bool
    }


type HanType
    = One
    | Two
    | Two_ConsiderFulouPenalty
    | Three
    | Three_ConsiderFulouPenalty
    | Six_ConsiderFulouPenalty
    | Yakuman


countHan : { isMenqian_ : Bool } -> List HanType -> Int
countHan state hanTypes =
    if List.member Yakuman hanTypes then
        calcHan state Yakuman

    else
        List.map (calcHan state) hanTypes
            |> List.sum


calcHan : { isMenqian_ : Bool } -> HanType -> Int
calcHan { isMenqian_ } hanType =
    case ( hanType, isMenqian_ ) of
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


type alias HandState =
    { groups : List Group
    , situations : List Situation
    }


check : Yaku -> HandState -> Bool
check yaku state =
    yaku.requirement state


type Situation
    = SeatWind Tile
    | RoundWind Tile
    | WaitType_OpenWait
    | Menqian
    | WinByTsumo
    | WinByRon
    | Reach
    | Ippatsu
    | Chankan
    | RinshanKaihou
    | Haitei
    | Tenho
    | Chiho
    | Chiitoitsu
    | KokushiMusou
    | ChurenPoto


findSeatWind : List Situation -> Maybe Tile
findSeatWind situations =
    List.Extra.findMap
        (\s ->
            case s of
                SeatWind t ->
                    Just t

                _ ->
                    Nothing
        )
        situations


findRoundWind : List Situation -> Maybe Tile
findRoundWind situations =
    List.Extra.findMap
        (\s ->
            case s of
                RoundWind t ->
                    Just t

                _ ->
                    Nothing
        )
        situations



-- 1飜


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check menzenTsumo { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian, WinByTsumo ] } --> True
    check menzenTsumo { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ WinByTsumo ] } --> False

-}
menzenTsumo : Yaku
menzenTsumo =
    { name = "門前清自摸和"
    , hanType = One
    , requirement = \{ situations } -> members [ Menqian, WinByTsumo ] situations
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check reach { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian, Reach ] } --> True
    check reach { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Reach ] } --> False

-}
reach : Yaku
reach =
    { name = "立直"
    , hanType = One
    , requirement = \{ situations } -> members [ Menqian, Reach ] situations
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check ippatsu { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian, Reach, Ippatsu ] } --> True
    check ippatsu { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Reach, Ippatsu ] } --> False

-}
ippatsu : Yaku
ippatsu =
    { name = "一発"
    , hanType = One
    , requirement = \{ situations } -> members [ Menqian, Reach, Ippatsu ] situations
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check yakuhai_SeatWind { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ SeatWind East ] } --> True
    check yakuhai_SeatWind { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ SeatWind South ] } --> False

-}
yakuhai_SeatWind : Yaku
yakuhai_SeatWind =
    { name = "役牌：自風牌"
    , hanType = One
    , requirement =
        \{ groups, situations } ->
            findSeatWind situations
                |> Maybe.map (\wind -> List.member (Triplet wind wind wind) groups)
                |> Maybe.withDefault False
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check yakuhai_RoundWind { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ RoundWind East ] } --> True
    check yakuhai_RoundWind { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ RoundWind South ] } --> False

-}
yakuhai_RoundWind : Yaku
yakuhai_RoundWind =
    { name = "役牌：場風牌"
    , hanType = One
    , requirement =
        \{ groups, situations } ->
            findRoundWind situations
                |> Maybe.map (\wind -> List.member (Triplet wind wind wind) groups)
                |> Maybe.withDefault False
    }


{-|

        import Data.Tile exposing (Tile(..))
        import Data.Group exposing (Group(..))

        check yakuhai_Sangen { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Pair East East, Triplet White White White ], situations = [] } --> True
        check yakuhai_Sangen { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [] } --> False

-}
yakuhai_Sangen : Yaku
yakuhai_Sangen =
    { name = "役牌：三元牌"
    , hanType = One
    , requirement =
        \{ groups } ->
            let
                isDragonTriplet group =
                    List.member group [ Triplet White White White, Triplet Green Green Green, Triplet Red Red Red ]
            in
            List.Extra.count isDragonTriplet groups >= 1
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check pinfu { groups = [ Run M1 M2 M3, Run M4 (M5 False) M6, Run P7 P8 P9, Run S7 S8 S9, Pair M9 M9 ], situations = [ WaitType_OpenWait, Menqian ] } --> True
    check pinfu { groups = [ Run M1 M2 M3, Run M4 (M5 False) M6, Run P7 P8 P9, Run S7 S8 S9, Pair North North ], situations = [ RoundWind East, SeatWind South, WaitType_OpenWait, Menqian ] } --> True

-}
pinfu : Yaku
pinfu =
    { name = "平和"
    , hanType = One
    , requirement =
        \{ groups, situations } ->
            let
                fourRuns =
                    List.Extra.count Group.isRun groups == 4

                yakuhaiWinds =
                    List.filterMap
                        (\s ->
                            case s of
                                SeatWind t ->
                                    Just t

                                RoundWind t ->
                                    Just t

                                _ ->
                                    Nothing
                        )
                        situations

                notYakuhaiWindPairs =
                    List.filter (\t -> not (List.member t yakuhaiWinds)) [ East, South, West, North ]
                        |> List.map (\t -> Pair t t)

                noYakuhaiPairs =
                    List.all (\g -> Group.isSuit g || List.member g notYakuhaiWindPairs) groups
            in
            members [ WaitType_OpenWait, Menqian ] situations && fourRuns && noYakuhaiPairs
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check tanyao { groups = [ Run M2 M3 M4, Run M6 M7 M8, Run P2 P3 P4, Run P6 P7 P8, Pair S2 S2 ], situations = [ Menqian ] } --> True
    check tanyao { groups = [ Run M2 M3 M4, Run M6 M7 M8, Run P2 P3 P4, Run P6 P7 P8, Pair S2 S2 ], situations = [] } --> True

    check tanyao { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> False

-}
tanyao : Yaku
tanyao =
    { name = "断么九"
    , hanType = One
    , requirement = \{ groups } -> List.all (\g -> not (Group.isYaojiu g)) groups
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check ipeko { groups = [ Run M1 M2 M3, Run M1 M2 M3, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> True
    check ipeko { groups = [ Run M1 M2 M3, Run M1 M2 M3, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [] } --> False

    check ipeko { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> False

-}
ipeko : Yaku
ipeko =
    { name = "一盃口"
    , hanType = One
    , requirement =
        \{ groups, situations } ->
            members [ Menqian ] situations
                && (List.filter Group.isRun groups
                        |> gatherEquals
                        |> List.Extra.count (\( head, tails ) -> List.length (head :: tails) >= 2)
                        |> (==) 1
                   )
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check haitei { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ WinByTsumo, Haitei ] } --> True
    check haitei { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ WinByTsumo ] } --> False

-}
haitei : Yaku
haitei =
    { name = "海底摸月"
    , hanType = One
    , requirement = \{ situations } -> members [ WinByTsumo, Haitei ] situations
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check houtei { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ WinByRon, Haitei ] } --> True
    check houtei { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ WinByRon ] } --> False

-}
houtei : Yaku
houtei =
    { name = "河底撈魚"
    , hanType = One
    , requirement = \{ situations } -> members [ WinByRon, Haitei ] situations
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check chankan { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ WinByRon, Chankan ] } --> True
    check chankan { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ WinByRon ] } --> False

-}
chankan : Yaku
chankan =
    { name = "槍槓"
    , hanType = One
    , requirement = \{ situations } -> members [ WinByRon, Chankan ] situations
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check rinshanKaihou { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ WinByTsumo, RinshanKaihou ] } --> True
    check rinshanKaihou { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ WinByTsumo ] } --> False

-}
rinshanKaihou : Yaku
rinshanKaihou =
    { name = "嶺上開花"
    , hanType = One
    , requirement = \{ situations } -> members [ WinByTsumo, RinshanKaihou ] situations
    }



-- 2飜


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check doubleReach { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian, Reach ] } --> True
    check doubleReach { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> False

-}
doubleReach : Yaku
doubleReach =
    { name = "ダブル立直"
    , hanType = Two
    , requirement = \{ situations } -> members [ Menqian, Reach ] situations
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check toitoi { groups = [ Triplet M1 M1 M1, Triplet P4 P4 P4, Triplet S7 S7 S7, Triplet East East East, Pair White White ], situations = [] } --> True
    check toitoi { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [] } --> False

-}
toitoi : Yaku
toitoi =
    { name = "対々和"
    , hanType = Two
    , requirement =
        \{ groups } ->
            List.Extra.count (\g -> Group.isTriplet g || Group.isGang g) groups
                |> (==) 4
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check sananko { groups = [ Triplet M1 M1 M1, Triplet P4 P4 P4, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [] } --> True
    check sananko { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [] } --> False

-}
sananko : Yaku
sananko =
    { name = "三暗刻"
    , hanType = Two
    , requirement = \{ groups } -> List.Extra.count Group.isTriplet groups |> (==) 3
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check sanshokuDoukou { groups = [ Triplet M1 M1 M1, Triplet P1 P1 P1, Triplet S1 S1 S1, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> True
    check sanshokuDoukou { groups = [ Triplet M1 M1 M1, Triplet P1 P1 P1, Triplet S1 S1 S1, Triplet East East East, Pair White White ], situations = [] } --> True

    check sanshokuDoukou { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [] } --> False

-}
sanshokuDoukou : Yaku
sanshokuDoukou =
    { name = "三色同刻"
    , hanType = Two
    , requirement =
        \{ groups } ->
            members [ Triplet M1 M1 M1, Triplet P1 P1 P1, Triplet S1 S1 S1 ] groups
                || members [ Triplet M2 M2 M2, Triplet P2 P2 P2, Triplet S2 S2 S2 ] groups
                || members [ Triplet M3 M3 M3, Triplet P3 P3 P3, Triplet S3 S3 S3 ] groups
                || members [ Triplet M4 M4 M4, Triplet P4 P4 P4, Triplet S4 S4 S4 ] groups
                || members [ Triplet (M5 False) (M5 False) (M5 False), Triplet (P5 False) (P5 False) (P5 False), Triplet (S5 False) (S5 False) (S5 False) ] groups
                || members [ Triplet M6 M6 M6, Triplet P6 P6 P6, Triplet S6 S6 S6 ] groups
                || members [ Triplet M7 M7 M7, Triplet P7 P7 P7, Triplet S7 S7 S7 ] groups
                || members [ Triplet M8 M8 M8, Triplet P8 P8 P8, Triplet S8 S8 S8 ] groups
                || members [ Triplet M9 M9 M9, Triplet P9 P9 P9, Triplet S9 S9 S9 ] groups
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check sankantsu { groups = [ Gang M1 M1 M1 M1, Gang P4 P4 P4 P4, Run S7 S8 S9, Gang East East East East, Pair White White ], situations = [] } --> True
    check sankantsu { groups = [ Gang M1 M1 M1 M1, Gang P4 P4 P4 P4, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [] } --> False

-}
sankantsu : Yaku
sankantsu =
    { name = "三槓子"
    , hanType = Two
    , requirement = \{ groups } -> List.Extra.count Group.isGang groups |> (==) 3
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check shousangen { groups = [ Run M1 M2 M3, Triplet East East East, Triplet White White White, Triplet Green Green Green, Pair Red Red ], situations = [ Menqian ] } --> True
    check shousangen { groups = [ Run M1 M2 M3, Triplet East East East, Triplet White White White, Triplet Green Green Green, Pair Red Red ], situations = [] } --> True

    check shousangen { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> False

-}
shousangen : Yaku
shousangen =
    { name = "小三元"
    , hanType = Two
    , requirement =
        \{ groups } ->
            let
                isDragonTriplet group =
                    List.member group [ Triplet White White White, Triplet Green Green Green, Triplet Red Red Red ]

                isDragonPair group =
                    List.member group [ Pair White White, Pair Green Green, Pair Red Red ]

                twoDragonTriplets =
                    List.Extra.count isDragonTriplet groups == 2

                oneDragonPair =
                    List.Extra.count isDragonPair groups == 1
            in
            twoDragonTriplets && oneDragonPair
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check honroutou { groups = [ Triplet M1 M1 M1, Triplet P9 P9 P9, Triplet S1 S1 S1, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> True
    check honroutou { groups = [ Triplet M1 M1 M1, Triplet P9 P9 P9, Triplet S1 S1 S1, Triplet East East East, Pair White White ], situations = [] } --> True

    check honroutou { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> False

-}
honroutou : Yaku
honroutou =
    { name = "混老頭"
    , hanType = Two
    , requirement =
        \{ groups } ->
            fourTriplets groups
                && List.all Group.isYaojiu groups
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check sanshokuDoujun { groups = [ Run M1 M2 M3, Run P1 P2 P3, Run S1 S2 S3, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> True
    check sanshokuDoujun { groups = [ Run M1 M2 M3, Run P1 P2 P3, Run S1 S2 S3, Triplet East East East, Pair White White ], situations = [] } --> True

    check sanshokuDoujun { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> False

-}
sanshokuDoujun : Yaku
sanshokuDoujun =
    { name = "三色同順"
    , hanType = Two_ConsiderFulouPenalty
    , requirement =
        \{ groups } ->
            members [ Run M1 M2 M3, Run P1 P2 P3, Run S1 S2 S3 ] groups
                || members [ Run M2 M3 M4, Run P2 P3 P4, Run S2 S3 S4 ] groups
                || members [ Run M3 M4 (M5 False), Run P3 P4 (P5 False), Run S3 S4 (S5 False) ] groups
                || members [ Run M4 (M5 False) M6, Run P4 (P5 False) P6, Run S4 (S5 False) S6 ] groups
                || members [ Run (M5 False) M6 M7, Run (P5 False) P6 P7, Run (S5 False) S6 S7 ] groups
                || members [ Run M6 M7 M8, Run P6 P7 P8, Run S6 S7 S8 ] groups
                || members [ Run M7 M8 M9, Run P7 P8 P9, Run S7 S8 S9 ] groups
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check ittsu { groups = [ Run M1 M2 M3, Run M4 (M5 False) M6, Run M7 M8 M9, Triplet East East East, Pair White White ], situations = [] } --> True
    check ittsu { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [] } --> False

-}
ittsu : Yaku
ittsu =
    { name = "一気通貫"
    , hanType = Two_ConsiderFulouPenalty
    , requirement =
        \{ groups } ->
            members [ Run M1 M2 M3, Run M4 (M5 False) M6, Run M7 M8 M9 ] groups
                || members [ Run P1 P2 P3, Run P4 (P5 False) P6, Run P7 P8 P9 ] groups
                || members [ Run S1 S2 S3, Run S4 (S5 False) S6, Run S7 S8 S9 ] groups
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check chanta { groups = [ Run M1 M2 M3, Run P7 P8 P9, Run S1 S2 S3, Triplet East East East, Pair White White ], situations = [] } --> True
    check chanta { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [] } --> False

-}
chanta : Yaku
chanta =
    { name = "混全帯么九"
    , hanType = Two_ConsiderFulouPenalty
    , requirement =
        \{ groups } ->
            List.all (\g -> Group.containsTerminal g || Group.isHonor g) groups
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check chiitoitsu { groups = [ Pair M1 M1, Pair M7 M7, Pair P2 P2, Pair P9 P9, Pair S2 S2, Pair East East, Pair White White ], situations = [ Menqian, Chiitoitsu ] } --> True
    check chiitoitsu { groups = [ Pair M1 M1, Pair M7 M7, Pair P2 P2, Pair P9 P9, Pair S2 S2, Pair East East, Pair White White ], situations = [ Chiitoitsu ] } --> False

    check chiitoitsu { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> False

-}
chiitoitsu : Yaku
chiitoitsu =
    { name = "七対子"
    , hanType = Two
    , requirement =
        \{ groups, situations } ->
            members [ Menqian, Chiitoitsu ] situations
                && (List.Extra.count Group.isPair groups == 7)
    }



-- 3飜


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check ryanpeikou { groups = [ Run M1 M2 M3, Run M1 M2 M3, Run S7 S8 S9, Run S7 S8 S9, Pair White White ], situations = [ Menqian ] } --> True
    check ryanpeikou { groups = [ Run M1 M2 M3, Run M1 M2 M3, Run S7 S8 S9, Run S7 S8 S9, Pair White White ], situations = [] } --> False

    check ryanpeikou { groups = [ Run M1 M2 M3, Run M1 M2 M3, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> False

-}
ryanpeikou : Yaku
ryanpeikou =
    { name = "二盃口"
    , hanType = Three
    , requirement =
        \{ groups, situations } ->
            members [ Menqian ] situations
                && (List.filter Group.isRun groups
                        |> gatherEquals
                        |> List.Extra.count (\( head, tails ) -> List.length (head :: tails) >= 2)
                        |> (==) 2
                   )
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check honitsu { groups = [ Run M1 M2 M3, Run M4 (M5 False) M6, Run M7 M8 M9, Triplet East East East, Pair White White ], situations = [] } --> True
    check honitsu { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [] } --> False

-}
honitsu : Yaku
honitsu =
    { name = "混一色"
    , hanType = Three_ConsiderFulouPenalty
    , requirement =
        \{ groups } ->
            let
                tiles =
                    List.concatMap Group.toTiles groups

                allManOrHonor =
                    List.all (\t -> Tile.isMan t || Tile.isHonor t) tiles

                allPinOrHonor =
                    List.all (\t -> Tile.isPin t || Tile.isHonor t) tiles

                allSouOrHonor =
                    List.all (\t -> Tile.isSou t || Tile.isHonor t) tiles
            in
            allManOrHonor || allPinOrHonor || allSouOrHonor
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check junchan { groups = [ Run M1 M2 M3, Triplet P1 P1 P1, Run P7 P8 P9, Run S1 S2 S3, Pair S9 S9 ], situations = [] } --> True

    check junchan { groups = [ Run M1 M2 M3, Run P7 P8 P9, Run S1 S2 S3, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> False

-}
junchan : Yaku
junchan =
    { name = "純全帯么九"
    , hanType = Three_ConsiderFulouPenalty
    , requirement = \{ groups } -> List.all Group.containsTerminal groups
    }



-- 6飜


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))


    check chinitsu { groups = [ Run M1 M2 M3, Run M1 M2 M3, Run (M5 False) M6 M7, Run (M5 False) M6 M7, Pair M9 M9 ], situations = [] } --> True

    check chinitsu { groups = [ Run M1 M2 M3, Run M4 (M5 False) M6, Run M7 M8 M9, Triplet East East East, Pair White White ], situations = [] } --> False

-}
chinitsu : Yaku
chinitsu =
    { name = "清一色"
    , hanType = Six_ConsiderFulouPenalty
    , requirement =
        \{ groups } ->
            let
                tiles =
                    List.concatMap Group.toTiles groups

                allMan =
                    List.all Tile.isMan tiles

                allPin =
                    List.all Tile.isPin tiles

                allSou =
                    List.all Tile.isSou tiles
            in
            allMan || allPin || allSou
    }



-- 役満


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check tenho { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian, Tenho ] } --> True
    check tenho { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> False

-}
tenho : Yaku
tenho =
    { name = "天和"
    , hanType = Yakuman
    , requirement = \{ situations } -> members [ Menqian, Tenho ] situations
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check chiho { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian, Chiho ] } --> True
    check chiho { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> False

-}
chiho : Yaku
chiho =
    { name = "地和"
    , hanType = Yakuman
    , requirement = \{ situations } -> members [ Menqian, Chiho ] situations
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check kokushiMusou { groups = [ Kokushi M1 M1 M9 P1 P9 S1 S9 East South West North White Green Red ], situations = [ Menqian, KokushiMusou ] } --> True
    check kokushiMusou { groups = [ Kokushi M1 M1 M9 P1 P9 S1 S9 East South West North White Green Red ], situations = [ KokushiMusou ] } --> False

    check kokushiMusou { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> False

-}
kokushiMusou : Yaku
kokushiMusou =
    { name = "国士無双"
    , hanType = Yakuman
    , requirement =
        \{ groups, situations } ->
            members [ Menqian, KokushiMusou ] situations
                && (List.Extra.count Group.isKokushi groups == 1)
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check suanko { groups = [ Triplet M1 M1 M1, Triplet P4 P4 P4, Triplet S7 S7 S7, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> True
    check suanko { groups = [ Triplet M1 M1 M1, Triplet P4 P4 P4, Triplet S7 S7 S7, Triplet East East East, Pair White White ], situations = [] } --> False

    check suanko { groups = [ Triplet M1 M1 M1, Triplet P4 P4 P4, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> False

-}
suanko : Yaku
suanko =
    { name = "四暗刻"
    , hanType = Yakuman
    , requirement =
        \{ groups, situations } ->
            members [ Menqian ] situations
                && fourTriplets groups
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check daisangen { groups = [ Run M1 M2 M3, Pair East East, Triplet White White White, Triplet Green Green Green, Triplet Red Red Red ], situations = [] } --> True
    check daisangen { groups = [ Run M1 M2 M3, Triplet East East East, Triplet White White White, Triplet Green Green Green, Pair Red Red ], situations = [] } --> False

-}
daisangen : Yaku
daisangen =
    { name = "大三元"
    , hanType = Yakuman
    , requirement =
        \{ groups } ->
            members
                [ Triplet White White White
                , Triplet Green Green Green
                , Triplet Red Red Red
                ]
                groups
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check ryuiso { groups = [ Run S2 S3 S4, Run S2 S3 S4, Triplet S6 S6 S6, Triplet S8 S8 S8, Pair Green Green ], situations = [] } --> True
    check ryuiso { groups = [ Run S2 S3 S4, Run S2 S3 S4, Triplet S6 S6 S6, Triplet S9 S9 S9, Pair Green Green ], situations = [] } --> False

-}
ryuiso : Yaku
ryuiso =
    { name = "緑一色"
    , hanType = Yakuman
    , requirement =
        \{ groups } ->
            List.concatMap Group.toTiles groups
                |> List.all (\t -> List.member t [ S2, S3, S4, S6, S8, Green ])
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check tsuiso { groups = [ Triplet East East East, Triplet South South South, Triplet West West West, Triplet White White White, Pair Red Red ], situations = [] } --> True
    check tsuiso { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [] } --> False

-}
tsuiso : Yaku
tsuiso =
    { name = "字一色"
    , hanType = Yakuman
    , requirement = \{ groups } -> List.all Group.isHonor groups
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check shosushi { groups = [ Run M1 M2 M3, Triplet East East East, Triplet South South South, Triplet West West West, Pair North North ], situations = [] } --> True
    check shosushi { groups = [ Triplet East East East, Triplet South South South, Triplet West West West, Triplet White White White, Pair Red Red ], situations = [] } --> False

-}
shosushi : Yaku
shosushi =
    { name = "小四喜"
    , hanType = Yakuman
    , requirement =
        \{ groups } ->
            let
                isWindTriplet group =
                    List.member group [ Triplet East East East, Triplet South South South, Triplet West West West, Triplet North North North ]

                isWindPair group =
                    List.member group [ Pair East East, Pair South South, Pair West West, Pair North North ]

                threeWindTriplets =
                    List.Extra.count isWindTriplet groups == 3

                oneWindPair =
                    List.Extra.count isWindPair groups == 1
            in
            threeWindTriplets && oneWindPair
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check daishushi { groups = [ Pair M1 M1, Triplet East East East, Triplet South South South, Triplet West West West, Triplet North North North ], situations = [] } --> True
    check daishushi { groups = [ Run M1 M2 M3, Triplet East East East, Triplet South South South, Triplet West West West, Pair North North ], situations = [] } --> False

-}
daishushi : Yaku
daishushi =
    { name = "大四喜"
    , hanType = Yakuman
    , requirement =
        \{ groups } ->
            members
                [ Triplet East East East
                , Triplet South South South
                , Triplet West West West
                , Triplet North North North
                ]
                groups
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check chinroto { groups = [ Triplet M1 M1 M1, Triplet M9 M9 M9, Triplet P1 P1 P1, Triplet P9 P9 P9, Pair S1 S1 ], situations = [] } --> True
    check chinroto { groups = [ Triplet M1 M1 M1, Triplet P9 P9 P9, Triplet S1 S1 S1, Triplet East East East, Pair White White ], situations = [] } --> False

-}
chinroto : Yaku
chinroto =
    { name = "清老頭"
    , hanType = Yakuman
    , requirement =
        \{ groups } ->
            fourTriplets groups
                && List.all Group.isTerminal groups
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check sukantsu { groups = [ Gang M1 M1 M1 M1, Gang P4 P4 P4 P4, Gang S7 S7 S7 S7, Gang East East East East, Pair White White ], situations = [] } --> True
    check sukantsu { groups = [ Gang M1 M1 M1 M1, Gang P4 P4 P4 P4, Run S7 S8 S9, Gang East East East East, Pair White White ], situations = [] } --> False

-}
sukantsu : Yaku
sukantsu =
    { name = "四槓子"
    , hanType = Yakuman
    , requirement = \{ groups } -> List.Extra.count Group.isGang groups == 4
    }


{-|

    import Data.Tile exposing (Tile(..))
    import Data.Group exposing (Group(..))

    check churenPoto { groups = [ Triplet M1 M1 M1, Pair M2 M2, Run M3 M4 (M5 False), Run M6 M7 M8, Triplet M9 M9 M9 ], situations = [ Menqian, ChurenPoto ] } --> True
    check churenPoto { groups = [ Triplet M1 M1 M1, Pair M2 M2, Run M3 M4 (M5 False), Run M6 M7 M8, Triplet M9 M9 M9 ], situations = [ ChurenPoto ] } --> False

    check churenPoto { groups = [ Run M1 M2 M3, Run P4 (P5 False) P6, Run S7 S8 S9, Triplet East East East, Pair White White ], situations = [ Menqian ] } --> False

-}
churenPoto : Yaku
churenPoto =
    { name = "九蓮宝燈"
    , hanType = Yakuman
    , requirement = \{ groups, situations } -> members [ Menqian, ChurenPoto ] situations && (groups == [ Triplet M1 M1 M1, Pair M2 M2, Run M3 M4 (M5 False), Run M6 M7 M8, Triplet M9 M9 M9 ])
    }



-- HELPERS


members : List a -> List a -> Bool
members xs ys =
    List.all (\x -> List.member x ys) xs


fourTriplets : List Group -> Bool
fourTriplets groups =
    List.Extra.count Group.isTriplet groups == 4

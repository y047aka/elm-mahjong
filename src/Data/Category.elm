module Data.Category exposing
    ( Category(..)
    , isSameCategory2, isSameCategory3
    , toString
    )

{-|

@docs Category
@docs isSameCategory2, isSameCategory3
@docs toString

-}


type Category
    = Man
    | Pin
    | Sou
    | Honor


isSameCategory2 : Category -> Category -> Bool
isSameCategory2 a b =
    a == b


isSameCategory3 : Category -> Category -> Category -> Bool
isSameCategory3 a b c =
    a == b && b == c


toString : Category -> String
toString category =
    case category of
        Man ->
            "m"

        Pin ->
            "p"

        Sou ->
            "s"

        Honor ->
            "z"

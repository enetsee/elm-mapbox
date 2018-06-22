module Json.Decode.Extra
    exposing
        ( apply
        , andMap
        , required
        , optional
        , constant
        )

{-|
@docs apply, andMap, required, optional, constant
-}

import Json.Decode as Decode exposing (Decoder)


{-| -}
optional : String -> a -> Decoder a -> Decoder (a -> b) -> Decoder b
optional fieldName default decoder =
    andMap (optionalField fieldName default decoder)


{-| -}
required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required fieldName decoder =
    andMap (Decode.field fieldName decoder)


{-| -}
optionalField : String -> a -> Decoder a -> Decoder a
optionalField fieldName default decoder =
    Decode.oneOf
        [ Decode.field fieldName decoder
        , Decode.succeed default
        ]


{-| -}
apply : Decoder (a -> b) -> Decoder a -> Decoder b
apply df dx =
    Decode.andThen
        (\f -> Decode.map f dx)
        df


{-| -}
andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap dx df =
    apply df dx


{-| -}
constant : a -> Decoder a
constant x =
    Decode.succeed x

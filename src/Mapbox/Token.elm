module Mapbox.Token exposing (Token, toString, decoder, encode)

{-|
@docs Token , toString, decoder, encode
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


{-| -}
type Token
    = Token String


{-| -}
decoder : Decoder Token
decoder =
    Decode.string |> Decode.map Token


{-| -}
toString : Token -> String
toString (Token str) =
    str


{-| -}
encode : Token -> Encode.Value
encode =
    toString >> Encode.string

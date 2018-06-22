module Json.Encode.Extra exposing (maybe, colorHex, colorRgb, colorRgba)

{-|
@docs maybe,colorHex, colorRgb, colorRgba
-}

import Json.Encode as Encode exposing (Value)
import Color exposing (Color)
import Char


{-| -}
colorHex : Color -> Encode.Value
colorHex color =
    Encode.string <| colorToHex color


{-| -}
colorRgb : Color -> Encode.Value
colorRgb color =
    Encode.string <| colorToRgbString color


{-| -}
colorRgba : Color -> Encode.Value
colorRgba color =
    Encode.string <| colorToRgbaString color


{-| -}
maybe : (a -> Value) -> Maybe a -> Value
maybe encode maybeVal =
    case maybeVal of
        Just x ->
            encode x

        _ ->
            Encode.null



-- Helpers ---------------------------------------------------------------------


{-| -}
colorToRgbString : Color -> String
colorToRgbString cl =
    let
        { red, green, blue } =
            Color.toRgb cl
    in
        "rgb(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ ")"


{-| -}
colorToRgbaString : Color -> String
colorToRgbaString cl =
    let
        { red, green, blue, alpha } =
            Color.toRgb cl
    in
        "rgba(" ++ toString red ++ "," ++ toString green ++ "," ++ toString blue ++ "," ++ toString alpha ++ ")"


{-| -}
colorToHex : Color -> String
colorToHex cl =
    let
        { red, green, blue } =
            Color.toRgb cl
    in
        List.map toHex [ red, green, blue ]
            |> (::) "#"
            |> String.join ""


{-| -}
toHex : Int -> String
toHex n =
    toRadix n
        |> String.padLeft 2 '0'


{-| -}
toRadix : Int -> String
toRadix n =
    let
        getChr c =
            if c < 10 then
                toString c
            else
                String.fromChar <| Char.fromCode (87 + c)
    in
        if n < 16 then
            getChr n
        else
            toRadix (n // 16) ++ getChr (n % 16)

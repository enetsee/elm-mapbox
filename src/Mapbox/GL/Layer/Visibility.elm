module Mapbox.GL.Layer.Visibility exposing (Visibility, visible, none, encode)

{-|
@docs Visibility
@docs visible,none
@docs encode
-}

import Json.Encode as Encode


{-| Visibility
-}
type Visibility
    = Visible
    | None


visible : Visibility
visible =
    Visible


none : Visibility
none =
    None


encode : Visibility -> Encode.Value
encode vis =
    case vis of
        Visible ->
            Encode.string "visible"

        None ->
            Encode.string "none"

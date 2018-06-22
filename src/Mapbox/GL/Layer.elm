module Mapbox.GL.Layer exposing (Layer, symbol, heatmap, encode)

{-|
@docs Layer
@docs symbol,heatmap
@docs encode
-}

import Json.Encode as Encode
import Mapbox.GL.Layer.Heatmap as Heatmap
import Mapbox.GL.Layer.Symbol as Symbol


{-| -}
type Layer
    = Symbol Symbol.Symbol
    | Heatmap Heatmap.Heatmap


{-| -}
symbol : Symbol.Symbol -> Layer
symbol =
    Symbol


{-| -}
heatmap : Heatmap.Heatmap -> Layer
heatmap =
    Heatmap


{-| -}
encode : Layer -> Encode.Value
encode layer =
    case layer of
        Symbol symbolLayer ->
            Symbol.encode symbolLayer

        Heatmap heatmapLayer ->
            Heatmap.encode heatmapLayer


{-| Line-cap
-}
type LineCap
    = CapButt
    | CapRound
    | CapSquare


type Linejoin
    = Bevel
    | Round (Maybe Float)
    | Miter (Maybe Float)

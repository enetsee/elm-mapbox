module Mapbox.GL.Layer.Symbol
    exposing
        ( Symbol
        , symbol
        , minZoom
        , maxZoom
        , filter
        , clearFilter
        , avoidEdges
        , spacing
        , placement
        , visibility
        , textField
        , textFont
        , iconImage
        , textMaxWidth
        , textLineHeight
        , textSize
        , textLetterSpacing
        , textJustify
        , textAnchor
        , textMaxAngle
        , textRotate
        , textPadding
        , textKeepUpright
        , textTransform
        , textRotationAlignment
        , textPitchAlignment
        , textOffset
        , textAllowOverlap
        , textIgnorePlacement
        , textOpacity
        , textColor
        , textHaloColor
        , textHaloWidth
        , textHaloBlur
        , textTranslate
        , textOptional
        , iconSize
        , iconAnchor
        , iconRotate
        , iconPadding
        , iconKeepUpright
        , iconRotationAlignment
        , iconPitchAlignment
        , iconOffset
        , iconAllowOverlap
        , iconIgnorePlacement
        , iconOpacity
        , iconColor
        , iconHaloColor
        , iconHaloWidth
        , iconHaloBlur
        , iconTranslate
        , iconOptional
        , iconTextFit
        , iconTextFitPadding
        , encode
        , Offset
        , Anchor(..)
        )

{-|
@docs Symbol
@docs symbol, minZoom, maxZoom, filter, clearFilter, avoidEdges, spacing, placement, visibility
@docs textField , iconImage
@docs textFont , textMaxWidth, textLineHeight, textSize, textLetterSpacing, textJustify, textAnchor, textMaxAngle, textRotate, textPadding, textKeepUpright, textTransform, textRotationAlignment, textPitchAlignment, textOffset, textAllowOverlap, textIgnorePlacement, textOpacity, textColor, textHaloColor, textHaloWidth, textHaloBlur, textTranslate, textOptional
@docs iconSize, iconAnchor, iconRotate, iconPadding, iconKeepUpright, iconRotationAlignment, iconPitchAlignment, iconOffset, iconAllowOverlap, iconIgnorePlacement, iconOpacity, iconColor, iconHaloColor, iconHaloWidth, iconHaloBlur , iconTranslate, iconOptional,iconTextFit,iconTextFitPadding
@docs  encode
@docs  Offset , Anchor
-}

import Color exposing (Color)
import Json.Encode as Encode
import Json.Encode.Extra as Encode
import Mapbox.GL.Source exposing (Source)
import Mapbox.GL.Expression.Internal as Expression exposing (Expression(E), Expr, unE)
import Mapbox.GL.Layer.Visibility as Visibility exposing (Visibility)


{-| -}
type Symbol
    = Symbol SymbolLayerInternal



-- internal representation -----------------------------------------------------


type alias SymbolLayerInternal =
    { id : String
    , source : Source
    , minZoom : Maybe Int
    , maxZoom : Maybe Int
    , filter : Maybe Expr
    , avoidEdges : Maybe Bool
    , spacing : Maybe Expr
    , placement : Maybe SymbolPlacement
    , visibility : Maybe Visibility
    , config : Maybe SymbolConfig
    }



-- public api ------------------------------------------------------------------


{-| -}
symbol : String -> Source -> Symbol
symbol id source =
    Symbol <|
        { id = id
        , source = source
        , minZoom = Nothing
        , maxZoom = Nothing
        , filter = Nothing
        , avoidEdges = Nothing
        , spacing = Nothing
        , placement = Nothing
        , visibility = Nothing
        , config = Nothing
        }



-- top-level layout ------------------------------------------------------------


{-| -}
minZoom : Int -> Symbol -> Symbol
minZoom zoom (Symbol layer) =
    Symbol <|
        { layer | minZoom = Just zoom }


{-| -}
maxZoom : Int -> Symbol -> Symbol
maxZoom zoom (Symbol layer) =
    Symbol <|
        { layer | maxZoom = Just zoom }


{-| -}
filter : Expression a -> Symbol -> Symbol
filter (E filterExpr) (Symbol layer) =
    Symbol <|
        { layer | filter = Just filterExpr }


{-| -}
clearFilter : Symbol -> Symbol
clearFilter (Symbol layer) =
    Symbol <|
        { layer | filter = Nothing }


{-| -}
avoidEdges : Bool -> Symbol -> Symbol
avoidEdges b (Symbol layer) =
    Symbol <|
        { layer | avoidEdges = Just b }


{-| -}
spacing : Expression Float -> Symbol -> Symbol
spacing (E spacing) (Symbol layer) =
    Symbol <|
        { layer | spacing = Just spacing }


{-| -}
placement : SymbolPlacement -> Symbol -> Symbol
placement x (Symbol layer) =
    Symbol <|
        { layer | placement = Just x }


{-| -}
visibility : Visibility -> Symbol -> Symbol
visibility x (Symbol layer) =
    Symbol <|
        { layer | visibility = Just x }



-- text-field  layout ----------------------------------------------------------


{-| -}
textField : String -> Symbol -> Symbol
textField field (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Nothing ->
                    SymbolText <| symbolTextField field

                Just (SymbolIcon symbolIconImage) ->
                    SymbolBoth <|
                        fromIconImage symbolIconImage field

                Just (SymbolText symbolTextField) ->
                    SymbolText <|
                        textField_ field symbolTextField

                Just (SymbolBoth symbolIconText) ->
                    SymbolBoth <|
                        textField_ field symbolIconText
    in
        config symbol <| Just newConfig


{-| -}
textField_ : a -> { c | textField : b } -> { c | textField : a }
textField_ textField symbol =
    { symbol | textField = textField }


{-| -}
textFont : List String -> Symbol -> Symbol
textFont fonts (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newLayout =
                            textFont_ fonts (cfg.layout)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            textFont_ fonts (cfg.textFieldLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| Text leading value for multi-line text. Units in ems.
-}
textLineHeight : Expression Float -> Symbol -> Symbol
textLineHeight x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newLayout =
                            textLineHeight_ x (cfg.layout)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            textLineHeight_ x (cfg.textFieldLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| The maximum line width for text wrapping. Units in ems.
-}
textMaxWidth : Expression Float -> Symbol -> Symbol
textMaxWidth x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newLayout =
                            textMaxWidth_ x (cfg.layout)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            textMaxWidth_ x (cfg.textFieldLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| Font size. Units in px.
-}
textSize : Expression Float -> Symbol -> Symbol
textSize x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newLayout =
                            textSize_ x (cfg.layout)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            textSize_ x (cfg.textFieldLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| Text tracking amount. Units in ems.
-}
textLetterSpacing : Expression Float -> Symbol -> Symbol
textLetterSpacing x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newLayout =
                            textLetterSpacing_ x (cfg.layout)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            textLetterSpacing_ x (cfg.textFieldLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
textJustify : Justify -> Symbol -> Symbol
textJustify x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newLayout =
                            textJustify_ x (cfg.layout)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            textJustify_ x (cfg.textFieldLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
textAnchor : Anchor -> Symbol -> Symbol
textAnchor x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newLayout =
                            textAnchor_ x (cfg.layout)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            textAnchor_ x (cfg.textFieldLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| Maximum angle change between adjacent characters. Units in degrees.
-}
textMaxAngle : Expression Float -> Symbol -> Symbol
textMaxAngle x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newLayout =
                            textMaxAngle_ x (cfg.layout)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            textMaxAngle_ x (cfg.textFieldLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| Rotates the text clockwise. Units in degrees.
-}
textRotate : Expression Float -> Symbol -> Symbol
textRotate x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newLayout =
                            textRotate_ x (cfg.layout)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            textRotate_ x (cfg.textFieldLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
textPadding : Float -> Symbol -> Symbol
textPadding x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newLayout =
                            textPadding_ x (cfg.layout)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            textPadding_ x (cfg.textFieldLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
textKeepUpright : Bool -> Symbol -> Symbol
textKeepUpright x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newLayout =
                            textKeepUpright_ x (cfg.layout)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            textKeepUpright_ x (cfg.textFieldLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
textTransform : TextTransform -> Symbol -> Symbol
textTransform x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newLayout =
                            textTransform_ x (cfg.layout)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            textTransform_ x (cfg.textFieldLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
textRotationAlignment : Alignment -> Symbol -> Symbol
textRotationAlignment x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newLayout =
                            textRotationAlignment_ x (cfg.layout)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            textRotationAlignment_ x (cfg.textFieldLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
textPitchAlignment : Alignment -> Symbol -> Symbol
textPitchAlignment x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newLayout =
                            textPitchAlignment_ x (cfg.layout)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            textPitchAlignment_ x (cfg.textFieldLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
textOffset : Offset -> Symbol -> Symbol
textOffset x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newLayout =
                            textOffset_ x (cfg.layout)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            textOffset_ x (cfg.textFieldLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
textAllowOverlap : Bool -> Symbol -> Symbol
textAllowOverlap x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newLayout =
                            textAllowOverlap_ x (cfg.layout)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            textAllowOverlap_ x (cfg.textFieldLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
textIgnorePlacement : Bool -> Symbol -> Symbol
textIgnorePlacement x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newLayout =
                            textIgnorePlacement_ x (cfg.layout)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            textIgnorePlacement_ x (cfg.textFieldLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
textOptional : Bool -> Symbol -> Symbol
textOptional isOptional (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolBoth cfg) ->
                    Just <| SymbolBoth { cfg | textOptional = Just isOptional }

                otherwise ->
                    otherwise
    in
        config symbol newConfig



-- icon-image  layout ----------------------------------------------------------


{-| -}
iconImage : String -> Symbol -> Symbol
iconImage image (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Nothing ->
                    SymbolIcon <| symbolIconImage image

                Just (SymbolText cfg) ->
                    SymbolBoth <|
                        fromTextField cfg image

                Just (SymbolIcon cfg) ->
                    SymbolIcon <|
                        iconImage_ image cfg

                Just (SymbolBoth cfg) ->
                    SymbolBoth <|
                        iconImage_ image cfg
    in
        config symbol <| Just newConfig


{-| -}
iconImage_ : a -> { c | iconImage : b } -> { c | iconImage : a }
iconImage_ iconImage symbol =
    { symbol | iconImage = iconImage }


{-| -}
iconOptional : Bool -> Symbol -> Symbol
iconOptional isOptional (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolBoth cfg) ->
                    Just <| SymbolBoth { cfg | iconOptional = Just isOptional }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
iconTextFit : IconTextFit -> Symbol -> Symbol
iconTextFit textFit (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolBoth cfg) ->
                    Just <| SymbolBoth { cfg | iconTextFit = Just textFit }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
iconTextFitPadding : Padding -> Symbol -> Symbol
iconTextFitPadding padding (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolBoth cfg) ->
                    Just <| SymbolBoth { cfg | iconTextFitPadding = Just padding }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
iconSize : Expression Float -> Symbol -> Symbol
iconSize x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolIcon cfg) ->
                    let
                        newLayout =
                            iconSize_ x (cfg.layout)
                    in
                        Just <|
                            SymbolIcon <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            iconSize_ x (cfg.iconImageLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | iconImageLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| Part of the icon placed closest to the anchor.
    - `Center`: The center of the icon is placed closest to the anchor.
    - `Left`: The left side of the icon is placed closest to the anchor.
    - `Right`: The right side of the icon is placed closest to the anchor.
    - `Top`: The top of the icon is placed closest to the anchor.
    - `Bottom`: The bottom of the icon is placed closest to the anchor.
    - `TopLeft`: The top left corner of the icon is placed closest to the anchor.
    - `TopRight`: The top right corner of the icon is placed closest to the anchor.
    - `BottomLeft`: The bottom left corner of the icon is placed closest to the anchor.
    - `BottomRight`: The bottom right corner of the icon is placed closest to the anchor.
-}
iconAnchor : Anchor -> Symbol -> Symbol
iconAnchor x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolIcon cfg) ->
                    let
                        newLayout =
                            iconAnchor_ x (cfg.layout)
                    in
                        Just <|
                            SymbolIcon <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            iconAnchor_ x (cfg.iconImageLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | iconImageLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| Rotates the icon clockwise. Units in degrees.
-}
iconRotate : Expression Float -> Symbol -> Symbol
iconRotate x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolIcon cfg) ->
                    let
                        newLayout =
                            iconRotate_ x (cfg.layout)
                    in
                        Just <|
                            SymbolIcon <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            iconRotate_ x (cfg.iconImageLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | iconImageLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| Size of the additional area around the icon bounding box used for
    detecting symbol collisions.
-}
iconPadding : Expression Float -> Symbol -> Symbol
iconPadding x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolIcon cfg) ->
                    let
                        newLayout =
                            iconPadding_ x (cfg.layout)
                    in
                        Just <|
                            SymbolIcon <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            iconPadding_ x (cfg.iconImageLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | iconImageLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
iconKeepUpright : Bool -> Symbol -> Symbol
iconKeepUpright x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolIcon cfg) ->
                    let
                        newLayout =
                            iconKeepUpright_ x (cfg.layout)
                    in
                        Just <|
                            SymbolIcon <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            iconKeepUpright_ x (cfg.iconImageLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | iconImageLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
iconRotationAlignment : Alignment -> Symbol -> Symbol
iconRotationAlignment x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolIcon cfg) ->
                    let
                        newLayout =
                            iconRotationAlignment_ x (cfg.layout)
                    in
                        Just <|
                            SymbolIcon <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            iconRotationAlignment_ x (cfg.iconImageLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | iconImageLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
iconPitchAlignment : Alignment -> Symbol -> Symbol
iconPitchAlignment x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolIcon cfg) ->
                    let
                        newLayout =
                            iconPitchAlignment_ x (cfg.layout)
                    in
                        Just <|
                            SymbolIcon <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            iconPitchAlignment_ x (cfg.iconImageLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | iconImageLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| Offset distance of icon from its anchor.
    Positive values indicate right and down, while negative values
    indicate left and up. Each component is multiplied by the value of
    icon-size to obtain the final offset in pixels. When combined with
    icon-rotate the offset will be as if the rotated direction was up.

    TODO: switch to expression.
-}
iconOffset : Offset -> Symbol -> Symbol
iconOffset x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolIcon cfg) ->
                    let
                        newLayout =
                            iconOffset_ x (cfg.layout)
                    in
                        Just <|
                            SymbolIcon <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            iconOffset_ x (cfg.iconImageLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | iconImageLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
iconAllowOverlap : Bool -> Symbol -> Symbol
iconAllowOverlap x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolIcon cfg) ->
                    let
                        newLayout =
                            iconAllowOverlap_ x (cfg.layout)
                    in
                        Just <|
                            SymbolIcon <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            iconAllowOverlap_ x (cfg.iconImageLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | iconImageLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
iconIgnorePlacement : Bool -> Symbol -> Symbol
iconIgnorePlacement x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolIcon cfg) ->
                    let
                        newLayout =
                            iconIgnorePlacement_ x (cfg.layout)
                    in
                        Just <|
                            SymbolIcon <|
                                { cfg | layout = newLayout }

                Just (SymbolBoth cfg) ->
                    let
                        newLayout =
                            iconIgnorePlacement_ x (cfg.iconImageLayout)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | iconImageLayout = newLayout }

                otherwise ->
                    otherwise
    in
        config symbol newConfig



-- text-field paint ------------------------------------------------------------


{-| -}
textOpacity : Float -> Symbol -> Symbol
textOpacity x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newPaint =
                            paintOpacity x (cfg.paint)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | paint = newPaint }

                Just (SymbolBoth cfg) ->
                    let
                        newPaint =
                            paintOpacity x (cfg.textFieldPaint)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldPaint = newPaint }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
textColor : Color -> Symbol -> Symbol
textColor x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newPaint =
                            paintColor x (cfg.paint)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | paint = newPaint }

                Just (SymbolBoth cfg) ->
                    let
                        newPaint =
                            paintColor x (cfg.textFieldPaint)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldPaint = newPaint }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
textHaloColor : Color -> Symbol -> Symbol
textHaloColor x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newPaint =
                            paintHaloColor x (cfg.paint)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | paint = newPaint }

                Just (SymbolBoth cfg) ->
                    let
                        newPaint =
                            paintHaloColor x (cfg.textFieldPaint)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldPaint = newPaint }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
textHaloWidth : Float -> Symbol -> Symbol
textHaloWidth x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newPaint =
                            paintHaloWidth x (cfg.paint)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | paint = newPaint }

                Just (SymbolBoth cfg) ->
                    let
                        newPaint =
                            paintHaloWidth x (cfg.textFieldPaint)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldPaint = newPaint }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
textHaloBlur : Float -> Symbol -> Symbol
textHaloBlur x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newPaint =
                            paintHaloBlur x (cfg.paint)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | paint = newPaint }

                Just (SymbolBoth cfg) ->
                    let
                        newPaint =
                            paintHaloBlur x (cfg.textFieldPaint)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldPaint = newPaint }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
textTranslate : Offset -> TranslateAnchor -> Symbol -> Symbol
textTranslate x y (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolText cfg) ->
                    let
                        newPaint =
                            paintTranslate x y (cfg.paint)
                    in
                        Just <|
                            SymbolText <|
                                { cfg | paint = newPaint }

                Just (SymbolBoth cfg) ->
                    let
                        newPaint =
                            paintTranslate x y (cfg.textFieldPaint)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | textFieldPaint = newPaint }

                otherwise ->
                    otherwise
    in
        config symbol newConfig



-- icon-image paint -----------------------------------------------------


{-| -}
iconOpacity : Float -> Symbol -> Symbol
iconOpacity x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolIcon cfg) ->
                    let
                        newPaint =
                            paintOpacity x (cfg.paint)
                    in
                        Just <|
                            SymbolIcon <|
                                { cfg | paint = newPaint }

                Just (SymbolBoth cfg) ->
                    let
                        newPaint =
                            paintOpacity x (cfg.iconImagePaint)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | iconImagePaint = newPaint }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
iconColor : Color -> Symbol -> Symbol
iconColor x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolIcon cfg) ->
                    let
                        newPaint =
                            paintColor x (cfg.paint)
                    in
                        Just <|
                            SymbolIcon <|
                                { cfg | paint = newPaint }

                Just (SymbolBoth cfg) ->
                    let
                        newPaint =
                            paintColor x (cfg.iconImagePaint)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | iconImagePaint = newPaint }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
iconHaloColor : Color -> Symbol -> Symbol
iconHaloColor x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolIcon cfg) ->
                    let
                        newPaint =
                            paintHaloColor x (cfg.paint)
                    in
                        Just <|
                            SymbolIcon <|
                                { cfg | paint = newPaint }

                Just (SymbolBoth cfg) ->
                    let
                        newPaint =
                            paintHaloColor x (cfg.iconImagePaint)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | iconImagePaint = newPaint }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
iconHaloWidth : Float -> Symbol -> Symbol
iconHaloWidth x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolIcon cfg) ->
                    let
                        newPaint =
                            paintHaloWidth x (cfg.paint)
                    in
                        Just <|
                            SymbolIcon <|
                                { cfg | paint = newPaint }

                Just (SymbolBoth cfg) ->
                    let
                        newPaint =
                            paintHaloWidth x (cfg.iconImagePaint)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | iconImagePaint = newPaint }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
iconHaloBlur : Float -> Symbol -> Symbol
iconHaloBlur x (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolIcon cfg) ->
                    let
                        newPaint =
                            paintHaloBlur x (cfg.paint)
                    in
                        Just <|
                            SymbolIcon <|
                                { cfg | paint = newPaint }

                Just (SymbolBoth cfg) ->
                    let
                        newPaint =
                            paintHaloBlur x (cfg.iconImagePaint)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | iconImagePaint = newPaint }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
iconTranslate : Offset -> TranslateAnchor -> Symbol -> Symbol
iconTranslate x y (Symbol symbol) =
    let
        newConfig =
            case symbol.config of
                Just (SymbolIcon cfg) ->
                    let
                        newPaint =
                            paintTranslate x y (cfg.paint)
                    in
                        Just <|
                            SymbolIcon <|
                                { cfg | paint = newPaint }

                Just (SymbolBoth cfg) ->
                    let
                        newPaint =
                            paintTranslate x y (cfg.iconImagePaint)
                    in
                        Just <|
                            SymbolBoth <|
                                { cfg | iconImagePaint = newPaint }

                otherwise ->
                    otherwise
    in
        config symbol newConfig


{-| -}
config : SymbolLayerInternal -> Maybe SymbolConfig -> Symbol
config symbol newConfig =
    Symbol { symbol | config = newConfig }



-- Encoder / decode (todo) -----------------------------------------------------


{-| -}
encode : Symbol -> Encode.Value
encode (Symbol layer) =
    let
        topLevelLayout =
            [ Maybe.map (\x -> ( "symbol-avoid-edges", Encode.bool x ))
                layer.avoidEdges
            , Maybe.map
                (\x -> ( "symbol-spacing", Expression.encode x ))
                layer.spacing
            , Maybe.map
                (\x -> ( "symbol-placement", encodeSymbolPlacement x ))
                layer.placement
            , Maybe.map
                (\x -> ( "visibility", Visibility.encode x ))
                layer.visibility
            ]
                |> List.filterMap identity

        ( layoutProps, paintProps ) =
            Maybe.map encodeSymbolConfig layer.config
                |> Maybe.withDefault ( [], [] )

        layout =
            case topLevelLayout ++ layoutProps of
                [] ->
                    Nothing

                xs ->
                    Just ( "layout", Encode.object xs )

        paint =
            case paintProps of
                [] ->
                    Nothing

                xs ->
                    Just ( "paint", Encode.object xs )
    in
        Encode.object <|
            List.filterMap identity
                [ Just ( "id", Encode.string layer.id )
                , Just ( "type", Encode.string "symbol" )
                , Just ( "source", Mapbox.GL.Source.encode layer.source )
                , Maybe.map (\x -> ( "maxzoom", Encode.int x )) layer.maxZoom
                , Maybe.map (\x -> ( "minzoom", Encode.int x )) layer.minZoom
                , Maybe.map (\x -> ( "filter", Expression.encode x )) layer.filter
                , layout
                , paint
                ]



-- Symbol layout and paint config ----------------------------------------------


type SymbolConfig
    = SymbolIcon SymbolIconImage
    | SymbolText SymbolTextField
    | SymbolBoth SymbolIconText


encodeSymbolConfig : SymbolConfig -> ( List ( String, Encode.Value ), List ( String, Encode.Value ) )
encodeSymbolConfig cfg =
    case cfg of
        SymbolIcon symbol ->
            encodeSymbolIconImage symbol

        SymbolText symbol ->
            encodeSymbolTextField symbol

        SymbolBoth symbol ->
            encodeSymbolBoth symbol



-- A symbol with `text-field` defined ------------------------------------------


type alias SymbolTextField =
    { textField : String
    , layout : TextFieldLayout
    , paint : Paint
    }


symbolTextField : String -> SymbolTextField
symbolTextField textField =
    SymbolTextField textField textFieldLayoutEmpty paintEmpty


encodeSymbolTextField : SymbolTextField -> ( List ( String, Encode.Value ), List ( String, Encode.Value ) )
encodeSymbolTextField symbol =
    let
        layout =
            encodeTextFieldLayout symbol.layout

        paint =
            encodePaint "text" symbol.paint
    in
        ( ( "text-field", Encode.string symbol.textField ) :: layout, paint )



-- A symbol with `icon-image` defined ------------------------------------------


type alias SymbolIconImage =
    { iconImage : String
    , layout : IconImageLayout
    , paint : Paint
    }


symbolIconImage : String -> SymbolIconImage
symbolIconImage iconImage =
    SymbolIconImage iconImage iconImageLayoutEmpty paintEmpty


encodeSymbolIconImage : SymbolIconImage -> ( List ( String, Encode.Value ), List ( String, Encode.Value ) )
encodeSymbolIconImage symbol =
    let
        layout =
            encodeIconImageLayout symbol.layout

        paint =
            encodePaint "icon" symbol.paint
    in
        ( ( "icon-image", Encode.string symbol.iconImage ) :: layout, paint )



-- A symbol with both a `text-field` and `icon-image` defined ------------------


type alias SymbolIconText =
    { textField : String
    , textOptional : Maybe Bool
    , textIgnorePlacement : Maybe Bool
    , textFieldLayout : TextFieldLayout
    , textFieldPaint : Paint
    , iconImage : String
    , iconOptional : Maybe Bool
    , iconTextFit : Maybe IconTextFit
    , iconTextFitPadding : Maybe Padding
    , iconImageLayout : IconImageLayout
    , iconImagePaint : Paint
    }


fromIconImage : SymbolIconImage -> String -> SymbolIconText
fromIconImage { iconImage, layout, paint } textField =
    { textField = textField
    , textOptional = Nothing
    , textIgnorePlacement = Nothing
    , textFieldLayout = textFieldLayoutEmpty
    , textFieldPaint = paintEmpty
    , iconImage = iconImage
    , iconOptional = Nothing
    , iconTextFit = Nothing
    , iconTextFitPadding = Nothing
    , iconImageLayout = layout
    , iconImagePaint = paint
    }


fromTextField : SymbolTextField -> String -> SymbolIconText
fromTextField { textField, layout, paint } iconImage =
    { textField = textField
    , textOptional = Nothing
    , textIgnorePlacement = Nothing
    , textFieldLayout = layout
    , textFieldPaint = paint
    , iconImage = iconImage
    , iconOptional = Nothing
    , iconTextFit = Nothing
    , iconTextFitPadding = Nothing
    , iconImageLayout = iconImageLayoutEmpty
    , iconImagePaint = paintEmpty
    }


encodeSymbolBoth : SymbolIconText -> ( List ( String, Encode.Value ), List ( String, Encode.Value ) )
encodeSymbolBoth symbol =
    let
        topLevelLayout =
            List.filterMap identity
                [ Just ( "text-field", Encode.string symbol.textField )
                , Maybe.map (\x -> ( "text-optional", Encode.bool x )) symbol.textOptional
                , Maybe.map (\x -> ( "text-ignore-placement", Encode.bool x )) symbol.textIgnorePlacement
                , Just ( "icon-image", Encode.string symbol.iconImage )
                , Maybe.map (\x -> ( "icon-optional", Encode.bool x )) symbol.iconOptional
                , Maybe.map (\x -> ( "icon-text-fit", encodeIconTextFit x )) symbol.iconTextFit
                , Maybe.map (\x -> ( "icon-text-fit-padding", encodePadding x )) symbol.iconTextFitPadding
                ]

        textLayout =
            encodeTextFieldLayout symbol.textFieldLayout

        iconLayout =
            encodeIconImageLayout symbol.iconImageLayout

        textPaint =
            encodePaint "text" symbol.textFieldPaint

        iconPaint =
            encodePaint "icon" symbol.iconImagePaint
    in
        ( List.concat [ topLevelLayout, textLayout, iconLayout ]
        , List.concat [ textPaint, iconPaint ]
        )



-- Icon image layout -----------------------------------------------------------


type alias IconImageLayout =
    { iconSize : Maybe Expr
    , iconAnchor : Maybe Anchor
    , iconRotate : Maybe Expr
    , iconPadding : Maybe Expr
    , iconKeepUpright : Maybe Bool
    , iconRotationAlignment : Maybe Alignment
    , iconPitchAlignment : Maybe Alignment
    , iconOffset : Maybe Offset
    , iconAllowOverlap : Maybe Bool
    , iconIgnorePlacement : Maybe Bool
    }


iconImageLayoutEmpty : IconImageLayout
iconImageLayoutEmpty =
    { iconSize = Nothing
    , iconAnchor = Nothing
    , iconRotate = Nothing
    , iconPadding = Nothing
    , iconKeepUpright = Nothing
    , iconRotationAlignment = Nothing
    , iconPitchAlignment = Nothing
    , iconOffset = Nothing
    , iconAllowOverlap = Nothing
    , iconIgnorePlacement = Nothing
    }


encodeIconImageLayout : IconImageLayout -> List ( String, Encode.Value )
encodeIconImageLayout layout =
    List.filterMap identity
        [ Maybe.map (\x -> ( "icon-size", Expression.encode x )) layout.iconSize
        , Maybe.map (\x -> ( "icon-anchor", encodeAnchor x )) layout.iconAnchor
        , Maybe.map (\x -> ( "icon-rotate", Expression.encode x )) layout.iconRotate
        , Maybe.map (\x -> ( "icon-padding", Expression.encode x )) layout.iconPadding
        , Maybe.map (\x -> ( "icon-keep-upright", Encode.bool x )) layout.iconKeepUpright
        , Maybe.map (\x -> ( "icon-rotation-alignment", encodeAlignment x )) layout.iconRotationAlignment
        , Maybe.map (\x -> ( "icon-pitch-alignment", encodeAlignment x )) layout.iconPitchAlignment
        , Maybe.map (\x -> ( "icon-offset", encodeOffset x )) layout.iconOffset
        , Maybe.map (\x -> ( "icon-allow-overlap", Encode.bool x )) layout.iconAllowOverlap
        , Maybe.map (\x -> ( "icon-ignore-placement", Encode.bool x )) layout.iconIgnorePlacement
        ]


iconSize_ : Expression Float -> IconImageLayout -> IconImageLayout
iconSize_ (E x) layout =
    { layout | iconSize = Just x }


iconAnchor_ : Anchor -> IconImageLayout -> IconImageLayout
iconAnchor_ x layout =
    { layout | iconAnchor = Just x }


iconRotate_ : Expression Float -> IconImageLayout -> IconImageLayout
iconRotate_ (E x) layout =
    { layout | iconRotate = Just x }


iconPadding_ : Expression Float -> IconImageLayout -> IconImageLayout
iconPadding_ (E x) layout =
    { layout | iconPadding = Just x }


iconKeepUpright_ : Bool -> IconImageLayout -> IconImageLayout
iconKeepUpright_ x layout =
    { layout | iconKeepUpright = Just x }


iconRotationAlignment_ : Alignment -> IconImageLayout -> IconImageLayout
iconRotationAlignment_ x layout =
    { layout | iconRotationAlignment = Just x }


iconPitchAlignment_ : Alignment -> IconImageLayout -> IconImageLayout
iconPitchAlignment_ x layout =
    { layout | iconPitchAlignment = Just x }


iconOffset_ : Offset -> IconImageLayout -> IconImageLayout
iconOffset_ x layout =
    { layout | iconOffset = Just x }


iconAllowOverlap_ : Bool -> IconImageLayout -> IconImageLayout
iconAllowOverlap_ x layout =
    { layout | iconAllowOverlap = Just x }


iconIgnorePlacement_ : Bool -> IconImageLayout -> IconImageLayout
iconIgnorePlacement_ x layout =
    { layout | iconIgnorePlacement = Just x }



-- Text field layout -----------------------------------------------------------


type alias TextFieldLayout =
    { textFont : Maybe (List String)
    , textLineHeight : Maybe Expr
    , textMaxWidth : Maybe Expr
    , textSize : Maybe Expr
    , textLetterSpacing : Maybe Expr
    , textJustify : Maybe Justify
    , textAnchor : Maybe Anchor
    , textMaxAngle : Maybe Expr
    , textRotate : Maybe Expr
    , textPadding : Maybe Float
    , textKeepUpright : Maybe Bool
    , textTransform : Maybe TextTransform
    , textRotationAlignment : Maybe Alignment
    , textPitchAlignment : Maybe Alignment
    , textOffset : Maybe Offset
    , textAllowOverlap : Maybe Bool
    , textIgnorePlacement : Maybe Bool
    }


textFieldLayoutEmpty : TextFieldLayout
textFieldLayoutEmpty =
    { textFont = Nothing
    , textLineHeight = Nothing
    , textMaxWidth = Nothing
    , textSize = Nothing
    , textLetterSpacing = Nothing
    , textJustify = Nothing
    , textAnchor = Nothing
    , textMaxAngle = Nothing
    , textRotate = Nothing
    , textPadding = Nothing
    , textKeepUpright = Nothing
    , textTransform = Nothing
    , textRotationAlignment = Nothing
    , textPitchAlignment = Nothing
    , textOffset = Nothing
    , textAllowOverlap = Nothing
    , textIgnorePlacement = Nothing
    }


encodeTextFieldLayout : TextFieldLayout -> List ( String, Encode.Value )
encodeTextFieldLayout layout =
    List.filterMap identity
        [ Maybe.map (\x -> ( "text-font", Encode.list <| List.map Encode.string x )) layout.textFont
        , Maybe.map (\x -> ( "text-line-height", Expression.encode x )) layout.textLineHeight
        , Maybe.map (\x -> ( "text-max-width", Expression.encode x )) layout.textMaxWidth
        , Maybe.map (\x -> ( "text-size", Expression.encode x )) layout.textSize
        , Maybe.map (\x -> ( "text-letter-spacing", Expression.encode x )) layout.textLetterSpacing
        , Maybe.map (\x -> ( "text-justify", encodeJustify x )) layout.textJustify
        , Maybe.map (\x -> ( "text-anchor", encodeAnchor x )) layout.textAnchor
        , Maybe.map (\x -> ( "text-max-angle", Expression.encode x )) layout.textMaxAngle
        , Maybe.map (\x -> ( "text-rotate", Expression.encode x )) layout.textRotate
        , Maybe.map (\x -> ( "text-padding", Encode.float x )) layout.textPadding
        , Maybe.map (\x -> ( "text-keep-upright", Encode.bool x )) layout.textKeepUpright
        , Maybe.map (\x -> ( "text-transform", encodeTextTransform x )) layout.textTransform
        , Maybe.map (\x -> ( "text-rotation-alignment", encodeAlignment x )) layout.textRotationAlignment
        , Maybe.map (\x -> ( "text-pitch-alignment", encodeAlignment x )) layout.textPitchAlignment
        , Maybe.map (\x -> ( "text-offset", encodeOffset x )) layout.textOffset
        , Maybe.map (\x -> ( "text-allow-overlap", Encode.bool x )) layout.textAllowOverlap
        , Maybe.map (\x -> ( "text-ignore-placement", Encode.bool x )) layout.textIgnorePlacement
        ]


textFont_ : List String -> TextFieldLayout -> TextFieldLayout
textFont_ fonts layout =
    { layout | textFont = Just fonts }


textLineHeight_ : Expression Float -> TextFieldLayout -> TextFieldLayout
textLineHeight_ (E x) layout =
    { layout | textLineHeight = Just x }


textMaxWidth_ : Expression Float -> TextFieldLayout -> TextFieldLayout
textMaxWidth_ (E x) layout =
    { layout | textMaxWidth = Just x }


textSize_ : Expression Float -> TextFieldLayout -> TextFieldLayout
textSize_ (E x) layout =
    { layout | textSize = Just x }


textLetterSpacing_ : Expression Float -> TextFieldLayout -> TextFieldLayout
textLetterSpacing_ (E x) layout =
    { layout | textLetterSpacing = Just x }


textJustify_ : Justify -> TextFieldLayout -> TextFieldLayout
textJustify_ x layout =
    { layout | textJustify = Just x }


textAnchor_ : Anchor -> TextFieldLayout -> TextFieldLayout
textAnchor_ x layout =
    { layout | textAnchor = Just x }


textMaxAngle_ : Expression Float -> TextFieldLayout -> TextFieldLayout
textMaxAngle_ (E x) layout =
    { layout | textMaxAngle = Just x }


textRotate_ : Expression Float -> TextFieldLayout -> TextFieldLayout
textRotate_ (E x) layout =
    { layout | textRotate = Just x }


textPadding_ : Float -> TextFieldLayout -> TextFieldLayout
textPadding_ x layout =
    { layout | textPadding = Just x }


textKeepUpright_ : Bool -> TextFieldLayout -> TextFieldLayout
textKeepUpright_ x layout =
    { layout | textKeepUpright = Just x }


textTransform_ : TextTransform -> TextFieldLayout -> TextFieldLayout
textTransform_ x layout =
    { layout | textTransform = Just x }


textRotationAlignment_ : Alignment -> TextFieldLayout -> TextFieldLayout
textRotationAlignment_ x layout =
    { layout | textRotationAlignment = Just x }


textPitchAlignment_ : Alignment -> TextFieldLayout -> TextFieldLayout
textPitchAlignment_ x layout =
    { layout | textPitchAlignment = Just x }


textOffset_ : Offset -> TextFieldLayout -> TextFieldLayout
textOffset_ x layout =
    { layout | textOffset = Just x }


textAllowOverlap_ : Bool -> TextFieldLayout -> TextFieldLayout
textAllowOverlap_ x layout =
    { layout | textAllowOverlap = Just x }


textIgnorePlacement_ : Bool -> TextFieldLayout -> TextFieldLayout
textIgnorePlacement_ x layout =
    { layout | textIgnorePlacement = Just x }



-- Paint -----------------------------------------------------------------------


type alias Paint =
    { opacity : Maybe Float
    , color : Maybe Color
    , haloColor : Maybe Color
    , haloWidth : Maybe Float
    , haloBlur : Maybe Float
    , translate : Maybe ( Offset, TranslateAnchor )
    }


paintEmpty : Paint
paintEmpty =
    { opacity = Nothing
    , color = Nothing
    , haloColor = Nothing
    , haloWidth = Nothing
    , haloBlur = Nothing
    , translate = Nothing
    }


encodePaint : String -> Paint -> List ( String, Encode.Value )
encodePaint prefix paint =
    List.filterMap identity
        [ Maybe.map (\x -> ( prefix ++ "-opacity", Encode.float x )) paint.opacity
        , Maybe.map (\x -> ( prefix ++ "-color", Encode.colorHex x )) paint.color
        , Maybe.map (\x -> ( prefix ++ "-halo-color", Encode.colorHex x )) paint.haloColor
        , Maybe.map (\x -> ( prefix ++ "-halo-width", Encode.float x )) paint.haloWidth
        , Maybe.map (\x -> ( prefix ++ "-halo-blur", Encode.float x )) paint.haloBlur
        , Maybe.map (\( x, _ ) -> ( prefix ++ "-translate", encodeOffset x )) paint.translate
        , Maybe.map (\( _, x ) -> ( prefix ++ "-translate-anchor", encodeTranslateAnchor x )) paint.translate
        ]


paintOpacity : Float -> Paint -> Paint
paintOpacity opacity paint =
    { paint | opacity = Just opacity }


paintColor : Color -> Paint -> Paint
paintColor color paint =
    { paint | color = Just color }


paintHaloColor : Color -> Paint -> Paint
paintHaloColor color paint =
    { paint | haloColor = Just color }


paintHaloWidth : Float -> Paint -> Paint
paintHaloWidth width paint =
    { paint | haloWidth = Just width }


paintHaloBlur : Float -> Paint -> Paint
paintHaloBlur blur paint =
    { paint | haloBlur = Just blur }


paintTranslate : Offset -> TranslateAnchor -> Paint -> Paint
paintTranslate offset anchor paint =
    { paint | translate = Just ( offset, anchor ) }



-- Common ----------------------------------------------------------------------


{-| Symbol placement
-}
type SymbolPlacement
    = Point
    | Line


encodeSymbolPlacement : SymbolPlacement -> Encode.Value
encodeSymbolPlacement p =
    case p of
        Point ->
            Encode.string "point"

        Line ->
            Encode.string "line"


{-| Icon text fit
-}
type IconTextFit
    = IcNone
    | IcWidth
    | IcHeight
    | IcBoth


encodeIconTextFit : IconTextFit -> Encode.Value
encodeIconTextFit ic =
    case ic of
        IcNone ->
            Encode.string "none"

        IcWidth ->
            Encode.string "width"

        IcHeight ->
            Encode.string "height"

        IcBoth ->
            Encode.string "both"


{-| text transform
-}
type TextTransform
    = Uppercase
    | Lowercase
    | TransformNone


encodeTextTransform : TextTransform -> Encode.Value
encodeTextTransform tr =
    case tr of
        Uppercase ->
            Encode.string "uppercase"

        Lowercase ->
            Encode.string "lowercase"

        TransformNone ->
            Encode.string "none"


{-| Translate anchor
-}
type TranslateAnchor
    = TranslateMap
    | TranslateViewport


encodeTranslateAnchor : TranslateAnchor -> Encode.Value
encodeTranslateAnchor ta =
    case ta of
        TranslateMap ->
            Encode.string "map"

        TranslateViewport ->
            Encode.string "viewport"


{-| Aligment
-}
type Alignment
    = AlignMap
    | AlignViewport
    | AlignAuto


encodeAlignment : Alignment -> Encode.Value
encodeAlignment align =
    case align of
        AlignMap ->
            Encode.string "map"

        AlignViewport ->
            Encode.string "viewport"

        AlignAuto ->
            Encode.string "auto"


{-| Anchor
-}
type Anchor
    = AnchorCenter
    | AnchorLeft
    | AnchorRight
    | AnchorTop
    | AnchorBottom
    | AnchorTopLeft
    | AnchorTopRight
    | AnchorBottomLeft
    | AnchorBottomRight


encodeAnchor : Anchor -> Encode.Value
encodeAnchor anchor =
    case anchor of
        AnchorCenter ->
            Encode.string "center"

        AnchorLeft ->
            Encode.string "left"

        AnchorRight ->
            Encode.string "right"

        AnchorTop ->
            Encode.string "top"

        AnchorBottom ->
            Encode.string "bottom"

        AnchorTopLeft ->
            Encode.string "top-left"

        AnchorTopRight ->
            Encode.string "top-right"

        AnchorBottomLeft ->
            Encode.string "bottom-left"

        AnchorBottomRight ->
            Encode.string "bottom-right"


{-| Justify
-}
type Justify
    = JustifyLeft
    | JustifyRight
    | JustifyCenter


encodeJustify : Justify -> Encode.Value
encodeJustify justify =
    case justify of
        JustifyLeft ->
            Encode.string "left"

        JustifyRight ->
            Encode.string "right"

        JustifyCenter ->
            Encode.string "center"


{-| Offset
-}
type alias Offset =
    { dx : Float, dy : Float }


encodeOffset : Offset -> Encode.Value
encodeOffset { dx, dy } =
    Encode.list [ Encode.float dx, Encode.float dy ]


{-| Padding
-}
type alias Padding =
    { top : Float, right : Float, bottom : Float, left : Float }


encodePadding :
    { a | bottom : Float, left : Float, right : Float, top : Float }
    -> Encode.Value
encodePadding { top, right, bottom, left } =
    Encode.list
        [ Encode.float top
        , Encode.float right
        , Encode.float bottom
        , Encode.float left
        ]

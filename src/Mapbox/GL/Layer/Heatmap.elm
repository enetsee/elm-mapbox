module Mapbox.GL.Layer.Heatmap
    exposing
        ( Heatmap
        , heatmap
        , encode
        , minZoom
        , maxZoom
        , filter
        , visibility
        , heatmapRadius
        , heatmapWeight
        , heatmapIntensity
        , heatmapColor
        , heatmapOpacity
        )

{-|
@docs Heatmap, heatmap
@docs encode
@docs minZoom, maxZoom, filter
@docs visibility
@docs heatmapRadius, heatmapWeight, heatmapIntensity, heatmapColor, heatmapOpacity
-}

import Color exposing (Color)
import Mapbox.GL.Source exposing (Source)
import Mapbox.GL.Expression.Internal as Expression exposing (Expression(E), Expr, unE)
import Mapbox.GL.Layer.Visibility as Visibility exposing (Visibility)
import Json.Encode as Encode


{-| -}
type Heatmap
    = Heatmap HeatmapInternal


type alias HeatmapInternal =
    { id : String
    , source : Source
    , minZoom : Maybe Int
    , maxZoom : Maybe Int
    , filter : Maybe Expr
    , visibility : Maybe Visibility
    , heatmapRadius : Maybe Expr
    , heatmapWeight : Maybe Expr
    , heatmapIntensity : Maybe Expr
    , heatmapColor : Maybe Expr
    , heatmapOpacity : Maybe Expr
    }


{-| -}
encode : Heatmap -> Encode.Value
encode (Heatmap layer) =
    let
        layout =
            Maybe.map
                (\a ->
                    ( "layout"
                    , Encode.object [ ( "visibility", Visibility.encode a ) ]
                    )
                )
                layer.visibility

        paint =
            encodePaint layer
    in
        [ Just ( "id", Encode.string layer.id )
        , Just ( "type", Encode.string "heatmap" )
        , Just ( "source", Mapbox.GL.Source.encode layer.source )
        , Maybe.map (\x -> ( "minzoom", Encode.int x )) layer.minZoom
        , Maybe.map (\x -> ( "maxzoom", Encode.int x )) layer.maxZoom
        , Maybe.map (\x -> ( "filter", Expression.encode x )) layer.filter
        , layout
        , paint
        ]
            |> List.filterMap identity
            |> Encode.object


encodePaint :
    { a
        | heatmapColor : Maybe Expr
        , heatmapIntensity : Maybe Expr
        , heatmapOpacity : Maybe Expr
        , heatmapRadius : Maybe Expr
        , heatmapWeight : Maybe Expr
    }
    -> Maybe ( String, Encode.Value )
encodePaint { heatmapRadius, heatmapWeight, heatmapIntensity, heatmapColor, heatmapOpacity } =
    let
        props =
            [ Maybe.map (\x -> ( "heatmap-radius", Expression.encode x )) heatmapRadius
            , Maybe.map (\x -> ( "heatmap-weight", Expression.encode x )) heatmapWeight
            , Maybe.map (\x -> ( "heatmap-intensity", Expression.encode x )) heatmapIntensity
            , Maybe.map (\x -> ( "heatmap-color", Expression.encode x )) heatmapColor
            , Maybe.map (\x -> ( "heatmap-opacity", Expression.encode x )) heatmapOpacity
            ]
                |> List.filterMap identity
    in
        case props of
            [] ->
                Nothing

            _ ->
                Just ( "paint", Encode.object props )



-- API -------------------------------------------------------------------------


{-| -}
heatmap : String -> Source -> Heatmap
heatmap id source =
    Heatmap
        { id = id
        , source = source
        , minZoom = Nothing
        , maxZoom = Nothing
        , filter = Nothing
        , visibility = Nothing
        , heatmapRadius = Nothing
        , heatmapWeight = Nothing
        , heatmapIntensity = Nothing
        , heatmapColor = Nothing
        , heatmapOpacity = Nothing
        }


{-| -}
minZoom : Int -> Heatmap -> Heatmap
minZoom zoom (Heatmap layer) =
    Heatmap <|
        { layer | minZoom = Just zoom }


{-| -}
maxZoom : Int -> Heatmap -> Heatmap
maxZoom zoom (Heatmap layer) =
    Heatmap <|
        { layer | maxZoom = Just zoom }


{-| -}
filter : Expression a -> Heatmap -> Heatmap
filter (E filterExpr) (Heatmap layer) =
    Heatmap <|
        { layer | filter = Just filterExpr }


{-| -}
clearFilter : Heatmap -> Heatmap
clearFilter (Heatmap layer) =
    Heatmap <|
        { layer | filter = Nothing }



-- Layout ----------------------------------------------------------------------


{-| -}
visibility : Visibility -> Heatmap -> Heatmap
visibility x (Heatmap layer) =
    Heatmap <|
        { layer | visibility = Just x }



-- Paint -----------------------------------------------------------------------


{-| -}
heatmapRadius : Expression Float -> Heatmap -> Heatmap
heatmapRadius (E x) (Heatmap layer) =
    Heatmap { layer | heatmapRadius = Just x }


{-| -}
heatmapWeight : Expression Float -> Heatmap -> Heatmap
heatmapWeight (E x) (Heatmap layer) =
    Heatmap { layer | heatmapWeight = Just x }


{-| -}
heatmapIntensity : Expression Float -> Heatmap -> Heatmap
heatmapIntensity (E x) (Heatmap layer) =
    Heatmap { layer | heatmapIntensity = Just x }


{-| -}
heatmapColor : Expression Color -> Heatmap -> Heatmap
heatmapColor (E x) (Heatmap layer) =
    Heatmap { layer | heatmapColor = Just x }


{-| -}
heatmapOpacity : Expression Float -> Heatmap -> Heatmap
heatmapOpacity (E x) (Heatmap layer) =
    Heatmap { layer | heatmapOpacity = Just x }

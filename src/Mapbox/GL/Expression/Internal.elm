module Mapbox.GL.Expression.Internal
    exposing
        ( Expression(..)
        , unE
        , Interpolation(..)
        , Expr(..)
        , Lit(..)
        , BinOp(..)
        , UnOp(..)
        , encode
        )

{-|
@docs Expression, unE
@docs Expr
@docs Lit
@docs BinOp, UnOp, Interpolation
-}

import Color exposing (Color)
import Json.Encode as Encode


-- `Typed` Expressions ---------------------------------------------------------


{-| -}
type Expression a
    = E Expr


{-| -}
unE : Expression a -> Expr
unE (E expr) =
    expr



-- Untyped Expressions ---------------------------------------------------------


{-| -}
encode : Expr -> Encode.Value
encode expr =
    case expr of
        ConstantE ->
            Encode.list [ Encode.string "e" ]

        ConstantPi ->
            Encode.list [ Encode.string "pi" ]

        Literal lit ->
            encodeLit lit

        LetIn vars body ->
            (Encode.string "let"
                :: List.concatMap (\( name, expr ) -> [ Encode.string name, encode expr ]) vars
            )
                ++ [ encode body ]
                |> Encode.list

        Var name ->
            Encode.list [ Encode.string "var", Encode.string name ]

        GeometryType ->
            Encode.list [ Encode.string "geometry-type" ]

        Id ->
            Encode.list [ Encode.string "id" ]

        Properties ->
            Encode.list [ Encode.string "properties" ]

        At idx expr1 ->
            Encode.list
                [ Encode.string "at", Encode.int idx, encode expr1 ]

        Get prop maybeExpr ->
            [ Just <| Encode.string "get"
            , Just <| Encode.string prop
            , Maybe.map encode maybeExpr
            ]
                |> List.filterMap identity
                |> Encode.list

        Has prop maybeExpr ->
            [ Just <| Encode.string "has"
            , Just <| Encode.string prop
            , Maybe.map encode maybeExpr
            ]
                |> List.filterMap identity
                |> Encode.list

        Length expr ->
            Encode.list [ Encode.string "length", encode expr ]

        BinOp binOp expr1 expr2 ->
            Encode.list [ encodeBinOp binOp, encode expr1, encode expr2 ]

        UnOp unOp expr1 ->
            Encode.list [ encodeUnOp unOp, encode expr1 ]

        All exprs ->
            Encode.string "all"
                :: List.map encode exprs
                |> Encode.list

        Any exprs ->
            Encode.string "any"
                :: List.map encode exprs
                |> Encode.list

        Case tests default ->
            (Encode.string "case"
                :: List.concatMap (\( test, expr ) -> [ encode test, encode expr ]) tests
                ++ [ encode default ]
            )
                |> Encode.list

        Max exprs ->
            Encode.string "max"
                :: List.map encode exprs
                |> Encode.list

        Min exprs ->
            Encode.string "max"
                :: List.map encode exprs
                |> Encode.list

        Concat exprs ->
            Encode.string "concat"
                :: List.map encode exprs
                |> Encode.list

        Downcase expr1 ->
            Encode.list [ Encode.string "downcase", encode expr1 ]

        Upcase expr1 ->
            Encode.list [ Encode.string "upcase", encode expr1 ]

        Rgb exprR exprG exprB ->
            Encode.list
                [ Encode.string "rgb"
                , encode exprR
                , encode exprG
                , encode exprB
                ]

        Rgba exprR exprG exprB exprA ->
            Encode.list
                [ Encode.string "rgba"
                , encode exprR
                , encode exprG
                , encode exprB
                , encode exprA
                ]

        ToRgba exprC ->
            Encode.list
                [ Encode.string "to-rgba"
                , encode exprC
                ]

        Interpolate interpolation input stops ->
            (Encode.string "interpolate"
                :: encodeInterpolation interpolation
                :: encode input
                :: List.concatMap (\( input, output ) -> [ encode input, encode output ]) stops
            )
                |> Encode.list

        Step input output0 stops ->
            (Encode.string "step"
                :: encode input
                :: encode output0
                :: List.concatMap (\( input, output ) -> [ encode input, encode output ]) stops
            )
                |> Encode.list

        Zoom ->
            Encode.list [ Encode.string "zoom" ]

        HeatmapDensity ->
            Encode.list [ Encode.string "heatmap-density" ]


{-| -}
type Expr
    = ConstantE
    | ConstantPi
    | Literal Lit
    | LetIn (List ( String, Expr )) Expr
    | Var String
    | GeometryType
    | Id
    | Properties
    | At Int Expr
    | Get String (Maybe Expr)
    | Has String (Maybe Expr)
    | Length Expr
    | BinOp BinOp Expr Expr
    | UnOp UnOp Expr
    | All (List Expr)
    | Any (List Expr)
    | Case (List ( Expr, Expr )) Expr
    | Max (List Expr)
    | Min (List Expr)
    | Concat (List Expr)
    | Downcase Expr
    | Upcase Expr
    | Rgb Expr Expr Expr
    | Rgba Expr Expr Expr Expr
    | ToRgba Expr
    | Interpolate Interpolation Expr (List ( Expr, Expr ))
    | Step Expr Expr (List ( Expr, Expr ))
    | Zoom
    | HeatmapDensity


{-| -}
type Interpolation
    = Linear
    | Exponential Float
    | CubicBezier Float Float Float Float


encodeInterpolation : Interpolation -> Encode.Value
encodeInterpolation interp =
    case interp of
        Linear ->
            Encode.list [ Encode.string "linear" ]

        Exponential base ->
            Encode.list [ Encode.string "exponential", Encode.float base ]

        CubicBezier x1 y1 x2 y2 ->
            Encode.list
                [ Encode.string "cubic-bezier"
                , Encode.float x1
                , Encode.float y1
                , Encode.float x2
                , Encode.float y2
                ]


{-| -}
type Lit
    = LitString String
    | LitColor Color
    | LitNumber Float
    | LitBool Bool
    | LitArray (List Lit)
    | LitObject (List ( String, Lit ))


encodeLit : Lit -> Encode.Value
encodeLit lit =
    case lit of
        LitString str ->
            Encode.string str

        LitColor color ->
            let
                rgba =
                    Color.toRgb color
            in
                "rgba("
                    ++ toString rgba.red
                    ++ ","
                    ++ toString rgba.green
                    ++ ","
                    ++ toString rgba.blue
                    ++ ","
                    ++ toString rgba.alpha
                    ++ ")"
                    |> Encode.string

        LitNumber num ->
            Encode.float num

        LitBool b ->
            Encode.bool b

        LitArray ls ->
            Encode.list <| List.map encodeLit ls

        LitObject fields ->
            Encode.object <| List.map (\( n, l ) -> ( n, encodeLit l )) fields


{-| -}
type BinOp
    = Eq
    | NEq
    | GT
    | LT
    | LTE
    | GTE
    | Add
    | Sub
    | Mult
    | Div
    | Mod
    | Pow


encodeBinOp : BinOp -> Encode.Value
encodeBinOp binop =
    case binop of
        Eq ->
            Encode.string "=="

        NEq ->
            Encode.string "!="

        GT ->
            Encode.string ">"

        LT ->
            Encode.string "<"

        LTE ->
            Encode.string "<="

        GTE ->
            Encode.string ">="

        Add ->
            Encode.string "+"

        Sub ->
            Encode.string "-"

        Mult ->
            Encode.string "*"

        Div ->
            Encode.string "/"

        Mod ->
            Encode.string "%"

        Pow ->
            Encode.string "^"


{-| -}
type UnOp
    = Not
    | Neg
    | Abs
    | Acos
    | Asin
    | Atan
    | Ceil
    | Cos
    | Floor
    | Ln
    | Ln2
    | Log10
    | Round
    | Sin
    | Sqrt
    | Tan


encodeUnOp : UnOp -> Encode.Value
encodeUnOp unop =
    case unop of
        Not ->
            Encode.string "!"

        Neg ->
            Encode.string "-"

        Abs ->
            Encode.string "abs"

        Acos ->
            Encode.string "acos"

        Asin ->
            Encode.string "asin"

        Atan ->
            Encode.string "atan"

        Ceil ->
            Encode.string "ceil"

        Cos ->
            Encode.string "cos"

        Floor ->
            Encode.string "floor"

        Ln ->
            Encode.string "ln"

        Ln2 ->
            Encode.string "ln2"

        Log10 ->
            Encode.string "log10"

        Round ->
            Encode.string "round"

        Sin ->
            Encode.string "sin"

        Sqrt ->
            Encode.string "sqrt"

        Tan ->
            Encode.string "tan"

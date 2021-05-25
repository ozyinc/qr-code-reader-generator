module QRFormat exposing (..)


type QRFormat
    = PNG
    | GIF
    | JPG
    | SVG


formatToPathString : QRFormat -> String
formatToPathString f =
    case f of
        PNG ->
            "png"

        GIF ->
            "gif"

        JPG ->
            "jpg"

        SVG ->
            "svg"


formatToText : QRFormat -> String
formatToText f =
    case f of
        PNG ->
            "PNG (*.png)"

        GIF ->
            "GIF (*.gif)"

        JPG ->
            "JPG (*.jpg, *.jpeg)"

        SVG ->
            "svg (*.svg) (vector)"


pathToFormatString : String -> QRFormat
pathToFormatString f =
    case f of
        "png" ->
            PNG

        "gif" ->
            GIF

        "jpg" ->
            JPG

        "svg" ->
            SVG

        _ ->
            PNG


readList =
    [ PNG, GIF, JPG ]


formatToMime : QRFormat -> String
formatToMime f =
    case f of
        PNG ->
            "image/png"

        JPG ->
            "image/jpeg"

        GIF ->
            "image/gif"

        _ ->
            "err"

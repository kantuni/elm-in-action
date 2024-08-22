module PhotoGrooveTests exposing (..)

import Expect
import Fuzz
import Json.Decode
import Json.Encode
import PhotoGroove exposing (Msg(..), initialModel, update)
import Test exposing (Test)


photoDecoderTest : Test
photoDecoderTest =
    Test.fuzz2
        Fuzz.string
        Fuzz.int
        "title defaults to (untitled)"
        (\url size ->
            [ ( "url", Json.Encode.string url )
            , ( "size", Json.Encode.int size )
            ]
                |> Json.Encode.object
                |> Json.Decode.decodeValue PhotoGroove.photoDecoder
                |> Result.map (\photo -> photo.title)
                |> Expect.equal (Ok "(untitled)")
        )


slidHueTest : Test
slidHueTest =
    Test.fuzz Fuzz.int
        "SlidHue sets the hue"
        (\amount ->
            initialModel
                |> update (SlidHue amount)
                |> Tuple.first
                |> (\model -> model.hue)
                |> Expect.equal amount
        )

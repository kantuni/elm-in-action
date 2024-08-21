module PhotoGrooveTests exposing (..)

import Expect
import Fuzz
import Json.Decode
import Json.Encode
import PhotoGroove
import Test exposing (Test)


decoderTest : Test
decoderTest =
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

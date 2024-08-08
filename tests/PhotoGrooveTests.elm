module PhotoGrooveTests exposing (..)

import Expect
import Json.Decode exposing (decodeString)
import PhotoGroove
import Test exposing (Test, test)


decoderTest : Test
decoderTest =
    test "title defaults to (untitled)"
        (\_ ->
            """
            { "url": "fruits.com", "size": 5 }
            """
                |> decodeString PhotoGroove.photoDecoder
                |> Result.map (\photo -> photo.title)
                |> Expect.equal (Ok "(untitled)")
        )

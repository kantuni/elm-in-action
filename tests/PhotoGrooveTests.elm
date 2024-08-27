module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Html
import Html.Attributes exposing (src)
import Json.Decode
import Json.Encode
import PhotoGroove exposing (Model, Msg(..), Photo, Status(..), initialModel, update, urlPrefix, view)
import Test exposing (Test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag)


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


slidersTest : Test
slidersTest =
    Test.describe "Slider sets the desired field in the Model"
        [ sliderTest "SlidHue" SlidHue .hue
        , sliderTest "SlidRipple" SlidRipple .ripple
        , sliderTest "SlidNoise" SlidNoise .noise
        ]


sliderTest : String -> (Int -> Msg) -> (Model -> Int) -> Test
sliderTest description toMsg getAmountFromModel =
    Test.fuzz Fuzz.int
        description
        (\amount ->
            initialModel
                |> PhotoGroove.update (toMsg amount)
                |> Tuple.first
                |> getAmountFromModel
                |> Expect.equal amount
        )


noPhotosNoThumbnailsTest : Test
noPhotosNoThumbnailsTest =
    Test.test "No thumbnails render when there are no photos to render."
        (\_ ->
            initialModel
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)
        )


thumbnailsTest : Test
thumbnailsTest =
    Test.fuzz urlFuzzer
        "URLs render as thumbnails"
        (\urls ->
            let
                thumbnailChecks : List (Query.Single msg -> Expectation)
                thumbnailChecks =
                    List.map thumbnailRendered urls
            in
            { initialModel | status = Loaded (List.map photoFromUrl urls) "" }
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Expect.all thumbnailChecks
        )



-- HELPERS


photoFromUrl : String -> Photo
photoFromUrl url =
    { url = url
    , size = 0
    , title = ""
    }


urlsFromCount : Int -> List String
urlsFromCount urlCount =
    List.range 1 urlCount
        |> List.map (\num -> String.fromInt num ++ ".png")


urlFuzzer : Fuzzer (List String)
urlFuzzer =
    Fuzz.intRange 1 5
        |> Fuzz.map urlsFromCount


thumbnailRendered : String -> Query.Single msg -> Expectation
thumbnailRendered url query =
    query
        |> Query.findAll
            [ tag "img"
            , attribute (src (urlPrefix ++ url))
            ]
        |> Query.count (Expect.atLeast 1)

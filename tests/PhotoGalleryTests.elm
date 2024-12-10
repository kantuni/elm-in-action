module PhotoGalleryTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Html
import Html.Attributes exposing (src)
import Json.Decode
import Json.Encode
import PhotoGallery exposing (Model, Msg(..), Photo, Status(..), initialModel, update, view)
import Test exposing (Test)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag)
import Utils exposing (urlPrefix)


photoDecoderTest : Test
photoDecoderTest =
    Test.fuzz2
        Fuzz.string
        Fuzz.int
        "Title defaults to (untitled)"
        (\url size ->
            [ ( "url", Json.Encode.string url )
            , ( "size", Json.Encode.int size )
            ]
                |> Json.Encode.object
                |> Json.Decode.decodeValue PhotoGallery.photoDecoder
                |> Result.map (\photo -> photo.title)
                |> Expect.equal (Ok "(untitled)")
        )


slidersTest : Test
slidersTest =
    Test.describe
        "Slider sets the desired field in the Model"
        [ sliderTest "SlidHue" SlidHue .hue
        , sliderTest "SlidRipple" SlidRipple .ripple
        , sliderTest "SlidNoise" SlidNoise .noise
        ]


sliderTest : String -> (Int -> Msg) -> (Model -> Int) -> Test
sliderTest description toMsg getAmountFromModel =
    Test.fuzz
        Fuzz.int
        description
        (\amount ->
            initialModel
                |> PhotoGallery.update (toMsg amount)
                |> Tuple.first
                |> getAmountFromModel
                |> Expect.equal amount
        )


noPhotosNoThumbnailsTest : Test
noPhotosNoThumbnailsTest =
    Test.test
        "No thumbnails render when there are no photos to render."
        (\_ ->
            initialModel
                |> PhotoGallery.view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)
        )


thumbnailsTest : Test
thumbnailsTest =
    Test.fuzz
        urlFuzzer
        "URLs render as thumbnails"
        (\urls ->
            let
                thumbnailChecks : List (Query.Single msg -> Expectation)
                thumbnailChecks =
                    List.map thumbnailRendered urls
            in
            { initialModel | status = Loaded (List.map photoFromUrl urls) "" }
                |> PhotoGallery.view
                |> Query.fromHtml
                |> Expect.all thumbnailChecks
        )


clickThumbnailTest : Test
clickThumbnailTest =
    Test.fuzz3
        urlFuzzer
        Fuzz.string
        urlFuzzer
        "Clicking a thumbnail selects it"
        (\urlsBefore urlToSelect urlsAfter ->
            let
                url : String
                url =
                    urlToSelect ++ ".jpeg"

                srcToSelect : String
                srcToSelect =
                    urlPrefix ++ url

                photos : List Photo
                photos =
                    (urlsBefore ++ url :: urlsAfter)
                        |> List.map photoFromUrl
            in
            { initialModel | status = Loaded photos "" }
                |> PhotoGallery.view
                |> Query.fromHtml
                |> Query.find [ tag "img", attribute (src srcToSelect) ]
                |> Event.simulate Event.click
                |> Event.expect (ClickedPhoto url)
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

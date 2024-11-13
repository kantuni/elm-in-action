port module PhotoGallery exposing (Model, Msg(..), Photo, Status(..), init, initialModel, photoDecoder, subscriptions, update, urlPrefix, view)

-- TODO
-- 1) Introduce a new update function that returns an `Effect`
--    instead of `Cmd Msg`.
-- 2) Add a `toCmd` function that maps the `Effect` to `Cmd Msg`
-- 3) Add a mapper function that maps the new update function to
--    the old one.
-- 4) Add tests for all effects.

import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (alt, checked, class, classList, id, name, size, src, title, type_, value)
import Html.Events exposing (on, onCheck, onClick)
import Http
import Json.Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode
import Random



-- INIT


urlPrefix : String
urlPrefix =
    "https://elm-in-action.com/"


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


type Status
    = Loading
    | Errored String
    | Loaded (List Photo) String


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Model =
    { status : Status
    , activity : String
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }


initialModel : Model
initialModel =
    { status = Loading
    , activity = ""
    , chosenSize = Medium
    , hue = 5
    , ripple = 5
    , noise = 5
    }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = urlPrefix ++ "photos/list.json"
        , expect = Http.expectJson GotPhotos (Json.Decode.list photoDecoder)
        }


init : Float -> ( Model, Cmd Msg )
init flags =
    let
        activity =
            "Initializing Pasta v" ++ String.fromFloat flags
    in
    ( { initialModel | activity = activity }, initialCmd )



-- PORTS


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }


port setFilters : FilterOptions -> Cmd msg


port activityChanges : (String -> msg) -> Sub msg



-- UPDATE


type Msg
    = ClickedPhoto String
    | SlidHue Int
    | SlidRipple Int
    | SlidNoise Int
    | ChoseSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotActivity String
    | GotPhotos (Result Http.Error (List Photo))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            applyFilters { model | status = selectUrl url model.status }

        SlidHue hue ->
            applyFilters { model | hue = hue }

        SlidRipple ripple ->
            applyFilters { model | ripple = ripple }

        SlidNoise noise ->
            applyFilters { model | noise = noise }

        ChoseSize size ->
            ( { model | chosenSize = size }
            , Cmd.none
            )

        ClickedSurpriseMe ->
            case model.status of
                Loading ->
                    ( model, Cmd.none )

                Errored _ ->
                    ( model, Cmd.none )

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loaded (firstPhoto :: otherPhotos) _ ->
                    let
                        randomPhoto : Random.Generator Photo
                        randomPhoto =
                            Random.uniform firstPhoto otherPhotos
                    in
                    ( model, Random.generate GotRandomPhoto randomPhoto )

        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        GotActivity activity ->
            ( { model | activity = activity }, Cmd.none )

        GotPhotos (Ok photos) ->
            case photos of
                first :: _ ->
                    applyFilters { model | status = Loaded photos first.url }

                [] ->
                    ( { model | status = Errored "0 photos found" }
                    , Cmd.none
                    )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server error!" }
            , Cmd.none
            )



-- UPDATE HELPERS


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loading ->
            ( model, Cmd.none )

        Errored _ ->
            ( model, Cmd.none )

        Loaded _ selectedUrl ->
            let
                filters : List { name : String, amount : Float }
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]

                url : String
                url =
                    urlPrefix ++ "large/" ++ selectedUrl

                cmd : Cmd msg
                cmd =
                    setFilters { url = url, filters = filters }
            in
            ( model, cmd )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loading ->
            status

        Errored _ ->
            status

        Loaded photos _ ->
            Loaded photos url



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "content" ]
        (case model.status of
            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]

            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model
        )


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (Json.Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]


viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model =
    [ button [ onClick ClickedSurpriseMe ] [ text "Surprise Me!" ]
    , div [ class "activity" ] [ text model.activity ]
    , div [ class "filters" ]
        [ viewFilter SlidHue "Hue" model.hue
        , viewFilter SlidRipple "Ripple" model.ripple
        , viewFilter SlidNoise "Noise" model.noise
        ]
    , h3 [] [ text "Thumbnail size:" ]
    , div [ id "choose-size" ]
        (List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
    , canvas [ id "main-canvas", class "large" ] []
    ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , alt "Thumbnail"
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser chosenSize size =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , value (sizeToString size)
            , checked (size == chosenSize)
            , onCheck (\_ -> ChoseSize size)
            ]
            []
        , text (sizeToString size)
        ]



-- VIEW HELPERS


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    let
        detailUserSlidTo : Json.Decode.Decoder Int
        detailUserSlidTo =
            Json.Decode.at [ "detail", "userSlidTo" ] Json.Decode.int

        msgDecoder : Json.Decode.Decoder msg
        msgDecoder =
            Json.Decode.map toMsg detailUserSlidTo
    in
    on "slide" msgDecoder



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    activityChanges GotActivity



-- DECODERS


photoDecoder : Json.Decode.Decoder Photo
photoDecoder =
    -- If decoding succeeds, pass these values to the `Photo` constructor.
    Json.Decode.succeed Photo
        |> required "url" Json.Decode.string
        |> required "size" Json.Decode.int
        -- The fallback value will be used if the "title" field is either missing or null.
        |> optional "title" Json.Decode.string "(untitled)"

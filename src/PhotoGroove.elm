module PhotoGroove exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (alt, checked, class, classList, id, name, size, src, type_, value)
import Html.Events exposing (onCheck, onClick)
import Http
import Platform.Cmd as Cmd
import Random


urlPrefix : String
urlPrefix =
    "https://elm-in-action.com/"


type Msg
    = ClickedPhoto String
    | ChoseSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error String)


view : Model -> Html Msg
view model =
    div [ class "content" ]
        (case model.status of
            Loading ->
                []

            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.chosenSize

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]
        )


viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
    [ h1 [] [ text "Photo Groove" ]
    , button [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , h3 [] [ text "Thumbnail size:" ]
    , div [ id "choose-size" ]
        (List.map (viewSizeChooser chosenSize) [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
    , img
        [ src (urlPrefix ++ "large/" ++ selectedUrl)
        , alt "Selected thumbnail"
        , class "large"
        ]
        []
    ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , alt "Thumbnail"
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


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Photo =
    { url : String
    }


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    }


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = urlPrefix ++ "photos/list"
        , expect = Http.expectString GotPhotos
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }
            , Cmd.none
            )

        ChoseSize size ->
            ( { model | chosenSize = size }
            , Cmd.none
            )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored _ ->
                    ( model, Cmd.none )

        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }
            , Cmd.none
            )

        GotPhotos result ->
            case result of
                Ok responseStr ->
                    case String.split "," responseStr of
                        (firstUrl :: _) as urls ->
                            let
                                photos =
                                    List.map Photo urls
                            in
                            ( { model | status = Loaded photos firstUrl }
                            , Cmd.none
                            )

                        [] ->
                            ( { model | status = Errored "0 photos found" }
                            , Cmd.none
                            )

                Err _ ->
                    ( { model | status = Errored "Server error" }
                    , Cmd.none
                    )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        _ ->
            status


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

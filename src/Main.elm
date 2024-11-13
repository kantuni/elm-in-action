module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)
import Html.Lazy exposing (lazy)
import PhotoFolders as Folders
import PhotoGallery as Gallery
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)



-- MAIN


type alias Flags =
    Float


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }



-- INIT


type alias Model =
    { key : Nav.Key
    , page : Page
    , pastaVersion : Float
    }


type Page
    = FoldersPage Folders.Model
    | GalleryPage Gallery.Model
    | NotFound


type Route
    = Folders
    | Gallery
    | SelectedPhoto String


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init pastaVersion url key =
    updateUrl url
        { key = key
        , page = NotFound
        , pastaVersion = pastaVersion
        }



-- INIT HELPERS


updateUrl : Url -> Model -> ( Model, Cmd Msg )
updateUrl url model =
    case Parser.parse parser url of
        Just Folders ->
            Folders.init Nothing
                |> toFolders model

        Just Gallery ->
            Gallery.init model.pastaVersion
                |> toGallery model

        Just (SelectedPhoto filename) ->
            Folders.init (Just filename)
                |> toFolders model

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Folders Parser.top
        , Parser.map Gallery (s "gallery")
        , Parser.map SelectedPhoto (s "photos" </> Parser.string)
        ]



-- UPDATE


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotFoldersMsg Folders.Msg
    | GotGalleryMsg Gallery.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ChangedUrl url ->
            updateUrl url model

        GotFoldersMsg foldersMsg ->
            case model.page of
                FoldersPage foldersModel ->
                    toFolders model (Folders.update foldersMsg foldersModel)

                _ ->
                    ( model, Cmd.none )

        GotGalleryMsg galleryMsg ->
            case model.page of
                GalleryPage galleryModel ->
                    toGallery model (Gallery.update galleryMsg galleryModel)

                _ ->
                    ( model, Cmd.none )



-- UPDATE HELPERS


toFolders : Model -> ( Folders.Model, Cmd Folders.Msg ) -> ( Model, Cmd Msg )
toFolders model ( foldersModel, foldersCmd ) =
    ( { model | page = FoldersPage foldersModel }
    , Cmd.map GotFoldersMsg foldersCmd
    )


toGallery : Model -> ( Gallery.Model, Cmd Gallery.Msg ) -> ( Model, Cmd Msg )
toGallery model ( galleryModel, galleryCmd ) =
    ( { model | page = GalleryPage galleryModel }
    , Cmd.map GotGalleryMsg galleryCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        GalleryPage galleryModel ->
            Gallery.subscriptions galleryModel
                |> Sub.map GotGalleryMsg

        _ ->
            Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    let
        content =
            case model.page of
                FoldersPage foldersModel ->
                    Folders.view foldersModel
                        |> Html.map GotFoldersMsg

                GalleryPage galleryModel ->
                    Gallery.view galleryModel
                        |> Html.map GotGalleryMsg

                NotFound ->
                    text "Not Found"
    in
    { title = "Photo Groove, SPA Style"
    , body =
        [ lazy viewHeader model.page
        , content
        , viewFooter
        ]
    }


viewHeader : Page -> Html Msg
viewHeader page =
    let
        logo =
            h1 [] [ text "Photo Groove" ]

        links =
            ul []
                [ navLink Folders { url = "/", caption = "Folders" }
                , navLink Gallery { url = "/gallery", caption = "Gallery" }
                ]

        navLink : Route -> { url : String, caption : String } -> Html msg
        navLink route { url, caption } =
            li
                [ classList
                    [ ( "active", isActive { link = route, page = page } )
                    ]
                ]
                [ a [ href url ] [ text caption ] ]
    in
    nav [] [ logo, links ]


viewFooter : Html msg
viewFooter =
    footer []
        [ text "One is never alone with a rubber duck. – Douglas Adams" ]



-- VIEW HELPERS


isActive : { link : Route, page : Page } -> Bool
isActive { link, page } =
    case ( link, page ) of
        ( Gallery, GalleryPage _ ) ->
            True

        ( Gallery, _ ) ->
            False

        ( Folders, FoldersPage _ ) ->
            True

        ( Folders, _ ) ->
            False

        ( SelectedPhoto _, _ ) ->
            False

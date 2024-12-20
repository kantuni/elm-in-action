module PhotoFolders exposing (Model, Msg, init, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (alt, class, href, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Utils exposing (urlPrefix)



-- INIT


type Folder
    = Folder
        { name : String
        , expanded : Bool
        , photoUrls : List String
        , subfolders : List Folder
        }


type alias Photo =
    { title : String
    , size : Int
    , relatedUrls : List String
    , url : String
    }


type alias Model =
    { selectedPhotoUrl : Maybe String
    , photos : Dict String Photo
    , root : Folder
    }


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing
    , photos = Dict.empty
    , root =
        Folder
            { name = "Loading..."
            , expanded = True
            , photoUrls = []
            , subfolders = []
            }
    }


init : Maybe String -> ( Model, Cmd Msg )
init selectedFilename =
    ( { initialModel | selectedPhotoUrl = selectedFilename }
    , Http.get
        { url = urlPrefix ++ "folders/list"
        , expect = Http.expectJson GotInitialModel modelDecoder2
        }
    )



-- UPDATE


type FolderPath
    = End
    | Subfolder Int FolderPath


type Msg
    = GotInitialModel (Result Http.Error Model)
    | ClickedPhoto String
    | ClickedRelatedPhoto String
    | ClickedFolder FolderPath


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInitialModel (Ok newModel) ->
            -- Preserve the currently selected photo on refresh
            ( { newModel | selectedPhotoUrl = model.selectedPhotoUrl }, Cmd.none )

        GotInitialModel (Err _) ->
            ( model, Cmd.none )

        ClickedPhoto url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )

        ClickedRelatedPhoto url ->
            ( { model
                | selectedPhotoUrl = Just url
                , root = expandFoldersToPhoto url model.root
              }
            , Cmd.none
            )

        ClickedFolder path ->
            ( { model | root = toggleExpanded path model.root }, Cmd.none )



-- UPDATE HELPERS


toggleExpanded : FolderPath -> Folder -> Folder
toggleExpanded path (Folder folder) =
    case path of
        End ->
            Folder { folder | expanded = not folder.expanded }

        Subfolder targetIndex remainingPath ->
            let
                subfolders : List Folder
                subfolders =
                    List.indexedMap transform folder.subfolders

                transform : Int -> Folder -> Folder
                transform currentIndex currentSubfolder =
                    if currentIndex == targetIndex then
                        toggleExpanded remainingPath currentSubfolder

                    else
                        currentSubfolder
            in
            Folder { folder | subfolders = subfolders }



-- EXTRA CHALLENGE
-- Clicking a related photo should expand the folder path to that photo.


expandFoldersToPhoto : String -> Folder -> Folder
expandFoldersToPhoto url (Folder folder) =
    if List.member url folder.photoUrls then
        Folder { folder | expanded = True }

    else
        let
            updatedSubfolders =
                folder.subfolders
                    |> List.map (\subfolder -> expandFoldersToPhoto url subfolder)

            shouldExpandCurrentFolder =
                updatedSubfolders
                    |> List.any (\(Folder subfolder) -> subfolder.expanded)
        in
        Folder
            { folder
                | expanded =
                    -- `folder.expanded` preserves the previous expansion state.
                    -- Without it, all adjacent folders would be collapsed.
                    folder.expanded || shouldExpandCurrentFolder
                , subfolders = updatedSubfolders
            }



-- VIEW


view : Model -> Html Msg
view model =
    let
        photoByUrl : String -> Maybe Photo
        photoByUrl url =
            Dict.get url model.photos

        selectedPhoto : Html Msg
        selectedPhoto =
            case Maybe.andThen photoByUrl model.selectedPhotoUrl of
                Just photo ->
                    viewSelectedPhoto photo

                Nothing ->
                    text ""
    in
    div [ class "content" ]
        [ div [ class "folders" ]
            [ viewFolder End model.root
            ]
        , selectedPhoto
        ]


viewPhoto : String -> Html Msg
viewPhoto url =
    a [ href ("/photos/" ++ url), class "photo" ]
        [ text url ]


viewSelectedPhoto : Photo -> Html Msg
viewSelectedPhoto photo =
    div [ class "selected-photo" ]
        [ h2 [] [ text photo.title ]
        , img
            [ src (urlPrefix ++ "photos/" ++ photo.url ++ "/full")
            , alt photo.title
            ]
            []
        , span [] [ text (String.fromInt photo.size ++ "KB") ]
        , h3 [] [ text "Related" ]
        , div [ class "related-photos" ]
            (List.map viewRelatedPhoto photo.relatedUrls)
        ]


viewRelatedPhoto : String -> Html Msg
viewRelatedPhoto url =
    img
        [ class "related-photo"
        , onClick (ClickedRelatedPhoto url)
        , src (urlPrefix ++ "photos/" ++ url ++ "/thumb")
        , alt url
        ]
        []


viewFolder : FolderPath -> Folder -> Html Msg
viewFolder path (Folder folder) =
    let
        viewSubfolder : Int -> Folder -> Html Msg
        viewSubfolder index subfolder =
            viewFolder (appendIndex index path) subfolder

        folderLabel =
            label [ onClick (ClickedFolder path) ] [ text folder.name ]
    in
    if folder.expanded then
        let
            contents =
                -- Show folders then files
                List.append
                    (List.indexedMap viewSubfolder folder.subfolders)
                    (List.map viewPhoto folder.photoUrls)
        in
        div [ class "folder expanded" ]
            [ folderLabel
            , div [ class "contents" ] contents
            ]

    else
        div [ class "folder collapsed" ] [ folderLabel ]



-- VIEW HELPERS


appendIndex : Int -> FolderPath -> FolderPath
appendIndex index path =
    case path of
        End ->
            Subfolder index End

        Subfolder subfolderIndex remainingPath ->
            Subfolder subfolderIndex (appendIndex index remainingPath)



-- DECODERS


type alias JsonPhoto =
    -- An intermediary representation
    { title : String
    , size : Int
    , relatedUrls : List String
    }


photosDecoder : Decoder (Dict String Photo)
photosDecoder =
    Decode.keyValuePairs jsonPhotoDecoder
        |> Decode.map fromPairs


jsonPhotoDecoder : Decoder JsonPhoto
jsonPhotoDecoder =
    Decode.succeed JsonPhoto
        |> required "title" Decode.string
        |> required "size" Decode.int
        |> required "related_photos" (Decode.list Decode.string)


fromPairs : List ( String, JsonPhoto ) -> Dict String Photo
fromPairs pairs =
    pairs
        |> List.map finishPhoto
        |> Dict.fromList


finishPhoto : ( String, JsonPhoto ) -> ( String, Photo )
finishPhoto ( url, json ) =
    ( url
    , { url = url
      , title = json.title
      , size = json.size
      , relatedUrls = json.relatedUrls
      }
    )


folderDecoder : Decoder Folder
folderDecoder =
    Decode.succeed folderFromJson
        |> required "name" Decode.string
        |> required "photos" photosDecoder
        -- Decode.lazy is used here to prevent a cyclic definition
        |> required "subfolders" (Decode.lazy (\_ -> Decode.list folderDecoder))


folderFromJson : String -> Dict String Photo -> List Folder -> Folder
folderFromJson name photos subfolders =
    Folder
        { name = name
        , expanded = True
        , photoUrls = Dict.keys photos
        , subfolders = subfolders
        }


modelPhotosDecoder : Decoder (Dict String Photo)
modelPhotosDecoder =
    Decode.succeed modelPhotosFromJson
        |> required "photos" photosDecoder
        -- Decode.lazy is used here to prevent a cyclic definition
        |> required "subfolders" (Decode.lazy (\_ -> Decode.list modelPhotosDecoder))


modelPhotosFromJson : Dict String Photo -> List (Dict String Photo) -> Dict String Photo
modelPhotosFromJson folderPhotos subfolderPhotos =
    List.foldl Dict.union folderPhotos subfolderPhotos


modelDecoder : Decoder Model
modelDecoder =
    Decode.map2
        (\photos root ->
            { photos = photos
            , root = root
            , selectedPhotoUrl = Nothing
            }
        )
        modelPhotosDecoder
        folderDecoder



-- EXTRA CHALLENGE
-- Combine `modelPhotoDecoder` and `folderDecoder` into a single
-- Decoder ( Folder, Dict String Photo ) that decodes both the folders
-- and the photos in one pass.


modelDecoder2 : Decoder Model
modelDecoder2 =
    folderAndPhotosDecoder
        |> Decode.map
            (\( root, photos ) ->
                { photos = photos
                , root = root
                , selectedPhotoUrl = Nothing
                }
            )


folderAndPhotosDecoder : Decoder ( Folder, Dict String Photo )
folderAndPhotosDecoder =
    Decode.succeed folderAndPhotosFromJson
        |> required "name" Decode.string
        |> required "photos" photosDecoder
        -- Decode.lazy is used here to prevent a cyclic definition
        |> required "subfolders" (Decode.lazy (\_ -> Decode.list folderAndPhotosDecoder))


folderAndPhotosFromJson : String -> Dict String Photo -> List ( Folder, Dict String Photo ) -> ( Folder, Dict String Photo )
folderAndPhotosFromJson name folderPhotos subfolderAndPhotosList =
    let
        subfolders : List Folder
        subfolders =
            List.map Tuple.first subfolderAndPhotosList

        subfolderPhotos : List (Dict String Photo)
        subfolderPhotos =
            List.map Tuple.second subfolderAndPhotosList

        allPhotos : Dict String Photo
        allPhotos =
            List.foldl Dict.union folderPhotos subfolderPhotos
    in
    ( Folder
        { name = name
        , expanded = True
        , photoUrls = Dict.keys folderPhotos
        , subfolders = subfolders
        }
    , allPhotos
    )

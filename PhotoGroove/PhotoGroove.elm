module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Random
import Http

urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


type ThumbnailSize
    = Small
    | Medium
    | Large

type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo
    , selectedUrl : Maybe String
    , loadingError : Maybe String
    , chosenSize: ThumbnailSize
    }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


initialModel : Model
initialModel =
    { photos =
        []
    , selectedUrl = Nothing
    , loadingError = Nothing
    , chosenSize = Medium
    }


initialCmd : Cmd Msg
initialCmd =
    "//photos/list"
        |> Http.getString
        |> Http.send LoadPhotos

type Msg
    = SelectByUrl String
    | SelectByIndex Int
    | SurpriseMe
    | SetSize ThumbnailSize
    | LoadPhotos (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectByUrl url ->
            ({ model | selectedUrl = Just url }, Cmd.none)
        SelectByIndex index ->
            let
                newSelectedPhoto : Maybe Photo
                newSelectedPhoto =
                    Array.get index (Array.fromList model.photos)

                newSelectedUrl : Maybe String
                newSelectedUrl =
                    model.photos
                        |> Array.fromList
                        |> Array.get index
                        |> Maybe.map .url
             in
                ({ model | selectedUrl = newSelectedUrl }, Cmd.none)
        SurpriseMe  ->
            let
                randomPhotoPicker =
                    Random.int 0 (List.length model.photos - 1)
            in
                ( model, Random.generate SelectByIndex randomPhotoPicker)
        SetSize size ->
            ({ model | chosenSize = size }, Cmd.none)
        LoadPhotos (Ok responseStr) ->
                    let
                        urls =
                            String.split "," responseStr
                        photos =
                            List.map Photo urls
                    in
                        ( { model | photos = photos}, Cmd.none )

        LoadPhotos (Err _) ->
                    ( model, Cmd.none )


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (SetSize size) ] []
        , text (sizeToString size)
        ]


getPhotoUrl : Int -> Maybe String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            Just photo.url
        Nothing ->
            Nothing


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"

viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)
        , classList [ ( "selected", selectedUrl == Just thumbnail.url ) ]
        , onClick ( SelectByUrl thumbnail.url )
        ]
        []


viewLarge : Maybe String -> Html Msg
viewLarge maybeUrl =
    case maybeUrl of
        Nothing ->
            text ""

        Just url ->
            img [ class "large", src (urlPrefix ++ "large/" ++ url)] []
           

view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick SurpriseMe ]
            [ text "Surprise Me!" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            (List.map viewSizeChooser [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , viewLarge model.selectedUrl
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = ( \_ -> Sub.none )
        }

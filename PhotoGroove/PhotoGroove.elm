port module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (id, class, classList, src, max, name, type_, title)
import Json.Decode exposing (string, int, list, Decoder, at)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Html.Events exposing (onClick, on)
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

port setFilters : FilterOptions -> Cmd msg

type alias FilterOptions =
    { url : String
    , filters: List { name : String, amount: Float }
    }


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


type alias Model =
    { photos : List Photo
    , selectedUrl : Maybe String
    , loadingError : Maybe String
    , chosenSize: ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }


photoDecoder : Decoder Photo
photoDecoder =
    decode buildPhoto
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"


buildPhoto : String -> Int -> String -> Photo
buildPhoto url size title =
    { url = url, size = size, title = title }

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
    , hue = 0
    , ripple = 0
    , noise = 0
    }


initialCmd : Cmd Msg
initialCmd =
    list photoDecoder
        |> Http.get "/books__elm-in-action/photos/list.json"
        |> Http.send LoadPhotos

type Msg
    = SelectByUrl String
    | SetHue Int
    | SetRipple Int
    | SetNoise Int
    | SelectByIndex Int
    | SurpriseMe
    | SetSize ThumbnailSize
    | LoadPhotos (Result Http.Error (List Photo))


applyFilters: Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.selectedUrl of
        Just selectedUrl ->
            let
                filters = [ { name = "Hue", amount = toFloat model.hue / 11 }
                          , { name = "Ripple", amount = toFloat model.ripple / 11 }
                          , { name = "Noise", amount = toFloat model.noise / 11 }
                          ]
                url = urlPrefix ++ "large/" ++ selectedUrl
            in
                ( model, setFilters { url = url, filters = filters } )

        Nothing ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectByUrl url ->
            applyFilters { model | selectedUrl = Just url }
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
                applyFilters { model | selectedUrl = newSelectedUrl }
        SurpriseMe  ->
            let
                randomPhotoPicker =
                    Random.int 0 (List.length model.photos - 1)
            in
                ( model, Random.generate SelectByIndex randomPhotoPicker)
        SetSize size ->
            ({ model | chosenSize = size }, Cmd.none)
        LoadPhotos (Ok photos) ->
                    applyFilters
                        { model
                            | photos = photos
                            , selectedUrl = Maybe.map .url (List.head photos)
                        }
        LoadPhotos (Err _) ->
                    ( { model
                          | loadingError = Just "Error! (Try turning it off and on again?)" }
                          , Cmd.none )
        SetHue hue ->
            applyFilters { model | hue = hue }
        SetRipple ripple ->
            applyFilters { model | ripple = ripple }
        SetNoise noise ->
            applyFilters { model | noise = noise }



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


onImmediateValueChange : (Int -> msg) -> Attribute msg
onImmediateValueChange toMsg =
    at [ "target", "immediateValue" ] int
        |> Json.Decode.map toMsg
        |> on "immediate-value-changed" 


viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
    img
        [ src (urlPrefix ++ thumbnail.url)
        , title (thumbnail.title ++ " [" ++ toString thumbnail.size ++ "KB]")
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
            canvas [ id "main-canvas", class "large" ] []


paperSlider : List (Attribute msg) -> List (Html msg) -> Html msg
paperSlider =
    node "paper-slider"


viewFilter : String -> (Int -> Msg) -> Int -> Html Msg
viewFilter name toMsg magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , paperSlider [ Attr.max "11", onImmediateValueChange toMsg ] []
        , label [] [ text (toString magnitude) ]
        ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick SurpriseMe ]
            [ text "Surprise Me!" ]
        , div [ class "filters" ]
            [ viewFilter "Hue" SetHue model.hue
            , viewFilter "Ripple" SetRipple model.ripple
            , viewFilter "Noise" SetNoise model.noise
            ]
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

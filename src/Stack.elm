module Stack exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled)
import Html.Events exposing (onClick)



-- MODEL


type Stack a
    = Nil
    | Cons a (Stack a)


type alias Model =
    { head : Maybe Int
    , index : Int
    , stack : Stack Int
    , tail : Maybe (Stack Int)
    }


initialModel : Model
initialModel =
    { head = Nothing
    , index = 0
    , stack = Nil
    , tail = Nothing
    }



-- UPDATE


type Msg
    = ClickedCons Int
    | ClickedHead
    | ClickedReset
    | ClickedTail


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCons int ->
            ( { model | index = model.index + 1, stack = cons int model.stack }, Cmd.none )

        ClickedHead ->
            ( { model | head = head model.stack, index = 0, stack = Nil, tail = Nothing }, Cmd.none )

        ClickedReset ->
            ( initialModel, Cmd.none )

        ClickedTail ->
            ( { model | head = Nothing, index = 0, stack = Nil, tail = tail model.stack }, Cmd.none )


empty : Stack a
empty =
    Nil


isEmpty : Stack a -> Bool
isEmpty stack =
    case stack of
        Nil ->
            True

        Cons _ _ ->
            False


cons : a -> Stack a -> Stack a
cons a stack =
    case stack of
        Cons hd tl ->
            Cons a (Cons hd tl)

        Nil ->
            Cons a Nil


head : Stack a -> Maybe a
head stack =
    case stack of
        Cons a _ ->
            Just a

        Nil ->
            Nothing


tail : Stack a -> Maybe (Stack a)
tail stack =
    case stack of
        Cons _ tl ->
            Just tl

        Nil ->
            Nothing



-- VIEW


view model =
    let
        emptyStack =
            isEmpty model.stack
    in
    div []
        [ viewStackText emptyStack
        , div [ class "stack" ] <| viewStack model.stack
        , div [] [ text "Head: ", viewHead model.head ]
        , button [ onClick <| ClickedCons model.index ] [ text "Cons" ]
        , button [ disabled emptyStack, onClick ClickedHead ] [ text "Head" ]
        , button [ disabled emptyStack, onClick ClickedTail ] [ text "Tail" ]
        , button [ onClick ClickedReset ] [ text "Reset" ]
        ]


viewStackText : Bool -> Html Msg
viewStackText emptyStack =
    if emptyStack then
        text ""

    else
        text "Stack: "


viewHead : Maybe Int -> Html Msg
viewHead hd =
    case hd of
        Nothing ->
            text ""

        Just a ->
            text <| String.fromInt a


viewStack : Stack Int -> List (Html Msg)
viewStack stack =
    case stack of
        Nil ->
            [ text "" ]

        Cons hd tl ->
            [ div [ class "cons" ] [ text <| String.fromInt hd ] ] ++ viewStack tl



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }

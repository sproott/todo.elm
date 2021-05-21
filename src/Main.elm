module Main exposing (..)

import Browser
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, placeholder, src, type_, value)
import Html.Styled.Events exposing (onCheck, onClick, onInput, onSubmit)



---- MODEL ----


type alias Todo =
    { id : Int, name : String, completed : Bool }


type alias Model =
    { todos : List Todo, currentId : Int, newTodoName : String, hideCompleted : Bool }


init : ( Model, Cmd Msg )
init =
    ( { todos = [], currentId = 0, newTodoName = "", hideCompleted = False }, Cmd.none )



---- UPDATE ----


type Msg
    = AddTodo
    | ToggleTodo Int
    | WriteName String
    | ToggleHideCompleted Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTodo ->
            case model.newTodoName of
                "" ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | todos = model.todos ++ [ { id = model.currentId, name = model.newTodoName, completed = False } ], currentId = model.currentId + 1, newTodoName = "" }, Cmd.none )

        ToggleTodo id ->
            ( { model | todos = List.map (toggleTodo id) model.todos }, Cmd.none )

        WriteName newTodoName ->
            ( { model | newTodoName = newTodoName }, Cmd.none )

        ToggleHideCompleted newValue ->
            ( { model | hideCompleted = newValue }, Cmd.none )


toggleTodo : Int -> Todo -> Todo
toggleTodo id todo =
    if todo.id == id then
        { todo | completed = not todo.completed }

    else
        todo



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ css [ displayFlex, flexDirection column, alignItems center, margin (rem 2), property "gap" "0.5rem" ] ]
        ([ form [ onSubmit AddTodo, css [ displayFlex, flexDirection column, alignItems center, property "gap" "0.5rem" ] ]
            [ input [ placeholder "New Todo Name", value model.newTodoName, onInput WriteName ] []
            , button [] [ text "Add Todo" ]
            ]
         , label [ css [ property "user-select" "none" ] ]
            [ text "Hide completed"
            , input [ type_ "checkbox", onCheck ToggleHideCompleted ] []
            ]
         ]
            ++ drawTodos model.hideCompleted model.todos
        )


drawTodos : Bool -> List Todo -> List (Html Msg)
drawTodos hideCompleted todos =
    let
        filteredTodos =
            case hideCompleted of
                True ->
                    List.filter (\todo -> not todo.completed) todos

                False ->
                    todos
    in
    List.map drawTodo filteredTodos


drawTodo : Todo -> Html Msg
drawTodo { id, name, completed } =
    div
        [ css
            [ width (px 200), displayFlex, justifyContent spaceBetween ]
        ]
        [ div [] [ text name ]
        , div [ onClick (ToggleTodo id), css [ cursor pointer, property "user-select" "none" ] ]
            [ text
                (if completed then
                    "✔️"

                 else
                    "❌"
                )
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }

module MainDemo exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import ListView
import ListView.Viewers exposing (ListViewMsg)
import Markdown
import Utils exposing (class_)


type alias Character =
    { fullName : String
    , tagLine : String
    , power : Int
    , imageUrl : String
    , hatType : HatType
    , values : Array Int
    }


type HatType
    = None
    | Gandalf
    | Sports
    | Wtf
    | Regular


initialValues : Array Int
initialValues =
    Array.repeat numberOfDays 0


initialRows : List Character
initialRows =
    [ { fullName = "McGucket", tagLine = "beard-aid", power = 10000, hatType = Gandalf, imageUrl = "https://vignette.wikia.nocookie.net/gravityfalls/images/f/f5/S1e14_if_you_survive.png/revision/latest?cb=20130305201426", values = initialValues }
    , { fullName = "Soos", tagLine = "a vida é um regabofe", power = 50, hatType = Sports, imageUrl = "https://vignette.wikia.nocookie.net/gravityfalls/images/b/bb/S1e13_whole_busload_of_em.png/revision/latest/top-crop/width/200/height/150?cb=20151221063833", values = initialValues }
    , { fullName = "Mable", tagLine = "let's go girls", power = 1000, hatType = None, imageUrl = "https://vignette.wikia.nocookie.net/gravityfalls/images/b/b2/S1e3_mabel_new_wax_figure.png/revision/latest/top-crop/width/200/height/150?cb=20160204031402", values = initialValues }
    , { fullName = "Dipper", tagLine = "let's investigate", power = 1000, hatType = Sports, imageUrl = "https://vignette.wikia.nocookie.net/gravityfalls/images/c/cb/S1e16_dipper_will_take_room.png/revision/latest/top-crop/width/200/height/150?cb=20160119145706", values = initialValues }
    , { fullName = "Toby Carbonato", tagLine = "it's me", power = 999999999, hatType = Regular, imageUrl = "https://vignette.wikia.nocookie.net/gravityfalls/images/9/9e/S1e3_toby_determined_hands_in_air.png/revision/latest?cb=20120705020811", values = initialValues }
    , { fullName = "Stan Pines", tagLine = "não é pequeno", power = 20000, hatType = Wtf, imageUrl = "https://vignette.wikia.nocookie.net/gravityfalls/images/9/92/S1e16_something_about_you.png/revision/latest/top-crop/width/200/height/150?cb=20130530141339", values = initialValues }
    , { fullName = "Pacifica", tagLine = "who?", power = 10, hatType = None, imageUrl = "https://vignette.wikia.nocookie.net/gravityfalls/images/f/f7/S1e7_Pacifica_Northwest.png/revision/latest/top-crop/width/200/height/150?cb=20151116020530", values = initialValues }
    , { fullName = "Bill", tagLine = "ora ora ora ora ora ora", power = 20000, hatType = Regular, imageUrl = "https://vignette.wikia.nocookie.net/gravityfalls/images/e/ea/S1e19_bill_snap.png/revision/latest/top-crop/width/200/height/150?cb=20130905173319", values = initialValues }
    , { fullName = "Waddles", tagLine = "oinc oinc", power = 2, hatType = None, imageUrl = "https://vignette.wikia.nocookie.net/gravityfalls/images/0/09/S1e18_Waddles_stare.png/revision/latest/scale-to-width-down/350?cb=20160119145820", values = initialValues }
    ]


listViewConfig : ListView.Config Character Msg
listViewConfig =
    let
        cols =
            [ ListView.makeColumn.html "" viewPicture
                |> ListView.updateColumn.withCode "img"
            , ListView.makeColumn.string "Name" .fullName
            , ListView.makeColumn.string "Tagline" .tagLine
            , ListView.makeColumn.int "Power" .power
            , ListView.makeColumn.html "Hat" (.hatType >> viewHat)
            ]
    in
    ListView.makeConfig
        |> ListView.withColumns cols


viewPicture : Character -> Html msg
viewPicture row =
    Html.img [ class_ "avatar", src row.imageUrl ] []


viewHat : HatType -> Html msg
viewHat hatType =
    case hatType of
        None ->
            Html.text ""

        Wtf ->
            Html.img [ class_ "avatar avatar--small", src "https://image.shutterstock.com/image-vector/red-party-hat-isolated-on-260nw-563621065.jpg" ] []

        Sports ->
            Html.img [ class_ "avatar avatar--small", src "https://richardsonsports.com/media/catalog/product/cache/1/is_home_image/9df78eab33525d08d6e5fb8d27136e95/2/0/2018_featured_174_1.jpg" ] []

        _ ->
            Html.img [ class_ "avatar avatar--small", src "https://images.pexels.com/photos/35185/hats-fedora-hat-manufacture-stack.jpg?auto=compress&cs=tinysrgb&dpr=1&w=500" ] []


type Example2ViewMode
    = Example2HtmlTable
    | Example2CssGrid
    | Example2FlexCards


numberOfDays : Int
numberOfDays =
    5


type alias Model =
    { rows : List Character

    -- example #1
    , example1State : ListView.State

    -- example #2
    , example2State : ListView.State
    , example2ViewMode : Example2ViewMode

    -- example #3
    , example3State : ListView.State
    }


initialModel : Model
initialModel =
    { rows = initialRows
    , example1State =
        ListView.makeState
            |> ListView.updateState.withRowsPerPage 4
    , example2State =
        ListView.makeState
            |> ListView.updateState.withRowsPerPage 6
    , example2ViewMode = Example2HtmlTable
    , example3State = ListView.makeState
    }


type Msg
    = Example1ListViewMsg ListViewMsg
    | Example2ListViewMsg ListViewMsg
    | SwitchExample2ViewMode Example2ViewMode
    | Ex3UpdateCell Character Int String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Example1ListViewMsg listViewMsg ->
            { model | example1State = ListView.Viewers.update model.rows listViewMsg model.example1State }

        Example2ListViewMsg listViewMsg ->
            { model | example2State = ListView.Viewers.update model.rows listViewMsg model.example2State }

        SwitchExample2ViewMode newViewMode ->
            { model | example2ViewMode = newViewMode }

        Ex3UpdateCell character dayNumber strValue ->
            let
                intValue =
                    String.toInt strValue |> Maybe.withDefault 0

                updateValuesForCharacter =
                    \c -> { c | values = Array.set dayNumber intValue c.values }

                newRows =
                    List.map
                        (\c ->
                            if c == character then
                                updateValuesForCharacter c

                            else
                                c
                        )
                        model.rows
            in
            { model | rows = newRows }


view : Model -> Html Msg
view model =
    Html.div [ class_ "container" ]
        [ Html.h1 [] [ Html.text "elm-listview" ]
        , Html.p [] [ Markdown.toHtml [] "An **Elm** library for viewing data in a `List`." ]
        , viewExample1 model
        , viewExample2 model
        , viewExample3 model
        ]


viewExample1 : Model -> Html Msg
viewExample1 model =
    Html.article [ class_ "example1" ]
        [ Html.h2 [] [ Html.text "Simple table" ]
        , Html.p [] [ Markdown.toHtml [] """Render data in a HTML table.

Cells can contain simple data (`String`, `Int`, `Float`), but can also contain any HTML element.""" ]
        , Html.div [ class_ "gravityTable" ]
            [ ListView.Viewers.viewAsHtmlTable Example1ListViewMsg listViewConfig model.example1State model.rows
            ]
        ]


viewExample2 : Model -> Html Msg
viewExample2 model =
    Html.article [ class_ "example2" ]
        [ Html.h2 [] [ Html.text "Different views for the same data, config, and state" ]
        , Markdown.toHtml [] "You can use the same `ListView.Config` and `ListView.State` with different viewing functions. \n\nNote that, since we're reusing the `ListView.State`, the current sort order and page are retained when we switch views."
        , Html.section [ class_ "example2-container" ]
            [ Html.header []
                [ Html.button [ class_ "example2-viewSelectorButton", Html.Attributes.type_ "button", onClick (SwitchExample2ViewMode Example2HtmlTable) ] [ Html.text "HTML table" ]
                , Html.button [ class_ "example2-viewSelectorButton", Html.Attributes.type_ "button", onClick (SwitchExample2ViewMode Example2CssGrid) ] [ Html.text "CSS grid" ]
                , Html.button [ class_ "example2-viewSelectorButton", Html.Attributes.type_ "button", onClick (SwitchExample2ViewMode Example2FlexCards) ] [ Html.text "Flexbox cards" ]
                ]
            , case model.example2ViewMode of
                Example2HtmlTable ->
                    Html.div [ class_ "gravityTable" ]
                        [ Markdown.toHtml [] "This view uses a simple HTML table with `layout: auto`"
                        , ListView.Viewers.viewAsHtmlTable Example2ListViewMsg listViewConfig model.example2State model.rows
                        ]

                Example2CssGrid ->
                    Html.div [ class_ "gravityGrid" ]
                        [ Markdown.toHtml [] "This view uses `CSS Grid` to try and mimic the HTML table from the first view. It actually uses a grid for each row because of the padding between rows.\n\nThis table is responsive, it's rendered differently on smaller screens (there's probably a better way to achieve that)."
                        , ListView.Viewers.viewAsCssGrid Example2ListViewMsg listViewConfig model.example2State model.rows
                        ]

                Example2FlexCards ->
                    Html.div [ class_ "gravityDeck" ]
                        [ Markdown.toHtml [] "This view uses `Flexbox` to display each row as a card. \n\nNote this is actually the same viewer as for `CSS Grid`, only the layout is changed using CSS."
                        , ListView.Viewers.viewAsCssGrid Example2ListViewMsg listViewConfig model.example2State model.rows
                        ]
            ]
        ]


getIntFromArray : Int -> Array Int -> Int
getIntFromArray i array =
    Array.get i array
        |> Maybe.withDefault 0


viewExample3 : Model -> Html Msg
viewExample3 model =
    let
        makeDayColumn =
            \i ->
                ListView.makeColumn.html ("Day " ++ String.fromInt i)
                    (\character ->
                        Html.input
                            [ class_ "example3-dayInput"
                            , Html.Attributes.value
                                (getIntFromArray i character.values
                                    |> String.fromInt
                                )
                            , Html.Events.onInput (Ex3UpdateCell character i)
                            ]
                            []
                    )

        cols =
            List.range 1 numberOfDays
                |> List.map makeDayColumn

        lvConfig =
            ListView.makeConfig
                |> ListView.withColumn (ListView.makeColumn.string "Name" .fullName)
                |> ListView.withColumns cols
    in
    Html.article [ class_ "example2" ]
        [ Html.h2 [] [ Html.text "Editable table" ]
        , Html.div [ class_ "gravityTable" ]
            [ ListView.Viewers.viewAsHtmlTable Example1ListViewMsg lvConfig model.example1State model.rows ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }

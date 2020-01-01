module ListView exposing
    ( Config, makeConfig, makeColumn, withColumn, withColumns, State, makeState
    , getViewInfo, updateState, updateColumn, SortDirection(..), ListViewSortState(..), PageChange(..), ListViewInfo, PagingViewInfo, ColumnViewInfo, ColumnSortInfo(..)
    )

{-| A component for viewing data from a `List`. Features currently include _sorting_ and _pagination_.

A key characteristic of this library is that it tries to seperate **transforming** the data (i.e., sort,
paginate, group, filter, etc.) from actually **viewing** it (e.g., rendering a HTML table, CSS grid, etc.).

This module, `ListView`, is only responsible for the first part, however the `ListView.Viewers` module provides
some standard viewers that should be sufficient for many use cases.


# Basic usage

Here's a small example of how to render a list of data as a HTML table, using the viewer `ListView.Viewers.viewAsHtmlTable`.
Note that we also use the `TableMsg` type and `updateTable` function from that same module, which are needed by
the `viewHtmlTable` function. As is ususal with other **Elm** libraries, the `updateTable` function needs to be
called from your main `update` function somehow.

    -- main entity type we want to view
    type alias Character =
        { fullName : String
        , power : Int
        , imageUrl : String
        }

    -- some data, this is the list we want to view
    rows : List Character
    rows = [...]

    -- this is just a helper function to render an image
    viewPicture : character -> Html msg
    viewPicture row =
        Html.img [ class_ "avatar", src row.imageUrl ] []


    -- config for my list to be viewed as an HTML table with 3 columns
    tableConfig : Config Character Msg
    tableConfig =
        ListView.makeConfig
            |> ListView.withColumn (ListView.makeColumn.html "" viewPicture)
            |> ListView.withColumn (ListView.makeColumn.string "Name" .fullName)
            |> ListView.withColumn (ListView.makeColumn.int "Power" .power)

    -- we need to store the `DataList` state in our model
    type alias Model = { tableState : ListView.State }


    -- our app needs to be able to process messages from the `DataListViewer` (e.g., sort by this column)
    type Msg
        = OnTableMsg ListView.Viewers.ListViewMsg

    -- our main `update` function needs to handle messages from the table
    update : Msg -> Model -> Model
    update msg model =
        case msg of
            OnTableMsg tableMsg ->
                { model | tableState = ListView.Viewers.update rows tableMsg model.tableState }


    view : Model -> Html Msg
    view model =
        ListView.Viewers.viewAsHtmlTable OnTableMsg tableConfig model.tableState rows

    main : Program () Model Msg
    main =
        Browser.sandbox
            { init = { tableState: ListView.makeState }
            , view = view
            , update = update
            }

@docs Config, makeConfig, makeColumn, withColumn, withColumns, State, makeState


# Advanced usage

Sometimes the output of the standard viewers defined in `ListView.Viewers` may not be suitable for
your needs; maybe you want to layout your page using [elm-ui](https://github.com/mdgriffith/elm-ui) instead; or maybe you just need a
simpler paginator with only **Previous** and **Next** buttons.

In those cases, you may want to skip the `ListView.Viewers` altogether, and use the `ListView.getViewInfo`
instead, which returns all the information needed (hopefully) to render the list, already sorted, filtered, etc. This is
exactly what the viewers from the `ListView.Viewers` module do, so be sure to check the source code.

One caveat is that, in order for external libraries to be able to render data from the `ListView` module, a lot
of (otherwise internal) details had to be exposed. This creates a very large public API, which will mostly likely change
when this library evolves, creating many breaking changes along the way. More so in codebases that define their
own viewers. Keep this in mind when going down this route!

A final note: many customizations can be made to the standard viewers using CSS alone. You may not need
build your own viewer!

@docs getViewInfo, updateState, updateColumn, SortDirection, ListViewSortState, PageChange, ListViewInfo, PagingViewInfo, ColumnViewInfo, ColumnSortInfo

-}

import Html exposing (Html)
import Utils



-- TABLE CONFIGURATION --


{-| Opaque type holding all the static configuration of the list (basically,
a list with all the viewable columns and their behavior)
-}
type Config a msg
    = Config
        { -- ordered list of columns to display
          columns : List (ColumnConfig a msg)
        }


{-| Make an empty (i.e., without columns) table configuration.
This is the starting point for adding new columns

    makeConfig
        |> withColumn (makeColumn.html "" viewPicture)
        |> withColumn (makeColumn.string "Name" .fullName)
        |> withColumn (makeColumn.string "Tagline" .tagLine)
        |> withColumn (makeColumn.int "Power" .power)

-}
makeConfig : Config a msg
makeConfig =
    Config
        { columns = []
        }


{-| Add a new column to a table configuration. Columns can be created with `makeColumn.XXX` functions
-}
withColumn : ColumnConfig a msg -> Config a msg -> Config a msg
withColumn colCfg (Config tableCfg) =
    Config
        { tableCfg | columns = List.append tableCfg.columns [ colCfg ] }


{-| Add a bunch of new columns to a table configuration. Columns can be created with `makeColumn.XXX` functions
-}
withColumns : List (ColumnConfig a msg) -> Config a msg -> Config a msg
withColumns colConfigs (Config tableCfg) =
    Config
        { tableCfg | columns = List.append tableCfg.columns colConfigs }



-- COLUMN CONFIGURATION --


{-| Configuration for a single column of the `ListView`, contains a bunch of functions
for rendering the column, sorting its data, etc.

Callers can call the `makeColumn.XXX` functions for creating new columns.

-}
type ColumnConfig a msg
    = ColumnConfig
        { name : String
        , code : String
        , viewer : a -> RowIndex -> ColumnOutput msg
        , sorter : ColumnSorting a
        }


type alias RowIndex =
    Int


type alias ColumnIndex =
    Int


{-| Type of outputs that a column can render
-}
type ColumnOutput msg
    = Text String
    | Html (Html msg)


{-| Type for defining if a column is sortable and, if it is, how it can be sorted.
-}
type ColumnSorting a
    = Unsortable
    | Sortable (Utils.Comparer a)


{-| Namespace for aggregating all the functions that create new columns for the data list. Some examples:

    makeColumn.string "Car" .carName

`.carName` must return a `String` value. Column is sortable alphabetically by default.

---

    makeColumn.int "Age" .age
    makeColumn.float "Salary" .salary

Values are converted to `String` for rendering, but are sorted numerically by default.

---

    makeColumn.html "Company url" (\row rowIndex -> Html.a [href row.url] [Html.text row.name])

The accessor function must return `Html msg`. Can be used to render just about anything. These columns are not sortable by default.
An important difference from the other column types is that the accessor function has access to the index of the row
in the original list. This is sometimes usefule if the generated `Html` wants to produce messages that need to identify
the row by its index (e.g., a message to update the row somehow).

---

**Note**: This namespace only exists because there are already too many exposed functions in this module,
and I wanted to group related functions into a common namespace, without creating a
separate module.

Not really sure this is a good idea, may change in the future.

-}
makeColumn :
    { -- Builds a new column whose value is a `String`. Can be sorted alphabetically by default.
      string : String -> (a -> String) -> ColumnConfig a msg

    --Builds a new column whose value is an `Int`. Can be sorted by default.
    , int : String -> (a -> Int) -> ColumnConfig a msg

    --Builds a new column whose value is a `Float`. Can be sorted by default.
    , float : String -> (a -> Float) -> ColumnConfig a msg

    -- Builds a new column whose value is some `Html`. Cannot be sorted by default.
    , html : String -> (a -> RowIndex -> Html msg) -> ColumnConfig a msg
    }
makeColumn =
    let
        emptyColumn name =
            ColumnConfig
                { name = name
                , code = Utils.codify name
                , viewer = \_ _ -> Text ""
                , sorter = Unsortable
                }

        viewerIgnoringIndex accessor a _ =
            accessor a
    in
    { string =
        \name accessor ->
            emptyColumn name
                |> updateColumn.withSorter accessor
                |> updateColumn.withViewer (viewerIgnoringIndex (Text << accessor))
    , int =
        \name accessor ->
            emptyColumn name
                |> updateColumn.withSorter accessor
                |> updateColumn.withViewer (viewerIgnoringIndex (Text << String.fromInt << accessor))
    , float =
        \name accessor ->
            emptyColumn name
                |> updateColumn.withSorter accessor
                |> updateColumn.withViewer (viewerIgnoringIndex (Text << String.fromFloat << accessor))
    , html =
        \name accessor ->
            emptyColumn name
                |> updateColumn.withViewer (\value index -> Html (accessor value index))
    }


{-| Namespace for aggregating all the functions that change the configuration
an existing `Column` (i.e., how it's displayed or sorted, or even its name).
For example, to create a sortable HTML column:

    showUsername person =
        Html.span []
            [ Html.img [ src person.avatar ] []
            , Html.text person.name
            ]
    makeColumn.html "Name" showUsername .name

-}
updateColumn :
    { -- change the name of a column
      withName : String -> ColumnConfig a msg -> ColumnConfig a msg

    -- change the code of a column
    , withCode : String -> ColumnConfig a msg -> ColumnConfig a msg

    -- change whether and how a column is sorted
    , withSorter : (a -> comparable) -> ColumnConfig a msg -> ColumnConfig a msg

    -- change the way a column is rendered
    , withViewer : (a -> RowIndex -> ColumnOutput msg) -> ColumnConfig a msg -> ColumnConfig a msg
    }
updateColumn =
    { withName = \newName (ColumnConfig cfg) -> ColumnConfig { cfg | name = newName }
    , withCode = \newCode (ColumnConfig cfg) -> ColumnConfig { cfg | code = newCode }
    , withSorter = \newSorter (ColumnConfig cfg) -> ColumnConfig { cfg | sorter = Sortable (Utils.comparing newSorter) }
    , withViewer = \newViewer (ColumnConfig cfg) -> ColumnConfig { cfg | viewer = newViewer }
    }



-- TABLE STATE, stores information like how the table is currently ordered
-- also includes some functions for manipulating the state


{-| Holds all the state of the `ListView`, like how it's currently being sorted or what the current page is.

You definately want to store this in your `Model`. In the future this should be exportable/importable in order to
be stored in local storage, for example.

-}
type State
    = State
        { sortState : ListViewSortState
        , rowsPerPage : Int
        , currentPage : Int
        }


{-| Direction of sorting. Note that currently, if a column is sortable, it can always be sorted in both directions.
-}
type SortDirection
    = ASC
    | DESC


{-| Information on how the table is currently being sorted. Part of `State`
-}
type ListViewSortState
    = Unsorted
    | Sorted ColumnIndex SortDirection


{-| Creates a `State` with default setting: page size of 10 elements and no sorting.
-}
makeState : State
makeState =
    State
        { sortState = Unsorted
        , rowsPerPage = 10
        , currentPage = 1
        }


{-| Represents a change in the current page
-}
type PageChange
    = GotoFirstPage
    | GotoLastPage
    | GotoNextPage
    | GotoPreviousPage
    | GotoPage Int


{-| Calculates the total number of pages, given the total number of rows and how many rows are in a page.
-}
pageCount : Int -> Int -> Int
pageCount rowCount rowsPerPage =
    let
        partialPage =
            if modBy rowsPerPage rowCount > 0 then
                1

            else
                0
    in
    (rowCount // rowsPerPage) + partialPage


{-| Namespace for aggregating all the functions that update the given `ListView.State`.
-}
updateState :
    { -- moves to another page
      withPage : Int -> PageChange -> State -> State

    -- changes the sorting configuration of the table
    , withSorting : ListViewSortState -> State -> State

    -- change the maximum number of rows on each page
    , withRowsPerPage : Int -> State -> State
    }
updateState =
    { withPage = gotoPage
    , withSorting =
        \newSortState (State tableState) ->
            State
                { tableState | sortState = newSortState }
    , withRowsPerPage =
        \newPageSize (State tableState) ->
            State { tableState | rowsPerPage = newPageSize }
    }


gotoPage : Int -> PageChange -> State -> State
gotoPage rowCount pageChange (State tableState) =
    let
        numberOfPages =
            pageCount rowCount tableState.rowsPerPage

        newPage =
            case pageChange of
                GotoFirstPage ->
                    1

                GotoLastPage ->
                    numberOfPages

                GotoNextPage ->
                    if tableState.currentPage >= numberOfPages then
                        numberOfPages

                    else
                        tableState.currentPage + 1

                GotoPreviousPage ->
                    if tableState.currentPage > 1 then
                        tableState.currentPage - 1

                    else
                        1

                GotoPage n ->
                    if n >= 1 && n <= numberOfPages then
                        n

                    else
                        tableState.currentPage
    in
    State { tableState | currentPage = newPage }


{-| Helper function for getting the sort function for a given column and direction, if applicable.
-}
getSortingConfig : ListViewSortState -> List (ColumnConfig a msg) -> Maybe (Utils.Comparer a)
getSortingConfig sortState columns =
    case sortState of
        Unsorted ->
            Nothing

        Sorted sortColIndex sortDirection ->
            Utils.getIndexFromList columns sortColIndex
                |> Maybe.andThen (getColumnSorter sortDirection)


{-| Helper function for getting the sort function for a given column and direction, if applicable.
-}
getColumnSorter : SortDirection -> ColumnConfig a msg -> Maybe (Utils.Comparer a)
getColumnSorter sortDirection (ColumnConfig colConfig) =
    case colConfig.sorter of
        Unsortable ->
            Nothing

        Sortable comparer ->
            case sortDirection of
                ASC ->
                    Just comparer

                DESC ->
                    Just (Utils.toggleSorter comparer)



-- TABLE VIEWER HELPERS
-- This sections contains types and functions necessary for an external module to be able to render a `ListView`
-- It ends up exposing a lot of details about the `ListView`, and I'm not sure we really want to do that; so, it may well
-- change a lot in the future


{-| Type that holds all the necessary information for rendering a `ListView`
-}
type alias ListViewInfo a msg =
    { -- get the content of each column for a given row, as defined by the `viewer` function for the columns
      columnsForRow : a -> RowIndex -> List (Html msg)

    -- rows that are visible according to the current table's configuration and state
    -- for example, only these rows will be sorted according to the current sort order; also, this only included rows for the current page
    -- Note that we include the index of the each row in the original list, so the caller can later access an item by its index
    , rowsToDisplay : List ( RowIndex, a )

    -- some generic info about the columns, such as name, or whether the table is currently being sorted by a specific column
    , columnsViewInfo : List ColumnViewInfo

    -- pagination information
    , pagingViewInfo : PagingViewInfo
    }


{-| Information about the `ListView` paging, should contain all that's needed to render a paginator
-}
type alias PagingViewInfo =
    { -- total number of results
      numberOfRows : Int

    -- total number of pages with results
    , numberOfPages : Int

    -- number of rows in a page
    , rowsPerPage : Int

    -- the currently visible page
    , currentPage : Int

    -- the index of the first element in the current page
    , currentPageStartIndex : Int

    -- the index of the last element in the current page
    , currentPageEndIndex : Int
    }


{-| Type that holds all the necessary information for rendering a column header
-}
type alias ColumnViewInfo =
    { name : String
    , code : String
    , index : ColumnIndex
    , sortInfo : ColumnSortInfo
    }


{-| Represents sort state for a single column
-}
type ColumnSortInfo
    = -- ListView can't be sorted by this particular column
      UnsortableColumn
      -- ListView can be sorted by this columns, but currently isn't
    | UnsortedColumn
      -- ListView is being sorted by this column, in the specified direction (ASC | DESC)
    | SortedColumn SortDirection


{-| Generates all the information needed for an external module to be able to render the `ListView` columns.
For each column, it should include its name, whether the `ListView` is sorted by that column (and its sort direction), etc.
-}
getColumnsViewInfo : Config a msg -> State -> List ColumnViewInfo
getColumnsViewInfo (Config tableConfig) tableState =
    List.indexedMap (getColumnViewInfo tableState) tableConfig.columns


getColumnViewInfo : State -> ColumnIndex -> ColumnConfig a msg -> ColumnViewInfo
getColumnViewInfo (State tableState) colIndex (ColumnConfig colConfig) =
    { name = colConfig.name
    , code = colConfig.code
    , index = colIndex
    , sortInfo =
        if colConfig.sorter == Unsortable then
            UnsortableColumn

        else
            case tableState.sortState of
                Unsorted ->
                    UnsortedColumn

                Sorted sortedColIndex sortDir ->
                    if sortedColIndex == colIndex then
                        SortedColumn sortDir

                    else
                        UnsortedColumn
    }


{-| Returns the data that should be displayed after sorting and
paginating, according to the `ListView`'s state and configuration.

This is the function that takes in the entire data list and applies to it all
the transformations (sorting, pagination, etc.) defined in the config and state.

Note that we also return the index of each item in the original list.

-}
getDataToDisplay : Config a msg -> State -> List a -> List ( RowIndex, a )
getDataToDisplay (Config tableCfg) (State tableState) rows =
    let
        -- index of the first row in the current page
        beginIndex =
            (tableState.currentPage - 1) * tableState.rowsPerPage

        -- index of the last possible row in the current page
        endIndex =
            beginIndex + tableState.rowsPerPage

        -- save the original index of each row
        indexedRows =
            List.indexedMap Tuple.pair rows
    in
    -- 1. `getSortingConfig` returns a `Comparar a`
    getSortingConfig tableState.sortState tableCfg.columns
        -- 2. we must turn it into a `Comparer (RowIndex, a)`
        |> Maybe.map (\sorter -> \( _, row1 ) ( _, row2 ) -> sorter row1 row2)
        -- 3. now we can sort our `List (RowIndex, a)`
        |> Maybe.map (\sorter -> List.sortWith sorter indexedRows)
        -- 4. in case no sorter exists, we just use the original list
        |> Maybe.withDefault indexedRows
        -- 5. slice for the current page
        |> Utils.subList beginIndex endIndex


{-| Takes in all the data, configuration and state and produces all the
information needed to view the `ListView`, poperly sorted, paginated and formatted.

Advanced usages can call this function to completely customize what gets rendered,
and what messages are produced.

Typical usages that just need an HTML table or a CSS grid/flexbox should just use the
corresponding viewer from `ListView.Viewers` (which all call this function under the hoods).

-}
getViewInfo : Config a msg -> State -> List a -> ListViewInfo a msg
getViewInfo (Config tableCfg) (State tableState) rows =
    let
        numberOfPages =
            pageCount (List.length rows) tableState.rowsPerPage
    in
    { columnsForRow = \row rowIndex -> List.map (viewColumnOutput row rowIndex) tableCfg.columns
    , rowsToDisplay = getDataToDisplay (Config tableCfg) (State tableState) rows
    , columnsViewInfo = getColumnsViewInfo (Config tableCfg) (State tableState)
    , pagingViewInfo =
        { numberOfRows = List.length rows
        , numberOfPages = numberOfPages
        , rowsPerPage = tableState.rowsPerPage
        , currentPage = tableState.currentPage
        , currentPageStartIndex = (tableState.currentPage - 1) * tableState.rowsPerPage
        , currentPageEndIndex =
            min (List.length rows - 1)
                (tableState.rowsPerPage * tableState.currentPage - 1)
        }
    }


{-| Renders a single column, depending on its output type
-}
viewColumnOutput : a -> RowIndex -> ColumnConfig a msg -> Html msg
viewColumnOutput row rowIndex (ColumnConfig col) =
    case col.viewer row rowIndex of
        Text s ->
            -- wrap the text in a span, just so it can easily be styled using CSS
            Html.span [] [ Html.text s ]

        Html html ->
            -- no need to wrap the content, it's already HTML
            html

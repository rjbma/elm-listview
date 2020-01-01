module ListView.Viewers exposing (viewAsHtmlTable, viewAsCssGrid, update, ListViewMsg)

{-| Standard viewers for `ListView`.

The idea is that you first configure the list using functions from
the `ListView` module and then use this module to transform that
into `Html`. Check the documentation of `ListView` for details.

@docs viewAsHtmlTable, viewAsCssGrid, update, ListViewMsg

-}

import Html exposing (Html)
import Html.Attributes exposing (colspan)
import Html.Events exposing (onClick)
import ListView
    exposing
        ( ColumnSortInfo(..)
        , ColumnViewInfo
        , ListViewSortState(..)
        , PageChange(..)
        , PagingViewInfo
        , SortDirection(..)
        )
import Utils exposing (class_)


{-| Messages specific to the standard `ListView` viewers included in this module.

Note that, even if you decide to implement your own viewer, you can still use this
message type (provided it's enough for your use case, of course). In that case, you'll
probably want to use `ListView.Viewers.update` as well.

-}
type ListViewMsg
    = OnSort ListViewSortState
    | OnPageChange PageChange


{-| Main function for updating the `ListView` state. Should be called by the app's update function.

Note that, even if you decide to implement your own viewer, you can still use this
update function message type (provided it's enough for your use case, of course). In that case, you'll
also have to user `ListViewMsg` as well.

-}
update : List a -> ListViewMsg -> ListView.State -> ListView.State
update rows msg state =
    case msg of
        OnSort newSortState ->
            ListView.updateState.withSorting newSortState state

        OnPageChange pageChange ->
            ListView.updateState.withPage (List.length rows) pageChange state


{-| Views the `ListView` as a HTML table.

Generated markup looks like:

```html
    <table class="listView listView--htmlTable">
        <thead class="listView-header">
            <tr>
                <th class="listView-headerColumn listView-headerColumn--colCode">...</th>
                ...
            </tr>
        </thead>
        <tbody>
            <tr class="listView-row">
                <td>
                    <div class="listView-column listView-column--colCode">...</div>
                </td>
                ...
            </tr>
        </tbody>
        <tfoot class="listView-footer">
            <tr>
                <td colspan="$numCols">
                    <div class="listView-paginator">...</div>
                </td>
            </tr>
        </tfoot>
    </table>
```

-}
viewAsHtmlTable : (ListViewMsg -> msg) -> ListView.Config a msg -> ListView.State -> List a -> Html msg
viewAsHtmlTable msgMapper config state allRows =
    let
        gvi =
            ListView.getViewInfo config state allRows

        viewTh =
            viewColumnHeader (msgMapper << OnSort)

        viewTd ( colInfo, colContent ) =
            Html.td
                [ class_ ("listView-column listView-column--" ++ colInfo.code) ]
                [ colContent ]

        viewTr rowIndex row =
            let
                cols =
                    Utils.zip gvi.columnsViewInfo (gvi.columnsForRow row rowIndex)
            in
            [ Html.tr [ class_ "listView-row" ]
                (List.map viewTd cols)
            ]

        numberOfColumns =
            List.length gvi.columnsViewInfo
    in
    Html.table [ class_ "listView listView--htmlTable" ]
        [ Html.thead [ class_ "listView-header" ]
            [ Html.tr []
                (List.map viewTh gvi.columnsViewInfo)
            ]
        , Html.tbody []
            (List.map (Utils.tupleApply2 viewTr) gvi.rowsToDisplay |> List.concat)
        , Html.tfoot [ class_ "listView-footer" ]
            [ Html.tr []
                [ Html.td [ colspan numberOfColumns ]
                    [ viewPaginator msgMapper gvi.pagingViewInfo ]
                ]
            ]
        ]


{-| Views the `List` as a bunch of HTML elements that can by laid
out using CSS only (ususlly CSS Grid or Flexbox) as a CSS grid.

Generated markup looks like:

```html
    <div class="listView listView--cssGrid">
        <div class="listView-header">
            <th class="listView-headerColumn listView-headerColumn--colCode">...</th>
            ...
        </div>
        <div class="listView-row">
            <div class="listView-column listView-column--colCode">...</div>
            ...
        </div>
        ...
        <div class="listView-paginator">...</div>
    </div>
```

-}
viewAsCssGrid : (ListViewMsg -> msg) -> ListView.Config a msg -> ListView.State -> List a -> Html msg
viewAsCssGrid msgMapper config state allRows =
    let
        gvi =
            ListView.getViewInfo config state allRows

        viewCardWithRow rowIndex row =
            let
                cols =
                    Utils.zip gvi.columnsViewInfo (gvi.columnsForRow row rowIndex)

                viewColumn ( colInfo, colContent ) =
                    Html.div
                        [ class_ ("listView-column listView-column--" ++ colInfo.code) ]
                        [ colContent ]
            in
            Html.div [ class_ "listView-row" ]
                (List.map viewColumn cols)

        viewHeaderColumn =
            viewColumnHeader (msgMapper << OnSort)

        header =
            Html.div [ class_ "listView-header" ]
                (List.map viewHeaderColumn gvi.columnsViewInfo)

        paginator =
            viewPaginator msgMapper gvi.pagingViewInfo
    in
    Html.div [ class_ "listView listView--cssGrid" ]
        (header :: List.map (Utils.tupleApply2 viewCardWithRow) gvi.rowsToDisplay ++ [ paginator ])


{-| Render a column header with the given `ColumnViewInfo`
-}
viewColumnHeader : (ListViewSortState -> msg) -> ColumnViewInfo -> Html msg
viewColumnHeader onSort colInfo =
    let
        cssClass =
            class_ ("listView-headerColumn listView-headerColumn--" ++ Utils.codify colInfo.code)

        -- these HTML attributes are only added to sortable columns
        sortableAttributes =
            case colInfo.sortInfo of
                UnsortableColumn ->
                    []

                UnsortedColumn ->
                    [ class_ "listView-headerColumn-isSortable"
                    , onClick <| onSort (Sorted colInfo.index ASC)
                    ]

                SortedColumn ASC ->
                    [ class_ "listView-headerColumn-isSortable listView-headerColumn-isSorted listView-headerColumn-isSortedAscending"
                    , onClick <| onSort (Sorted colInfo.index DESC)
                    ]

                SortedColumn DESC ->
                    [ class_ "listView-headerColumn-isSortable listView-headerColumn-isSorted listView-headerColumn-isSortedDescending"
                    , onClick <| onSort (Sorted colInfo.index ASC)
                    ]
    in
    Html.th
        (cssClass :: sortableAttributes)
        [ Html.text colInfo.name ]


viewPaginator : (ListViewMsg -> msg) -> PagingViewInfo -> Html msg
viewPaginator msgMapper pvi =
    let
        viewInt int css =
            Html.span [ class_ css ] [ Html.text (String.fromInt int) ]

        summary =
            [ viewInt (pvi.currentPageStartIndex + 1) "listView-paginatorStartIndex"
            , Html.span [] [ Html.text "-" ]
            , viewInt (pvi.currentPageEndIndex + 1) "listView-paginatorEndIndex"
            , Html.span [] [ Html.text "of" ]
            , viewInt pvi.numberOfRows "listView-paginatorRowCount"
            ]
    in
    Html.div [ class_ "listView-paginator" ]
        [ Html.span [ class_ "listView-paginatorSummary" ] summary
        , Html.span [ class_ "listView-paginatorControls" ]
            [ Html.button
                [ class_ "listView-paginatorFirstButton"
                , onClick (msgMapper <| OnPageChange GotoFirstPage)
                ]
                [ Html.text "⏮" ]
            , Html.button
                [ class_ "listView-paginatorPrevButton"
                , onClick (msgMapper <| OnPageChange GotoPreviousPage)
                ]
                [ Html.text "⏴" ]
            , Html.button
                [ class_ "listView-paginatorNextButton"
                , onClick (msgMapper <| OnPageChange GotoNextPage)
                ]
                [ Html.text "⏵" ]
            , Html.button
                [ class_ "listView-paginatorLastButton"
                , onClick (msgMapper <| OnPageChange GotoLastPage)
                ]
                [ Html.text "⏭" ]
            ]
        ]

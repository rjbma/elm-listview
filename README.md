# elm-listview
A package for displaying some list of data in Elm. Most likely will be used to display tables or lists of cards.

## Goals
### Main goals:
- as simple to use as possible
- sortable
- paginated
- filterable (column or whole grid?)
- simple to display html content in the grid
- (some) state should be updatable from the outside, without exposing specific internal details (goto next page, filter by 'X', sort by 'y', etc.)
- as much agnostic of the view as possible. Must provide some default viewers for viewing a list as an HTML table and a collection of HTML elements that can be styled in CSS. However, when the default viewers are not enough, the caller should be able to specify how the list is rendered

### Other goals:
- tree grids for displaying hierarquic data (presumably in a table)
- totals by column
- groupable: ability to group similar rows based on some property of each row of data (e.g., group by country). Totals would also be applied to groups.
- virtualization
- sticky headers
- sticky colums

## Main entities
- a **ListView** consists of a list of **Rows**, usually rendered as a HTML `table`. A **ListView** is configured by defining its **Columns**'s (see `ListView.Config`).
- a **Row** is a piece of data we want to display in the grid. It will usually be rendered as a HTML `tr` or a `div` representing a card, for example
- a **Column** is a piece of information of a **Row**, for example a person's name or age. It will usually be rendered as a HTML `td`, or some `span` inside a card. The configuration of a **Column** defines the column's name, how it is rendered, sorted by, etc. (see `ListView.ColumnConfig`).

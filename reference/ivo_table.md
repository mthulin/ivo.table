# Create pretty frequency/contingency tables

`ivo_table()` lets you easily create a table using pretty fonts and
colors. If you want the table with masked values use
[`ivo_table_masked()`](https://mthulin.github.io/ivo.table/reference/ivo_table_masked.md).

## Usage

``` r
ivo_table(
  df,
  extra_header = TRUE,
  exclude_missing = FALSE,
  missing_string = "(Missing)",
  colsums = FALSE,
  rowsums = FALSE,
  sums_string = "Total",
  caption = NA,
  highlight_cols = NULL,
  highlight_rows = NULL,
  percent_by = NA,
  color = "darkgreen",
  font_name = "Arial",
  bold_cols = NULL,
  long_table = FALSE,
  remove_zero_rows = FALSE
)
```

## Arguments

- df:

  A data frame with 1-4 columns

- extra_header:

  Should the variable name be displayed? Defaults to TRUE.

- exclude_missing:

  Whether to exclude missing values from the table. Defaults to FALSE.

- missing_string:

  A string used to indicate missing values. Defaults to "(Missing)".

- colsums:

  A logical indicating whether the sum of each column should be
  computed. Defaults to FALSE.

- rowsums:

  A logical indicating whether the sum of each row should be computed.
  Defaults to FALSE.

- sums_string:

  A string that is printed in the column/row where row/column sums are
  shown. Defaults to "Total".

- caption:

  An optional string containing a table caption.

- highlight_cols:

  A numeric vector containing the indices of the columns that should be
  highlighted.

- highlight_rows:

  A numeric vector containing the indices of the rows that should be
  highlighted.

- percent_by:

  Used to get percentages instead of frequencies. There are three
  options: "row" to get percentages by row (each row sum is 100
  percent), "col" to get percentages by column (each the sum of each row
  to 100 percent) and "tot" to get percentages out of the total (the sum
  of all cells is 100 percent). The default, NA, means that frequencies
  are displayed instead.

- color:

  A named color or a color HEX code, used for the lines in the table.
  Defaults to "darkgreen".

- font_name:

  The name of the font to be used in the table. Defaults to "Arial".

- bold_cols:

  A numeric vector containing the indices of the columns that should use
  a bold font.

- long_table:

  For one-way tables: FALSE (the default) means that the table will be
  wide and consist of a single row, TRUE means that the table will be
  long and consist of a single column.

- remove_zero_rows:

  If set to TRUE, removes all rows that contain nothing but zeros. The
  default is FALSE.

## Value

A stylized `flextable`.

## Details

The functions `ivo_table()` and
[`ivo_table_masked()`](https://mthulin.github.io/ivo.table/reference/ivo_table_masked.md)
takes a `data.frame` with 1-4 columns. The order of the columns in the
`data.frame` will determine where they will be displayed in the table.
The first column will always be displayed at the top of the table. If
there are more than one column the following 2-4 columns will be
displayed to the left in the order 2, 3, 4. To change how the columns
are displayed in the table; change the place of the columns in the
`data.frame` using
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).

## See also

ivo_table_add_mask

## Author

Måns Thulin and Kajsa Grind

## Examples

``` r
# Generate example data
example_data <- data.frame(Year = sample(2020:2023, 50, replace = TRUE),
A = sample(c("Type 1", "Type 2"), 50, replace = TRUE),
B = sample(c("Apples", "Oranges", "Bananas"), 50, replace = TRUE),
C = sample(c("Swedish", "Norwegian", "Chilean"), 50, replace = TRUE))

### 1 way tables ###
data1 <- example_data |> dplyr::select(Year)

ivo_table(data1)
#> Warning: 'flextable::regulartable' is deprecated.
#> Use 'flextable' instead.
#> See help("Deprecated")


.cl-d8bad469{}.cl-31e46462{font-family:'Arial';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-9f1c1ce9{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-a87b1e2d{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-a1958ec5{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-ef6ffceb{width:0.674in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(153, 193, 153, 1.00);border-top: 3pt solid rgba(0, 100, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-0ad4ec36{width:0.674in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(153, 193, 153, 1.00);border-top: 2pt solid rgba(153, 193, 153, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-5af25830{width:0.674in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(153, 193, 153, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Year
```

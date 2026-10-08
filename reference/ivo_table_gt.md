# Create pretty frequency/contingency GT tables

`ivo_table_gt()` lets you easily create a GT table using pretty fonts
and colors.

## Usage

``` r
ivo_table_gt(
  df,
  color = "darkgreen",
  font_name = "Arial",
  caption = NULL,
  subtitle = NULL,
  extra_header = TRUE,
  source_note = NULL,
  mask = NULL,
  missing_string = "(Missing)",
  sums = NULL
)
```

## Arguments

- df:

  A data frame with 1-3 columns

- color:

  A named color or a color HEX code, used for the lines in the table.
  Defaults to "darkgreen".

- font_name:

  The name of the font to be used in the table. Defaults to "Arial".

- caption:

  An optional string containing a table title.

- subtitle:

  An optional string containing a table subtitle. Only usable together
  with title.

- extra_header:

  Should the variable name be displayed? Defaults to TRUE.

- source_note:

  An optional string for a table source note.

- mask:

  An optional integer to mask counts below given value.

- missing_string:

  A string used to indicate missing values. Defaults to "(Missing)".

- sums:

  An optional vector to add sums to "rows" and "cols".

## Value

A stylized `GT` table.

## Details

The functions `ivo_table_gt()` takes a `data.frame` with 1-3 columns.
The order of the columns in the `data.frame` will determine where they
will be displayed in the table. The first column will always be
displayed at the top of the table. If there are more than one column the
following 2-3 columns will be displayed to the left in order. To change
how the columns are displayed in the table; change the place of the
columns in the `data.frame` using
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).

## See also

ivo_gt_theme

## Author

Stefan Furne

## Examples

``` r
# Generate example data
example_data <- data.frame(
    Year = sample(2020:2023, 50, replace = TRUE),
    A = sample(c("Type 1", "Type 2"), 50, replace = TRUE),
    B = sample(c("Apples", "Oranges", "Bananas"), 50, replace = TRUE),
    C = sample(c("Swedish", "Norwegian", "Chilean"), 50, replace = TRUE)
)

### 1 way tables ###
data1 <- example_data |> dplyr::select(Year)

ivo_table_gt(data1)


  

Year
```

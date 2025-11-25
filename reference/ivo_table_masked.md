# Create a masked frequency/contingency table

`ivo_table_masked()` lets you easily create pretty masked tables. If you
want the table without masked values use
[`ivo_table()`](https://mthulin.github.io/ivo.table/reference/ivo_table.md)
instead.

## Usage

``` r
ivo_table_masked(
  df,
  cell = 5,
  extra_header = TRUE,
  exclude_missing = FALSE,
  missing_string = "(Missing)",
  colsums = FALSE,
  rowsums = FALSE,
  sums_string = "Total",
  caption = NA,
  highlight_cols = NULL,
  highlight_rows = NULL,
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

- cell:

  The largest value that will be masked. Defaults to 5, meaning that
  values between 1 and 5 are masked.

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

The functions
[`ivo_table()`](https://mthulin.github.io/ivo.table/reference/ivo_table.md)
and `ivo_table_masked()` takes a `data.frame` with 1-4 columns. The
order of the columns in the `data.frame` will determine where they will
be displayed in the table. The first column will always be displayed at
the top of the table. If there are more than one column the following
2-4 columns will be displayed to the left in the order 2, 3, 4. To
change how the columns are displayed in the table; change the place of
the columns in the `data.frame` using
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

### 1-way tables ###
data1 <- example_data |> dplyr::select(Year)
ivo_table_masked(data1) # No masking because all counts are >=5


.cl-8aff2446{}.cl-8af6894e{font-family:'Arial';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-8af68962{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-8af8fae4{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-8af8faf8{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-8af91560{width:0.674in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(153, 193, 153, 1.00);border-top: 3pt solid rgba(0, 100, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-8af9156a{width:0.674in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(153, 193, 153, 1.00);border-top: 2pt solid rgba(153, 193, 153, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-8af9157e{width:0.674in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(153, 193, 153, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Year
```

2020

2021

2022

2023

10

13

12

15

ivo_table_masked(data1, cell = 15) \# Counts below \<=15 are masked

| Year |      |      |      |
|------|------|------|------|
| 2020 | 2021 | 2022 | 2023 |
| 1-15 | 1-15 | 1-15 | 1-15 |

\# With pipes example_data \|\>
dplyr::[select](https://dplyr.tidyverse.org/reference/select.html)(Year)
\|\>
[ivo_table](https://mthulin.github.io/ivo.table/reference/ivo_table.md)()

| Year |      |      |      |
|------|------|------|------|
| 2020 | 2021 | 2022 | 2023 |
| 10   | 13   | 12   | 15   |

\### 2-way tables \### data2 \<- example_data \|\>
dplyr::[select](https://dplyr.tidyverse.org/reference/select.html)(A, B)
ivo_table_masked(data2)

|         | A      |        |
|---------|--------|--------|
| B       | Type 1 | Type 2 |
| Apples  | 7      | 8      |
| Bananas | 7      | 9      |
| Oranges | 12     | 7      |

ivo_table_masked(data2, cell = 7) \# Counts \<= 7 are masked

|         | A      |        |
|---------|--------|--------|
| B       | Type 1 | Type 2 |
| Apples  | 1-7    | 8      |
| Bananas | 1-7    | 9      |
| Oranges | 12     | 1-7    |

\# Row and column sums are also masked: ivo_table_masked( data2, cell =
3, colsums = TRUE, rowsums = TRUE)

|         | A      |        |       |
|---------|--------|--------|-------|
| B       | Type 1 | Type 2 | Total |
| Apples  | 7      | 8      | 15    |
| Bananas | 7      | 9      | 16    |
| Oranges | 12     | 7      | 19    |
| Total   | 26     | 24     | 50    |

\### 3-way tables \### data3 \<- example_data \|\>
dplyr::[select](https://dplyr.tidyverse.org/reference/select.html)(C, B,
Year) ivo_table_masked( data3, cell = 3, caption = "Values between 1 and
3 are masked." )

|         |      | C       |           |         |
|---------|------|---------|-----------|---------|
| B       | Year | Chilean | Norwegian | Swedish |
| Apples  | 2020 | 1-3     | 1-3       | 0       |
|         | 2021 | 4       | 1-3       | 0       |
|         | 2022 | 0       | 1-3       | 1-3     |
|         | 2023 | 1-3     | 1-3       | 1-3     |
| Bananas | 2020 | 1-3     | 1-3       | 1-3     |
|         | 2021 | 1-3     | 1-3       | 1-3     |
|         | 2022 | 0       | 1-3       | 1-3     |
|         | 2023 | 1-3     | 1-3       | 0       |
| Oranges | 2020 | 0       | 1-3       | 1-3     |
|         | 2021 | 1-3     | 0         | 1-3     |
|         | 2022 | 0       | 5         | 1-3     |
|         | 2023 | 6       | 0         | 0       |

Values between 1 and 3 are masked.

\### 4-way tables \### data4 \<- example_data \|\>
dplyr::[select](https://dplyr.tidyverse.org/reference/select.html)(Year,
B, C, A) ivo_table_masked(data4, colsums = TRUE, rowsums = TRUE)

|         |           |        | Year |      |      |      |       |
|---------|-----------|--------|------|------|------|------|-------|
| B       | C         | A      | 2020 | 2021 | 2022 | 2023 | Total |
| Apples  | Chilean   | Type 1 | 0    | 1-5  | 0    | 1-5  | NA    |
|         |           | Type 2 | 1-5  | 1-5  | 0    | 0    | NA    |
|         | Norwegian | Type 1 | 1-5  | 1-5  | 1-5  | 0    | NA    |
|         |           | Type 2 | 1-5  | 0    | 0    | 1-5  | NA    |
|         | Swedish   | Type 1 | 0    | 0    | 0    | 0    | 0     |
|         |           | Type 2 | 0    | 0    | 1-5  | 1-5  | NA    |
| Bananas | Chilean   | Type 1 | 0    | 0    | 0    | 0    | 0     |
|         |           | Type 2 | 1-5  | 1-5  | 0    | 1-5  | NA    |
|         | Norwegian | Type 1 | 0    | 0    | 1-5  | 1-5  | NA    |
|         |           | Type 2 | 1-5  | 1-5  | 1-5  | 0    | NA    |
|         | Swedish   | Type 1 | 1-5  | 1-5  | 0    | 0    | NA    |
|         |           | Type 2 | 0    | 0    | 1-5  | 0    | NA    |
| Oranges | Chilean   | Type 1 | 0    | 1-5  | 0    | 1-5  | NA    |
|         |           | Type 2 | 0    | 0    | 0    | 1-5  | NA    |
|         | Norwegian | Type 1 | 1-5  | 0    | 1-5  | 0    | NA    |
|         |           | Type 2 | 0    | 0    | 1-5  | 0    | NA    |
|         | Swedish   | Type 1 | 1-5  | 1-5  | 1-5  | 0    | NA    |
|         |           | Type 2 | 0    | 0    | 0    | 0    | 0     |
| Total   |           |        | NA   | NA   | NA   | NA   | NA    |

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


.cl-849e2ebc{}.cl-84966ca4{font-family:'Arial';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-84966cb8{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-8498c6ca{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-8498c6d4{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-8498e056{width:0.674in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(153, 193, 153, 1.00);border-top: 3pt solid rgba(0, 100, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-8498e060{width:0.674in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(153, 193, 153, 1.00);border-top: 2pt solid rgba(153, 193, 153, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-8498e06a{width:0.674in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(153, 193, 153, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Year
```

2020

2021

2022

2023

9

8

16

17

ivo_table(data1, extra_header = FALSE) \# Remove the header

| 2020 | 2021 | 2022 | 2023 |
|------|------|------|------|
| 9    | 8    | 16   | 17   |

ivo_table(data1, color = "orange") \# Change color on table lines

| Year |      |      |      |
|------|------|------|------|
| 2020 | 2021 | 2022 | 2023 |
| 9    | 8    | 16   | 17   |

ivo_table(data1, long_table = TRUE) \# Draw the table in a long format

| Year | Count |
|------|-------|
| 2020 | 9     |
| 2021 | 8     |
| 2022 | 16    |
| 2023 | 17    |

ivo_table(data1, font_name = "Garamond") \# Use a different font

| Year |      |      |      |
|------|------|------|------|
| 2020 | 2021 | 2022 | 2023 |
| 9    | 8    | 16   | 17   |

[ivo_table_masked](https://mthulin.github.io/ivo.table/reference/ivo_table_masked.md)(data1)
\# No masking because all counts are \>=5

| Year |      |      |      |
|------|------|------|------|
| 2020 | 2021 | 2022 | 2023 |
| 9    | 8    | 16   | 17   |

[ivo_table_masked](https://mthulin.github.io/ivo.table/reference/ivo_table_masked.md)(data1,
cell = 15) \# Counts below \<=15 are masked

| Year |      |      |      |
|------|------|------|------|
| 2020 | 2021 | 2022 | 2023 |
| 1-15 | 1-15 | 16   | 17   |

\# With pipes example_data \|\>
dplyr::[select](https://dplyr.tidyverse.org/reference/select.html)(Year)
\|\> ivo_table()

| Year |      |      |      |
|------|------|------|------|
| 2020 | 2021 | 2022 | 2023 |
| 9    | 8    | 16   | 17   |

\### 2-way tables \### data2 \<- example_data \|\>
dplyr::[select](https://dplyr.tidyverse.org/reference/select.html)(A, B)
data2_swap \<- example_data \|\>
dplyr::[select](https://dplyr.tidyverse.org/reference/select.html)(B, A)
\# Basic tables: ivo_table(data2)

|         | A      |        |
|---------|--------|--------|
| B       | Type 1 | Type 2 |
| Apples  | 5      | 8      |
| Bananas | 7      | 10     |
| Oranges | 8      | 12     |

ivo_table(data2_swap) \# Swap order of the columns

|        | B      |         |         |
|--------|--------|---------|---------|
| A      | Apples | Bananas | Oranges |
| Type 1 | 5      | 7       | 8       |
| Type 2 | 8      | 10      | 12      |

ivo_table(data2, colsums = TRUE) \# Add the sum of each column

|         | A      |        |
|---------|--------|--------|
| B       | Type 1 | Type 2 |
| Apples  | 5      | 8      |
| Bananas | 7      | 10     |
| Oranges | 8      | 12     |
| Total   | 20     | 30     |

ivo_table(data2, rowsums = TRUE) \# Add the sum of each row

|         | A      |        |       |
|---------|--------|--------|-------|
| B       | Type 1 | Type 2 | Total |
| Apples  | 5      | 8      | 13    |
| Bananas | 7      | 10     | 17    |
| Oranges | 8      | 12     | 20    |

ivo_table(data2, caption = "Awesome table") \# Add a caption

|         | A      |        |
|---------|--------|--------|
| B       | Type 1 | Type 2 |
| Apples  | 5      | 8      |
| Bananas | 7      | 10     |
| Oranges | 8      | 12     |

Awesome table

ivo_table(data2, highlight_cols = 3) \# Highlight column 3

|         | A      |        |
|---------|--------|--------|
| B       | Type 1 | Type 2 |
| Apples  | 5      | 8      |
| Bananas | 7      | 10     |
| Oranges | 8      | 12     |

ivo_table(data2, highlight_rows = 2, highlight_cols = 3) \# Highlight
cell at row 2 column 3

|         | A      |        |
|---------|--------|--------|
| B       | Type 1 | Type 2 |
| Apples  | 5      | 8      |
| Bananas | 7      | 10     |
| Oranges | 8      | 12     |

ivo_table(data2, bold_cols = 1) \# Make the names in the first column
bold

|         | A      |        |
|---------|--------|--------|
| B       | Type 1 | Type 2 |
| Apples  | 5      | 8      |
| Bananas | 7      | 10     |
| Oranges | 8      | 12     |

\# Tables with percentages: ivo_table(data2, percent_by = "row") \# By
row

|         | A      |        |
|---------|--------|--------|
| B       | Type 1 | Type 2 |
| Apples  | 38,5 % | 61,5 % |
| Bananas | 41,2 % | 58,8 % |
| Oranges | 40,0 % | 60,0 % |

ivo_table(data2, percent_by = "col") \# By column

|         | A      |        |
|---------|--------|--------|
| B       | Type 1 | Type 2 |
| Apples  | 25,0 % | 26,7 % |
| Bananas | 35,0 % | 33,3 % |
| Oranges | 40,0 % | 40,0 % |

ivo_table(data2, percent_by = "tot") \# By total

|         | A      |        |
|---------|--------|--------|
| B       | Type 1 | Type 2 |
| Apples  | 10,0 % | 16,0 % |
| Bananas | 14,0 % | 20,0 % |
| Oranges | 16,0 % | 24,0 % |

\# Masked tables:
[ivo_table_masked](https://mthulin.github.io/ivo.table/reference/ivo_table_masked.md)(data2)

|         | A      |        |
|---------|--------|--------|
| B       | Type 1 | Type 2 |
| Apples  | 1-5    | 8      |
| Bananas | 7      | 10     |
| Oranges | 8      | 12     |

[ivo_table_masked](https://mthulin.github.io/ivo.table/reference/ivo_table_masked.md)(data2,
cell = 7) \# Counts \<= 7 are masked

|         | A      |        |
|---------|--------|--------|
| B       | Type 1 | Type 2 |
| Apples  | 1-7    | 8      |
| Bananas | 1-7    | 10     |
| Oranges | 8      | 12     |

\# Row and column sums are also masked:
[ivo_table_masked](https://mthulin.github.io/ivo.table/reference/ivo_table_masked.md)(
data2, cell = 3, colsums = TRUE, rowsums = TRUE)

|         | A      |        |       |
|---------|--------|--------|-------|
| B       | Type 1 | Type 2 | Total |
| Apples  | 5      | 8      | 13    |
| Bananas | 7      | 10     | 17    |
| Oranges | 8      | 12     | 20    |
| Total   | 20     | 30     | 50    |

\# Add a note at the end of the table: \# (colwidths must be set to the
number of columns in the table) ivo_table(data2) \|\>
flextable::[add_footer_row](https://davidgohel.github.io/flextable/reference/add_footer_row.html)(values
= "This is a footnote.", colwidths = 3)

|                     | A      |        |
|---------------------|--------|--------|
| B                   | Type 1 | Type 2 |
| Apples              | 5      | 8      |
| Bananas             | 7      | 10     |
| Oranges             | 8      | 12     |
| This is a footnote. |        |        |

\# Add footnotes to cells in the table: ivo_table(data2) \|\>
flextable::[footnote](https://davidgohel.github.io/flextable/reference/footnote.html)(i
= [c](https://rdrr.io/r/base/c.html)(1, 3), j =
[c](https://rdrr.io/r/base/c.html)(1, 2), value =
flextable::[as_paragraph](https://davidgohel.github.io/flextable/reference/as_paragraph.html)([c](https://rdrr.io/r/base/c.html)(
"Some remark.", "Some comment.")), ref_symbols =
[c](https://rdrr.io/r/base/c.html)("a", "b"))

|                | A      |        |
|----------------|--------|--------|
| B              | Type 1 | Type 2 |
| Applesa        | 5      | 8      |
| Bananas        | 7      | 10     |
| Oranges        | 8b     | 12     |
| aSome remark.  |        |        |
| bSome comment. |        |        |

\# Add footnotes to cells in the table header: ivo_table(data2) \|\>
flextable::[footnote](https://davidgohel.github.io/flextable/reference/footnote.html)(i
= 2, j = [c](https://rdrr.io/r/base/c.html)(1, 3), value =
flextable::[as_paragraph](https://davidgohel.github.io/flextable/reference/as_paragraph.html)([c](https://rdrr.io/r/base/c.html)(
"Some remark.", "Some comment.")), ref_symbols =
[c](https://rdrr.io/r/base/c.html)("a", "b"), part = "header")

|                | A      |         |
|----------------|--------|---------|
| Ba             | Type 1 | Type 2b |
| Apples         | 5      | 8       |
| Bananas        | 7      | 10      |
| Oranges        | 8      | 12      |
| aSome remark.  |        |         |
| bSome comment. |        |         |

\### 3-way tables \### data3 \<- example_data \|\>
dplyr::[select](https://dplyr.tidyverse.org/reference/select.html)(C, B,
Year) ivo_table(data3)

|         |      | C       |           |         |
|---------|------|---------|-----------|---------|
| B       | Year | Chilean | Norwegian | Swedish |
| Apples  | 2020 | 1       | 2         | 0       |
|         | 2021 | 0       | 0         | 1       |
|         | 2022 | 3       | 2         | 0       |
|         | 2023 | 1       | 1         | 2       |
| Bananas | 2020 | 0       | 3         | 1       |
|         | 2021 | 0       | 2         | 1       |
|         | 2022 | 1       | 1         | 2       |
|         | 2023 | 1       | 2         | 3       |
| Oranges | 2020 | 1       | 1         | 0       |
|         | 2021 | 0       | 2         | 2       |
|         | 2022 | 1       | 4         | 2       |
|         | 2023 | 3       | 1         | 3       |

ivo_table(data3, colsums = TRUE, rowsums = TRUE) \# Add the sum of each
column and each row

|         |      | C       |           |         |       |
|---------|------|---------|-----------|---------|-------|
| B       | Year | Chilean | Norwegian | Swedish | Total |
| Apples  | 2020 | 1       | 2         | 0       | 3     |
|         | 2021 | 0       | 0         | 1       | 1     |
|         | 2022 | 3       | 2         | 0       | 5     |
|         | 2023 | 1       | 1         | 2       | 4     |
| Bananas | 2020 | 0       | 3         | 1       | 4     |
|         | 2021 | 0       | 2         | 1       | 3     |
|         | 2022 | 1       | 1         | 2       | 4     |
|         | 2023 | 1       | 2         | 3       | 6     |
| Oranges | 2020 | 1       | 1         | 0       | 2     |
|         | 2021 | 0       | 2         | 2       | 4     |
|         | 2022 | 1       | 4         | 2       | 7     |
|         | 2023 | 3       | 1         | 3       | 7     |
| Total   |      | 12      | 21        | 17      | 50    |

[ivo_table_masked](https://mthulin.github.io/ivo.table/reference/ivo_table_masked.md)(
data3, cell = 3, caption = "Values between 1 and 3 are masked." )

|         |      | C       |           |         |
|---------|------|---------|-----------|---------|
| B       | Year | Chilean | Norwegian | Swedish |
| Apples  | 2020 | 1-3     | 1-3       | 0       |
|         | 2021 | 0       | 0         | 1-3     |
|         | 2022 | 1-3     | 1-3       | 0       |
|         | 2023 | 1-3     | 1-3       | 1-3     |
| Bananas | 2020 | 0       | 1-3       | 1-3     |
|         | 2021 | 0       | 1-3       | 1-3     |
|         | 2022 | 1-3     | 1-3       | 1-3     |
|         | 2023 | 1-3     | 1-3       | 1-3     |
| Oranges | 2020 | 1-3     | 1-3       | 0       |
|         | 2021 | 0       | 1-3       | 1-3     |
|         | 2022 | 1-3     | 4         | 1-3     |
|         | 2023 | 1-3     | 1-3       | 1-3     |

Values between 1 and 3 are masked.

\### 4-way tables \### data4 \<- example_data \|\>
dplyr::[select](https://dplyr.tidyverse.org/reference/select.html)(Year,
B, C, A) ivo_table(data4)

|         |           |        | Year |      |      |      |
|---------|-----------|--------|------|------|------|------|
| B       | C         | A      | 2020 | 2021 | 2022 | 2023 |
| Apples  | Chilean   | Type 1 | 0    | 0    | 1    | 0    |
|         |           | Type 2 | 1    | 0    | 2    | 1    |
|         | Norwegian | Type 1 | 1    | 0    | 0    | 0    |
|         |           | Type 2 | 1    | 0    | 2    | 1    |
|         | Swedish   | Type 1 | 0    | 1    | 0    | 2    |
|         |           | Type 2 | 0    | 0    | 0    | 0    |
| Bananas | Chilean   | Type 1 | 0    | 0    | 0    | 1    |
|         |           | Type 2 | 0    | 0    | 1    | 0    |
|         | Norwegian | Type 1 | 2    | 0    | 0    | 0    |
|         |           | Type 2 | 1    | 2    | 1    | 2    |
|         | Swedish   | Type 1 | 1    | 1    | 1    | 1    |
|         |           | Type 2 | 0    | 0    | 1    | 2    |
| Oranges | Chilean   | Type 1 | 1    | 0    | 0    | 2    |
|         |           | Type 2 | 0    | 0    | 1    | 1    |
|         | Norwegian | Type 1 | 0    | 2    | 1    | 0    |
|         |           | Type 2 | 1    | 0    | 3    | 1    |
|         | Swedish   | Type 1 | 0    | 1    | 1    | 0    |
|         |           | Type 2 | 0    | 1    | 1    | 3    |

ivo_table(data4, remove_zero_rows = TRUE) \# Remove the row with zeros

|         |           |        | Year |      |      |      |
|---------|-----------|--------|------|------|------|------|
| B       | C         | A      | 2020 | 2021 | 2022 | 2023 |
| Apples  | Chilean   | Type 1 | 0    | 0    | 1    | 0    |
|         |           | Type 2 | 1    | 0    | 2    | 1    |
|         | Norwegian | Type 1 | 1    | 0    | 0    | 0    |
|         |           | Type 2 | 1    | 0    | 2    | 1    |
|         | Swedish   | Type 1 | 0    | 1    | 0    | 2    |
| Bananas | Chilean   | Type 1 | 0    | 0    | 0    | 1    |
|         |           | Type 2 | 0    | 0    | 1    | 0    |
|         | Norwegian | Type 1 | 2    | 0    | 0    | 0    |
|         |           | Type 2 | 1    | 2    | 1    | 2    |
|         | Swedish   | Type 1 | 1    | 1    | 1    | 1    |
|         |           | Type 2 | 0    | 0    | 1    | 2    |
| Oranges | Chilean   | Type 1 | 1    | 0    | 0    | 2    |
|         |           | Type 2 | 0    | 0    | 1    | 1    |
|         | Norwegian | Type 1 | 0    | 2    | 1    | 0    |
|         |           | Type 2 | 1    | 0    | 3    | 1    |
|         | Swedish   | Type 1 | 0    | 1    | 1    | 0    |
|         |           | Type 2 | 0    | 1    | 1    | 3    |

\# Add the sum of each column and each row and highlight column 6:
ivo_table( data4, colsums = TRUE, rowsums = TRUE, highlight_cols = 6)

|         |           |        | Year |      |      |      |       |
|---------|-----------|--------|------|------|------|------|-------|
| B       | C         | A      | 2020 | 2021 | 2022 | 2023 | Total |
| Apples  | Chilean   | Type 1 | 0    | 0    | 1    | 0    | 1     |
|         |           | Type 2 | 1    | 0    | 2    | 1    | 4     |
|         | Norwegian | Type 1 | 1    | 0    | 0    | 0    | 1     |
|         |           | Type 2 | 1    | 0    | 2    | 1    | 4     |
|         | Swedish   | Type 1 | 0    | 1    | 0    | 2    | 3     |
|         |           | Type 2 | 0    | 0    | 0    | 0    | 0     |
| Bananas | Chilean   | Type 1 | 0    | 0    | 0    | 1    | 1     |
|         |           | Type 2 | 0    | 0    | 1    | 0    | 1     |
|         | Norwegian | Type 1 | 2    | 0    | 0    | 0    | 2     |
|         |           | Type 2 | 1    | 2    | 1    | 2    | 6     |
|         | Swedish   | Type 1 | 1    | 1    | 1    | 1    | 4     |
|         |           | Type 2 | 0    | 0    | 1    | 2    | 3     |
| Oranges | Chilean   | Type 1 | 1    | 0    | 0    | 2    | 3     |
|         |           | Type 2 | 0    | 0    | 1    | 1    | 2     |
|         | Norwegian | Type 1 | 0    | 2    | 1    | 0    | 3     |
|         |           | Type 2 | 1    | 0    | 3    | 1    | 5     |
|         | Swedish   | Type 1 | 0    | 1    | 1    | 0    | 2     |
|         |           | Type 2 | 0    | 1    | 1    | 3    | 5     |
| Total   |           |        | 9    | 8    | 16   | 17   | 50    |

[ivo_table_masked](https://mthulin.github.io/ivo.table/reference/ivo_table_masked.md)(data4,
colsums = TRUE, rowsums = TRUE)

|         |           |        | Year |      |      |      |       |
|---------|-----------|--------|------|------|------|------|-------|
| B       | C         | A      | 2020 | 2021 | 2022 | 2023 | Total |
| Apples  | Chilean   | Type 1 | 0    | 0    | 1-5  | 0    | NA    |
|         |           | Type 2 | 1-5  | 0    | 1-5  | 1-5  | NA    |
|         | Norwegian | Type 1 | 1-5  | 0    | 0    | 0    | NA    |
|         |           | Type 2 | 1-5  | 0    | 1-5  | 1-5  | NA    |
|         | Swedish   | Type 1 | 0    | 1-5  | 0    | 1-5  | NA    |
|         |           | Type 2 | 0    | 0    | 0    | 0    | 0     |
| Bananas | Chilean   | Type 1 | 0    | 0    | 0    | 1-5  | NA    |
|         |           | Type 2 | 0    | 0    | 1-5  | 0    | NA    |
|         | Norwegian | Type 1 | 1-5  | 0    | 0    | 0    | NA    |
|         |           | Type 2 | 1-5  | 1-5  | 1-5  | 1-5  | NA    |
|         | Swedish   | Type 1 | 1-5  | 1-5  | 1-5  | 1-5  | NA    |
|         |           | Type 2 | 0    | 0    | 1-5  | 1-5  | NA    |
| Oranges | Chilean   | Type 1 | 1-5  | 0    | 0    | 1-5  | NA    |
|         |           | Type 2 | 0    | 0    | 1-5  | 1-5  | NA    |
|         | Norwegian | Type 1 | 0    | 1-5  | 1-5  | 0    | NA    |
|         |           | Type 2 | 1-5  | 0    | 1-5  | 1-5  | NA    |
|         | Swedish   | Type 1 | 0    | 1-5  | 1-5  | 0    | NA    |
|         |           | Type 2 | 0    | 1-5  | 1-5  | 1-5  | NA    |
| Total   |           |        | NA   | NA   | NA   | NA   | NA    |

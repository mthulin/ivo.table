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

Count

2020

17

2021

11

2022

14

2023

8

ivo_table_gt(data1, extra_header = FALSE) \# Remove the header

| Year | Count |
|:-----|------:|
| 2020 |    17 |
| 2021 |    11 |
| 2022 |    14 |
| 2023 |     8 |

ivo_table_gt(data1, color = "orange") \# Change color on table lines

| Year | Count |
|:-----|------:|
| 2020 |    17 |
| 2021 |    11 |
| 2022 |    14 |
| 2023 |     8 |

ivo_table_gt(data1, mask = 15) \# Counts below \<=15 are masked

| Year |  Count |
|:-----|-------:|
| 2020 |     17 |
| 2021 | 1 - 15 |
| 2022 | 1 - 15 |
| 2023 | 1 - 15 |

\# With pipes example_data \|\>
dplyr::[select](https://dplyr.tidyverse.org/reference/select.html)(Year)
\|\> ivo_table_gt()

| Year | Count |
|:-----|------:|
| 2020 |    17 |
| 2021 |    11 |
| 2022 |    14 |
| 2023 |     8 |

\### 2-way tables \### data2 \<- example_data \|\>
dplyr::[select](https://dplyr.tidyverse.org/reference/select.html)(A, B)
data2_swap \<- example_data \|\>
dplyr::[select](https://dplyr.tidyverse.org/reference/select.html)(B, A)
\# Basic tables: ivo_table_gt(data2)

[TABLE]

ivo_table_gt(data2_swap) \# Swap order of the columns

[TABLE]

ivo_table_gt(data2, sums = "cols") \# Add the sum of each column

[TABLE]

ivo_table_gt(data2, sums = "rows") \# Add the sum of each row

[TABLE]

ivo_table_gt(data2, sums = [c](https://rdrr.io/r/base/c.html)("cols",
"rows")) \# Add the sum of each row and column

[TABLE]

ivo_table_gt(data2, caption = "Awesome table") \# Add a caption

[TABLE]

ivo_table_gt(data2, caption = "Awesome table", subtitle = "It's really
awesome" ) \# Add a subtitle for the title

[TABLE]

\# Masked tables: ivo_table_gt(data2, mask = 7) \# Counts \<= 7 are
masked

[TABLE]

\# Row and column sums are also masked: ivo_table_gt( data2, mask = 3,
sums = [c](https://rdrr.io/r/base/c.html)("cols", "rows"), )

[TABLE]

\# Add a note at the end of the table: \# (colwidths must be set to the
number of columns in the table) ivo_table_gt(data2, source_note = "This
is a footnote.")

[TABLE]

\### 3-way tables \### data3 \<- example_data \|\>
dplyr::[select](https://dplyr.tidyverse.org/reference/select.html)(C, B,
Year) ivo_table_gt(data3)

[TABLE]

ivo_table_gt(data3, sums = [c](https://rdrr.io/r/base/c.html)("cols",
"rows")) \# Add the sum of each column and each row

[TABLE]

ivo_table_gt( data3, mask = 3, caption = "Values between 1 and 3 are
masked." )

[TABLE]

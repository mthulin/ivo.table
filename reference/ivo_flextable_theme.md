# Use nice fonts and colors for tables

A flextable theme for ivo_table objects.

## Usage

``` r
ivo_flextable_theme(
  x,
  kway = 2,
  rowsums = FALSE,
  caption = NA,
  highlight_cols = NULL,
  highlight_rows = NULL,
  color = "darkgreen",
  font_name = "Arial",
  bold_cols = NULL
)
```

## Arguments

- x:

  A flextable.

- kway:

  The number of "horizontal" variables in the table.

- rowsums:

  A logical, saying whether the rightmost column in the table contains
  the sum of each row. Defaults to FALSE.

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

## Value

A styled flextable.

## Details

The default settings use a dark green color and a sans serif font.

## Author

Måns Thulin

## Examples

``` r
library(tidyr)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(flextable)
example_data <- data.frame(Year = sample(2020:2023, 50, replace = TRUE),
A = sample(c("Type 1", "Type 2"), 50, replace = TRUE),
B = sample(c("Apples", "Oranges", "Bananas"), 50, replace = TRUE),
C = sample(c("Swedish", "Norwegian", "Chilean"), 50, replace = TRUE))
example_data |> select(B, A) |>
  ftable(exclude=NULL) |>
  data.frame() |>
  spread(A, Freq) |>
  regulartable() |>
  ivo_flextable_theme()


.cl-d204132e{}.cl-d1fcfefe{font-family:'Arial';font-size:11pt;font-weight:bold;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-d1fcff12{font-family:'Arial';font-size:11pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-d1fffb5e{margin:0;text-align:left;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-d1fffb72{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:5pt;padding-top:5pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-d20019a4{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(153, 193, 153, 1.00);border-top: 3pt solid rgba(0, 100, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-d20019ae{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(153, 193, 153, 1.00);border-top: 3pt solid rgba(0, 100, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-d20019b8{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0.5pt solid rgba(153, 193, 153, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-d20019c2{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0.5pt solid rgba(153, 193, 153, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-d20019c3{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0.5pt solid rgba(153, 193, 153, 1.00);border-top: 0.5pt solid rgba(153, 193, 153, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-d20019cc{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0.5pt solid rgba(153, 193, 153, 1.00);border-top: 0.5pt solid rgba(153, 193, 153, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-d20019d6{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(153, 193, 153, 1.00);border-top: 0.5pt solid rgba(153, 193, 153, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-d20019e0{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(153, 193, 153, 1.00);border-top: 0.5pt solid rgba(153, 193, 153, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


B
```

Type 1

Type 2

Apples

11

12

Bananas

6

9

Oranges

4

8

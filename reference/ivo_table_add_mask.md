# Add masking (censoring) to a table

Table masking using cell counts.

## Usage

``` r
ivo_table_add_mask(df, cell = 5)
```

## Arguments

- df:

  A data frame containing a column called "Freq", e.g. a frequency table
  created using `ftable(exclude=NULL) |> data.frame()`.

- cell:

  The cell count at which masking should be used. Cell counts between 1
  and this number will be masked. The default is 5.

## Value

A data frame with masked cell counts.

## Details

Masking is used to prevent the distribution of tables where individuals
could be identified.

## See also

ivo_table_masked for masked tables.

## Author

Måns Thulin

## Examples

``` r
library(dplyr)
example_data <- data.frame(Year = sample(2020:2023, 50, replace = TRUE),
A = sample(c("Type 1", "Type 2"), 50, replace = TRUE),
B = sample(c("Apples", "Oranges", "Bananas"), 50, replace = TRUE),
C = sample(c("Swedish", "Norwegian", "Chilean"), 50, replace = TRUE))
# With masking limit set at 7:
example_data |> select(Year, A) |>
  ftable(exclude=NULL) |>
  data.frame() |>
  ivo_table_add_mask(cell = 7)
#>   Year      A Freq
#> 1 2020 Type 1    8
#> 2 2021 Type 1  1-7
#> 3 2022 Type 1  1-7
#> 4 2023 Type 1  1-7
#> 5 2020 Type 2  1-7
#> 6 2021 Type 2  1-7
#> 7 2022 Type 2  1-7
#> 8 2023 Type 2  1-7
```

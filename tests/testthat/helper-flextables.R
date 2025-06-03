library(janitor)
library(palmerpenguins)
library(dplyr)

make_cases <- function() {
  tables <- list(
    oneway = penguins |> select(species) |> clean_names("sentence"),
    twoway = penguins |> mutate(bill_length_mm = cut(bill_length_mm, 3)) |>
      select(species, bill_length_mm) |> clean_names("sentence") # ,
    # threeway = penguins |> mutate(bill_length_mm = cut(bill_length_mm, 3)) |>
    #   select(species, bill_length_mm, island) |> clean_names("sentence"),
    # fourway = penguins |> mutate(bill_length_mm = cut(bill_length_mm, 3)) |>
    #  select(species, bill_length_mm, island, sex) |> clean_names("sentence")
  )

  expand_grid(
    test_data = tables,
    extra_header = c(FALSE, TRUE),
    exclude_missing = c(FALSE, TRUE),
    colsums = c(FALSE, TRUE),
    rowsums = c(FALSE, TRUE),
    long_table = c(FALSE, TRUE),
  ) |>
    filter(!(grepl("oneway", names(test_data)) & colsums & !long_table)) |> # Exclude colsums for one-way tables since it's not relevant
    filter(!(grepl("oneway", names(test_data)) & rowsums & long_table)) |> # Exclude long_table for all tables that are not one-way
    mutate(.test_name = paste(names(test_data), row_number(), sep = "_"))
}

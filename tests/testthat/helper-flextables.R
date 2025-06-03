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

normalize_html <- function(input_file) {
  html <- readLines(input_file)
  html_text <- paste(html, collapse = "\n")

  # Find all randomized tokens used in the HTML output from a flextable
  # A token looks something like: "cl-ba57f9y1"
  cl_matches <- gregexpr("cl-[a-f0-9]{6,}", html_text, perl = TRUE)
  cl_tokens <- unique(regmatches(html_text, cl_matches)[[1]])

  # Create mapping to deterministic names instead (cl-1, cl-2, cl-3 and so on)
  cl_map <- setNames(
    paste0("cl-", seq_along(cl_tokens)),
    cl_tokens
  )

  # Now replace all the randomized tokens
  for (original in names(cl_map)) {
    html_text <- gsub(original, cl_map[[original]], html_text, fixed = TRUE)
  }

  # And write back the cleaned HTML
  writeLines(html_text, input_file)
}

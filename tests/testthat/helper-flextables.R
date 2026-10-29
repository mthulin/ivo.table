#' @title Create test cases for flextable output
#' @description Creates a \code{data.frame} with parameterized test to ensure consistent table visual output.
#' @return A \code{data.frame} with parameters used for function calls.
#' @noRd
#' @importFrom dplyr select mutate filter row_number
#' @importFrom tidyr expand_grid
#' @importFrom janitor clean_names
make_flextable_cases <- function(table_list) {
  expand_grid(
    test_data = table_list,
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

#' @title Normalize HTML from a flextable output
#' @description Removes randomized tokens to ensure deterministic output for tables in a HTML file
#' @param file_path Path to HTML file containing the table.
#' @return The same path used for input.
#' @noRd
normalize_html <- function(file_path) {
  html <- readLines(file_path)
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
  writeLines(html_text, file_path)

  # Return the path
  return(file_path)
}

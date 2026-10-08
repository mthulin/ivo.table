#' @title Create test cases for flextable output
#' @description Creates a \code{data.frame} with parameterized test to ensure consistent table visual output.
#' @return A \code{data.frame} with parameters used for function calls.
#' @noRd
#' @importFrom dplyr row_number mutate, across, everything
#' @importFrom tidyr expand_grid crossing
make_gt_cases <- function(table_list) {
    expand_grid(
        test_data = table_list,
        extra_header = c(FALSE, TRUE),
        sums = c(c("rows", "cols"), "rows", "cols"),
        caption = c("Penguins", NA),
        subtitle = c("So many penguins", NA),
        mask = c(50, NA),
        source_note = c("Sauce", NA)
    ) |>
        mutate(
            .test_name = paste(names(test_data), row_number(), sep = "_")
        )
}


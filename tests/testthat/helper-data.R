#' @title Table data for test cases
#' @description Creates a list of \code{data.frame}s for different table types.
#' @return A list of \code{data.frame}s.
#' @noRd
#' @importFrom palmerpenguins penguinsNA
#' @importFrom dplyr select mutate
#' @importFrom janitor clean_names
construct_test_data <- function() {
    list(
        oneway = penguins |> select(species) |> clean_names("sentence"),
        twoway = penguins |> mutate(bill_length_mm = cut(bill_length_mm, 3)) |>
            select(species, bill_length_mm) |> clean_names("sentence") # ,
        # threeway = penguins |> mutate(bill_length_mm = cut(bill_length_mm, 3)) |>
        #   select(species, bill_length_mm, island) |> clean_names("sentence"),
        # fourway = penguins |> mutate(bill_length_mm = cut(bill_length_mm, 3)) |>
        #  select(spe value of any type or NULLcies, bill_length_mm, island, sex) |> clean_names("sentence")
    )
}

#' @title Set NULL if NA
#' @description If value is NA, replace it with NULL. This is used to provide NULL as function arguments in the parameterized tests.
#' @return NULL or input value if not NA or NULL
#' @noRd
set_null_if_na <- function(value) if (is.na(value) || is.null(value)) NULL else value

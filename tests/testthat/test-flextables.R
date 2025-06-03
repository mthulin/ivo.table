library(janitor)
library(palmerpenguins)
library(flextable)
library(patrick)

with_parameters_test_that(
    "the ivo_table flextable output is consistent",
    {
        input <- test_data |>
            ivo_table(
                extra_header = extra_header,
                exclude_missing = exclude_missing,
                colsums = colsums,
                rowsums = rowsums,
                long_table = long_table
            ) |>
            save_as_image(path = paste0(tempdir(), "/", .test_name, ".png"))


        expect_snapshot_file(path = input)
    },
    .cases = make_cases()
)

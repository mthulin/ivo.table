library(janitor)
library(palmerpenguins)
library(gt)
library(patrick)

table_data <- construct_test_data()

with_parameters_test_that(
    "the ivo_table gt output is consistent",
    {
        html <- test_data |>
            ivo_table_gt(
                extra_header = extra_header,
                sums = sums,
                caption = set_null_if_na(caption),
                subtitle = set_null_if_na(subtitle),
                mask = set_null_if_na(mask),
                source_note = set_null_if_na(source_note)
            ) |>
            as_raw_html() |>
            # Ensure deterministic output by replacing the random div id's
            (\(txt) gsub('id="[^"]+"', paste0('id="', .test_name, '"'), txt, perl = TRUE))() # Or use stringr, not sure yet (str_replace('id="[^"]+"', paste0('id="', .test_name, '"')))


        path <- paste0(tempdir(), "/", .test_name, ".html")
        writeLines(html, path)

        expect_snapshot_file(path = path)
    },
    .cases = make_gt_cases(table_data)
)

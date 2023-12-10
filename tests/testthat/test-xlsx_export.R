test_that("wrong values give error", {
  data1 <- dplyr::starwars |> dplyr::select(homeworld) |> flextable::flextable()
  data2 <- dplyr::starwars |> dplyr::select(sex, species) |> flextable::flextable()

  expect_error(ivo_flextable_to_xlsx(1))
  expect_error(ivo_flextable_to_xlsx("1"))
  expect_error(ivo_flextable_to_xlsx(data1, 1))
  expect_error(ivo_flextable_to_xlsx(data1, data2))
  expect_error(ivo_flextable_to_xlsx(list(data1, data2), 1))
  expect_error(ivo_flextable_to_xlsx(list(data1, data2), data2))
  expect_error(ivo_flextable_to_xlsx(data1, format = 1))
  expect_error(ivo_flextable_to_xlsx(data1, format = "1"))
  expect_error(ivo_flextable_to_xlsx(data1, colwidths = 1))
  expect_error(ivo_flextable_to_xlsx(data1, colwidths = "1"))

})

# File snapshots to test and verify correct output
test_that("output is not different from snapshot", {

  test_df <- dplyr::starwars |> 
    dplyr::filter(homeworld %in% c("Naboo", "Tatooine", "Coruscant", "Kamino"))
  
  # A 2-way table with caption, rest default
  test_tbl1 <- test_df |>
    dplyr::select(homeworld, species) |>
    ivo_table(caption = "A table with a caption")


  # A 3-way table with caption that is really long and larger font size than default and with no extra_header
  test_tbl2 <- test_df |>
    dplyr::select(homeworld, species, sex) |>
    ivo_table(extra_header = FALSE, caption = "This is a caption that is fairly long so that we can properly evaluate the results in cases such as this one")

  # A 3-way table with no caption
  test_tbl3 <- test_df |>
    dplyr::select(homeworld, species, sex) |>
    ivo_table(extra_header = FALSE)

  # A 3-way table with caption, different color and sums
  test_tbl4 <- dplyr::starwars |>
    dplyr::select(homeworld, species, sex) |>
    ivo_table(caption = "Sums and a different color", color = "orange", rowsums = TRUE, colsums = TRUE)

  # A 4-way table in a different color and row sums
  test_tbl5 <- test_df |>
    dplyr::select(homeworld, eye_color, gender) |>
    ivo_table(caption = "A different color than default", color = "orange", rowsums = TRUE)

  # A 4-way table with a highlighted cell, col sums and no extra header
  test_tbl6 <- dplyr::starwars |>
    dplyr::select(homeworld, species, sex) |>
    ivo_table(caption = "A highlighted cell", highlight_cols = 2, highlight_rows = 4, colsums = TRUE, extra_header = FALSE)

  # A masked 3-way table
  test_tbl7 <- test_df |>
    dplyr::select(homeworld, species, gender) |>
    ivo_table_masked(caption = "A masked table")

  # A 2-way table with sums inga mergade cieller, inklusive summor och med gridlines
  test_tbl8 <- test_df |>
    dplyr::select(homeworld, species, eye_color) |>
    ivo_table(colsums = TRUE, rowsums = TRUE)

  # En namnad lista med alla tabeller samlade
  test_tbl_list <- list("Tbl1" = test_tbl1, "Tbl2" = test_tbl2, "Tbl3" = test_tbl3, "Tbl4" = test_tbl4, "Tbl5" = test_tbl5, "Tbl6" = test_tbl6, "Tbl7" = test_tbl7, "Tbl8" = test_tbl8)

  # A custom function to compare xlsx files since they are bundled with timestamps
  # of when they are created. This make every file "unique" which is a problem when using default snapshot testing.
  # This function unpacks the old and the new file, picks out the data and style sheets (XML) and the compare these
  # in order to verify that the output is in fact identical to the user.
  unpack_xlsx <- function(xlsx, path) {

    xlsx_name <- basename(xlsx)

    unpacked_dir <- paste0(xlsx_name, "_unpacked")

    to_dir <- normalizePath(file.path(path, unpacked_dir), mustWork = FALSE)

    zip_file <- paste0(to_dir, ".zip")

    file.copy(xlsx, zip_file)

    dir.create(to_dir, showWarnings = FALSE)
    utils::unzip(zip_file, exdir = to_dir)

    sheets_dir <- normalizePath(file.path(to_dir, "xl", "worksheets"), mustWork = FALSE)
    xml_files <- list.files(sheets_dir, pattern = "*.xml")

    raw_data <- c()

    for (file in xml_files) {

      file_path <- normalizePath(file.path(sheets_dir, file))
      worksheet <- read_file(file_path)
      raw_data <- append(raw_data, worksheet)
    }

    styles_path <- normalizePath(file.path(path, unpacked_dir, "xl", "styles.xml"), mustWork = FALSE)
    styles <- read_file(styles_path)

    raw_data <- append(raw_data, styles)

    raw_data
  }

  read_file <- function(file_path) {

    # Open file in binary mode
    con <- file(file_path, "rb")

    # Read data
    raw_data <-readBin(con, what = "raw", n = file.info(file_path)$size)

    # Close file
    close(con)

    raw_data
  }

  compare_xlsx <- function(old, new) {

    # Extract the path to the working directory from the new file
    # so we can pass it along to unpack_xlsx
    workdir <- dirname(new)

    new_unpacked <- unpack_xlsx(new, workdir)
    old_unpacked <- unpack_xlsx(old, workdir)

    identical(new_unpacked, old_unpacked)

  }
  expect_snapshot_xlsx <- function(name, args, path) {

    name <- paste0(name, ".xlsx")

    tmp_file <- tempfile(tmpdir = path, fileext = ".xlsx")
    announce_snapshot_file(name = name)

    args <- append(args, list("filename" = tmp_file))

    suppressMessages(do.call("ivo_flextable_to_xlsx", args))

    expect_snapshot_file(tmp_file, name, compare = compare_xlsx)
  }

  # We create a working directory in tmpdir for file comparison.
  # But before that we check if the path already exists, in which case remove it
  # first so we don't get strange test results.
  tmp_path <- paste0(tempdir(check = TRUE), "/ivo-table-tests")

  if (dir.exists(tmp_path)) unlink(tmp_path, recursive = TRUE)
  dir.create(tmp_path, recursive = TRUE)

  expect_snapshot_xlsx("Table_1", args = list("tables" = test_tbl1), path = tmp_path)
  expect_snapshot_xlsx("Table_2", args = list("tables" = test_tbl2, "caption_size" = 18), path = tmp_path)
  expect_snapshot_xlsx("Table_3", args = list("tables" = test_tbl3), path = tmp_path)
  expect_snapshot_xlsx("Table_4", args = list("tables" = test_tbl4), path = tmp_path)
  expect_snapshot_xlsx("Table_5", args = list("tables" = test_tbl5), path = tmp_path)
  expect_snapshot_xlsx("Table_6", args = list("tables" = test_tbl6), path = tmp_path)
  expect_snapshot_xlsx("Table_7", args = list("tables" = test_tbl7), path = tmp_path)
  expect_snapshot_xlsx("Table_8", args = list("tables" = test_tbl8, "merge_cells" = FALSE, "gridlines" = TRUE), path = tmp_path)
  expect_snapshot_xlsx("Table_List", args = list("tables" = test_tbl_list), path = tmp_path)
})

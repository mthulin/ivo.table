utils::globalVariables(".data")

# Internal functions used for adding headers, dealing with missing data,----
# adding the sum of each column and converting frequencies to percentages:

ivo_add_extra_header <- function(df, name_v4, ncol_v4, kway = 2, add = TRUE, rowsums)
{
  if(add) {
    if(rowsums) { flextable::add_header_row(df, values = c("", name_v4, ""), colwidths = c(kway - 1, ncol_v4, 1)) } else {
      flextable::add_header_row(df, values = c("", name_v4), colwidths = c(kway - 1, ncol_v4))
    }
  } else { df }
}

ivo_add_extra_header_1way <- function(df, name_v4, ncol_v4, add = TRUE)
{
  if(add) { df |> flextable::add_header_row(values = c(name_v4), colwidths = c(ncol_v4))} else { df }
}

ivo_excl_missing <- function(df, exclude_missing = FALSE, missing_string = "(Missing)")
{
  if(!exclude_missing) { df[is.na(df)] <- missing_string }
  return(df)
}

ivo_num_sum <- function(x)
{
  suppressWarnings(x <- sum(as.numeric(unlist(x))))
  x[is.na(x)] <- "-"
  return(x)
}

ivo_add_row_sums <- function(df, new_row, colsums)
{
  if(colsums) { flextable::add_footer(df, values = new_row) } else { df }
}

ivo_to_percent <- function(df, k = 2, margin = NULL)
{
  df[, k:ncol(df)] <- proportions(as.matrix(df[, k:ncol(df)]), margin = margin)*100
  return(df)
}

# The function below is used to fix a bug concerning colors with alpha in flextable.
# It converts transparent colors to non-transparent. Without this, the wrong
# colors are used on non-white backgrounds.
# (Conversion: 8-digit HEX -> RGBA -> RGB -> 6-digit HEX)
ivo_hex8_to_hex6 <- function(HEX, background = "white")
{
  RGBA_color <- grDevices::col2rgb(HEX, alpha = TRUE)
  bg_color <- grDevices::col2rgb(background)

  RGB_color <- ((1-RGBA_color[4]/255) * bg_color + RGBA_color[4]/255 * RGBA_color[1:3])
  return(grDevices::rgb(base::t(RGB_color), maxColorValue = 255))
}

# Helper functions----
# Helper functions for two-way tables:
ivo_tab2_step1 <- function(df, v1, v4, exclude_missing, missing_string)
{
  # Filter the data frame and create a frequency table
  df |> dplyr::select({{v1}}, {{v4}}) |>
    ivo_excl_missing(exclude_missing, missing_string) |>
    stats::ftable() |>
    base::data.frame() |>
    `colnames<-`(c({{v1}}, {{v4}}, "Freq"))
}


ivo_tab2_step2 <- function(df, v1, v4, extra_header, colsums, rowsums, percent_by, remove_zero_rows, sums_string)
{
  Total <- NULL
  ncol_v4 <- df |> dplyr::pull({{v4}}) |> unique() |> length()
  name_v4 <- df |> dplyr::select({{v4}}) |> names()

  # Format the table
  df |> tidyr::pivot_wider(names_from = {{v4}} , values_from = "Freq") -> df

  # Show numbers as percentages if requested:
  if(!is.na(percent_by)) {
    perc_margin <- switch(percent_by,
                          rows = 1,
                          row = 1,
                          cols = 2,
                          col = 2,
                          columns = 2,
                          total = NULL,
                          tot = NULL)
    df |> ivo_to_percent(k = 2, margin = perc_margin) -> df
    new_row <- NA } else {

      # Add column or the sum of each column if requested:
      df$Total <- unlist(apply(df[,-1], 1, ivo_num_sum))
      if(remove_zero_rows) { df |> dplyr::filter(Total > 0 | Total == "-") -> df }
      if(rowsums) { names(df)[names(df) == "Total"] <- sums_string }
      if(!rowsums) { df |> dplyr::select(-Total) -> df }
      new_row <- c(apply(df, 2, ivo_num_sum), sums_string)
      names(new_row)[length(new_row)] <- names(df)[1]
    }

  df |>
    flextable::regulartable() |>
    ivo_add_extra_header(name_v4, ncol_v4, 2, add = extra_header, rowsums) |>
    ivo_add_row_sums(new_row, colsums) |>
    flextable::autofit()
}

# Helper functions for three-way tables:
ivo_tab3_step1 <- function(df, v1, v3, v4, exclude_missing, missing_string)
{
  # Filter the data frame and create a frequency table
  df |> dplyr::select({{v1}}, {{v3}}, {{v4}}) |>
    ivo_excl_missing(exclude_missing, missing_string) |>
    stats::ftable() |>
    base::data.frame() |>
    # `colnames<-`(c({{v1}}, {{v3}}, {{v4}}, "Freq")) -> df
    # df[order(df[,1]),]
    `colnames<-`(c({{v1}}, {{v3}}, {{v4}}, "Freq")) |>
    dplyr::arrange(.data[[v1]])
}

ivo_tab3_step2 <- function(df, v1, v3, v4, extra_header, colsums, rowsums, percent_by, remove_zero_rows, sums_string)
{

  Total <- NULL
  ncol_v4 <- df |> dplyr::pull({{v4}}) |> unique() |> length()
  name_v4 <- df |> dplyr::select({{v4}}) |> names()
  name_v1 <- df |> dplyr::select({{v1}}) |> names()

  # Format the table
  df |> tidyr::pivot_wider(names_from = {{v4}} , values_from = "Freq")  -> df

  # Show numbers as percentages if requested:
  if(!is.na(percent_by)) {
    perc_margin <- switch(percent_by,
                          rows = 1,
                          row = 1,
                          cols = 2,
                          col = 2,
                          columns = 2,
                          total = NULL,
                          tot = NULL)
    df |> ivo_to_percent(k = 3, margin = perc_margin) -> df
    new_row <- NA } else {

      # Add column or the sum of each column if requested:
      df$Total <- unlist(apply(df[,c(-1, -2)], 1, ivo_num_sum))
      if(remove_zero_rows) { df |> dplyr::filter(Total > 0 | Total == "-") -> df }
      if(rowsums) { names(df)[names(df) == "Total"] <- sums_string }
      if(!rowsums) { df |> dplyr::select(-Total) -> df }
      new_row <- c(apply(df[,c(-1,-2)], 2, ivo_num_sum), sums_string, NA)
      names(new_row)[length(new_row) + c(-1, 0)] <- names(df)[1:2]
    }


  df |>
    flextable::regulartable() |>
    ivo_add_extra_header(name_v4, ncol_v4, 3, add = extra_header, rowsums) |>
    ivo_add_row_sums(new_row, colsums) |>
    flextable::merge_v(j = name_v1) |>
    flextable::autofit()
}

# Helper functions for four-way tables:
ivo_tab4_step1 <- function(df, v1, v2, v3, v4, exclude_missing, missing_string)
{
  # Filter the data frame and create a frequency table
  df |> dplyr::select({{v1}}, {{v2}}, {{v3}}, {{v4}}) |>
    ivo_excl_missing(exclude_missing, missing_string) |>
    stats::ftable() |>
    base::data.frame() |>
    `colnames<-`(c({{v1}}, {{v2}}, {{v3}}, {{v4}}, "Freq")) |>
    dplyr::arrange(.data[[v1]], .data[[v2]])
  # -> df
  #   df[do.call(order, list(df[,1], df[,2])),]
}

ivo_tab4_step2 <- function(df, v1, v2, v3, v4, extra_header, colsums, rowsums, percent_by, remove_zero_rows, sums_string)
{

  Total <- NULL
  ncol_v4 <- df |> dplyr::pull({{v4}}) |> unique() |> length()
  name_v4 <- df |> dplyr::select({{v4}}) |> names()
  name_v2 <- df |> dplyr::select({{v2}}) |> names()
  name_v1 <- df |> dplyr::select({{v1}}) |> names()

  # Format the table
  df |> tidyr::pivot_wider(names_from = {{v4}} , values_from = "Freq") -> df

  # Show numbers as percentages if requested:
  if(!is.na(percent_by)) {
    perc_margin <- switch(percent_by,
                          rows = 1,
                          row = 1,
                          cols = 2,
                          col = 2,
                          columns = 2,
                          total = NULL,
                          tot = NULL)
    df |> ivo_to_percent(k = 4, margin = perc_margin) -> df
    new_row <- NA } else {

      # Add column or the sum of each column if requested:
      df$Total <- unlist(apply(df[,c(-1, -2, -3)], 1, ivo_num_sum))
      if(remove_zero_rows) { df |> dplyr::filter(Total > 0 | Total == "-") -> df }
      if(rowsums) { names(df)[names(df) == "Total"] <- sums_string }
      if(!rowsums) { df |> dplyr::select(-Total) -> df }
      new_row <- c(apply(df[,c(-1,-2, -3)], 2, ivo_num_sum), sums_string, NA, NA)
      names(new_row)[length(new_row) + c(-2, -1, 0)] <- names(df)[1:3]
    }

  df |>
    flextable::regulartable() |>
    ivo_add_extra_header(name_v4, ncol_v4, 4, add = extra_header, rowsums) |>
    ivo_add_row_sums(new_row, colsums) |>
    flextable::merge_v(j = name_v1) |>
    flextable::merge_v(j = name_v2)  |>
    flextable::autofit()
}


# _____________________________________________________________
# IVO internal table functions to create correct ivo_table()----

ivo_table_1way <- function(df, varleft, extra_header = TRUE, exclude_missing = FALSE, missing_string = "(Missing)", caption = NA, highlight_cols = NULL, highlight_rows = NULL, percent_by = NA, color = "darkgreen", font_name = "Arial", long_table = FALSE)
{

  ncol_v4 <- df |> dplyr::pull({{varleft}}) |> unique() |> length()
  name_v4 <- df |> dplyr::select({{varleft}}) |> names()

  # Create the table
  if(long_table) {
  df |> dplyr::select({{varleft}}) |>
    ivo_excl_missing(exclude_missing, missing_string) |>
    stats::ftable() |>
    base::data.frame() |>
    `colnames<-`(c({{varleft}}, "Count")) -> df

    if(!is.na(percent_by)) {
      perc_margin <- switch(percent_by,
                            rows = 1,
                            row = 1,
                            cols = 2,
                            col = 2,
                            columns = 2,
                            total = NULL,
                            tot = NULL)
      df |> ivo_to_percent(k = 2, margin = perc_margin) -> df
      new_row <- NA }


    df |> flextable::regulartable() |>
    flextable::autofit() |>
    ivo_flextable_theme(1, rowsums = FALSE, caption, highlight_cols, highlight_rows, color, font_name) -> df } else {
      df |> dplyr::select({{varleft}}) |>
        ivo_excl_missing(exclude_missing, missing_string) |>
        stats::ftable() |>
        base::data.frame() |>
        `colnames<-`(c({{varleft}}, "Freq")) |>
        tidyr::pivot_wider(names_from = {{varleft}} , values_from = "Freq") -> df

      if(!is.na(percent_by)) {
        perc_margin <- switch(percent_by,
                              rows = 1,
                              row = 1,
                              cols = 2,
                              col = 2,
                              columns = 2,
                              total = NULL,
                              tot = NULL)
        df |> ivo_to_percent(k = 1, margin = perc_margin) -> df
        new_row <- NA }

        df |> flextable::regulartable() |>
        ivo_add_extra_header_1way(name_v4, ncol_v4, extra_header) |>
        flextable::autofit() |>
        ivo_flextable_theme(1, rowsums = FALSE, caption, highlight_cols, highlight_rows, color, font_name) -> df
    }

  if(!is.na(percent_by)) { df |> flextable::colformat_double(decimal.mark = ",", suffix = " %", big.mark = " ") } else { df }
}


ivo_masked_table_1way <- function(df, varleft, cell = 5, extra_header = TRUE, exclude_missing = FALSE, missing_string = "(Missing)", caption = NA, highlight_cols = NULL, highlight_rows = NULL, color = "darkgreen", font_name = "Arial",  long_table = FALSE){

  ncol_v4 <- df |> dplyr::pull({{varleft}}) |> unique() |> length()
  name_v4 <- df |> dplyr::select({{varleft}}) |> names()

  if(long_table) {
  df |> dplyr::select({{varleft}}) |>
      ivo_excl_missing(exclude_missing, missing_string) |>
      stats::ftable() |>
      base::data.frame() |>
      ivo_table_add_mask(cell)  |>
      `colnames<-`(c({{varleft}}, "Count")) |>
      flextable::regulartable() |>
      flextable::autofit() |>
      ivo_flextable_theme(1, rowsums = FALSE, caption, highlight_cols, highlight_rows, color, font_name) } else {
        df |> dplyr::select({{varleft}}) |> ivo_excl_missing(exclude_missing, missing_string) |>
        stats::ftable() |>
        base::data.frame() |>
        ivo_table_add_mask(cell)  |>
        `colnames<-`(c({{varleft}}, "Freq")) |>
        tidyr::pivot_wider(names_from = {{varleft}} , values_from = "Freq") |>
        flextable::regulartable() |>
        ivo_add_extra_header_1way(name_v4, ncol_v4, extra_header) |>
        flextable::autofit() |>
        ivo_flextable_theme(1, rowsums = FALSE, caption, highlight_cols, highlight_rows, color, font_name)
    }


}


ivo_table_2way <- function(df, varleft, vartop, extra_header = TRUE, exclude_missing = FALSE, missing_string = "(Missing)", colsums = FALSE, rowsums = FALSE, sums_string = "Total", caption = NA, highlight_cols = NULL, highlight_rows = NULL, percent_by = NA, color = "darkgreen", font_name = "Arial",  remove_zero_rows = FALSE)
{

  # Create the table
  df |> ivo_tab2_step1({{ varleft }}, {{ vartop }}, exclude_missing, missing_string) |>
    ivo_tab2_step2({{ varleft }}, {{ vartop }}, extra_header, colsums, rowsums, percent_by, remove_zero_rows, sums_string) |>
    ivo_flextable_theme(2, rowsums, caption, highlight_cols, highlight_rows, color, font_name) -> df

  if(!is.na(percent_by)) { df |> flextable::colformat_double(decimal.mark = ",", suffix = " %", big.mark = " ") } else { df }
}


ivo_masked_table_2way <- function(df, varleft, vartop, cell = 5, extra_header = TRUE, exclude_missing = FALSE, missing_string = "(Missing)", colsums = FALSE, rowsums = FALSE, sums_string = "Total", caption = NA, highlight_cols = NULL, highlight_rows = NULL, color = "darkgreen", font_name = "Arial",  remove_zero_rows = FALSE)
{

  # Create the masked table
  df |> ivo_tab2_step1({{ varleft }}, {{ vartop }}, exclude_missing, missing_string) |>
    ivo_table_add_mask(cell) |>
    ivo_tab2_step2({{ varleft }}, {{ vartop }}, extra_header, colsums, rowsums, percent_by = NA, remove_zero_rows, sums_string) |>
    ivo_flextable_theme(2, rowsums, caption, highlight_cols, highlight_rows, color)
}


ivo_table_3way <- function(df, varleft, varright, vartop, extra_header = TRUE, exclude_missing = FALSE, missing_string = "(Missing)", colsums = FALSE, rowsums = FALSE, sums_string = "Total", caption = NA, highlight_cols = NULL, highlight_rows = NULL, percent_by = NA, color = "darkgreen", font_name = "Arial",  remove_zero_rows = FALSE)
{

  # Create the table
  df |> ivo_tab3_step1({{ varleft }}, {{ varright }}, {{ vartop }}, exclude_missing, missing_string) |>
    ivo_tab3_step2({{ varleft }}, {{ varright }}, {{ vartop }}, extra_header, colsums, rowsums, percent_by, remove_zero_rows, sums_string) |>
    ivo_flextable_theme(3, rowsums, caption, highlight_cols, highlight_rows, color, font_name) -> df

  if(!is.na(percent_by)) { df |> flextable::colformat_double(decimal.mark = ",", suffix = " %", big.mark = " ") } else { df }
}

ivo_masked_table_3way <- function(df, varleft, varright, vartop, cell = 5, extra_header = TRUE, exclude_missing = FALSE, missing_string = "(Missing)", colsums = FALSE, rowsums = FALSE, sums_string = "Total", caption = NA, highlight_cols = NULL, highlight_rows = NULL, color = "darkgreen", font_name = "Arial",  remove_zero_rows = FALSE)
{
  # Create the masked table
  df |> ivo_tab3_step1({{ varleft }}, {{ varright }}, {{ vartop }}, exclude_missing, missing_string) |>
    ivo_table_add_mask(cell) |>
    ivo_tab3_step2({{ varleft }}, {{ varright }}, {{ vartop }}, extra_header, colsums, rowsums, percent_by = NA, remove_zero_rows, sums_string) |>
    ivo_flextable_theme(3, rowsums, caption, highlight_cols, highlight_rows, color, font_name)
}

ivo_table_4way <- function(df, varleft, varleft2, varright, vartop, extra_header = TRUE, exclude_missing = FALSE, missing_string = "(Missing)", colsums = FALSE, rowsums = FALSE, sums_string = "Total", caption = NA, highlight_cols = NULL, highlight_rows = NULL, percent_by = NA, color = "darkgreen", font_name = "Arial",  remove_zero_rows = FALSE)
{
  # Create the table
  df |> ivo_tab4_step1({{ varleft }}, {{ varleft2 }}, {{ varright }}, {{ vartop }}, exclude_missing, missing_string) |>
    ivo_tab4_step2({{ varleft }}, {{ varleft2 }}, {{ varright }}, {{ vartop }}, extra_header, colsums, rowsums, percent_by, remove_zero_rows, sums_string) |>
    ivo_flextable_theme(4, rowsums, caption, highlight_cols, highlight_rows, color, font_name) -> df

  if(!is.na(percent_by)) { df |> flextable::colformat_double(decimal.mark = ",", suffix = " %", big.mark = " ") } else { df }
}

ivo_masked_table_4way <- function(df, varleft, varleft2, varright, vartop, cell = 5, extra_header = TRUE, exclude_missing = FALSE, missing_string = "(Missing)", colsums = FALSE, sums_string = "Total", rowsums = FALSE, caption = NA, highlight_cols = NULL, highlight_rows = NULL, color = "darkgreen", font_name = "Arial",  remove_zero_rows = FALSE)
{

  # Create the table
  df |> ivo_tab4_step1({{ varleft }}, {{ varleft2 }}, {{ varright }}, {{ vartop }}, exclude_missing, missing_string) |>
    ivo_table_add_mask(cell) |>
    ivo_tab4_step2({{ varleft }}, {{ varleft2 }}, {{ varright }}, {{ vartop }}, extra_header, colsums, rowsums, percent_by = NA, remove_zero_rows, sums_string) |>
    ivo_flextable_theme(4, rowsums, caption, highlight_cols, highlight_rows, color, font_name)
}


# IVO Exported tables ####

#' @title Add masking (censoring) to a table
#' @name ivo_table_add_mask
#' @encoding UTF-8
#' @description Table masking using cell counts..
#'
#' @param df A data frame containing a column called "Freq", e.g. a frequency table created using \code{ftable() |> data.frame()}.
#' @param cell The cell count at which masking should be used. Cell counts between 1 and this number will be masked. The default is 5.
#'
#' @return A data frame with masked cell counts.
#' @details Masking is used to prevent the distribution of tables where individuals could be identified.
#' @seealso {ivo_table_masked} for masked tables.
#' @author Måns Thulin
#' @examples
#'
#' library(dplyr)
#' example_data <- data.frame(Year = sample(2020:2023, 50, replace = TRUE),
#' A = sample(c("Type 1", "Type 2"), 50, replace = TRUE),
#' B = sample(c("Apples", "Oranges", "Bananas"), 50, replace = TRUE),
#' C = sample(c("Swedish", "Norwegian", "Chilean"), 50, replace = TRUE))
#' # With masking limit set at 7:
#' example_data |> select(Year, A) |>
#'   ftable() |>
#'   data.frame() |>
#'   ivo_table_add_mask(cell = 7)
#'
#' @export
ivo_table_add_mask <- function(df, cell = 5)
{
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(df, add = coll)
  if(!"Freq" %in% names(df)){
    coll$push(paste("The data frame, df, needs to have a column namned 'Freq'."))
  }
  checkmate::assert_number(cell, lower = 0, add = coll)
  checkmate::reportAssertions(coll)

    df |>
      dplyr::mutate(Freq = ifelse(dplyr::between(.data$Freq, 1, cell), base::paste("1-", cell, sep = ""), .data$Freq))
  }


#' @title Use nice fonts and colors for tables
#' @name ivo_flextable_theme
#'
#' @description A flextable theme for ivo_table objects.
#' @encoding UTF-8
#' @param x A flextable.
#' @param kway The number of "horizontal" variables in the table.
#' @param rowsums A logical, saying whether the rightmost column in the table contains the sum of each row. Defaults to FALSE.
#' @param caption An optional string containing a table caption.
#' @param highlight_cols A numeric vector containing the indices of the columns that should be highlighted.
#' @param highlight_rows A numeric vector containing the indices of the rows that should be highlighted.
#' @param color A named color or a color HEX code, used for the lines in the table. Defaults to "darkgreen".
#' @param font_name The name of the font to be used in the table. Defaults to "Arial".
#'
#' @return A styled flextable.
#' @details The default settings use a dark green color and a sans serif font.
#' @author Måns Thulin
#' @examples
#' library(tidyr)
#' library(dplyr)
#' library(flextable)
#' example_data <- data.frame(Year = sample(2020:2023, 50, replace = TRUE),
#' A = sample(c("Type 1", "Type 2"), 50, replace = TRUE),
#' B = sample(c("Apples", "Oranges", "Bananas"), 50, replace = TRUE),
#' C = sample(c("Swedish", "Norwegian", "Chilean"), 50, replace = TRUE))
#' example_data |> select(B, A) |>
#'   ftable() |>
#'   data.frame() |>
#'   spread(A, Freq) |>
#'   regulartable() |>
#'   ivo_flextable_theme()
#' @export
ivo_flextable_theme <- function(x, kway = 2, rowsums = FALSE, caption = NA, highlight_cols = NULL, highlight_rows = NULL, color = "darkgreen", font_name = "Arial")
{


  # Funktionskontroller----
  coll <- checkmate::makeAssertCollection()
  # Byt ut till relevanta kontroller
  if (!inherits(x, "flextable")) {
    coll$push(paste("ivo_flextable_theme can only be used on flextable objects."))
  }
  checkmate::assert_number(kway, lower = 1, add = coll)
  checkmate::assert_choice(rowsums, c(TRUE, FALSE), add = coll)
  checkmate::assert_string(caption, na.ok = TRUE, add = coll)
  checkmate::assert_numeric(highlight_cols, lower = 1, any.missing = FALSE, min.len = 1, null.ok = TRUE, add = coll)
  checkmate::assert_numeric(highlight_rows, lower = 1, any.missing = FALSE, min.len = 1, null.ok = TRUE, add = coll)
  checkmate::reportAssertions(coll)

  # Definiera vilka färger som ska användas
  col_dark <- color
  col_mid <-  col_dark |> grDevices::adjustcolor(0.4) |> ivo_hex8_to_hex6()
  col_light <- col_dark |> grDevices::adjustcolor(0.1) |> ivo_hex8_to_hex6()

  # Definiera linjer som följer IVO:s grafiska profil
  tjockare_linje <- officer::fp_border(width = 3, color = col_dark)
  tjock_linje <- officer::fp_border(width = 2, color = col_mid)
  tunn_linje <- officer::fp_border(width = 0.5, color = col_mid)

  # Ta bort default-designen:
  x <- flextable::border_remove(x)

  # Lägg in tabellrubrik:
  if(!is.na(caption)) { x <- flextable::set_caption(x, caption) }

  # Fixa textjustering:
  x <- flextable::align_text_col(x, align = "left", header = TRUE)
  x <- flextable::align_nottext_col(x, align = "right", header = TRUE)
  if(rowsums) { x <- flextable::align(x, j = flextable::ncol_keys(x), align = "right", part = "all") }

  # Lägg in linjer för raderna i tabellen:
  x <- flextable::hline(x, border = tunn_linje, part = "body")
  x <- flextable::hline_top(x, border = tjockare_linje, part = "header")
  x <- flextable::hline_top(x, border = tjock_linje, part = "body")
  x <- flextable::hline_bottom(x, border = tjock_linje, part = "body")

  # GYear en linje under rubriken för varright om extra_header=TRUE
  if(flextable::nrow_part(x, part = "header") > 1) {
    x <- flextable::align(x, i = 1, align = "center", part = "header")
    x <- flextable::hline(x, j = kway:(flextable::ncol_keys(x)-rowsums), border = tjock_linje, part = "header")
  }

  # Färglägg kolumner/rader som ska highlightas:
  if(any(!is.null(highlight_cols), !is.null(highlight_rows))) { x <- flextable::bg(x, i = highlight_rows, j = highlight_cols, bg = col_light) }

  # Gör rubrik och summa för kolumnsumner till fetstil:
  x <- flextable::bold(x = x, bold = TRUE, part = "header")
  x <- flextable::bold(x = x,  bold = TRUE, part = "footer")
  if(rowsums) { x <- flextable::bold(x, j = flextable::ncol_keys(x), bold = TRUE, part = "body") }

  # Formatera siffror till svensk standard (decimalkomma, mellanslag mellan tusental):
  x <- flextable::colformat_double(x, big.mark = " ", decimal.mark = ",")
  x <- flextable::colformat_int(x, big.mark = " ")

  # Use custom font:
  x <- flextable::font(x, fontname = font_name, part = "all")

  # Fixa fel som uppstYear när celler slås ihop:
  flextable::fix_border_issues(x)
}

#' @title Create pretty frequency/contingency tables
#' @name ivo_table
#' @description \code{ivo_table()} lets you easily create a table using pretty fonts and colors. If you want the table with masked values use \code{ivo_table_masked()}.
#' @param df A data frame with 1-4 columns
#' @param extra_header Should the variable name be displayed? Defaults to TRUE.
#' @param exclude_missing Whether to exclude missing values from the table. Defaults to FALSE.
#' @param missing_string A string used to indicate missing values. Defaults to "(Missing)".
#' @param colsums A logical indicating whether the sum of each column should be computed. Defaults to FALSE.
#' @param rowsums A logical indicating whether the sum of each row should be computed. Defaults to FALSE.
#' @param sums_string A string that is printed in the column/row where row/column sums are shown. Defaults to "Total".
#' @param caption An optional string containing a table caption.
#' @param highlight_cols A numeric vector containing the indices of the columns that should be highlighted.
#' @param highlight_rows A numeric vector containing the indices of the rows that should be highlighted.
#' @param percent_by Used to get percentages instead of frequencies. There are three options: "row" to get percentages by row (each row sum is 100 percent), "col" to get percentages by column (each the sum of each row to 100 percent) and "tot" to get percentages out of the total (the sum of all cells is 100 percent). The default, NA, means that frequencies are displayed instead.
#' @param color A named color or a color HEX code, used for the lines in the table. Defaults to "darkgreen".
#' @param font_name The name of the font to be used in the table. Defaults to "Arial".
#' @param long_table For one-way tables: FALSE (the default) means that the table will be wide and consist of a single row, TRUE means that the table will be long and consist of a single column.
#' @param remove_zero_rows If set to TRUE, removes all rows that contain nothing but zeros. The default is FALSE.
#' @return A stylized \code{flextable}.
#' @details The functions \code{ivo_table()} and \code{ivo_table_masked()} takes a \code{data.frame} with 1-4 columns. The order of the columns in the \code{data.frame} will determine where they will be displayed in the table. The first column will always be displayed at the top of the table. If there are more than one column the following 2-4 columns will be displayed to the left in the order 2, 3, 4. To change how the columns are displayed in the table; change the place of the columns in the \code{data.frame} using \code{dplyr::select()}.
#' @author Måns Thulin and Kajsa Grind
#' @encoding UTF-8
#' @seealso {ivo_table_add_mask}
#' @examples # Generate example data
#' example_data <- data.frame(Year = sample(2020:2023, 50, replace = TRUE),
#' A = sample(c("Type 1", "Type 2"), 50, replace = TRUE),
#' B = sample(c("Apples", "Oranges", "Bananas"), 50, replace = TRUE),
#' C = sample(c("Swedish", "Norwegian", "Chilean"), 50, replace = TRUE))
#'
#' ### 1 way tables ###
#' data1 <- example_data |> dplyr::select(Year)
#'
#' ivo_table(data1)
#' ivo_table(data1, extra_header = FALSE) # Remove the header
#' ivo_table(data1, color = "orange") # Change color on table lines
#' ivo_table(data1, long_table = TRUE) # Draw the table in a long format
#' ivo_table(data1, font_name = "Garamond") # Use a different font
#'
#' ivo_table_masked(data1) # No masking because all counts are >=5
#' ivo_table_masked(data1, cell = 15) # Counts below <=15 are masked
#'
#' # With pipes
#' example_data |> dplyr::select(Year) |> ivo_table()
#'
#' ### 2-way tables ###
#' data2 <- example_data |> dplyr::select(A, B)
#' data2_swap <- example_data |> dplyr::select(B, A)
#'
#' # Basic tables:
#' ivo_table(data2)
#' ivo_table(data2_swap) # Swap order of the columns
#' ivo_table(data2, colsums = TRUE) # Add the sum of each column
#' ivo_table(data2, rowsums = TRUE) # Add the sum of each row
#' ivo_table(data2, caption = "Awesome table") # Add a caption
#' ivo_table(data2, highlight_cols = 3) # Highlight column 3
#' ivo_table(data2, highlight_rows = 2, highlight_cols = 3) # Highlight cell at row 2 column 3
#'
#' # Tables with percentages:
#' ivo_table(data2, percent_by = "row") # By row
#' ivo_table(data2, percent_by = "col") # By column
#' ivo_table(data2, percent_by = "tot") # By total
#'
#' # Masked tables:
#' ivo_table_masked(data2)
#' ivo_table_masked(data2, cell = 7) # Counts <= 7 are masked
#' # Row and column sums are also masked:
#' ivo_table_masked(
#' data2,
#' cell = 3,
#' colsums = TRUE,
#' rowsums = TRUE)
#'
#' # Add a note at the end of the table:
#' # (colwidths must be set to the number of columns in the table)
#' ivo_table(data2) |>
#'   flextable::add_footer_row(values = "This is a footnote.",
#'                             colwidths = 3)
#'
#' # Add footnotes to cells in the table:
#' ivo_table(data2) |>
#' flextable::footnote(i = c(1, 3), j = c(1, 2),
#'                     value = flextable::as_paragraph(c(
#'                       "Some remark.",
#'                       "Some comment.")),
#'                     ref_symbols = c("a", "b"))
#'
#' # Add footnotes to cells in the table header:
#' ivo_table(data2) |>
#' flextable::footnote(i = 2, j = c(1, 3),
#'                     value = flextable::as_paragraph(c(
#'                       "Some remark.",
#'                       "Some comment.")),
#'                     ref_symbols = c("a", "b"),
#'                     part = "header")
#'
#' ### 3-way tables ###
#' data3 <- example_data |> dplyr::select(C, B, Year)
#'
#' ivo_table(data3)
#' ivo_table(data3, colsums = TRUE, rowsums = TRUE) # Add the sum of each column and each row
#'
#' ivo_table_masked(
#' data3,
#' cell = 3,
#' caption = "Values between 1 and 3 are masked."
#' )
#'
#' ### 4-way tables ###
#' data4 <- example_data |> dplyr::select(Year, B, C, A)
#'
#' ivo_table(data4)
#' ivo_table(data4, remove_zero_rows = TRUE) # Remove the row with zeros
#'
#' # Add the sum of each column and each row and highlight column 6:
#' ivo_table(
#' data4,
#' colsums = TRUE,
#' rowsums = TRUE,
#' highlight_cols = 6)
#'
#' ivo_table_masked(data4, colsums = TRUE, rowsums = TRUE)
#' @export
ivo_table <- function(df, extra_header = TRUE, exclude_missing = FALSE, missing_string = "(Missing)", colsums = FALSE, rowsums = FALSE, sums_string = "Total", caption = NA, highlight_cols = NULL, highlight_rows = NULL, percent_by = NA, color = "darkgreen", font_name = "Arial", long_table = FALSE, remove_zero_rows = FALSE) {
  # Funktionskontroller
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_data_frame(df, min.cols = 1, max.cols = 4, add = coll)
  checkmate::assert_choice(extra_header, c(TRUE, FALSE), add = coll)
  checkmate::assert_choice(exclude_missing, c(TRUE, FALSE), add = coll)
  checkmate::assert_choice(colsums, c(TRUE, FALSE), add = coll)
  checkmate::assert_choice(rowsums, c(TRUE, FALSE), add = coll)
  checkmate::assert_choice(long_table, c(TRUE, FALSE), add = coll)
  checkmate::assert_choice(remove_zero_rows, c(TRUE, FALSE), add = coll)
  checkmate::assert_string(caption, na.ok = TRUE, add = coll)
  checkmate::assert_numeric(highlight_cols, lower = 1, any.missing = FALSE, min.len = 1, null.ok = TRUE, add = coll)
  checkmate::assert_numeric(highlight_rows, lower = 1, any.missing = FALSE, min.len = 1, null.ok = TRUE, add = coll)
  checkmate::assert_choice(percent_by, c("row", "col", "tot", NA), add = coll)

  checkmate::reportAssertions(coll)

  # variables for checks
  var_name <- names(df)
  points <- length(c(grep("[.]", base::deparse(base::substitute(var_name)))))
  spaces <- length(c(grep(" ", base::deparse(base::substitute(var_name)))))
  if(points > 0 & spaces > 0){
    var_name <- gsub(".", " ", x = var_name, fixed = TRUE)
    df <- df |> dplyr::rename_at(dplyr::vars(names(df)), ~var_name)
    message("Dots have been replaced with spaces in the variable names.")
  }

  # Convert factors to characters (required for dealing with missing values):
  df <- apply(df, 2, as.character) |> as.data.frame()

  # Number of variables in df
  if (length(var_name) == 1) {
    ivo_table_1way(df = df, varleft = var_name[1], extra_header = extra_header, exclude_missing = exclude_missing, missing_string = missing_string, caption = caption, highlight_cols = highlight_cols, highlight_rows = highlight_rows, percent_by = percent_by, color = color, font_name = font_name,  long_table = long_table)
  } else if (length(var_name) == 2) {
    ivo_table_2way(df = df, varleft = var_name[2], vartop = var_name[1], extra_header = extra_header, exclude_missing = exclude_missing, missing_string = missing_string, colsums = colsums, rowsums = rowsums, sums_string = sums_string, caption = caption, highlight_cols = highlight_cols, highlight_rows = highlight_rows, percent_by = percent_by, color = color, font_name = font_name,  remove_zero_rows = remove_zero_rows)
  } else if (length(var_name) == 3) {
    ivo_table_3way(df = df, varleft = var_name[2], varright = var_name[3], vartop = var_name[1], extra_header = extra_header, exclude_missing = exclude_missing, missing_string = missing_string, colsums = colsums, rowsums = rowsums, sums_string = sums_string, caption = caption, highlight_cols = highlight_cols, highlight_rows = highlight_rows, percent_by = percent_by, color = color, font_name = font_name,  remove_zero_rows = remove_zero_rows)
  } else {
    ivo_table_4way(df = df, varleft = var_name[2], varleft2 = var_name[3], varright = var_name[4], vartop = var_name[1], extra_header = extra_header, exclude_missing = exclude_missing, missing_string = missing_string, colsums = colsums, rowsums = rowsums, sums_string = sums_string, caption = caption, highlight_cols = highlight_cols, highlight_rows = highlight_rows, percent_by = percent_by, color = color, font_name = font_name,  remove_zero_rows = remove_zero_rows)
  }
}

#' @title Create a masked frequency/contingency table
#' @name ivo_table_masked
#' @description \code{ivo_table_masked()} lets you easily create pretty masked tables. If you want the table without masked values use \code{ivo_table()} instead.
#' @param df A data frame with 1-4 columns
#' @param cell The largest value that will be masked. Defaults to 5, meaning that values between 1 and 5 are masked.
#' @param extra_header Should the variable name be displayed? Defaults to TRUE.
#' @param exclude_missing Whether to exclude missing values from the table. Defaults to FALSE.
#' @param missing_string A string used to indicate missing values. Defaults to "(Missing)".
#' @param colsums A logical indicating whether the sum of each column should be computed. Defaults to FALSE.
#' @param rowsums A logical indicating whether the sum of each row should be computed. Defaults to FALSE.
#' @param sums_string A string that is printed in the column/row where row/column sums are shown. Defaults to "Total".
#' @param caption An optional string containing a table caption.
#' @param highlight_cols A numeric vector containing the indices of the columns that should be highlighted.
#' @param highlight_rows A numeric vector containing the indices of the rows that should be highlighted.
#' @param color A named color or a color HEX code, used for the lines in the table. Defaults to "darkgreen".
#' @param font_name The name of the font to be used in the table. Defaults to "Arial".
#' @param long_table For one-way tables: FALSE (the default) means that the table will be wide and consist of a single row, TRUE means that the table will be long and consist of a single column.
#' @param remove_zero_rows If set to TRUE, removes all rows that contain nothing but zeros. The default is FALSE.
#' @return A stylized \code{flextable}.
#' @details The functions \code{ivo_table()} and \code{ivo_table_masked()} takes a \code{data.frame} with 1-4 columns. The order of the columns in the \code{data.frame} will determine where they will be displayed in the table. The first column will always be displayed at the top of the table. If there are more than one column the following 2-4 columns will be displayed to the left in the order 2, 3, 4. To change how the columns are displayed in the table; change the place of the columns in the \code{data.frame} using \code{dplyr::select()}.
#' @author Måns Thulin and Kajsa Grind
#' @encoding UTF-8
#' @seealso {ivo_table_add_mask}
#' @examples # Generate example data
#' example_data <- data.frame(Year = sample(2020:2023, 50, replace = TRUE),
#' A = sample(c("Type 1", "Type 2"), 50, replace = TRUE),
#' B = sample(c("Apples", "Oranges", "Bananas"), 50, replace = TRUE),
#' C = sample(c("Swedish", "Norwegian", "Chilean"), 50, replace = TRUE))
#'
#' ### 1-way tables ###
#' data1 <- example_data |> dplyr::select(Year)
#' ivo_table_masked(data1) # No masking because all counts are >=5
#' ivo_table_masked(data1, cell = 15) # Counts below <=15 are masked
#'
#' # With pipes
#' example_data |> dplyr::select(Year) |> ivo_table()
#'
#' ### 2-way tables ###
#' data2 <- example_data |> dplyr::select(A, B)
#' ivo_table_masked(data2)
#' ivo_table_masked(data2, cell = 7) # Counts <= 7 are masked
#' # Row and column sums are also masked:
#' ivo_table_masked(
#' data2,
#' cell = 3,
#' colsums = TRUE,
#' rowsums = TRUE)
#'
#' ### 3-way tables ###
#' data3 <- example_data |> dplyr::select(C, B, Year)
#' ivo_table_masked(
#' data3,
#' cell = 3,
#' caption = "Values between 1 and 3 are masked."
#' )
#'
#' ### 4-way tables ###
#' data4 <- example_data |> dplyr::select(Year, B, C, A)
#' ivo_table_masked(data4, colsums = TRUE, rowsums = TRUE)
#'
#' ### Exporting to Excel ###
#' \dontrun{
#' ivo_table_masked(data4, colsums = TRUE, rowsums = TRUE) |> ivo_flextable_to_xlsx("example_table")
#' }
#' @export
ivo_table_masked <- function(df, cell = 5, extra_header = TRUE, exclude_missing = FALSE, missing_string = "(Missing)", colsums = FALSE, rowsums = FALSE, sums_string = "Total", caption = NA, highlight_cols = NULL, highlight_rows = NULL, color = "darkgreen", font_name = "Arial", long_table = FALSE, remove_zero_rows = FALSE) {

  # Funktionskontroller
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_data_frame(df, min.cols = 1, max.cols = 4, add = coll)
  checkmate::assert_number(cell, na.ok = FALSE, lower = 0, add = coll)
  checkmate::assert_choice(extra_header, c(TRUE, FALSE), add = coll)
  checkmate::assert_choice(exclude_missing, c(TRUE, FALSE), add = coll)
  checkmate::assert_choice(colsums, c(TRUE, FALSE), add = coll)
  checkmate::assert_choice(rowsums, c(TRUE, FALSE), add = coll)
  checkmate::assert_choice(long_table, c(TRUE, FALSE), add = coll)
  checkmate::assert_choice(remove_zero_rows, c(TRUE, FALSE), add = coll)
  checkmate::assert_string(caption, na.ok = TRUE, add = coll)
  checkmate::assert_numeric(highlight_cols, lower = 1, any.missing = FALSE, min.len = 1, null.ok = TRUE, add = coll)
  checkmate::assert_numeric(highlight_rows, lower = 1, any.missing = FALSE, min.len = 1, null.ok = TRUE, add = coll)

  checkmate::reportAssertions(coll)

  # variables for checks
  var_name <- names(df)
  points <- length(c(grep("[.]", base::deparse(base::substitute(var_name)))))
  spaces <- length(c(grep(" ", base::deparse(base::substitute(var_name)))))
  if(points > 0 & spaces > 0){
    var_name <- gsub(".", " ", x = var_name, fixed = TRUE)
    df <- df |> dplyr::rename_at(dplyr::vars(names(df)), ~var_name)
    message("Dots have been replaced with spaces in the variable names.")
  }

  # Convert factors to characters (required for dealing with missing values):
  df <- apply(df, 2, as.character) |> as.data.frame()

  # Number of variables in df
  if (length(var_name) == 1) {
    ivo_masked_table_1way(df = df, varleft = var_name[1], cell = cell, extra_header = extra_header, exclude_missing = exclude_missing, missing_string = missing_string, caption = caption, highlight_cols = highlight_cols, highlight_rows = highlight_rows, color = color, font_name = font_name, long_table = long_table)
  } else if (length(var_name) == 2) {
    ivo_masked_table_2way(df = df, varleft = var_name[2], vartop = var_name[1], cell = cell, extra_header = extra_header, exclude_missing = exclude_missing, missing_string = missing_string, colsums = colsums, rowsums = rowsums, sums_string = sums_string, caption = caption, highlight_cols = highlight_cols, highlight_rows = highlight_rows, color = color, font_name = font_name, remove_zero_rows = remove_zero_rows)
  } else if (length(var_name) == 3) {
    ivo_masked_table_3way(df = df, varleft = var_name[2], varright = var_name[3], vartop = var_name[1], cell = cell, extra_header = extra_header, exclude_missing = exclude_missing, missing_string = missing_string, colsums = colsums, rowsums = rowsums, sums_string = sums_string, caption = caption, highlight_cols = highlight_cols, highlight_rows = highlight_rows, color = color, font_name = font_name, remove_zero_rows = remove_zero_rows)
  } else {
    ivo_masked_table_4way(df = df, varleft = var_name[2], varleft2 = var_name[3], varright = var_name[4], vartop = var_name[1], cell = cell, extra_header = extra_header, exclude_missing = exclude_missing, missing_string = missing_string, colsums = colsums, rowsums = rowsums, sums_string = sums_string, caption = caption, highlight_cols = highlight_cols, highlight_rows = highlight_rows, color = color, font_name = font_name, remove_zero_rows = remove_zero_rows)
  }
}

# Internal functions below to support the export of a flextable to Excel----
create_table_contents <- function(flxtbl) {
  list(caption = flxtbl$caption$value,
      top_var_label = get_top_var_label(flxtbl),
      push_top_var = get_push_top_var(flxtbl),
      header = flxtbl$header,
      footer = flxtbl$footer,
      body = flxtbl$body,
      footer_rows = nrow(flxtbl$footer$dataset),
      has_sum_col = as.integer(!is.null(flxtbl$body$dataset$Total)))
}

get_top_var_label <- function(table_contents) {
  result <- dplyr::slice(table_contents$header$dataset, 1) |> # Plocka ut en df från header där variabelnamnet finns
    purrr::as_vector() |> # Gör det till en vector
    unname() |> # Inga namn i vektorn, bara värden
    unique() |> # Bara unika värden
    stringr::str_subset(".+") # Inga tomma strings

  # Om man använder sig av argumentet extra_header = TRUE kommer detta vara en vektor som har många värden
  # och därför gör vi en enkel check för att se att vi faktiskt fått ut ett (1) unikt värde.
  if (length(result) > 1) NA else result
}

# Antal kolumner in att starta ifrån vid merge av kolumner för top var label
get_push_top_var <- function(table_contents) {
  dplyr::slice(table_contents$header$dataset, 1) |> # Plocka ut en df från header där variabelnamnet finns
    purrr::as_vector() |> # Gör det till en vector
    unname() |>  # Inga namn i vektorn, bara värden
    stringr::str_subset(".+", negate = TRUE) |> # Bara blanka strings "" som förekommer
    length() + 1 # För att fånga in antalet kolumner att skippa. +1 för att det ska bli rätt.
}

get_ref_symbols <- function(table_contents, part) {
  # Check if there are any reference symbols
  txt_values <- apply(table_contents[[part]]$content$content$data, c(1, 2), function(df) unlist(lapply(df, function(x) length(x$txt) > 1)))
  # Get the x, y coordinates so we can extract the values and put in correct place
  note_coords <- which(txt_values, arr.ind = T)
  # If there are coordinates we use those to extract the contents
  if (length(note_coords) > 0) {
    for (i in 1:nrow(note_coords)) {
        row_i <- note_coords[i, 1]
        col_i <- note_coords[i, 2]

        # This is where we find the vector, which is extactly of length 2 (1 for value, 1 for ref symbol)
        txt <- table_contents[[part]]$content$content$data[[row_i, col_i]]$txt
        # Turn it into a string where we wrap the ref symbol in parenthesis
        new_txt <- paste0(txt[1], " (", txt[2], ")")

        switch(part,
        "footer" = {
          # Reverse the order of text
          new_txt <- paste0("(", txt[1], ") ", txt[2])
          table_contents$footer$dataset[[row_i, col_i]] <- new_txt
        },
        "header" = {
          # If the reference symbol is inside the header, we set the colname for both
          # the body and footer dataset so that we can bind them later
          names(table_contents$body$dataset)[col_i] <- new_txt
          names(table_contents$footer$dataset)[col_i] <- new_txt
        },
        "body" = {
          # If it's a categorical variable it will be stored as factor, so we need to add the new "value" to its levels
          if (inherits(table_contents[[part]]$dataset[[row_i, col_i]], "factor")) {
              levels(table_contents[[part]]$dataset[[col_i]]) <- c(levels(table_contents[[part]]$dataset[[col_i]]), new_txt)
          }
          # If it's in the body we just target the x,y coordinate in the table and adjust the value
          table_contents[[part]]$dataset[[row_i, col_i]] <- new_txt
        })
    }
  }
  return(table_contents)
}

get_footnotes <- function(table_contents, part) {

}

# Lägg till tabelldata till ett namnat blad
add_sheet <- function(workbook, sheet_name, table_contents, sums_string) {

  # Skapa bladet
  openxlsx::addWorksheet(workbook, sheet_name)

  # Sätt ett index för den rad vi börjar på, alltid 0 eftersom indexering
  # i funktionen openxlsx::writeData är nollindexerad...
  extra_content <- caption_and_topvar(table_contents)

  start_row <- 1

  # Gå igenom tabellobjektet
  for (item in names(table_contents)) {

    # Startkolumnen kan variera, så den återställs inne i loopen
    start_col <- 1

    switch(item,
      "caption" = {
        # Om caption eller top_var_label saknas (NA eller NULL) så skippar vi dom
        if (!extra_content[["caption"]]) next
        contents <- table_contents[[item]]
      },
      "top_var_label" = {
        if (!extra_content[["top_var_label"]]) next
        contents <- table_contents[[item]]
        # Om det är top_var_label så ska vi knuffa in texten så den hamnar i rätt kolumn,
        # men vi måste räknar bort om det finns en summa-kolumn att förhålla sig till
        start_col <- table_contents$push_top_var - table_contents$has_sum_col
      },
      "footer" = {
        if (table_contents$footer_rows > 0) {
          # Make sure to clear all columns past the first one, since they contain only repeated reference symbols
          # If there's a sum row, we need to avoid clearing that.
          if (table_contents$footer$dataset[1, 1] == sums_string) {
            table_contents$footer$dataset[-1,-1] <- NA
          } else {
            table_contents$footer$dataset[-1] <- NA
          }
          table_contents <- get_ref_symbols(table_contents, item)
        }
      },
      "header" = {
        # Check if the header contains any reference symbols
        if (table_contents$footer_rows > 0) table_contents <- get_ref_symbols(table_contents, item)
      },
      "body" = {
        # Body behöver vi packa upp och lägga till summa-raden som
        # finns i "footer".
        if (table_contents$footer_rows > 0) table_contents <- get_ref_symbols(table_contents, item)

        contents <- rbind(table_contents[[item]]$dataset, table_contents$footer$dataset)

      },
      # Allt annat skippar vi
      next
    )

    # Skriv innehållet till bladet
    if (exists("contents")) openxlsx::writeData(workbook, sheet_name, contents, startRow = start_row, startCol = start_col)

    # We add a start row for each round, except for header/footer where we only modify for reference symbols.
    # This should be refactored at some point.
    if (!item %in% c("header", "footer")) start_row <- start_row + 1
  }

}

# Översätt tjockleken på linjerna
check_border_thickness <- function(border_thickness) {
  dplyr::case_when(
    border_thickness == 0 ~ "none",
    border_thickness <= 1 ~ "thin",
    border_thickness <= 2 ~ "medium",
    border_thickness > 2 ~ "thick",
    TRUE ~ "none"
  )
}

# Kolla om texten är fetstilt, kursiv eller understruken
check_text_style <- function(text, row, col) {
  # De olika värden vi letar efter
  styles <- c("bold", "italic", "underlined")

  result <- c()

  for (style in styles) {
    flag <- text[[style]]$data[[row, col]]
    if (isTRUE(flag)) {
      result <- append(result, style)
    } else {
      result <- append(result, NULL)
    }
  }
  result
}

# Kolla om strängen är ett hexvärde eller finns i colors(), returnera i så fall värdet
# Vi vill inte ha några felaktiga värden eller saker som "transparent"
color_or_blank <- function(value) {
  if (grepl("^#(?:[0-9a-fA-F]{3}){1,2}$", value) || value %in% grDevices::colors()) value
}

# Calculate row heights by a general rule of thumb:
# Font size in points + 4. Here we add 16 to provide more padding in line with how ivo.table is rendered.
# This gives a fair amount of spacing and also consistent output across platforms.
# TODO: Allow user to set padding value of rows. Also, this function should probably take a row as input,
# find the largest font size (in case they vary across columns) and base its return value on that font size.
calc_row_height <- function(value) {
  value + 16
}

# Fix column width
# guess: By calculating the width based on a formula from ECMA-376-1:2016 p. 1601.
# width = Truncate([{Number of Characters} * {Maximum Digit Width} + {5 pixel padding}]/{Maximum Digit Width}*256)/256
# This should give a reasonable output that is also consistent across platforms.
# auto: Tries to match width using the openxlsx built in functionality.
# none: Use defaults without specific widths. :)
# TODO: Allow user to set same width across all columns as well as increase/decrease padding.
set_colwidths <- function(workbook, table_contents, sheet, colwidths) {
  switch(colwidths,
         "guess" = {
            # A vector for column widhts
            colwidths <- c()
            # Get the names of the dataset to iterate
            colnames <- names(table_contents$body$dataset)
            # Go over each column to calculate and add the width
            for (col in colnames) {

              # Get the longest string in the column (including the variable name itself)
              # TODO: Handle footer length as well
              longest_value_length <- max(nchar(paste0(col, as.character(table_contents$body$dataset[[col]]))))

              # Calculate the width based on ECMA-376-1:2016
              # The maximum digit width of 7 pixels (for Calibri 11pt) is an estimate used as a decent rule of thumb.
              # The 5 is used for padding.
              # TODO: Look into setting "maximum digit width" based on font size, using some kind of estimate
              width <- round((longest_value_length * 7 + 5) / 7 * 256 / 256, digits = 0)

              colwidths <- append(colwidths, width)

            }

            openxlsx::setColWidths(workbook, sheet, cols = seq_along(table_contents$body$dataset), widths = colwidths)
         },
         "auto" = {
           openxlsx::setColWidths(workbook, sheet, cols = seq_along(table_contents$body$dataset), widths = "auto")
         },
         "none" = {
           return()
        })
}

# Skapa formateringen
create_style <- function(table_contents, row, col, part) {
  openxlsx::createStyle(
    borderColour = c(
      color_or_blank(table_contents[[part]]$styles$cells$border.color.top$data[[row, col]]),
      color_or_blank(table_contents[[part]]$styles$cells$border.color.bottom$data[[row, col]]),
      color_or_blank(table_contents[[part]]$styles$cells$border.color.left$data[[row, col]]),
      color_or_blank(table_contents[[part]]$styles$cells$border.color.right$data[[row, col]])
    ),
    borderStyle = c(
      check_border_thickness(table_contents[[part]]$styles$cells$border.width.top$data[[row, col]]),
      check_border_thickness(table_contents[[part]]$styles$cells$border.width.bottom$data[[row, col]]),
      check_border_thickness(table_contents[[part]]$styles$cells$border.width.left$data[[row, col]]),
      check_border_thickness(table_contents[[part]]$styles$cells$border.width.right$data[[row, col]])
    ),
    border = c("top", "bottom", "left", "right"),
    textDecoration = check_text_style(table_contents[[part]]$styles$text, row, col),
    fgFill = color_or_blank(table_contents[[part]]$styles$cells$background.color$data[[row, col]]),
    fontColour = color_or_blank(table_contents[[part]]$styles$text$color$data[[row, col]]),
    fontSize = table_contents[[part]]$styles$text$font.size$data[[row, col]],
    fontName = table_contents[[part]]$styles$text$font.family$data[[row, col]],
    valign = table_contents[[part]]$styles$cells$vertical.align$data[[row, col]],
    halign = table_contents[[part]]$styles$pars$text.align$data[[row, col]]
  )
}

# Kontrollera om det finns en caption och/eller top_var_label
caption_and_topvar <- function(table_contents) {
  c("caption" = !is.null(table_contents$caption), "top_var_label" = !is.na(table_contents$top_var_label))
}

# Lägg till format för varje cell
add_style <- function(workbook, sheet, table_contents, caption_size, merge_cells) {

   # Sätt startrad (börjar på 1, men addera rader för caption och top_var
  extra_content <- caption_and_topvar(table_contents)
  start_row <- 1

  # Om vi har en rubrikrad i vYear tabell så behöver vi hantera styling
  # lite annorlunda.

  if (extra_content[["caption"]]) {

    # Och fixa tabellrubriken
    caption_style <- openxlsx::createStyle(fontSize = caption_size, textDecoration = c("bold", "italic"), valign = "center")
    openxlsx::addStyle(workbook, sheet, caption_style, rows = 1, cols = 1)

    # Gångra caption_size med 2.8 för att få en lite trevligare radhöjd och luftighet i rubriken
    openxlsx::setRowHeights(workbook, sheet, 1, caption_size * 2.8)

    # Skjut ner startraden
    start_row <- start_row + 1

  }

  # Här hanterar vi top_var_label om sådan finns
  if (extra_content[["top_var_label"]]) {
    # Antalet kolumner att merga label över samt applicera style på, men ignorera
    # en eventuell summa-kolumn om den finns.
    n_cols <- ncol(table_contents$body$dataset)
    merge_label_to <- n_cols - table_contents$has_sum_col
    merge_label_from <- table_contents$push_top_var - table_contents$has_sum_col

    # Merga över de kolumner som utgör tabelldata
    openxlsx::mergeCells(workbook, sheet, cols = merge_label_from:merge_label_to, rows = start_row)

    # Hämta style från första raden i header-delen av flextable
    top_var_label_style <- create_style(table_contents, 1, 1, "header")

    # Lägg till style från första kolumnen och hela vägen ut så att vi fYear med all formatering
    openxlsx::addStyle(workbook, sheet, top_var_label_style, rows = start_row, cols = 1:n_cols)

    # Hämta radhöjd från header, rad 1
    row_height <- calc_row_height(table_contents$header$styles$text$font.size$data[1])
    # Lägg till radhöjd
    openxlsx::setRowHeights(workbook, sheet, start_row, row_height)

    start_row <- start_row + 1

  }

   # Eftersom caption mm. utgör första raden måste vi räkna ut antalet rader genom att lägga på vYear
  # beräknade startrad och eventuell summarad (om sådan finns)
  xlsx_length <- nrow(table_contents$body$dataset) + table_contents$footer_rows + start_row

  # Iterera över varje cell för att skapa upp och lägga till formatet för den indivduella cellen
  for (col_i in seq_along(table_contents$body$dataset)) {

    # När vi hanterat rubriken kan vi gå vidare till att gå igenom varje cell för att tillämpa
    # temat för tabellen.

    # Sätt startraden för body (dvs. tabelldata)
    body_row <- 1

    if (merge_cells) {
      # Första radvärde i kolumnen
      previous_value <- table_contents$body$dataset[[body_row, col_i]]

      # En räknare för hur många gånger det förekommer
      value_counter <- 1
      # Startrad för merge
      merge_start_row <- start_row + 1
    }

    for (row_i in start_row:xlsx_length) {

      # Första raden kräver specialkoll eftersom det är kolumnrubriker
      if (row_i == start_row) {
        # Ta alltid den sista raden i header för att få "rätt" linjer och färg, ifall det
        # exempelvis är en tvåvägstabell med extra rubriknivåer
        last_header_row <- nrow(table_contents$header$content$content$data)
        style <- create_style(table_contents, last_header_row, col_i, "header")
        row_height <- calc_row_height(table_contents$header$styles$text$font.size$data[last_header_row])
        openxlsx::setRowHeights(workbook, sheet, row_i, row_height)
      } else if ( row_i >= xlsx_length - table_contents$footer_rows + 1) {
        # Get the right footer row index for each time we pass a footer row, starts at 1 and adds up for each row
        footer_i <- seq(table_contents$footer_rows, 1, by = -1)[xlsx_length - row_i + 1]
        # Create the style
        style <- create_style(table_contents, footer_i, col_i, "footer")
        # Get the largest font size from each row so we calculate row height correctly
        if (col_i == 1) {
          row_height <- max(calc_row_height(as.vector(table_contents$footer$styles$text$font.size$data[footer_i,])))
          openxlsx::setRowHeights(workbook, sheet, row_i, row_height)
        }

      } else {

        # Om vi flaggat för att vi vill merga celler med kategoriska variabler och det är en factor så sätter vi igång
        if (merge_cells && inherits(table_contents$body$dataset[[col_i]], c("factor"))) {

          # Om det aktuella värdet i loopen är detsamma som föregående värde och inte utgör första raden i kolumnen
          if (table_contents$body$dataset[[body_row, col_i]] == previous_value && body_row != 1) {
            value_counter <- value_counter + 1
            # Om det är sista raden i kolumnen och antalet värden hittills är fler än en så mergar vi
            # Eller så är vi på näst sista raden och har en summarad längst ned, då mergar vi också.
            if ((row_i == xlsx_length && value_counter > 1) || (row_i == xlsx_length - table_contents$footer_rows)) {
              openxlsx::mergeCells(workbook, sheet, cols = col_i, rows = merge_start_row:row_i)
            }
            # Om det däremot är ett nytt värde jämfört med föregående samt att antalsräknaren överstiger 1
          } else if (table_contents$body$dataset[[body_row, col_i]] != previous_value && value_counter > 1) {
            # Då mergar vi celler igen, från den rad vi stYear på minus 1 (och från den sparade startraden - merge_start_row)
            openxlsx::mergeCells(workbook, sheet, cols = col_i, rows = merge_start_row:(row_i - 1))
            # Vi sätter föregående värde till det aktuella värdet
            previous_value <- table_contents$body$dataset[[body_row, col_i]]
            # Vi återställer räknaren till en etta
            value_counter <- 1
            # Och vi sätter startraden för merge till nuvarande rad
            merge_start_row <- row_i
          } else {
            # I alla andra fall så nollställer vi bara allt utan att göra något och gYear vidare
            previous_value <- table_contents$body$dataset[[body_row, col_i]]
            value_counter <- 1
            merge_start_row <- row_i
          }
        }

        # För att applicera style på rader i body utgYear vi från body_row istället för row_i (annars hamnar vi utanför bounds)
        style <- create_style(table_contents, body_row, col_i, "body")

        # Only set row height when passing the row the first time
        if (col_i == 1) {
          row_height <- max(calc_row_height(table_contents$body$styles$text$font.size$data[body_row,]))
          # Obs, row_i måste användas för att pricka "rätt" cell i Excel
          openxlsx::setRowHeights(workbook, sheet, row_i, row_height)
        }

        # Och lägg på en rad till nästa runda
        body_row <- body_row + 1
      }
      openxlsx::addStyle(workbook, sheet, style, row_i, col_i)
    }
  }
}

#' @title Export flextable to Excel
#' @name ivo_flextable_to_xlsx
#' @description Saves one or more flextables to a .xlsx file.
#' @param tables A \code{flextable} object or a list of several \code{flextable} objects.
#' @param filename The name of the output file.
#' @param sums_string For table creating using \code{ivo_table} or \code{ivo_table_masked}, the string that is printed in the column/row where row/column sums are shown. Must be the same string that was used when creating the table. Defaults to "Total".
#' @param format Whether or not to use the flextable format options such as borders, colors etc. \code{TRUE} = uses the format options, \code{FALSE} = no formatting.
#' @param colwidths Method to determine widths for columns given their special nature in Excel.
#' @param caption_size The font size of the caption in the Excel output.
#' @param merge_cells Merge cells of repeated category values. This is in line with the regular output of \code{ivo_table}.
#' @param gridlines Show or hide gridlines in the Excel output.
#' @return An Excel file with each flextable on an own tab.
#' @details The function saves an Excel file in the current working directory with the provided flextables on separate sheets. When saving several flextables, add a name to the list elements to name the sheets in the output file. If the list elements are not named, default names will be used.
#' @author Stefan Furne
#' @examples
#' library(dplyr)
#' library(flextable)
#'
#' # Example flextables
#' tbl1 <- starwars |>
#' group_by(homeworld) |>
#' summarise(n = n()) |>
#' filter(n > 1 & !is.na(homeworld)) |>
#' flextable()
#'
#' tbl2 <- starwars |>
#' group_by(species) |>
#' summarise(mean_height = mean(height)) |>
#' flextable()
#'
#' # Only one flextable
#' ivo_flextable_to_xlsx(tbl1, "Flextable_test1.xlsx", format = FALSE, colwidths = "auto")
#'
#' # Two tables, no named sheets
#' ivo_flextable_to_xlsx(list(tbl1, tbl2), "Flextable_test2.xlsx")
#'
#' # Two tables, with named sheets
#' ivo_flextable_to_xlsx(
#'    list("Planets" = tbl1, "Mean height" = tbl2),
#'    "Flextable_test3.xlsx",
#'    colwidths = "auto"
#'    )
#' @export
ivo_flextable_to_xlsx <- function(tables, filename = "flextable_ex", sums_string = "Total", format = TRUE, colwidths = "guess", caption_size = 14, merge_cells = TRUE, gridlines = FALSE) {

  # Funktionskontroller----
  coll <- checkmate::makeAssertCollection()
  # Byt ut till relevanta kontroller
  checkmate::assertMultiClass(tables, c("list", "flextable"), add = coll)
  if (inherits(tables, "list")) {
    check <- all(lapply(tables, class) == "flextable") # FIXME - hur med inherits
    if (!check) {
      coll$push(paste("The argument 'tables' only accepts flextables or lists of flextables."))
    }
  }
  checkmate::assertCharacter(filename, add = coll)
  checkmate::assertLogical(format, add = coll)
  checkmate::assertChoice(colwidths, c("auto", "guess", "none"), add = coll)
  checkmate::assertNumeric(caption_size, add = coll)
  checkmate::assertLogical(merge_cells, add = coll)
  checkmate::assertLogical(gridlines, add = coll)
  # Rapportera eventuella fel
  checkmate::reportAssertions(coll)

  # Skapa en tom arbetsbok
  workbook <- openxlsx::createWorkbook()


  # Fixar filnamnet så att det inte skapas konstiga filer:
  if (grepl(".", filename)) {
    filename <- sub("\\..*", "", filename)
  }
  filename <- paste0(filename, ".xlsx")

  # Sätter namn på tabellerna om de saknar
  if (is.null(names(tables))) {
    names(tables) <- paste0("Sheet", as.character(seq(1, length(tables))))
  }

  # Om vi har fått en lista med Flextables att skriva ut så loopar
  # vi igenom och lägger till dem. "is.list" tycker att flextables är en
  # lista, så vi kollar det är en lista, men inte en flextable genom class()
  # Lite klumpigt men does the job s a s.
  if (inherits(tables, "list")) {
    # Gå igenom de tabeller som angetts
    for (i in seq_along(tables)) {
      # Kontrollerar om det specifika namnet för tabellen
      # lämnats blankt. Då fyller vi på med namnet "Blad <i>"
      if (is.na(names(tables[i])) || names(tables[i]) == "") {
        sheet_name <- paste0("Sheet", i)
      } else {
        sheet_name <- names(tables[i])
      }

      # Hämta ut innehåll ur tabellen
      table_contents <- create_table_contents(tables[[i]])

      # Lägg till data
      add_sheet(workbook, sheet_name, table_contents, sums_string)

      # Lägg till formatering
      if (format) add_style(workbook, sheet_name, table_contents, caption_size, merge_cells)

      # Och bredd
      set_colwidths(workbook, table_contents, sheet_name, colwidths)

      # Och gridlines av/på
      if (!gridlines) openxlsx::showGridLines(workbook, sheet_name, showGridLines = FALSE)

    }
  } else if (inherits(tables, "flextable")) {
    # Om det är en enskild flextable så lägger vi till den.
    table_contents <- create_table_contents(tables)

    add_sheet(workbook, "Sheet1", table_contents, sums_string)
    if (format) add_style(workbook, "Sheet1", table_contents, caption_size, merge_cells)
    set_colwidths(workbook, table_contents, "Sheet1", colwidths)

    if (!gridlines) openxlsx::showGridLines(workbook, "Sheet1", showGridLines = FALSE)

  }

  # Spara arbetsboken
  openxlsx::saveWorkbook(workbook, filename, overwrite = TRUE)
  message(paste0("File saved as ", filename))
}

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

ivo_add_extra_header_1way <- function(df, name_v4, ncol_v4, add = TRUE, rowsums = FALSE)
{
  if(add) {
    if(rowsums) {
      flextable::add_header_row(df, values = c(name_v4, ""), colwidths = c(ncol_v4, 1)) } else {
      flextable::add_header_row(df, values = c(name_v4), colwidths = c(ncol_v4))
    }
  } else { df }
}

ivo_excl_missing <- function(df, exclude_missing = FALSE, missing_string = "(Missing)")
{
  if(!exclude_missing) {
    for(i in 1:ncol(df)){
      if(is.factor(df[,i]) & sum(is.na(df[,i]))>0) {
        levs <- levels(df[,i])
        df[,i] <- as.character(df[,i])
        df[is.na(df[,i]),i] <- "6049122418972891471204127890512XY"
        df[,i] <- factor(df[,i], levels = c(levs, "6049122418972891471204127890512XY"), labels = c(levs, missing_string))
      } else { df[is.na(df[,i]), i] <- missing_string }
    }}
  return(df)
}

ivo_num_sum <- function(x)
{
  suppressWarnings(x <- sum(as.numeric(unlist(x))))
  x[is.na(x)] <- "-"
  return(x)
}

ivo_add_col_sums <- function(df, new_row, colsums)
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
    stats::ftable(exclude=NULL) |>
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
    ivo_add_col_sums(new_row, colsums) |>
    flextable::autofit()
}

# Helper functions for three-way tables:
ivo_tab3_step1 <- function(df, v1, v3, v4, exclude_missing, missing_string)
{
  # Filter the data frame and create a frequency table
  df |> dplyr::select({{v4}}, {{v1}}, {{v3}}) |>
    ivo_excl_missing(exclude_missing, missing_string) |>
    stats::ftable(exclude=NULL) |>
    base::data.frame() #|>
    # `colnames<-`(c({{v1}}, {{v3}}, {{v4}}, "Freq")) -> df
    # df[order(df[,1]),]
    #`colnames<-`(c({{v1}}, {{v3}}, {{v4}}, "Freq")) |>
    #dplyr::arrange(.data[[v4]])
}

ivo_tab3_step2 <- function(df, v1, v3, v4, extra_header, colsums, rowsums, percent_by, remove_zero_rows, sums_string)
{

  Total <- NULL
  ncol_v4 <- df |> dplyr::pull({{v4}}) |> unique() |> length()
  name_v4 <- df |> dplyr::select({{v4}}) |> names()
  name_v1 <- df |> dplyr::select({{v1}}) |> names()

  # Format the table
  df |> tidyr::pivot_wider(names_from = {{v4}} , values_from = "Freq") |>
    dplyr::arrange(.data[[v1]])  -> df

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
    ivo_add_col_sums(new_row, colsums) |>
    flextable::merge_v(j = name_v1) |>
    flextable::autofit()
}

# Helper functions for four-way tables:
ivo_tab4_step1 <- function(df, v1, v2, v3, v4, exclude_missing, missing_string)
{
  # Filter the data frame and create a frequency table
  df |> dplyr::select({{v4}}, {{v1}}, {{v2}}, {{v3}}) |>
    ivo_excl_missing(exclude_missing, missing_string) |>
    stats::ftable(exclude=NULL) |>
    base::data.frame() #|>
    #`colnames<-`(c({{v1}}, {{v2}}, {{v3}}, {{v4}}, "Freq"))
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
  df |> tidyr::pivot_wider(names_from = {{v4}} , values_from = "Freq") |>
    dplyr::arrange(.data[[v1]], .data[[v2]]) -> df

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
    ivo_add_col_sums(new_row, colsums) |>
    flextable::merge_v(j = name_v1) |>
    flextable::merge_v(j = name_v2)  |>
    flextable::autofit()
}


# _____________________________________________________________
# IVO internal table functions to create correct ivo_table()----

ivo_table_1way <- function(df, varleft, extra_header = TRUE, exclude_missing = FALSE, missing_string = "(Missing)", colsums = colsums, rowsums = rowsums, caption = NA, highlight_cols = NULL, highlight_rows = NULL, percent_by = NA, color = "darkgreen", font_name = "Arial", long_table = FALSE, sums_string = "Total")
{
  Total <- NULL
  ncol_v4 <- df |> dplyr::pull({{varleft}}) |> unique() |> length()
  name_v4 <- df |> dplyr::select({{varleft}}) |> names()

  # Create the table
  if(long_table) {
  df |> dplyr::select({{varleft}}) |>
    ivo_excl_missing(exclude_missing, missing_string) |>
    stats::ftable(exclude=NULL) |>
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
      new_row <- NA } else {
          if (rowsums) message("It looks like you want sums for your table. Use colsums instead of rowsums for 1-way tables in long format.")
          new_row <- c(apply(df, 2, ivo_num_sum), sums_string)
          names(new_row)[length(new_row)] <- names(df)[1]
        }


    df |> flextable::regulartable() |>
    ivo_add_col_sums(new_row, colsums) |>
    flextable::autofit() |>
    ivo_flextable_theme(1, rowsums = FALSE, caption, highlight_cols, highlight_rows, color, font_name) -> df } else {
      df |> dplyr::select({{varleft}}) |>
        ivo_excl_missing(exclude_missing, missing_string) |>
        stats::ftable(exclude=NULL) |>
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
        new_row <- NA } else {
          if (colsums) message("It looks like you want sums for your table. Use rowsums instead of colsums for 1-way tables.")
          df$Total <- unlist(apply(df, 1, ivo_num_sum))
          if(!rowsums) { df |> dplyr::select(-Total) -> df }
        }
        df |> flextable::regulartable() |>
        ivo_add_extra_header_1way(name_v4, ncol_v4, extra_header, rowsums) |>
        flextable::autofit() |>
        ivo_flextable_theme(1, rowsums, caption, highlight_cols, highlight_rows, color, font_name) -> df
    }

  if(!is.na(percent_by)) { df |> flextable::colformat_double(decimal.mark = ",", suffix = " %", big.mark = " ") } else { df }
}


ivo_masked_table_1way <- function(df, varleft, cell = 5, extra_header = TRUE, exclude_missing = FALSE, missing_string = "(Missing)", colsums = FALSE, rowsums = FALSE, caption = NA, highlight_cols = NULL, highlight_rows = NULL, color = "darkgreen", font_name = "Arial",  long_table = FALSE, sums_string = "Total"){
  Total <- NULL
  ncol_v4 <- df |> dplyr::pull({{varleft}}) |> unique() |> length()
  name_v4 <- df |> dplyr::select({{varleft}}) |> names()

  if(long_table) {
  df |> dplyr::select({{varleft}}) |>
      ivo_excl_missing(exclude_missing, missing_string) |>
      stats::ftable(exclude=NULL) |>
      base::data.frame() |>
      ivo_table_add_mask(cell)  |>
      `colnames<-`(c({{varleft}}, "Count")) -> df

      if (rowsums) message("It looks like you want sums for your table. Use colsums instead of rowsums for 1-way tables in long format.")
      new_row <- c(apply(df, 2, ivo_num_sum), sums_string)
          names(new_row)[length(new_row)] <- names(df)[1]

      df |> flextable::regulartable() |>
      ivo_add_col_sums(new_row, colsums) |>
      flextable::autofit() |>
      ivo_flextable_theme(1, rowsums = FALSE, caption, highlight_cols, highlight_rows, color, font_name) } else {
        df |> dplyr::select({{varleft}}) |> ivo_excl_missing(exclude_missing, missing_string) |>
        stats::ftable(exclude=NULL) |>
        base::data.frame() |>
        ivo_table_add_mask(cell)  |>
        `colnames<-`(c({{varleft}}, "Freq")) |>
        tidyr::pivot_wider(names_from = {{varleft}} , values_from = "Freq") -> df

        if (colsums) message("It looks like you want sums for your table. Use rowsums instead of colsums for 1-way tables.")
        df$Total <- unlist(apply(df, 1, ivo_num_sum))
        if(!rowsums) { df |> dplyr::select(-Total) -> df }

        df |> flextable::regulartable() |>
        ivo_add_extra_header_1way(name_v4, ncol_v4, extra_header, rowsums) |>
        flextable::autofit() |>
        ivo_flextable_theme(1, rowsums, caption, highlight_cols, highlight_rows, color, font_name)
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
#' @param df A data frame containing a column called "Freq", e.g. a frequency table created using \code{ftable(exclude=NULL) |> data.frame()}.
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
#'   ftable(exclude=NULL) |>
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
#'   ftable(exclude=NULL) |>
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

  # Number of variables in df
  if (length(var_name) == 1) {
    ivo_table_1way(df = df, varleft = var_name[1], extra_header = extra_header, exclude_missing = exclude_missing, missing_string = missing_string, colsums = colsums, rowsums = rowsums, sums_string = sums_string, caption = caption, highlight_cols = highlight_cols, highlight_rows = highlight_rows, percent_by = percent_by, color = color, font_name = font_name,  long_table = long_table)
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
    ivo_masked_table_1way(df = df, varleft = var_name[1], cell = cell, extra_header = extra_header, exclude_missing = exclude_missing, missing_string = missing_string, colsums = colsums, rowsums = rowsums, sums_string = sums_string, caption = caption, highlight_cols = highlight_cols, highlight_rows = highlight_rows, color = color, font_name = font_name, long_table = long_table)
  } else if (length(var_name) == 2) {
    ivo_masked_table_2way(df = df, varleft = var_name[2], vartop = var_name[1], cell = cell, extra_header = extra_header, exclude_missing = exclude_missing, missing_string = missing_string, colsums = colsums, rowsums = rowsums, sums_string = sums_string, caption = caption, highlight_cols = highlight_cols, highlight_rows = highlight_rows, color = color, font_name = font_name, remove_zero_rows = remove_zero_rows)
  } else if (length(var_name) == 3) {
    ivo_masked_table_3way(df = df, varleft = var_name[2], varright = var_name[3], vartop = var_name[1], cell = cell, extra_header = extra_header, exclude_missing = exclude_missing, missing_string = missing_string, colsums = colsums, rowsums = rowsums, sums_string = sums_string, caption = caption, highlight_cols = highlight_cols, highlight_rows = highlight_rows, color = color, font_name = font_name, remove_zero_rows = remove_zero_rows)
  } else {
    ivo_masked_table_4way(df = df, varleft = var_name[2], varleft2 = var_name[3], varright = var_name[4], vartop = var_name[1], cell = cell, extra_header = extra_header, exclude_missing = exclude_missing, missing_string = missing_string, colsums = colsums, rowsums = rowsums, sums_string = sums_string, caption = caption, highlight_cols = highlight_cols, highlight_rows = highlight_rows, color = color, font_name = font_name, remove_zero_rows = remove_zero_rows)
  }
}

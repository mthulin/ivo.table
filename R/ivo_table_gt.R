#' @title Create pretty frequency/contingency GT tables
#' @name ivo_table_gt
#' @description \code{ivo_table_gt()} lets you easily create a GT table using pretty fonts and colors.
#' @param df A data frame with 1-3 columns
#' @param extra_header Should the variable name be displayed? Defaults to TRUE.
#' @param totals An optional vector to add sums to "rows" and "cols".
#' @param missing_string A string used to indicate missing values. Defaults to "(Missing)".
#' @param title An optional string containing a table title.
#' @param subtitle An optional string containing a table subtitle. Only usable together with title.
#' @param source_note An optional string for a table source note.
#' @param mask An optional integer to mask counts below given value.
#' @return A stylized \code{GT} table.
#' @details The functions \code{ivo_table_gt()} takes a \code{data.frame} with 1-3 columns. The order of the columns in the \code{data.frame} will determine where they will be displayed in the table. The first column will always be displayed at the top of the table. If there are more than one column the following 2-3 columns will be displayed to the left in order. To change how the columns are displayed in the table; change the place of the columns in the \code{data.frame} using \code{dplyr::select()}.
#' @author Stefan Furne
#' @encoding UTF-8
#' @importFrom gt gt tab_header tab_spanner tab_source_note tab_stubhead
#' @export
# TODO examples
ivo_table_gt <- function(df,
                         title = NULL,
                         subtitle = NULL,
                         extra_header = TRUE,
                         source_note = NULL,
                         mask = FALSE,
                         missing_string = "(Missing)",
                         totals = NULL) {
    # Capture the columns present
    columns <- names(df)
    num_vars <- length(columns)

    if (num_vars > 3) stop("The GT table function supports a maximum of three variables (3-way table).")

    # Prepare the data
    df <- df |>
        group_and_count(columns) |>
        missing_string_and_zeros(missing_string) |>
        add_totals(totals, columns)

    # Create the GT table object
    # If it's a 2-way or 3-way table we'll set the row name and group name
    if (num_vars %in% c(2, 3)) {
        gt_table <- df |> gt(
            rowname_col = if (num_vars == 3) columns[3] else columns[2],
            groupname_col = if (num_vars == 3) columns[2] else NULL,
        )
        if (num_vars == 2) {
            gt_table <- gt_table |> tab_stubhead(columns[2])
        }
    } else {
        gt_table <- gt(df, rowname_col = columns[1]) # This is needed to make the bold totals work with 1-way table
    }

    # Apply optionals
    if (!is.null(title)) {
        if (!is.null(subtitle)) {
            gt_table <- gt_table |> tab_header(title = title, subtitle = subtitle)
        } else {
            gt_table <- gt_table |> tab_header(title = title)
        }
    }

    # Don't draw the spanner over "Total"
    if (extra_header & num_vars > 1) {
        gt_table <- gt_table |>
            tab_spanner(label = columns[1], columns = setdiff(names(df), c(columns, "Total")))
    }

    if (is.character(source_note)) {
        gt_table <- gt_table |>
            tab_source_note(source_note = source_note)
    }

    # Apply theming
    gt_table <- apply_gt_table_theme(gt_table)

    return(gt_table)
}

#' @title Add totals for columns and rows to a \code{data.frame}
#' @description Adds totals for columns and rows to a \code{data.frame} to be used as a GT object
#' @param df A \code{data.frame} object.
#' @param totals A character vector of that may contain "cols" and "rows".
#' @param columns A character vector of column names.
#' @return A \code{data.frame} with grouped and summarized counts.
#' @noRd
#' @importFrom dplyr group_by summarize across mutate bind_rows rowwise ungroup c_across
#' @importFrom tidyr replace_na
add_totals <- function(df, totals, columns) {
    if ("rows" %in% totals) {
        df <- df |>
            rowwise() |>
            mutate(Total = sum(c_across(where(is.numeric)))) |>
            ungroup()
    }
    if ("cols" %in% totals) {
        if (length(columns) == 3) {
            sums <- df |>
                group_by(.data[[columns[2]]]) |>
                summarize(across(where(is.numeric), sum), .groups = "drop")

            sums[[columns[3]]] <- "Total"

            df <- df |>
                bind_rows(sums)
        } else {
            sums <- df |>
                summarize(across(where(is.numeric), sum))
            df <- df |>
                bind_rows(sums) |>
                mutate(across(where(is.character), ~ replace_na(.x, "Total")))
        }
    }

    return(df)
}

#' @title Grouped and summarized \code{data.frame}
#' @description Creates a data.frame that is grouped and summarized with frequencies to be used with GT
#' @param df A \code{data.frame} object.
#' @param columns A character vector of column names.
#' @return A \code{data.frame} with grouped and summarized counts.
#' @noRd
#' @importFrom dplyr group_by reframe summarize left_join across all_of first n distinct select
#' @importFrom tidyr pivot_wider expand_grid everything
#' @importFrom purrr map set_names reduce
group_and_count <- function(df, columns) {
    # Group and count columns
    df <- df |>
        group_by(across(all_of(columns))) |>
        summarize(Count = n(), .groups = "drop")

    # Handle 2-way or 3-way tables
    if (length(columns) %in% c(2, 3)) {
        # Get combinations for all selected variables so we get a complete table
        all_combinations <- columns |>
            map(~ unique(df[[.x]])) |>
            reduce(expand_grid) |>
            set_names(columns)

        # Join back to original df and pivot
        df <- all_combinations |>
            left_join(df, by = columns) |>
            pivot_wider(names_from = first(columns), values_from = "Count")
    }

    return(df)
}

#' @title Replace missing values in a \code{data.frame}
#' @description Replaces missing values in a `data.frame` by assigning a custom string to character or factor columns, and replacing missing numeric values with zeros.
#' @param df A \code{data.frame} object.
#' @param missing_string A string to replace missing values in character or factor columns.
#' @return A \code{data.frame} with missing values replaced according to the specified rules.
#' @noRd
#' @importFrom dplyr mutate across where
#' @importFrom tidyr replace_na
#' @importFrom purrr set_names
missing_string_and_zeros <- function(df, missing_string) {
    df |>
        mutate(
            across(where(~ is.character(.x) | is.factor(.x)), ~ replace_na(as.character(.x), missing_string)),
            across(where(is.numeric), ~ replace_na(.x, 0))
        ) |>
        set_names(ifelse(names(df) == "NA", missing_string, names(df)))
}


#' @title A nice GT theme
#' @description Applies a predefined theme to a GT table, including font, colors, and alignment.
#' @param table A \code{gt} object.
#' @param color A named color or a color HEX code, used for the lines in the table. Defaults to "darkgreen".
#' @param font_name The name of the font to be used in the table. Defaults to "Arial".
#' @return A styled \code{gt} table.
#' @noRd
#' @importFrom gt opt_horizontal_padding opt_vertical_padding fmt_number fmt_integer tab_options
#' @importFrom gt cols_align tab_style cells_title cells_body cells_stub cells_column_labels cell_text matches
#' @importFrom dplyr all_of
apply_gt_table_theme <- function(table, color = "darkgreen", font_name = "Arial") {
    # TODO Add checkmate collection

    # Get theme colors
    # TODO This should probably be a shared internal function with flextable
    theme_color <- color
    theme_color_mid <- theme_color |>
        grDevices::adjustcolor(0.4) |>
        ivo_hex8_to_hex6()
    theme_color_light <- theme_color |>
        grDevices::adjustcolor(0.1) |>
        ivo_hex8_to_hex6()

    table <- table |>
        opt_horizontal_padding(scale = 3) |>
        opt_vertical_padding(scale = 1.5) |>
        fmt_number(columns = where(is.double), decimals = 1) |>
        fmt_integer(columns = where(is.integer)) |>
        cols_align(align = "right", columns = everything()) |>
        cols_align(align = "left", columns = where(is.character)) |>
        tab_options(
            table.font.names = font_name,
            table.font.color = "black",
            heading.title.font.size = "150%",
            heading.subtitle.font.size = "100%",
            heading.padding = "4px",
            column_labels.font.weight = "bold",
            column_labels.border.top.width = "4px",
            column_labels.border.top.style = ifelse(is.null(table[["_heading"]]$title), "hidden", "solid"),
            column_labels.padding = "10px",
            row_group.font.weight = "bold",
            stub_row_group.border.style = "hidden",
            column_labels.border.top.color = theme_color,
            column_labels.border.bottom.color = theme_color,
            row_group.border.bottom.color = theme_color_mid,
            row_group.border.top.color = theme_color_mid,
            row_group.border.top.width = "1px",
            row_group.border.bottom.width = "1px",
            row_group.background.color = theme_color_light,
            table.border.bottom.color = theme_color_mid,
            table_body.border.bottom.color = theme_color,
            table_body.hlines.color = theme_color_mid,
            table.border.top.style = "hidden",
            stub.border.style = "hidden",
            source_notes.border.bottom.style = "hidden",
            source_notes.font.size = "75%",
            source_notes.padding = "10px",
            table.width = "auto"
        ) |>
        tab_style(
            style = list(cell_text(weight = "bold", align = "left")),
            locations = cells_title(groups = "title")
        ) |>
        tab_style(
            style = list(cell_text(align = "left", color = grDevices::adjustcolor("black", 0.85) |>
                ivo_hex8_to_hex6())),
            locations = cells_title(groups = "subtitle")
        ) |>
        tab_style(
            style = cell_text(weight = "bold"),
            locations = list(
                cells_stub(rows = matches("Total")),
                cells_body(columns = matches("Total")),
                cells_body(rows = matches("Total"))
            )
        )

    return(table)
}

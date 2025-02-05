#' @title Create pretty frequency/contingency GT tables
#' @name ivo_table_gt
#' @description \code{ivo_table_gt()} lets you easily create a GT table using pretty fonts and colors.
#' @param df A data frame with 1-3 columns
#' @param extra_header Should the variable name be displayed? Defaults to TRUE.
#' @param totals WIP
#' @param missing_string A string used to indicate missing values. Defaults to "(Missing)".
#' @param title An optional string containing a table title.
#' @param subtitle An optional string containing a table subtitle. Only usable together with title.
#' @param source_note An optional string for a table source note.
#' @param mask WIP
#' @return A stylized \code{GT} table.
#' @details The functions \code{ivo_table_gt()} takes a \code{data.frame} with 1-3 columns. The order of the columns in the \code{data.frame} will determine where they will be displayed in the table. The first column will always be displayed at the top of the table. If there are more than one column the following 2-3 columns will be displayed to the left in order. To change how the columns are displayed in the table; change the place of the columns in the \code{data.frame} using \code{dplyr::select()}.
#' @author Stefan Furne
#' @encoding UTF-8
#' @importFrom dplyr group_by summarize left_join mutate across
#' @importFrom tidyr pivot_wider
#' @importFrom gt gt tab_header tab_spanner tab_source_note tab_stubhead
#' @examples
#' TODO
#' @export
ivo_table_gt <- function(df,
                         title = NULL,
                         subtitle = NULL,
                         extra_header = TRUE,
                         source_note = NULL,
                         mask = FALSE,
                         missing_string = "(Missing)",
                         totals = NULL) {
    # Capture selected columns
    columns <- names(df)
    num_vars <- length(columns)

    if (num_vars > 3) stop("The GT table function supports a maximum of three variables (3-way table).")

    # Group, count, and ensure character columns
    df <- df |>
        group_by(across(all_of(columns))) |>
        summarize(Antal = n(), .groups = "drop") |>
        replace_na(list(Antal = 0))

    # Pivot for 2-way or 3-way tables
    if (num_vars %in% c(2, 3)) {
        # Get unique values for all selected columns so we get a complete table
        all_combinations <- expand.grid(lapply(df[as.character(columns)], unique))

        # Join back to original df and pivot
        df <- all_combinations |>
            left_join(df, by = as.character(columns)) |>
            pivot_wider(names_from = first(columns), values_from = "Antal", values_fill = 0)
    }

    # Ensure we use the missing_string across all string values and fill zeroes for numerical
    df <- df |>
        mutate(
            across(where(~ is.character(.) | is.factor(.)), ~ replace_na(as.character(.), missing_string)),
            across(where(is.numeric), ~ replace_na(.x, 0))
        )

    # Create the GT table object
    gt_table <- df |> gt()

    # If it's a 2-way or 3-way table we'll set the row name and group name
    if (num_vars %in% c(2, 3)) {
        gt_table <- df |> gt(
            rowname_col = if (num_vars == 3) columns[3] else columns[2],
            groupname_col = if (num_vars == 3) columns[2] else NULL,
        )
        if (num_vars == 2) {
            gt_table <- gt_table |> tab_stubhead(columns[2])
        }
        # Get the groups right in the table
    }

    # Apply optionals
    if (is.character(title)) {
        gt_table <- gt_table |>
            tab_header(title = title)
    }

    if (is.character(title) & is.character(subtitle)) {
        gt_table <- gt_table |>
            tab_header(title = title, subtitle = subtitle)
    }

    if (extra_header) {
        gt_table <- gt_table |>
            tab_spanner(label = columns[1], columns = setdiff(names(df), columns))
    }

    if (is.character(source_note)) {
        gt_table <- gt_table |>
            tab_source_note(source_note = source_note)
    }

    return(gt_table)
}

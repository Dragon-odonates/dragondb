# Header #############################################################
#
# Author: Lisa Nicvert
# Email:  lisa.nicvert@fondationbiodiversite.fr
#
# Date: 2024-12-20
#
# Script Description: functions to access database


#' Get DB column names
#'
#' @param connexion connexion to the table (class `PqConnection`)
#' @param table table to retrieve names from
#' @param rm_ID Remove ID column? (columns that end with ID)
#'
#' @return The table column names
#'
#' @export
colnames_DB <- function(connexion,
                        table, rm_ID = TRUE) {

  colnames <- dbListFields(connexion, table)

  if (rm_ID) {
    colnames <- colnames[-grep("ID$", colnames)]
  }
  return(colnames)
}

#' Get all columns of a dataframe
#'
#' Get all columns, even if they don't exist
#'
#' @param df the original df
#' @param cols the columns we want in the final df
#'
#' @returns a dataframe where all colnames in `cols` are present:
#' if the column was in the original table, values are conserved,
#' else they are filled with `NA`s.
#'
#' @export
df_all_cols <- function(df, cols) {
  # Get df column names
  colnames_df <- colnames(df)

  # Determine which columns are in colnames or not
  cols_select <- cols[which(cols %in% colnames_df)]
  cols_not_select <- cols[which(!(cols %in% colnames_df))]

  # Select existing columns
  if (length(cols_select) != 0) {
    df1 <- copy(df[, ..cols_select])
  } else {
    df1 <- NULL
  }

  # Create empty df with the rest
  if (length(cols_not_select) != 0) {
    df2 <- data.frame(matrix(ncol = length(cols_not_select),
                             nrow = nrow(df)))
    colnames(df2) <- cols_not_select
  } else {
    df2 <- NULL
  }

  # final df
  if (!is.null(df1) & !is.null(df2)) {
    df_res <- cbind(df1, df2)
  } else if (!is.null(df1)) {
    df_res <- df1
  } else {
    df_res <- df2
  }

  return(df_res)
}

#' Remove rows with all NAs
#'
#' @param df a dataframe
#'
#' @returns the same dataframe where rows containing only `NA`s
#' have been discarded.
#'
#' @export
rm_all_na <- function(df) {
  all_na <- apply(is.na(df), 1, all)
  df <- df[!all_na, ]
  return(df)
}

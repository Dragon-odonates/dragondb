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


# Replace all NA values with "__NA__"

#' Replace `NA` values
#'
#' Replace `NA` values in a data.table with a placeholder
#'
#' @param dt the data.table
#' @param SDcols the columns in which to replace values
#' @param placeholder the placeholder
#' @param rev reverse (i.e. replace placeholder back with `NA`).
#'
#' @returns the data.table where NA values are replaced with `placeholder`
#' (or the reverse if `rev` is `TRUE`)
#' @export
replace_NA <- function(dt, SDcols, placeholder = "__NA__", rev = FALSE) {

  if (!rev) {
    dt[, (names(.SD)) := lapply(.SD,
                                function(x) fifelse(is.na(x), placeholder, x)),
       .SDcols = SDcols]
  } else {
    dt[, (names(.SD)) := lapply(.SD,
                                function(x) fifelse(x == placeholder, NA, x)),
       .SDcols = SDcols]
  }

}


#' Join data.tables on columns containing NAs
#'
#' @param dt1 First data.table (primary data.table for join)
#' @param dt2 Second data.table (secondary data.table)
#' @param cols1 Columns used for the join in primary data.table
#' @param cols2 Columns used for the join in secondary data.table
#' @param placeholder placeholder for the NAs
#'
#' @returns The merge of the two data.tables, considering NA values
#' as a match.
#' @export
join_dt_na <- function(dt1, dt2, cols1, cols2 = cols1,
                       placeholder = "__NA__") {

  dt1 <- copy(dt1)
  dt2 <- copy(dt2)

  # Store original classes
  orig_types <- lapply(dt1, class)
  # Make functions (as.xxx) from the data types (xxx)
  cast <- sapply(orig_types,
                 function(t) {
                   if (length(t) > 1) t <- t[1]
                   get(paste0("as.", t))
                 })

  # Convert to character to prepare for placeholder
  dt1[, names(.SD) := lapply(.SD, as.character),
      .SDcols = cols1]
  dt2[, names(.SD) := lapply(.SD, as.character),
      .SDcols = cols2]

  # Replace NAs with placeholder
  replace_NA(dt1,
             SDcols = cols1,
             placeholder = placeholder)
  replace_NA(dt2,
             SDcols = cols2,
             placeholder = placeholder)

  # Merge data.tables
  cols <- cols2
  names(cols) <- cols1

  res <- dt2[dt1,
             on = cols]

  # replace placeholders with NA
  replace_NA(res,
             SDcols = cols1,
             placeholder = placeholder,
             rev = TRUE)

  # Convert back columns to original type
  res[, names(.SD) := Map(function(fun, col) fun(col), cast, .SD),
      .SDcols = names(cast)]

  return(res)
}

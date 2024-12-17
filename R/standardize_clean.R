# Header #############################################################
#
# Author: Lisa Nicvert
# Email:  lisa.nicvert@fondationbiodiversite.fr
#
# Date: 2024-12-17
#
# Script Description: Functions to standardize and clean data.



# Standardize -------------------------------------------------------------


#' Rename columns
#'
#' @param key a dataframe containing the correspondence between the new
#' (in column `Standard`) and old (in column `nam`) column names.
#' @param dtable A datatable for which columns must be renamed
#' @param nam Name of the column of `key` in which to search old names.
#'
#' @return No value, sets the column names of `dtable`
#' @export
rename_cols <- function(key, dtable, nam) {
  cols <- c("Standard", nam)
  newnames_df <- key[!is.na(key[[nam]]), ..cols]

  setnames(dtable,
           old = newnames_df[[nam]],
           new = newnames_df$Standard)
}


# Clean -------------------------------------------------------------------


#' Remove leading and trailing spaces
#'
#' @param vec vector in which spaces should be removed
#'
#' @return values in the vector without leading or trailing spaces.
#' Multiple instances of blank characters are removed.
#' @export
rm_spaces <- function(vec) {
  gsub(pattern = "\\s+|\xc2\xa0+",
       replacement = "",
       vec,
       useBytes = TRUE)
}

#' Clean coordinates
#'
#' @param coord coordinates vector
#' @param na_char Regular expression to matcxh and replace with NA.
#' Defaults to all blank characters (or empty characters)
#'
#' @return the coordinates with only numbers and point as a decimal
#' separator (instead of comma).
#' @export
clean_coord <- function(coord, na_char = "^\\s*$") {
  if (!is.null(na_char)) {
    coord[grep(pattern = na_char, coord, useBytes = TRUE)] <- NA
  }
  # Replace comma
  coord <- gsub(pattern = "\\,", replacement = "\\.",
                coord, useBytes = TRUE)

  # Function to return the match or NA if no match
  match_or_na <- function(exp, reg) {
    m <- regmatches(exp, regexpr(reg, exp,
                                 useBytes = TRUE))
    if (length(m) == 0) {
      m <- NA
    }
    return(m)
  }

  coord <- unlist(lapply(coord,
                         match_or_na, reg = "\\d+\\.*\\d*")
  )
  coord <- as.numeric(coord)

  return(coord)
}

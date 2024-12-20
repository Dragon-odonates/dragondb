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

#' Coerce to NA
#'
#' Convert values of a vector to `NA`
#'
#' @param vec the vector to modify
#' @param na_char the characters t convert to NA: defaults to blank characters
#'
#' @return the vector where values matching `na_char` have been converted to `NA`
#' @export
coerce_to_na <- function(vec, na_char = "^\\s*$") {
  vec[grep(pattern = na_char, vec, useBytes = TRUE)] <- NA
  return(vec)
}

#' Chean count values
#'
#' Clean count values
#'
#' @param vec vector of counts
#' @param na_char values to replace by `NA`
#'
#' @return The count values cleaned and coerced to numeric.
#' Characters matching `na_char` are converted to `NA` and
#' "x" and "X" are converted to 1.
#'
#' @export
clean_count <- function(vec, na_char = "^\\s*$") {
  if (!is.null(na_char)) {
    res <- coerce_to_na(vec, na_char = na_char)
  } else {
    res <- vec
  }
  res <- gsub(pattern = "x|X", replacement = 1, res)
  res <- as.numeric(res)

  coerced_to_na(before = vec, after = res)
  return(res)
}

#' Clean coordinates
#'
#' @param coord coordinates vector
#' @param na_char Regular expression to match and replace with NA.
#' Defaults to all blank characters (or empty characters)
#'
#' @return the coordinates with only numbers and point as a decimal
#' separator (instead of comma).
#' @export
clean_coord <- function(coord, na_char = "^\\s*$") {
  if (!is.null(na_char)) {
    coord <- coerce_to_na(coord, na_char = na_char)
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


#' Check NA values
#'
#' Check which values have been coeced to NA.
#'
#' @param before The vector before conversion
#' @param after The vector after conversion
#' @param return Return the detailed results?
#'
#' @return If `return = FALSE` (default), prints a message to warn for
#' introduced NAs.
#' Else, returns the indices of values with NA and the initial values
#' converted to NA.
#' @export

coerced_to_na <- function(before, after, return = FALSE) {
  na_init <- which(is.na(before))
  na_final <- which(is.na(after))

  na_added <- na_final[!(na_final %in% na_init)]

  values_added <- before[na_added]

  if (length(values_added) != 0) {
    message(length(values_added), " NAs have been introduced. Unique values: ",
            paste(unique(values_added), collapse = ", "),
            ". For more information, check return values.")
  }

  if (return) {
    res <- list(na_final = na_final,
                na_init = na_init,
                na_added = na_added,
                values_added = values_added)
    return(res)
  }


}

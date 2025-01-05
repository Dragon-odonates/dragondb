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

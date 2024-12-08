# Header #############################################################
#
# Author: Lisa Nicvert
# Email:  lisa.nicvert@fondationbiodiversite.fr
#
# Date: 2024-12-05
#
# Script Description: standardize different files column names according to
# Darwin Core standard


# Libraries ---------------------------------------------------------------
library(readxl)
library(data.table)
library(here)


# Function ----------------------------------------------------------------


#' Rename columns
#'
#' @param key a dataframe containing the correspondence between the new
#' (in column `Standard`) and old (in column `nam`) column names.
#' @param dtable A datatable for which columns must be renamed
#' @param nam Name of the column of `key` in which to search old names.
#'
#' @return No value, sets the column names of `dtable`
rename_cols <- function(key, dtable, nam) {
  newnames_df <- names_key[!is.na(names_key[[nam]]), c("Standard", nam)]
  newnames <- as.character(newnames_df[[nam]])
  names(newnames) <- newnames_df$Standard

  setnames(dtable, newnames_df[[nam]], newnames_df$Standard)
}

# Read data ---------------------------------------------------------------

dat <- list()

dat[["Austria"]] <- data.table(read_excel("data/data_raw/Odonata_Austria_Export.xlsx",
                                          sheet = "Tabelle1"))

dat[["Belgium1"]] <- data.table(readRDS("data/data_raw/belgieData.rds"))
dat[["Belgium2"]] <- fread("data/data_raw/2024-076_libellen_EN.csv")

dat[["Cyprus1"]] <- fread("data/data_raw/Cyprus corrected.csv")

# Cyprus 2
lf <- list.files("data/data_raw/Cyprus Data dragonflies")

dat[["Cyprus2"]] <- data.table()
for(i in seq_along(lf)){
  cyp_data <- data.table(read_excel(file.path("data", "data_raw",
                                              "Cyprus Data dragonflies", lf[i]),
                                    col_types = "text",
                                    sheet = "Sheet1"))
  # Fix inconsistent names
  if ("CY#"  %in% colnames(cyp_data)) {
    setnames(cyp_data, "CY#", "Ref no")
  }
  if ("CY #"  %in% colnames(cyp_data)) {
    setnames(cyp_data, "CY #", "Ref no")
  }
  if ("CY ref"  %in% colnames(cyp_data)) {
    setnames(cyp_data, "CY ref", "Ref no")
  }
  if ("Altitude m"  %in% colnames(cyp_data)) {
    setnames(cyp_data, "Altitude m", "Altitude m asl")
  }
  if ("Exuvia"  %in% colnames(cyp_data)) {
    setnames(cyp_data, "Exuvia", "Exuviae")
  }
  if ("Total adults"  %in% colnames(cyp_data)) {
    setnames(cyp_data, "Total adults", "Adult total")
  }
  if ("Total Adults"  %in% colnames(cyp_data)) {
    setnames(cyp_data, "Total Adults", "Adult total")
  }
  if ("E deg dec"  %in% colnames(cyp_data)) {
    setnames(cyp_data, "E deg dec", "E dec deg")
  }
  if ("N deg dec"  %in% colnames(cyp_data)) {
    setnames(cyp_data, "N deg dec", "N dec deg")
  }
  if ("N dec dec"  %in% colnames(cyp_data)) {
    setnames(cyp_data, "N dec dec", "N dec deg")
  }

  dat[["Cyprus2"]] <- rbind(dat[["Cyprus2"]], cyp_data)
}

# Netherlands
dat[["Netherlands"]] <- data.table(readRDS("data/data_raw/Dutchdata2023.RDS"))

sp_ned <- fread("data/data_raw/Soortcodes_nl_sci.csv")
sp_ned[, srt_Nednaam := tolower(srt_Nednaam)]
dat[["Netherlands"]] <- merge(dat[["Netherlands"]], sp_ned,
                                   by.x = "soort_nl", by.y = "srt_Nednaam",
                                   all.x = TRUE)

dat[["France_STELI"]] <- fread("data/data_raw/STELI_data_FR_DMS.csv")

dat[["France_OPIE"]] <- fread("data/data_raw/France Opportunistics data (Opie)/odonata_202410091558.csv")


# # Extract column names ----------------------------------------------------
# dat_names <- lapply(dat, names)
#
# nlen <- sapply(dat_names, length)
# mlen <- max(nlen)
#
# nalen <- mlen - nlen
# dat_names_na <- lapply(seq_along(dat_names),
#                        function(i) c(dat_names[[i]], rep(NA, nalen[i]))
# )
# dat_names_df <- as.data.frame(dat_names_na)
#
# names(dat_names_df) <- names(dat_names)
# write.csv(dat_names_df,
#           file = here("outputs/column_names.csv"),
#           eol = "\r\n",
#           row.names = FALSE)

names_key <- read.csv(here("outputs/column_names.txt"),
                      sep = "\t", na.strings = "")



lapply(seq_along(dat),
       function(i) {
         rename_cols(key = names_key,
                     dtable = dat[[i]],
                     nam = names(dat)[i])
         })

# Reorder column
lapply(seq_along(dat),
       function(i) {
         cnames <- names_key$Standard[names_key$Standard %in% colnames(dat[[i]])]
         setcolorder(dat[[i]],
                     cnames)
       })

lapply(dat, names)


# Clean data in columns ---------------------------------------------------
library(rgbif)
library(stringr)

## scientificName -----

# Manually correct some names
dat$Belgium2[, scientificName := gsub(pattern = "\\s+spec.", replacement = "", scientificName)]

dat$Cyprus1[scientificName == "Crocothernis erythraea",
            scientificName := "Crocothemis erythraea"]
dat$Cyprus1[scientificName == "Anax Imperator",
            scientificName := "Anax imperator"]
dat$Cyprus1[scientificName == "Orthetrum Brunneum",
            scientificName := "Orthetrum brunneum"]
dat$Cyprus1[scientificName == "Anax Immaculifrons",
            scientificName := "Anax immaculifrons"]
dat$Cyprus1[scientificName == "Trimethis festiva",
            scientificName := "Trithemis festiva"]
dat$Cyprus1[scientificName == "Ishnura Intermedia",
            scientificName := "Ischnura intermedia"]

dat$Cyprus2[scientificName == "Crocothernis erythraea",
            scientificName := "Crocothemis erythraea"]
dat$Cyprus2[scientificName == "Isoaeschna isoceles",
            scientificName := "Aeshna isoceles"]
dat$Cyprus2[scientificName == "Anax Imperator",
            scientificName := "Anax imperator"]
dat$Cyprus2[scientificName == "Orthetrum Brunneum",
            scientificName := "Orthetrum brunneum"]
dat$Cyprus2[scientificName == "Anax Immaculifrons",
            scientificName := "Anax immaculifrons"]

dat$France_STELI[scientificName == "Calopteryx groupe splendens",
                 scientificName := "Calopteryx splendens"]
dat$France_STELI[scientificName == "Leste groupe sponsa",
                 scientificName := "Lestes sponsa"]
dat$France_STELI[scientificName == "Leste groupe viridis",
                 scientificName := "Lestes viridis"]
dat$France_STELI[scientificName == "Agrion Porte-coupe/vander Linden",
                 scientificName := "Enallagma cyathigerum"]
dat$France_STELI[scientificName == "Aeschne groupe cyanea",
                 scientificName := "Aeshna cyanea"]
dat$France_STELI[scientificName == "Aeschne groupe cyanea",
                 scientificName := "Aeshna cyanea"]


names_list_raw <- lapply(dat,
                         function(d) name_backbone_checklist(unique(d$scientificName)))
lapply(names_list_raw,
       function(n) unique(n$matchType))
lapply(names_list_raw,
       function(n) n[n$matchType == "HIGHERRANK",])
lapply(names_list_raw,
       function(n) n[n$matchType == "NONE" | n$matchType == "FUZZY" ,][, c("verbatim_name", "canonicalName", "genus", "matchType")])

names_merge <- lapply(names_list,
                      function(n) n[, c("canonicalName", "verbatim_name")])

lapply(seq_along(dat),
       function(i) merge(dat[[i]],
                         names_merge[[i]],
                         by.x = "scientificName", by.y = "verbatim_name"))


# cyp_data[, y_coord := gsub("\\xff", "",
#                            gsub("\\,", "\\.", y_coord, useBytes = TRUE),
#                            useBytes = TRUE)]
# cyp_data[, x_coord := gsub("\\xff", "",
#                            gsub("\\,", "\\.", x_coord, useBytes = TRUE),
#                            useBytes = TRUE)]


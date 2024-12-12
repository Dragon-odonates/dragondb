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

# remotes::install_github("hrbrmstr/mgrs")
library(mgrs)
library(sf)

library(readxl)
library(data.table)
library(here)

library(rgbif)
library(stringr)

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

#' Remove leading and trailing spaces
#'
#' @param vec vector in which spaces should be removed
#'
#' @return values in the vector without leading or trailing spaces.
#' Multiple instances of blank characters are removed.
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
clean_coord <- function(coord, na_char = "^\\s*$") {
  if (!is.null(na_char)) {
    coord[grep(pattern = na_char, coord, useBytes = TRUE)] <- NA
  }
  # Replace comma
  coord <- gsub(pattern = "\\,", replacement = "\\.", 
                coord, useBytes = TRUE)
  coord_new <- vector(mode = "character", 
                      length = length(coord))
  for (i in seq_along(coord_new)) {
    # Match one or more digits followed by 0 or more points 
    # followed by 0 or more digits
    m <- regmatches(coord[i], regexpr("\\d+\\.*\\d*", coord[i],
                                   useBytes = TRUE))
    if (length(m) == 0) {
      m <- NA
    }
    coord_new[i] <- m
  }

  return(coord_new)
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
                                    col_types = c("text", "text", "date",
                                                  "numeric", "numeric", "numeric",
                                                  "text", 
                                                  "text", "text", "text",
                                                  "text", "text", "text", "text",
                                                  "text", "numeric"),
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


# # Extract column names ---
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


# Standardize column names ------------------------------------------------

names_key <- read.csv(here("outputs/column_names.txt"),
                      sep = "\t", na.strings = "")

lapply(seq_along(dat),
       function(i) {
         rename_cols(key = names_key,
                     dtable = dat[[i]],
                     nam = names(dat)[i])
         })

# Reorder columns
i <- 1
cnames <- names_key$Standard[names_key$Standard %in% colnames(dat[[i]])]
setcolorder(dat[[i]],
            cnames)


lapply(seq_along(dat),
       function(i) {
         cnames <- names_key$Standard[names_key$Standard %in% colnames(dat[[i]])]
         setcolorder(dat[[i]],
                     cnames)
       })


lapply(dat, names)


# Clean data in columns ---------------------------------------------------

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


names_list <- lapply(dat,
                     function(d) name_backbone_checklist(unique(d$scientificName)))
lapply(names_list,
       function(n) unique(n$matchType))
lapply(names_list,
       function(n) n[n$matchType == "HIGHERRANK",])
lapply(names_list,
       function(n) n[n$matchType == "NONE" | n$matchType == "FUZZY" ,][, c("verbatim_name", "canonicalName", "genus", "matchType")])

names_merge <- lapply(names_list,
                      function(n) data.table(n[, c("canonicalName", "verbatim_name")]))

# lapply(seq_along(dat),
#        function(i) merge(dat[[i]],
#                          names_merge[[i]],
#                          by.x = "scientificName", by.y = "verbatim_name"))


dat <- lapply(seq_along(dat),
              function(i) {
                names_merge[[i]][dat[[i]], 
                                 on = c(verbatim_name = "scientificName")]
              })
names(dat) <- names(names_merge)

lapply(dat, setnames,
       old = c("canonicalName", "verbatim_name"),
       new = c("scientificName", "verbatimName"))

head(dat$Belgium1)

## Date -----
date_fmt <- vector(mode = "list", length = length(dat))
names(date_fmt) <- names(dat)

date_fmt$Austria <- NA
date_fmt$Belgium1 <- "%d/%m/%Y"
date_fmt$Belgium2 <- "%Y-%m-%d"
date_fmt$Cyprus1 <- "%Y-%m-%d"
date_fmt$Cyprus2 <- "%Y-%m-%d"
date_fmt$Netherlands <- NA
date_fmt$France_STELI <- "%d/%m/%Y" # + time & sampling effort
date_fmt$France_OPIE <- "%Y-%m-%d"

lapply(names(dat),
       function(n) {
         print(n)
         if ("eventDate" %in% colnames(dat[[n]])) {
           dat[[n]][, eventDate := as.IDate(eventDate, 
                                            format = date_fmt[[n]]
           )]
           dat[[n]][, c("year", "month", "day") := .(year(eventDate), 
                                                     month(eventDate),
                                                     mday(eventDate))]
         }
       }
       )

# Hour with STELI and sampling effort with STELI
dat$France_STELI[, eventTime := as.ITime(eventTime)]

# Negative values -> I suspect start and end dates have been inverted
effort_char <- dat$France_STELI$samplingEffort
effort_char[effort_char == ""] <- NA
# effort <- gsub(pattern = "^-", replacement = "", x = effort)
effort <- str_split(effort_char, ":")

effort_min <- lapply(effort, 
                     function(e) {
                       as.numeric(e[1])*60+ as.numeric(e[2]) + as.numeric(e[3])/60
                       })
effort_min <- unlist(effort_min)

dat$France_STELI[, samplingEffort := effort_min]

## Coordinates -----
lapply(dat, function(d) head(d$decimalLongitude))
lapply(dat, function(d) head(d$verbatimCoordinates))


cols <- c("decimalLatitude", "decimalLongitude")
lapply(dat,
       function(d) {
         # cols <- colnames(d)
         if ("decimalLongitude" %in% colnames(d)) {
           d[, c("decimalLongitude", "decimalLatitude") := .(clean_coord(decimalLongitude),
                                                             clean_coord(decimalLatitude))]
           return(NULL)
         }
       })

# Check values
i <- 0
for (d in dat) {
  i <- i + 1
  print(paste("Rep", i, "---------"))
  
  tst_lon_num <- as.numeric(d$decimalLongitude)
  print(d$decimalLongitude[which(is.na(tst_lon_num))])
  
  tst_lat_num <- as.numeric(d$decimalLatitude)
  print(d$decimalLatitude[which(is.na(tst_lat_num))])
}

# Convert coordinates

dat_convert <- lapply(dat,
                      function(d) !("decimalLongitude" %in% colnames(d)))

(names_convert <- names(dat_convert)[unlist(dat_convert)])

# Belgium 1
dat$Belgium1[, verbatimCoordinates := paste0("31U", verbatimCoordinates)]
dat$Belgium1[ , c("decimalLatitude", "decimalLongitude") := 
                .(mgrs_to_latlng(verbatimCoordinates)$lat, 
                  mgrs_to_latlng(verbatimCoordinates)$lng)]

any(is.na(dat$Belgium1$decimalLatitude))
any(is.na(dat$Belgium1$decimalLongitude))

# Netherlands
ned_coord <- st_coordinates(
  st_transform(
    st_as_sf(dat$Netherlands, 
             coords = c("verbatimLongitude", "verbatimLatitude"), 
             crs = 28992), 
    4326))

dat$Netherlands[, c("decimalLongitude", "decimalLatitude") 
                := .(ned_coord[,"X"], ned_coord[,"Y"])]

any(is.na(dat$Netherlands$decimalLatitude))
any(is.na(dat$Netherlands$decimalLongitude))

lapply(dat, colnames)

lapply(dat, nrow)

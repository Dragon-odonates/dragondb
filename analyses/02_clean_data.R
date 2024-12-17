# Header #############################################################
#
# Author: Lisa Nicvert
# Email:  lisa.nicvert@fondationbiodiversite.fr
#
# Date: 2024-12-17
#
# Script Description: clean data in columns of standardized files


# Libraries ---------------------------------------------------------------

# remotes::install_github("hrbrmstr/mgrs")
library(mgrs)
library(sf)

library(readxl)
library(data.table)
library(here)

library(rgbif)
library(stringr)

library(dragondb)

read_folder <- here("data/02_data_std")

read_folder_meta <- here("data/01_data_raw/metadata")

# Read data ---------------------------------------------------------------
ls <- list.files(read_folder,
                 full.names = TRUE)

nam <- gsub("\\.csv$", "", basename(ls))

dat <- lapply(ls,
              fread,
              header = TRUE,
              na.strings = c("", "NA"),
              sep = ",")
names(dat) <- nam


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

# Check against GBIF backbone
names_list <- lapply(dat,
                     function(d) name_backbone_checklist(unique(d$scientificName)))

# Check match types (ideally, exact only)
lapply(names_list,
       function(n) unique(n$matchType))

lapply(names_list,
       function(n) n[n$matchType == "HIGHERRANK",])
lapply(names_list,
       function(n) n[n$matchType == "NONE" |
                       n$matchType == "FUZZY" ,][, c("verbatim_name",
                                                     "canonicalName",
                                                     "genus",
                                                     "matchType")])

names_merge <- lapply(names_list,
                      function(n) data.table(n[, c("canonicalName",
                                                   "verbatim_name",
                                                   "genus",
                                                   "family",
                                                   "rank",
                                                   "usageKey")]))

dat <- lapply(seq_along(dat),
              function(i) {
                names_merge[[i]][dat[[i]],
                                 on = c(verbatim_name = "scientificName")]
              })
names(dat) <- names(names_merge)

lapply(dat, setnames,
       old = c("canonicalName", "verbatim_name", "rank", "usageKey"),
       new = c("scientificName", "verbatimName", "taxonRank", 'GBIFkey'))

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

# Hour and sampling effort with STELI
dat$France_STELI[, eventTime := as.ITime(eventTime)]

effort_char <- dat$France_STELI$samplingEffort # Negative values
# -> I suspect start and end dates have been inverted
effort_char[effort_char == ""] <- NA

effort <- str_split(effort_char, ":")

effort_min <- lapply(effort,
                     function(e) {
                       as.numeric(e[1])*60+ as.numeric(e[2]) + as.numeric(e[3])/60
                     })
effort_min <- unlist(effort_min)

dat$France_STELI[, samplingEffort := effort_min]

## Coordinates -----

# Clean coordinates

# First get the data names that should be cleaned
cols <- c("decimalLongitude", "decimalLatitude")
dat_clean <- unlist(lapply(dat,
                           function(d) all(cols %in% colnames(d)))
                    )
names_clean <- names(dat_clean[dat_clean])
names_convert <- names(dat_clean[!dat_clean])

lapply(names_clean,
       function(nam) {
         d <- dat[[nam]]
         d[, c("decimalLongitude",
               "decimalLatitude") :=
             .(clean_coord(decimalLongitude),
               clean_coord(decimalLatitude))]
         return(NULL)
       })

dat$France_STELI[, c("lon centroid site", "lat centroid site") :=
                   .(clean_coord("lon centroid site"),
                     clean_coord("lat centroid site"))]

# Convert coordinates
(names_convert)

# Belgium 1
dat$Belgium1[, verbatimCoordinates := paste0("31U", verbatimCoordinates)]
dat$Belgium1[ , c("decimalLatitude", "decimalLongitude") :=
                .(mgrs_to_latlng(verbatimCoordinates)$lat,
                  mgrs_to_latlng(verbatimCoordinates)$lng)]

# Netherlands
ned_coord <- st_coordinates(
  st_transform(
    st_as_sf(dat$Netherlands,
             coords = c("verbatimLongitude", "verbatimLatitude"),
             crs = 28992),
    4326))

dat$Netherlands[, c("decimalLongitude", "decimalLatitude")
                := .(ned_coord[,"X"], ned_coord[,"Y"])]

# Convert all to geometries
to_convert <- names(dat)[names(dat) != "France_STELI"]

lapply(to_convert,
       function(nam) {
         dat[[nam]][, decimalCoordinates :=
                      st_as_text(st_as_sf(dat[[nam]],
                                          coords = c("decimalLongitude",
                                                     "decimalLatitude"),
                                          na.fail = FALSE)$geometry)]
       })

# Pre-copy data because it takes a long time to clean coords
lapply(names(dat),
       function(nam) write.table(dat[[nam]],
                                 file = here(file.path("data/03_data_clean/tmp",
                                                       paste0(nam, ".csv"))),
                                 row.names = FALSE,
                                 qmethod = "double",
                                 sep = ",")
)

ls <- list.files(here("data/03_data_clean/tmp"),
                 full.names = TRUE)
nam <- gsub("\\.csv$", "", basename(ls))

dat <- lapply(ls,
              fread,
              header = TRUE,
              na.strings = c("", "NA"),
              sep = ",")
names(dat) <- nam

# Recode columns ----------------------------------------------------------

# To write


# Write files ------------------------------------------------------------------
# lapply(names(dat),
#        function(nam) write.table(dat[[nam]],
#                                  file = here(file.path("data/03_data_clean",
#                                                        paste0(nam, ".csv"))),
#                                  row.names = FALSE,
#                                  qmethod = "double",
#                                  sep = ",")
# )

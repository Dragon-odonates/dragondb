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

# Read data ---------------------------------------------------------------
ls <- list.files(read_folder,
                 full.names = TRUE)

nam <- gsub("\\.csv$", "", basename(ls))

dat <- lapply(ls,
              fread,
              header = TRUE,
              nrows = 300,
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
       function(n) unique(n$rank))

lapply(names_list,
       function(n) n[n$matchType == "SUBSPECIES",])
# tst <- names_list$France_STELI
# tst |>
#   filter(species == "Calopteryx virgo") |>
#   select(scientificName, species, speciesKey)

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
                                                   "species",
                                                   "speciesKey",
                                                   "usageKey")]))
dat <- lapply(seq_along(dat),
              function(i) {
                names_merge[[i]][dat[[i]],
                                 on = c(verbatim_name = "scientificName")]
              })
names(dat) <- names(names_merge)

lapply(dat, setnames,
       old = c("canonicalName", "verbatim_name", "rank", "usageKey", "speciesKey"),
       new = c("scientificName", "verbatimName", "taxonRank", "taxonID", "speciesID"))


# tst <- copy(dat$France_STELI)
# tst |>
#     filter(species == "Calopteryx virgo") |>
#     select(scientificName, species, taxonID, speciesID, taxonRank)
# tst |>
#   filter(taxonRank != "SPECIES") |>
#   select(scientificName, species, taxonID, speciesID, taxonRank)


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
                                            format = date_fmt[[n]])
                   ]
           dat[[n]][, dateUncertainty := "day"]
         } else {
           # Determine uncertainty & replace values
           for (i in 1:nrow(dat[[n]])) {
             if (is.na(dat[[n]][i, year])) {
               prec <- "nodate"
             } else if (is.na(dat[[n]][i, month])) {
               prec <- "year"
               dat[[n]][i, c("month", "day") := .(1, 1)]
             } else if (is.na(dat[[n]][i, day])) {
               prec <- "month"
               dat[[n]][i, day := 1]
             } else {
               prec <- "day"
             }
             dat[[n]][i, dateUncertainty := prec]
           }
           dat[[n]][, eventDate := as.IDate(paste(year,
                                                  month,
                                                  day, sep = "-"),
                                            format = "%Y-%m-%d")]
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

# dat$France_STELI[, c("lon centroid site", "lat centroid site") :=
#                    .(clean_coord(`lon centroid site`),
#                      clean_coord(`lat centroid site`))]

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

# # Pre-copy data because it takes a long time to clean coords
# lapply(names(dat),
#        function(nam) write.table(dat[[nam]],
#                                  file = here(file.path("data/03_data_clean/tmp",
#                                                        paste0(nam, ".csv"))),
#                                  row.names = FALSE,
#                                  qmethod = "double",
#                                  sep = ",")
# )
#
# ls <- list.files(here("data/03_data_clean/tmp"),
#                  full.names = TRUE)
# nam <- gsub("\\.csv$", "", basename(ls))
#
# dat <- lapply(ls,
#               fread,
#               header = TRUE,
#               na.strings = c("", "NA"),
#               sep = ",")
# names(dat) <- nam

# Recode columns ----------------------------------------------------------

## Recode life stage -----

# Select data that need recoding
lifestage_key <- c("adult", "exuvia", "larva")
names(lifestage_key) <- c("lifeStage_count_a",
                          "lifeStage_count_e",
                          "lifeStage_count_l")

dat_recode_names <- unlist(lapply(dat,
                                  function(d) any(grepl("^lifeStage_count",
                                                        names(d)))
                                  )
                           )

dat_recode_names <- names(dat_recode_names)[dat_recode_names]

# Convert to numeric
lapply(dat[dat_recode_names],
       function(d) {
         d[, names(.SD) := lapply(.SD, clean_count),
           .SDcols = patterns("^lifeStage_count", cols = names(d))]
         return(NULL)
         }
       )
# A few data from Cyprus are lost

# Convert pseudo-zeros to NA
for (nam in dat_recode_names) {

  d <- dat[[nam]]

  # Get lifeStage columns
  cols <- grep("^lifeStage_count", x = names(d), value = TRUE)

  # Get rows for which there was at least one obs
  pseudo_zeroes <- which(rowSums(d[, ..cols]) != 0)

  # For these rows, transform all zeroes to NAs
  d[pseudo_zeroes,
    names(.SD) := lapply(.SD, function(v) ifelse(v == 0, NA, v)),
    .SDcols = cols]
}

# Reshape data
for (nam in dat_recode_names) {
  d <- dat[[nam]]

  if ("lifeStage" %in% names(d)) {
    d[, lifeStage := NULL]
  }
  if ("count" %in% names(d)) {
    d[, count := NULL]
  }

  dat[[nam]] <- melt(d,
                     measure.vars = patterns("^lifeStage_count"),
                     variable.name = "lifeStage", value.name = "count",
                     na.rm = TRUE)
  dat[[nam]][, lifeStage := lifestage_key[lifeStage]]

  dat[[nam]] <- dat[[nam]][count != 0]
}

lapply(dat, head)

## eventType -----

lapply(dat, function(d) unique(d$eventType))

# Correct data for Belgium2 and Cyprus1

# For Belgium2 not yet with our data
# type_orig <- unique(dat$Belgium2$eventType)
#
# type_new <- c("transect", "site_counts")

type_orig <- unique(dat$Cyprus1$eventType)
type_new <- c("museum_specimen")
names(type_new) <- type_orig

dat$Cyprus1[, eventType := unname(type_new[eventType])]

# For other datasets: use info
datinfo <- data.table(read_excel(here("data/metadata/metadata.xlsx"),
                                 sheet = 1))

# Get dataset-sampling key
dat_protocol <- datinfo$samplingProtocol
names(dat_protocol) <- datinfo$datasetName

# Get data to increment
ind_auto <- which(!(names(dat) %in% c("Belgium2", "Cyprus1")))
lapply(ind_auto,
       function(i) {
         nam <- names(dat)[i]
         dat[[i]][, eventType := dat_protocol[nam]]
       })

lapply(dat, function(d) unique(d$eventType))


## Dataset -----

datinfo_par <- datinfo[isParentDataset == TRUE, ]
datinfo_nopar <- datinfo[isParentDataset == FALSE, ]

### Add datasets names and IDs to dat -----
ind_nopar <- which(names(dat) %in% datinfo_nopar$datasetName)

lapply(seq_along(dat),
       function(i) {
         if (i %in% ind_nopar) { # This dataset has no children datasets
           # Get dataset ID corresponding to list name
           id <- datinfo_nopar[datasetName == names(dat)[i], datasetID]

           # Set datasetID
           dat[[i]][, datasetID := id]

         } else { # This dataset has children datasets
           # Get dataset ID corresponding to list name
           id <- datinfo_par[datasetName == names(dat)[i], datasetID]

           # Set parentDatasetID
           dat[[i]][, parentDatasetID :=  id]

           # Set children datasetID

           # Get unique names of children datasets
           dnames <- unique(dat[[i]]$datasetName)

           # Generate datasetID on the model first dataset letter + X + number
           sq <- seq(1, length(dnames))
           did <- str_pad(sq, 2, pad = "0")
           did <- paste0(str_sub(id, 1, 1), "X", did)
           names(did) <- dnames

           # Set datasetID
           dat[[i]][, datasetID := did[datasetName]]
         }
       })


# Write files ------------------------------------------------------------------
lapply(names(dat),
       function(nam) write.table(dat[[nam]],
                                 file = here(file.path("data/03_data_clean/subset",
                                                       paste0(nam, ".csv"))),
                                 row.names = FALSE,
                                 qmethod = "double",
                                 sep = ",")
)

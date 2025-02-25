# Header #############################################################
#
# Author: Lisa Nicvert
# Email:  lisa.nicvert@fondationbiodiversite.fr
#
# Date: 2024-12-17
#
# Script Description: format data for database and add columns



# Libraries ---------------------------------------------------------------
library(here)
library(data.table)

library(RPostgres)

library(readxl)

library(stringr)

# For spatial info
library(rnaturalearth)
library(sf)
library(dplyr)

library(dragondb)

read_folder <- here("data/03_data_clean")

populate_db <- TRUE # run the code that populates the db?

# Read data ---------------------------------------------------------------

ls <- list.files(read_folder,
                 full.names = TRUE)
ls <- grep(pattern = "tmp$", ls, invert = TRUE, value = TRUE)
nam <- gsub("\\.csv$", "", basename(ls))

dat <- lapply(ls,
              fread,
              header = TRUE,
              # na.strings = c("", "NA"),
              # nrows = 1000,
              sep = ",")
names(dat) <- nam

# Connect to DB -----------------------------------------------------------
# Connect to "local" DB
con <- dbConnect(
  drv       = RPostgres::Postgres(), # dbDriver("PostgreSQL"),
  dbname    = "dragon",
  host      = "localhost", # "192.168.0.75",
  port      = 5432,
  user      = Sys.getenv('USERNAME'),
  password  = Sys.getenv('PASSWORD')
)
# Check DB
dbListTables(con)

# dbDisconnect(con)

# Atomize data & copy to DB -----------------------------------------------

## Species -----
colnames <- colnames_DB(con, "Taxon", rm_ID = FALSE)



# Filter out records that have too vague species
dat <- lapply(dat,
              function(d) d[taxonRank != "ORDER",])
# Filter out spp genus treated as spp
dat <- lapply(dat,
              function(d) d[!(scientificName %in% c("Anisoptera", "Zygoptera")),])

# # Issue with Ophiogomphus cecilia
# lapply(dat,
#        function(d) unique(d[scientificName == "Ophiogomphus cecilia", .(taxonID, scientificName, verbatimName)]))

# Minimal reproducible example
# tst  <- c("Ophiogomphus cecilia (Geoffroy in Fourcroy, 1785)",
#           "Ophiogomphus cecilia")
# library(rgbif)
# chk <- name_backbone_checklist(tst)

# tst <- c("Anisoptera", "Zygoptera")
# gbif <- name_backbone_checklist(tst, order = "Odonata")
# Quick fix
dat <- lapply(dat,
              function(d) d[scientificName == "Ophiogomphus cecilia",
                            taxonID := 1426058])
dat <- lapply(dat,
              function(d) d[scientificName == "Ophiogomphus cecilia",
                            speciesID := 1426058])

# Issue with genus too -> keep only spp
dat <- lapply(dat,
              function(d) d[taxonRank == "SPECIES", ])

spdf <- lapply(dat, function(d) d[, ..colnames])

spdf <- rbindlist(spdf, fill = TRUE)

spdf <- unique(spdf)
spdf <- rm_all_na(spdf)

if (populate_db) {
  dbAppendTable(con, "Taxon", spdf)
}


## Recorder -----
cols <- c("recordedBy", "recordedByID")

redf <- lapply(dat, df_all_cols, cols = cols)

redf <- lapply(redf, unique)

redf <- do.call("rbind",
                c(redf, fill = TRUE))

# Replace empty with NA
redf <- redf[recordedBy == "", recordedBy := NA]
redf <- rm_all_na(redf)

setnames(redf,
         old = c("recordedBy", "recordedByID"),
         new = c("name", "recorderID_orig"))

if (populate_db) {
  dbAppendTable(con, "Recorder", redf)
}

### Add recorder columns in dat -----
rec_db <- dbGetQuery(con, 'SELECT "recorderID", "recorderID_orig" AS "recordedByID",
                           "name" AS "recordedBy"
                            FROM "Recorder";')
rec_db <- data.table(rec_db)

dat <- lapply(dat,
              function(d) {
                # Add columns to data if not present
                cols_to_add <- cols[!(cols %in% colnames(d))]
                d[, (cols_to_add) := NA]

                # Join
                res <- join_dt_na(d, rec_db, cols1 = cols)
                res[, (cols_to_add) := NULL]
              })

lapply(dat, function(d) any(is.na(d$recorderID)))
# Belgium1,2 and Cyprus1,2 have NAs because they have no observer
# Netherlands too because some are NA

## EventDate -----
cols <- colnames_DB(con, "EventDate", rm_ID = TRUE)

# Correct Cyprus typo
dat$Cyprus1[, ":="(year = year(eventDate),
           month = month(eventDate),
           day = mday(eventDate))]
# dat$Cyprus1[year < 1200, year]
dat$Cyprus1[year == 21, year := 2021]
# dat$Cyprus1[year > 2030, year]
dat$Cyprus1[year == 3002, year := 2002]
dat$Cyprus1[year == 2921, year := 2021]

dat$Cyprus1[, eventDate := as.IDate(paste(year, month, day, sep = "-"))]

dtdf <- lapply(dat, df_all_cols, cols = cols)
dtdf <- lapply(dtdf, function(d) d[, eventTime := as.ITime(eventTime)])

dtdf <- do.call("rbind",
                c(dtdf, fill = TRUE))

dtdf <- unique(dtdf)
dtdf <- rm_all_na(dtdf)

if (populate_db) {
  dbAppendTable(con, "EventDate", dtdf)
}

### Add date ID column in dat -----
dt_db <- dbGetQuery(con, 'SELECT * FROM "EventDate";')
dt_db <- data.table(dt_db)

dat <- lapply(dat,
              function(d) {
                # Add columns to data if not present
                cols_to_add <- cols[!(cols %in% colnames(d))]
                d[, (cols_to_add) := NA]

                # Join
                res <- join_dt_na(d, dt_db, cols1 = cols)
                res[, (cols_to_add) := NULL]
              })

lapply(dat, function(d) any(is.na(d$eventDateID)))
# No NAs dates \o/

lapply(dat, function(d) df_all_cols(d[is.na(d$eventDateID),],
                                    c("eventDate", "eventTime", "eventDateUncertainty")))

## Dataset -----

# Get datasets IDs
dadf <- lapply(dat, df_all_cols, c("datasetID", "parentDatasetID"))
dadf <- lapply(dadf, unique)
dadf <- do.call("rbind",
                dadf)

# Add parents as standalone datasets
add_par <- na.omit(unique(dadf$parentDatasetID))
add_par <- data.table(datasetID = add_par,
                      parentDatasetID = NA)

dadf <- rbind(add_par, dadf)

# Add datasets infos
datinfo <- data.table(read_excel(here("data/metadata/metadata.xlsx"),
                                 sheet = 1))

datmerge <- datinfo[, .(datasetID, isParentDataset, datasetName,
                        description)]

dadf <- datmerge[dadf, on = "datasetID"]

# Remove dataests that are NA
dadf <- dadf[which(!is.na(dadf$datasetID)), ]

if (populate_db) {
  dbAppendTable(con, "Dataset", dadf)
}

## Contact -----
cols <- colnames_DB(con, "Contact", rm_ID = FALSE)

contacts_df <- data.table(read_excel(here("data/metadata/metadata.xlsx"),
                                     sheet = 2))
contacts_datasets <- data.table(read_excel(here("data/metadata/metadata.xlsx"),
                                           sheet = 3))

# Get only contacts for datasets in DB ---
# Get datasets in DB
dadf_db <- dbGetQuery(con, 'SELECT * FROM "Dataset";')
dadf_db <- data.table(dadf_db)

# Get contacts that are for this dataset
co_indb <- contacts_datasets[datasetID %in% dadf_db$datasetID, ]
contacts_df <- contacts_df[contactID %in% co_indb$contactID, ]

codf <- df_all_cols(contacts_df, cols = cols)

if (populate_db) {
  dbAppendTable(con, "Contact", codf)
}

## DatasetContact -----
dcdf <- contacts_datasets[datasetID %in% dadf_db$datasetID, ]

cols <- colnames_DB(con, "DatasetContact", rm_ID = FALSE)
dcdf <- df_all_cols(dcdf, cols = cols)

if (populate_db) {
  dbAppendTable(con, "DatasetContact", dcdf)
}

## Location -----

# Fix errors
# Fix Cyprus encoding (Cyprus1)
dat$Cyprus1[locality == "Ge\x87itkoy", locality := "Geçitkoy"]
dat$Cyprus1[locality == "G\x94nendere", locality := "Gönendere"]
dat$Cyprus1[locality == "Koru\x87am", locality := "Koruç87am"]
dat$Cyprus1[locality == "K\x94prula GDK", locality := "Köprula"]
dat$Cyprus1[locality == "Trimethousa near Evretou\xff", locality := "Trimethousa near Evretou"]
dat$Cyprus1[locality == "Koru\x87am/G\x94leti", locality := "Koruçam/Göleti"]
dat$Cyprus1[locality == "Kritou Terra caf\x82", locality := "Kritou Terra café"]
dat$Cyprus1[locality == "Stavros tis Psokas valley\xff", locality := "Stavros tis Psokas valley"]
dat$Cyprus1[locality == "Filousa hillside nr Evretou\xff", locality := "Filousa hillside nr Evretou"]
dat$Cyprus1[locality == "Ge\x87itkoy Lower", locality := "Geçitkoy Lower"]
dat$Cyprus1[locality == "Anarita Mast\xff", locality := "Anarita Mast"]

# Replace (inderminada) with NA
dat$Catalonia[verbatimCounty == "(indeterminada)",
              verbatimCounty := NA]

# Add country for Spain
dat$Catalonia[!is.na(verbatimCounty) | !is.na(locality) | !is.na(municipality),
              verbatimCountry := "Spain"]


# unique(grep("aaa", dat$Cyprus1$locality, value = TRUE))

# Coerce to numeric
# a <- lodf$coordinateUncertaintyInMeters
# b <- as.numeric(a)
# coerced_to_na(a, b) # "NULL" converted to NA
dat <- lapply(dat,
              function(d) {
                if ("coordinateUncertaintyInMeters" %in% colnames(d)) {
                  d[,  coordinateUncertaintyInMeters := as.numeric(coordinateUncertaintyInMeters)]
                } else {
                  d
                }
              })


# Get DB columns
cols <- colnames_DB(con, "Location", rm_ID = TRUE)

# Get columns to select in DB (from cols)
cols2 <- cols
cols2[cols2 == "country"] <- "verbatimCountry"
cols2[cols2 == "county"] <- "verbatimCounty"

### Prepare data -----
# Get a subset of data with only relevant columns
lodf <- lapply(dat, df_all_cols, cols = cols2)
lodf <- lapply(lodf, unique)

# Add parent coordinates
# IDs of datasets that have at least one parent coordinates
names_par <- names(which(sapply(dat,
                                function(d) !all(is.na(d$parentCoordinates)))))

# Get these datasets
par_loc <- copy(dat[names_par]) # For now it's only France_STELI

# Format parent coordinates
par_loc <- lapply(par_loc,
                  function(d) {
                    res <- unique(d[, .(parentCoordinates)])
                    setnames(res,
                             old = "parentCoordinates",
                             new = "decimalCoordinates")
                  })

# Add parents as standalone coordinates
for (n in names_par) {
  lodf[[n]] <-  rbind(par_loc[[n]], lodf[[n]], fill = TRUE)
}

# Read geographic info
geo <- fread(here("data/precomputed/country.csv"),
             header = TRUE,
             na.strings = c("", "NA"),
             sep = ",")

# Standardize (commented bc looong)
# geo[, decimalCoordinates := std_coord_text(decimalCoordinates)]
# lapply(lodf,
#        function(d) d[, decimalCoordinates := std_coord_text(decimalCoordinates)])

for (nam in names(lodf)) {
  geo_i <- geo[dataset == nam, .(decimalCoordinates, country, county)]

  lodf[[nam]] <- geo_i[lodf[[nam]], on = "decimalCoordinates"]

  na_coord <- grep("EMPTY", lodf[[nam]]$decimalCoordinates)

  if (length(na_coord) != 0) { # Some points have no coord
    # Rempace values with original ones
    lodf[[nam]][na_coord, country := verbatimCountry]
    lodf[[nam]][na_coord, county := verbatimCounty]
  }
}


# Check!! (should be all empty)
lapply(lodf,
       function(d) d[is.na(decimalCoordinates), .(is.na(decimalCoordinates), country, county)])

lodf <- do.call("rbind", lodf)

# Remove verbatim
lodf <- df_all_cols(lodf, cols)

# Unique
lodf <- unique(lodf)

# Commented out constraint because of error
# ERROR:  index row size 3048 exceeds btree version 4 maximum 2704 for index "Location_decimalCoordinates_coordinateUncertaintyInMeters_l_key"
# DETAIL:  Index row references tuple (3337,1) in relation "Location".
# HINT:  Values larger than 1/3 of a buffer page cannot be indexed.
# Consider a function index of an MD5 hash of the value, or use full text indexing.
# CONTEXT:  COPY Location, line 1086752
if (populate_db) {
  dbAppendTable(con, "Location", lodf)
}


### Add locationID to dat -----
# get locationID ---
# Complete request with country/county (some joins need these values)
loc_db_nocoord <- dbGetQuery(con, 'SELECT "locationID",
                             ST_AsText("decimalCoordinates") AS "decimalCoordinates",
                             "coordinateUncertaintyInMeters",
                             "locality", "municipality", "county", "country"
                             FROM "Location";')
loc_db_nocoord <- data.table(loc_db_nocoord)

# Standardize wkt
loc_db_nocoord[, decimalCoordinates := std_coord_text(decimalCoordinates)]

# Remove county/country (for joins that don't need it)
loc_db_coord <- copy(loc_db_nocoord[, c("county", "country") := NULL])

# Get columns to make join on
# If dat has coordinates
cols_coord <- c("decimalCoordinates",
                "coordinateUncertaintyInMeters",
                "locality", "municipality")
# If dat has no coordinates
# For original data
cols1_nocoord <- c(cols_coord, c("verbatimCountry", "verbatimCounty"))
# For db
cols2_nocoord <- c(cols_coord, c("country", "county"))

datnames <- names(dat)

dat <- lapply(names(dat),
               function(n) {
                 print(paste(n, "------------"))
                 # In all cases: add location ID for event

                 # Standardize wkt
                 dat[[n]][, decimalCoordinates := std_coord_text(decimalCoordinates)]

                 # Join
                 dat_na <- copy(dat[[n]][is.na(decimalCoordinates), ])
                 dat_coord <- copy(dat[[n]][!is.na(decimalCoordinates), ])

                 if (nrow(dat_na) != 0) { # for NA coordinates
                   # Add columns to data, including verbatimCountry/County
                   cols_to_add <- cols1_nocoord[!(cols1_nocoord %in% colnames(dat_na))]
                   dat_na[, (cols_to_add) := NA]

                   res1 <- join_dt_na(dat_na,
                                      loc_db_nocoord,
                                      cols1 = cols1_nocoord,
                                      cols2 = cols2_nocoord)
                 } else {
                   res1 <- data.table()
                 }
                 if (nrow(dat_coord) != 0) { # for non-na coordinates
                   # Add columns to data, without verbatimCountry/County
                   cols_to_add <- cols_coord[!(cols_coord %in% colnames(dat_coord))]
                   dat_coord[, (cols_to_add) := NA]

                   res2 <- join_dt_na(dat_coord,
                                      loc_db_coord,
                                      cols1 = cols_coord)
                   res2[, (cols_to_add) := NULL]
                 } else {
                   res2 <- data.table()
                 }

                 res <- rbind(res1, res2)

                 if ( any(is.na(res$eventLocationID)) ) {
                   warning("There are NA locations IDs in dataset ", n, " :(")
                 }

                 if ("parentCoordinates" %in% names(res)) { # If dataset has parentCoord
                   # Also add location for parent

                   # Standardize wkt
                   res[, parentCoordinates := std_coord_text(parentCoordinates)]

                   # Rename parent coordinates (to avoid conflicts with names already in data)
                   loc_db_par <- loc_db_coord[, .(locationID, decimalCoordinates)]
                   setnames(loc_db_par,
                            old = c("decimalCoordinates", "locationID"),
                            new = c("parentCoordinates", "parentLocationID"))
                   # Join
                   # We assume parent coordinates to have coordinates
                   res <- join_dt_na(res,
                                     loc_db_par,
                                     cols1 = "parentCoordinates")

                   if ( any(is.na(res$parentLocationID)) ) {
                       warning("There are NA parent locations IDs in dataset ", n, " :(")
                   }

                 }
                 return(res)
               })
names(dat) <- datnames

## Event -----
# Get columns names that are used to get table
cols <- colnames_DB(con, "Event", rm_ID = TRUE)
cols <- cols[cols!= "parentEventID"]
cols <- c(cols, "parentLocationID")

# Correct issues in eventType
lapply(dat,
       function(d) unique(d[, eventType]))

dat <- lapply(dat,
              function(d) d[eventType == "TransectCount",
                            eventType := "Transect"])

evdf <- lapply(dat, df_all_cols, cols = cols)

### Reformat to add parent event -----

# IDs of datasets that have parents
names_par <- names(which(sapply(dat,
                                function(d) !all(is.na(d$parentLocationID)))))

# Add parent events and isParent
evdf <- lapply(names(evdf),
               function(n) {
                 if (n %in% names_par) { # if this dataset has parent events
                   # Add parents as standalone events
                   res <- unique(evdf[[n]][, .(parentLocationID, datasetID)])
                   setnames(res,
                            old = "parentLocationID",
                            new = "locationID")

                   res[, isParentEvent := TRUE] # set isParent to TRUE for parents
                   evdf[[n]][, isParentEvent := FALSE] # FALSE for others

                   # Add parent events
                   rbind(res, evdf[[n]], fill = TRUE)

                   } else { # dataset has no parent
                     # All these events have no parent and are not parents
                     evdf[[n]][, isParentEvent := FALSE]
                     evdf[[n]][, parentLocationID := NA]
                   }
                 })
names(evdf) <- datnames

evdf <- lapply(evdf, df_all_cols, cols = cols)

evdf <- data.table(do.call("rbind", evdf))

evdf <- unique(evdf)

if (populate_db) {
  # Add parent events
  evdf_par <- evdf[isParentEvent == TRUE, ]
  evdf_par <- evdf_par[, parentLocationID := NULL] # remove LocationID column

  dbAppendTable(con, "Event", evdf_par)

  # Get parent event IDs
  par_id <- dbGetQuery(con, 'SELECT "eventID" AS "parentEventID",
                             "Event"."locationID" AS "parentLocationID"
                              FROM "Event"
                              LEFT JOIN "Location" ON "Event"."locationID" = "Location"."locationID";')
  par_id <- data.table(par_id)

  # Add parentEventID to children events
  evdf_nopar <- evdf[isParentEvent == FALSE, ]

  # Join on parent location
  evdf_nopar <- par_id[evdf_nopar,
                       on = "parentLocationID"]
  evdf_nopar <- evdf_nopar[, parentLocationID := NULL]

  # There are a lot of identical events except for covariates,
  # so removed database constraint on unicity (to check)
  dup <- evdf_nopar[duplicated(evdf_nopar[, .(locationID, eventDateID, recorderID,
                                              datasetID, eventType, parentEventID)]),]
  table(dup$datasetID, useNA = "always")

  dbAppendTable(con, "Event", evdf_nopar)
}

### Add event ID to dat -----

ev_db <- dbGetQuery(con, 'SELECT "eventID", "locationID", "eventDateID",
                          "recorderID", "datasetID", "eventType", "parentEventID",
                          "isParentEvent"
                           FROM "Event";')
ev_db <- data.table(ev_db)

# Remove parent events because we don't want to join them
ev_db <- ev_db[isParentEvent == FALSE, ]
ev_db[, isParentEvent := NULL] # also don(t need this column)

cols <- colnames(ev_db)
cols <- cols[!(cols %in% c("eventID", "parentEventID"))]

dat <- lapply(dat,
              function(d) {
                # Add columns to data if not present
                cols_to_add <- cols[!(cols %in% colnames(d))]
                d[, (cols_to_add) := NA]

                # Join
                res <- join_dt_na(d, ev_db, cols1 = cols)
                res[, (cols_to_add) := NULL]
              })

lapply(dat, function(d) any(is.na(d$eventID)))
# No events are NA

## Occurrence -----
cols <- colnames_DB(con, "Occurrence", rm_ID = TRUE)

oc_df <- lapply(dat, df_all_cols, cols)

# Should be corrected
lapply(oc_df, function(d) unique(d$sex))

oc_df <- do.call("rbind", oc_df)
# oc_df <- unique(oc_df)

if (populate_db) {
  dbAppendTable(con, "Occurrence", oc_df)
}


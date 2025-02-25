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
# ls <- ls[c(4, 7, 8, 11)]

nam <- gsub("\\.csv$", "", basename(ls))

dat <- lapply(ls,
              fread,
              header = TRUE,
              # nrows = 100,
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
dat$Cyprus1[scientificName == "Orthetrum sabIna",
            scientificName := "Orthetrum sabina"]

dat$Catalonia[scientificName == "Lestes sp.",
            scientificName := "Lestes"]

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
                     function(d) name_backbone_checklist(unique(d$scientificName),
                                                         order = "Odonata"))

# Check match types (ideally, exact only)
lapply(names_list,
       function(n) unique(n$matchType))

lapply(names_list,
       function(n) unique(n$rank))

lapply(names_list,
       function(n) n[n$rank == "SUBSPECIES",])
# tst <- names_list$France_STELI
# tst |>
#   filter(species == "Calopteryx virgo") |>
#   select(scientificName, species, speciesKey)

# Check matches that were not at spp rank

lapply(names_list,
       function(n) n[!(n$rank %in% c("SPECIES", "SUBSPECIES")),
                     c("verbatim_name", "rank", "canonicalName",
                       "genus", "family")] |> as.data.frame())

# Check spp that had no/fuzzy match
lapply(names_list,
       function(n) n[n$matchType %in% c("NONE", "FUZZY"),
                     c("verbatim_name", "canonicalName",
                       "genus", "matchType")] |> as.data.frame())

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
date_fmt <- list()

date_fmt$Belgium1 <- "%d/%m/%Y"
date_fmt$Belgium2 <- "%Y-%m-%d"
date_fmt$Cyprus2 <- "%Y-%m-%d"
date_fmt$France_STELI <- "%d/%m/%Y" # + time & sampling effort
date_fmt$France_Atlas <- "%Y-%m-%d"
date_fmt$UK <- "%Y-%m-%d"
date_fmt$Sweden <- "%Y-%m-%d"

lapply(names(dat),
       function(n) {
         print(n)
         if (n %in% names(date_fmt)) { # If we chose a date format (above)
           if (n == "Sweden") {
             # Special case because Sweden has start and end dates

             # Transform end/start to datetime
             start_time <- dat[[n]]$eventTimeStart
             start_time[is.na(start_time)] <- "00:00"
             start_dtime <- paste(dat[[n]]$eventDateStart, start_time)
             start_dtime <- as.POSIXct(start_dtime)

             end_time <- dat[[n]]$eventTimeEnd
             end_time[is.na(end_time)] <- "00:00"
             end_dtime <- paste(dat[[n]]$eventDateEnd, end_time)
             end_dtime <- as.POSIXct(end_dtime)

             # Compute effort
             eff <- as.numeric(difftime(end_dtime,
                                        start_dtime, units = "mins"))
             eff[eff == 0] <- NA

             dat[[n]][, samplingEffort := eff]

             setnames(dat[[n]],
                      old = c("eventDateStart", "eventTimeStart"),
                      new = c("eventDate", "eventTime"))

             dat[[n]][, eventTime := as.ITime(eventTime)]
           }
           dat[[n]][, eventDate := as.IDate(eventDate,
                                            format = date_fmt[[n]])]

           # Convert empty to NA
           dat[[n]][eventDate == "", eventDate := NA]

           # Create eventDateUncertainty column and initialize
           if ("eventDateUncertainty" %in% colnames(dat[[n]])) {
             dat[[n]][, eventDateUncertainty := NULL]
           }
           dat[[n]][, eventDateUncertainty := "day"]

           # Else, set precision to nodate when the date is NA
           dat[[n]][is.na(eventDate), eventDateUncertainty := "nodate"]

         } else { # No date format: date needs to be constructed

           # Special Catalonia case: recompute values
           if (n == "Catalonia") {
             date_time <- strsplit(dat[[n]]$eventDate, " ")

             date_elements <- lapply(date_time,
                                     function(x) str_split(x[1], "-"))
             date_elements <- lapply(date_elements, function(x) x[[1]])
             date_elements <- lapply(date_elements, as.numeric)

             dat[[n]][, year := sapply(date_elements, function(x) x[1])]
             dat[[n]][, month := sapply(date_elements, function(x) x[2])]
             dat[[n]][, day := sapply(date_elements, function(x) x[3])]
           }

           # Empty strings to NA (should already have been done at import but doesn't work)
           dat[[n]][year == "", year := NA]
           dat[[n]][month == "", month := NA]
           dat[[n]][day == "", day := NA]

           # Create eventDateUncertainty column and initialize
           if ("eventDateUncertainty" %in% colnames(dat[[n]])) {
             dat[[n]][, eventDateUncertainty := NULL]
           }
           dat[[n]][, eventDateUncertainty := "day"]

           # Determine uncertainty & replace values
           for (i in 1:nrow(dat[[n]])) {
             if (is.na(dat[[n]][i, year]) | dat[[n]][i, year] == 0) {
               prec <- "nodate"
             } else if (is.na(dat[[n]][i, month]) | dat[[n]][i, month] == 0) {
               prec <- "year"
               dat[[n]][i, c("month", "day") := .(1, 1)]
             } else if (is.na(dat[[n]][i, day]) | dat[[n]][i, day] == 0) {
               prec <- "month"
               dat[[n]][i, day := 1]
             } else {
               prec <- "day"
             }
             dat[[n]][i, eventDateUncertainty := prec]
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


# Check dates and effort
lapply(dat,
       function(d) df_all_cols(d, c("eventDate", "eventDateUncertainty",
                                    "eventTime", "samplingEffort")))

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

# STELI coordinates are clean
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

# Catalonia
dat$Catalonia[verbatimLongitude == 0, verbatimLongitude := NA]
dat$Catalonia[verbatimLatitude == 0, verbatimLatitude := NA]

cat_coord <- st_coordinates(
  st_transform(
    st_as_sf(dat$Catalonia,
             coords = c("verbatimLongitude", "verbatimLatitude"),
             crs = 23031,
             na.fail = FALSE),
    4326))

dat$Catalonia[, c("decimalLongitude", "decimalLatitude")
                := .(cat_coord[,"X"], cat_coord[,"Y"])]

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

# Check coordinates visually
lapply(dat,
       function(d) d[, decimalCoordinates])
lapply(dat,
       function(d) length(d[is.na(decimalCoordinates), decimalCoordinates]))
lapply(dat,
       function(d) length(d[grep("EMPTY", decimalCoordinates),
                            decimalCoordinates]))

# Recode columns ----------------------------------------------------------

## Recode life stage -----


# Code key/value
lifestage_key <- c("adult", "exuvia", "larva", "immature", NA)
names(lifestage_key) <- c("lifeStage_count_a",
                          "lifeStage_count_e",
                          "lifeStage_count_l",
                          "lifeStage_count_i",
                          "lifeStage_count_na")

# Select data that need recoding
dat_recode_names <- unlist(lapply(dat,
                                  function(d) any(grepl("^lifeStage_count",
                                                        names(d)))
                                  )
                           )
dat_recode_names <- names(dat_recode_names)[dat_recode_names]
dat_recode_names <- dat_recode_names[dat_recode_names != "Catalonia"]

# Recode life stages
dat[dat_recode_names] <- lapply(dat[dat_recode_names],
                                recode_counts, key = lifestage_key)

# Check
lapply(dat[dat_recode_names],
       function(d) d[, .(individualCount, lifeStage)])

# Catalonia special case (counts are also divided by sex)

# First get presences and absences
cols <- c("male_count",
          "female_count",
          "lifeStage_count_i",
          "lifeStage_count_na")

# If all counts are zero then it's an absence
dat$Catalonia[, occurrenceStatus := fifelse(rowSums(.SD, na.rm = TRUE) == 0,
                                  "absent", "present"),
    .SDcols = cols]

# There are true absences
table(dat$Catalonia$occurrenceStatus)

# Add ID for later merge
dat$Catalonia[, ID := 1:nrow(dat$Catalonia)]

dat$Catalonia[ID == 691, .(male_count, female_count,
                           lifeStage_count_na, lifeStage_count_i)]

# Data interpretation is unsure: males and females don't always sum up
# to give the result of lifestages. So we consider they add up
# (but count will probably not be reliable)
diff0 <- dat$Catalonia[lifeStage_count_na != 0,]
table(diff0$lifeStage_count_na == (diff0$male_count + diff0$female_count),
      useNA = "always")

diff0 <- dat$Catalonia[lifeStage_count_i != 0,]
table(diff0$lifeStage_count_i == (diff0$male_count + diff0$female_count),
      useNA = "always")

# First separate occurrences based on sex
sex_key <- c("male", "female")
names(sex_key) <- c("male_count",
                    "female_count")

# Get sex subtable
sex_df <- dat$Catalonia[, .(ID, male_count, female_count)]
sex_df <- recode_counts(sex_df, key = sex_key,
                        variable.name = "sex",
                        value.name = "individualCount_mf",
                        occurrenceStatus = "occurrenceStatus_mf",
                        all_zeroes_to_na = TRUE)

# Should be only present because zeroes were filtered out
unique(sex_df[, occurrenceStatus_mf])
sex_df[, occurrenceStatus_mf := NULL]

unique(sex_df[, .(sex, individualCount_mf)])

# Now do life stages
ls_key <- c("immature", "adult")
names(ls_key) <- c("lifeStage_count_i", "lifeStage_count_na")

ls_df <- dat$Catalonia[, .(ID, lifeStage_count_i, lifeStage_count_na)]

ls_df <- recode_counts(ls_df, key = ls_key,
                       variable.name = "lifeStage",
                       value.name = "individualCount_ls",
                       occurrenceStatus = "occurrenceStatus_ls",
                       all_zeroes_to_na = TRUE)

# Should be only present because zeroes were filtered out
unique(ls_df[, occurrenceStatus_ls])
ls_df[, occurrenceStatus_ls := NULL]

unique(ls_df[, .(lifeStage, individualCount_ls)])

# Remove columns that were computed
dat$Catalonia <- dat$Catalonia[, c("lifeStage_count_i", "lifeStage_count_na",
               "male_count", "female_count") := NULL]

# Merge sex
dat$Catalonia <- sex_df[dat$Catalonia, on = "ID"]
dup_sex <- duplicated(dat$Catalonia$ID)
sum(dup_sex) # Additional rows are occurrences that had males and females

dup_ID <- dat$Catalonia$ID[which(dup_sex)]
dat$Catalonia[ID == dup_ID[1], .(ID, sex, individualCount_mf)]

# Merge life stage
dat$Catalonia <- ls_df[dat$Catalonia, on = "ID"]

dup_lf <- duplicated(dat$Catalonia$ID)
dup_ID_lf <- dat$Catalonia$ID[which(dup_lf)]
dup_ID_lf <- dup_ID_lf[!(dup_ID_lf %in% dup_ID)]

length(dup_ID_lf) # Same
dat$Catalonia[ID == dup_ID_lf[2],
              .(ID, lifeStage, individualCount_ls,
                sex, individualCount_mf)]

# Set intermediate counts to NA
dat$Catalonia[is.na(individualCount_ls), individualCount_ls := 0]
dat$Catalonia[is.na(individualCount_mf), individualCount_mf := 0]

# Get sum
dat$Catalonia[, individualCount := individualCount_mf + individualCount_ls]

# Remove ID
length(unique(dat$Catalonia$ID))
dat$Catalonia[, ID := NULL]

# Check
dat$Catalonia[, .(individualCount, lifeStage, sex)]

## eventType -----

(ev_type <- lapply(dat, function(d) unique(d$eventType)))

ev_type <- sapply(ev_type, function(e) all(is.null(e)))
ind_auto <- names(ev_type)[ev_type]

# Datasets that already have sampling info
lapply(dat[!ev_type], function(d) unique(d$eventType))

# Belgium2
type_orig <- unique(dat$Belgium2$eventType)
type_new <- c(NA, "Transect", "SiteCounts")
names(type_new) <- type_orig

dat$Belgium2[, eventType := unname(type_new[eventType])]

# Cyprus1
type_orig <- unique(dat$Cyprus1$eventType)
type_new <- c("PreservedSpecimen", "Transect",
              "HumanObservation", "HumanObservation", "HumanObservation",
              NA)
names(type_new) <- type_orig

dat$Cyprus1[, eventType := unname(type_new[eventType])]

# Sweden
type_orig <- unique(dat$Sweden$eventType)
type_new <- c("HumanObservation", "PreservedSpecimen", "HumanObservation")
names(type_new) <- type_orig

dat$Sweden[, eventType := unname(type_new[eventType])]

# For other datasets: use info
datinfo <- data.table(read_excel(here("data/metadata/metadata.xlsx"),
                                 sheet = 1))

# Get dataset-sampling key
dat_protocol <- datinfo$samplingProtocol
names(dat_protocol) <- datinfo$datasetName

# Get data to increment
lapply(ind_auto,
       function(nam) {
         dat[[nam]][, eventType := dat_protocol[nam]]
       })

# Infer observation type for Catalonia
range(dat$Catalonia[!is.na(PUBLICACIO), ]$eventDate)

dat$Catalonia[!is.na(PUBLICACIO), eventType := "Historical"]

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

lapply(dat, function(d) unique(df_all_cols(d,
                                           c("datasetID", "datasetName",
                                             "parentDatasetID"))))

# Observer names ----------------------------------------------------------

res <- fread(here("data/precomputed/recorder/ned_names.csv"),
             header = TRUE,
             na.strings = c("", "NA"),
             sep = ",")

dat$Netherlands <- res[dat$Netherlands, on = "recordedByID"]

# Inspect results
unique(dat$Netherlands[, .(recordedBy, recordedByID)])
unique(dat$Netherlands[is.na(recordedBy),
                       .(recordedBy, recordedByID)])



# Count to integer --------------------------------------------------------

# UK has non-integer values (Unique values: 6–20, 2–5, Present)
lapply(names(dat),
       function(nam) {
         print(nam)
         d <- dat[[nam]]
         if ("individualCount" %in% colnames(d)) {
           b <- d$individualCount
           a <- as.integer(d$individualCount)
           d[, individualCount := a]
           print(coerced_to_na(b, a))
         }
       })


# Pre-copy ----------------------------------------------------------------

# Pre-copy data because above steps take a long time
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

# # Write files ------------------------------------------------------------------
lapply(names(dat),
       function(nam) write.table(dat[[nam]],
                                 file = here(file.path("data/03_data_clean",
                                                       paste0(nam, ".csv"))),
                                 row.names = FALSE,
                                 qmethod = "double",
                                 sep = ",")
)

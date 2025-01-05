# Header #############################################################
#
# Author: Lisa Nicvert
# Email:  lisa.nicvert@fondationbiodiversite.fr
#
# Date: 2024-12-13
#
# Script Description: Insert data into DB

# Libraries ---------------------------------------------------------------
library(here)
library(data.table)

library(RPostgres)


read_folder <- here("data/03_data_clean/tmp")

# Read CSV ----------------------------------------------------------------
ls <- list.files(read_folder,
                 full.names = TRUE)

nam <- gsub("\\.csv$", "", basename(ls))

dat <- lapply(ls,
              fread,
              sep = ",",
              nrows = 15) # Read a subset
names(dat) <- nam

df <- do.call("rbind",
              c(dat, fill = TRUE))

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

# Copy data to DB ---------------------------------------------------------

## Species -----
colnames <- colnames_DB(con, "Species", rm_ID = TRUE)

spdf <- df[, ..colnames]
spdf <- unique(spdf)
spdf <- spdf[which(!is.na(spdf$scientificName)), ]

dbAppendTable(con, "Species", spdf)
# head(dbReadTable(con, "Species"))

## ParentDataset -----
dat_info <- fread(here("data/data_std/dataset_info.csv"),
                  sep = ",")
colnames <- colnames_DB(con, "ParentDataset")

pddf <- unique(dat_info[, ..colnames])

dbAppendTable(con, "ParentDataset", pddf)
# head(dbReadTable(con, "ParentDataset"))

## Contact -----
colnames <- colnames_DB(con, "Contact")

pdID_df <- dbGetQuery(con, 'SELECT "parentDatasetID", "parentDatasetName"
                      FROM "ParentDataset";')

dat_info <- dat_info[pdID_df,
                     on = "parentDatasetName"]
setnames(dat_info,
         old = "parentDatasetID",
         new = "parentDataset")
codf <- dat_info[, ..colnames]

dbAppendTable(con, "Contact", codf)
# head(dbReadTable(con, "Contact"))

## Dataset -----
colnames <- colnames_DB(con, "Dataset")

dtdf <- unique(df[, ..colnames])
dtdf <- dtdf[pdID_df,
             on = c("parentDataset" = "parentDatasetName")]
dtdf[, parentDataset := NULL]
setnames(dtdf, old = "parentDatasetID", new = "parentDataset")

dbAppendTable(con, "Dataset", dtdf)
# head(dbReadTable(con, "Dataset"))

# Add datasets IDs
dID_df <- dbGetQuery(con, 'SELECT "datasetID", "datasetName", "parentDataset", "parentDatasetName"
                     FROM "Dataset"
                     LEFT JOIN "ParentDataset" ON "Dataset"."parentDataset" = "ParentDataset"."parentDatasetID";')

## Location -----
colnames <- colnames_DB(con, "Location")

df <- df[dID_df, on = .(datasetName,
                        parentDataset = parentDatasetName)]
ldfc <- copy(df)
setnames(lcdf,
         old = "datasetID",
         new = "dataset")

lcdf <- lcdf[, ..colnames]


dbAppendTable(con, "Location", lcdf)

head(dbReadTable(con, "Location"))

## Observer -----
colnames <- colnames_DB(con, "Observer")

obdf <- df[, .(recordedBy)]
setnames(obdf,
         old = "recordedBy", new = "name")
obdf[, url := NA]

dbAppendTable(con, "Observer", obdf)
head(dbReadTable(con, "Observer"))

## Event
colnames <- colnames_DB(con, "Event")
colnames <- colnames[!(colnames %in% c("observer", "location", "dataset"))]
colnames <- c(colnames, "datasetID")

setnames(df, old = "occurrenceRemarks", new = "eventRemarks")
setnames(df, old = "vent", new = "wind")

setnames(df, old = "datasetID", new = "dataset")
df[, cloudCover := NA]
evdf <- df[, ..colnames]

obID_df <- dbGetQuery(con, 'SELECT "observerID", "name"
                      FROM "Observer";')
locID_df <- dbGetQuery(con, 'SELECT "locationID", "decimalCoordinates"
                      FROM "Location";')

# BAD
evdf[, observer := obID_df$observerID]
evdf[, location := locID_df$location]

dbAppendTable(con, "Event", evdf)

## Occurrence -----


# Disconnect --------------------------------------------------------------

dbDisconnect(con)

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

library(dragondb)

read_folder <- here("data/01_data_raw")
read_folder_meta <- here("data/metadata")

# Read data ---------------------------------------------------------------

dat <- list()

dat[["Austria"]] <- data.table(read_excel(file.path(read_folder,
                                                    "Austria/Odonata_Austria_Export.xlsx"),
                                          sheet = "Tabelle1"))

dat[["Belgium1"]] <- data.table(readRDS(file.path(read_folder,
                                                  "Belgium1/belgieData.rds")))
dat[["Belgium2"]] <- fread(file.path(read_folder,
                                     "Belgium2/2024-076_libellen_EN.csv"))

dat[["Cyprus1"]] <- fread(file.path(read_folder,
                                    "Cyprus1/Cyprus corrected.csv"))

# Cyprus 2
lf <- list.files(file.path(read_folder,
                           "Cyprus2"),
                 full.names = TRUE)

dat[["Cyprus2"]] <- data.table()
for(i in seq_along(lf)){
  cyp_data <- data.table(read_excel(lf[i],
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
dat[["Netherlands"]] <- data.table(readRDS(file.path(read_folder,
                                                     "Netherlands/Dutchdata2023.RDS")))

sp_ned <- fread(file.path(read_folder,
                          "Netherlands/Soortcodes_nl_sci.csv"))
sp_ned[, srt_Nednaam := tolower(srt_Nednaam)]
dat[["Netherlands"]] <- merge(dat[["Netherlands"]], sp_ned,
                              by.x = "soort_nl", by.y = "srt_Nednaam",
                              all.x = TRUE)

dat[["France_STELI"]] <- fread(file.path(read_folder,
                                         "France_STELI/STELI_data_FR_DMS.csv"))

dat[["France_Atlas"]] <- fread(file.path(read_folder, "France_Atlas/odonata_202410091558.csv"))

# UK
path <- file.path(read_folder, "UK")
ld <- list.dirs(path,
                full.names = TRUE)
ld <- ld[ld != path]

dat[["UK"]] <- data.table()
for (d in ld) {
  print(paste(d, "---"))
  df <- fread(file.path(d, "data.csv"), sep = ",", header = TRUE)
  print(ncol(df))

  dat[["UK"]] <- rbind(dat[["UK"]], df, fill = TRUE)
}

# Sweden
path <- file.path(read_folder, "Sweden/Observations 2025-01-30 15.18 SOS export")
lf <- list.files(path,
                 full.names = TRUE)
lf <- lf[grep(lf, pattern = "\\.xlsx$")]

dat[["Sweden"]] <- data.table()
for (f in lf) {
  print(paste(f, "---"))
  df <- data.table(read_excel(f, sheet = 1,
                              col_types = "text"))
  print(ncol(df))

  dat[["Sweden"]] <- rbind(dat[["Sweden"]], df, fill = TRUE)
}

# Catalonia
dat[["Catalonia"]] <- data.table(read  _excel(file.path(read_folder,
                                                    "Catalonia/-Oxygastra definitiu 7 abril Mike.xls"),
                                            col_types = "text",
                                            sheet = 1))

# Standardize column names ------------------------------------------------

names_key <- read_excel(file.path(read_folder_meta,
                                  "column_names.xlsx"),
                        sheet = 1)
names_key <- data.table(names_key)

lapply(seq_along(dat),
       function(i) {
         rename_cols(key = names_key,
                     dtable = dat[[i]],
                     nam = names(dat)[i])
         })

# Reorder columns
lapply(seq_along(dat),
       function(i) {
         cnames <- names_key$Standard[names_key$Standard %in% colnames(dat[[i]])]
         setcolorder(dat[[i]],
                     cnames)
       })


lapply(dat, names)

# Write files ------------------------------------------------------------------
lapply(names(dat),
       function(nam) write.table(dat[[nam]],
                                 file = here(file.path("data/02_data_std",
                                                       paste0(nam, ".csv"))),
                                 row.names = FALSE,
                                 qmethod = "double",
                                 sep = ",")
)


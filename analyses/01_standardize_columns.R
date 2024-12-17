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

read_folder <- here("data/01_data_raw/occurrence_data")
read_folder_meta <- here("data/01_data_raw/metadata")

# Read data ---------------------------------------------------------------

dat <- list()

dat[["Austria"]] <- data.table(read_excel(file.path(read_folder,
                                                    "Odonata_Austria_Export.xlsx"),
                                          sheet = "Tabelle1"))

dat[["Belgium1"]] <- data.table(readRDS(file.path(read_folder,
                                                  "belgieData.rds")))
dat[["Belgium2"]] <- fread(file.path(read_folder,
                                     "2024-076_libellen_EN.csv"))

dat[["Cyprus1"]] <- fread(file.path(read_folder,
                                    "Cyprus corrected.csv"))

# Cyprus 2
lf <- list.files(file.path(read_folder,
                           "Cyprus Data dragonflies"))

dat[["Cyprus2"]] <- data.table()
for(i in seq_along(lf)){
  cyp_data <- data.table(read_excel(file.path(read_folder,
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
dat[["Netherlands"]] <- data.table(readRDS(file.path(read_folder,
                                                     "Dutchdata2023.RDS")))

sp_ned <- fread(file.path(read_folder,
                          "Soortcodes_nl_sci.csv"))
sp_ned[, srt_Nednaam := tolower(srt_Nednaam)]
dat[["Netherlands"]] <- merge(dat[["Netherlands"]], sp_ned,
                              by.x = "soort_nl", by.y = "srt_Nednaam",
                              all.x = TRUE)

dat[["France_STELI"]] <- fread(file.path(read_folder,
                                         "STELI_data_FR_DMS.csv"))

dat[["France_OPIE"]] <- fread(file.path(read_folder,
                                        "France Opportunistics data (Opie)/odonata_202410091558.csv"))

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


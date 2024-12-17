# Header #############################################################
#
# Author: Lisa Nicvert
# Email:  lisa.nicvert@fondationbiodiversite.fr
#
# Date: 2024-12-17
#
# Script Description: format data for database and add columns



read_folder <- here("data/03_data_clean/tmp")

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


# Format data -------------------------------------------------------------

# Add country
countries <- c("Austria",
               "Belgium",
               "Belgium",
               "Cyprus",
               "Cyprus",
               "Netherlands",
               "France",
               "France")
names(countries) <- names(dat)


## parentDataset ------------------------------------------------------

lapply(dat, colnames)
lapply(dat, nrow)


lapply(seq_along(dat),
       function(i) dat[[i]][, "parentDataset" := names(dat)[i]])

dat_info <- data.table(read_excel(here("data/data_raw/contacts.xlsx"),
                                  sheet = 1))
dat_info[, parentDataset := ""]

dat_info[c(1, 3, 6, 8, 11, 13, 14, 15),
         parentDatasetName := c("Belgium1", "Netherlands",
                                "Austria", "Belgium2",
                                "Cyprus1", "France_OPIE", "France_OPIE",
                                "France_STELI")]

cyp2 <- dat_info[parentDatasetName == "Cyprus1",]
cyp2$parentDatasetName <- "Cyprus2"

dat_info <- rbind(dat_info,
                  cyp2)

dat_info <- dat_info[parentDatasetName != "",
                     c("Name", "email", "parentDatasetName")]

# Not sure for Belgium1/2 data and Austria
dat_info[, samplingProtocol := c("transect",
                                 "opportunistic",
                                 "opportunistic",
                                 "opportunistic",
                                 "opportunistic",
                                 "site counts",
                                 "site counts",
                                 "opportunistic",
                                 "opportunistic")]

setnames(dat_info,
         old = c("Name", "email"),
         new = c("contactName", "contactEmail"))

write.table(dat_info,
            file = here("data/data_std/dataset_info.csv"),
            row.names = FALSE,
            sep = ",")

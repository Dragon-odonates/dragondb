# Header #############################################################
#
# Author: Lisa Nicvert
# Email:  lisa.nicvert@fondationbiodiversite.fr
#
# Date: 2025-02-21
#
# Script Description: query UK data through API

library(httr2)
library(brio)
library(here)

# Get years
step <- 5
years <- seq(1990, 2025, by = step)

for (year in years) {
  yend <- year + step - 1
  yrange <- paste(year, "TO", yend)
  zip_folder <- paste0(year, "-", yend, ".zip")

  print(paste(yrange, "--------------------"))

  # Build API query
  req <- request("https://records-ws.nbnatlas.org/occurrences/index/")
  q <- req |>
    req_url_path_append("download") |>
    req_url_query(reasonTypeId = 17,
                  q = paste0("year:[", yrange,"]"),
                  fq = "data_provider_uid:dp97",
                  fq = "basis_of_record:HumanObservation",
                  email = "lisa.nicvert@fondationbiodiversite.fr",
                  dwcHeaders = "true")
  res <- req_perform(q)

  # Check it succeeded (200)
  if (res$status_code != 200) {
    warning("Status code is not 200")
  }

  # Write zip folder
  zip_path <- file.path(here("data/01_data_raw/UK"),
                        zip_folder)
  res |>
    httr2::resp_body_raw() |>
    brio::write_file_raw(path = zip_path)

  # Unzip
  unzip(zip_path,
        exdir = tools::file_path_sans_ext(zip_path))
}

# Compare files
# df <- read.csv(here("data/to_process/UK_manual/2020-2025/records-2025-02-17.csv"))
#
# df2 <- read.csv((here("data/to_process/UK/2020-2029/data.csv", sep = ",", header = TRUE)
#
# table(df2$scientificName)
#
# table(df$scientificName.processed)

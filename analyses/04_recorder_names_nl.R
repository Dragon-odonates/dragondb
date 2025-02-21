# Header #############################################################
#
# Author: Lisa Nicvert
# Email:  lisa.nicvert@fondationbiodiversite.fr
#
# Date: 2025-02-21
#
# Script Description: query observer names for Netherlands from th URLs


# Libraries etc -----------------------------------------------------------
library(here)
library(data.table)
library(rvest) # read htlm


# Read data ---------------------------------------------------------------

df <- fread(here("data/02_data_std/Netherlands.csv"),
            header = TRUE,
            na.strings = c("", "NA"),
            sep = ",")

# Get valid URLs ----------------------------------------------------------

# Get Waarneming IDs
recorderID <- unique(na.omit(df$recordedByID))

# Keep only Waarneming (valid URLs)
recorderID <- grep("waarneming.nl",
                   recorderID, value = TRUE)
# recc <- str_split(recorderID, "/")
# domains <- unique(sapply(recc, function(r) r[3]))
# grep("nhgl-ecolog.nl", recorderID, value = TRUE)


# Query websites ----------------------------------------------------------

recorder_css_class <- ".app-content-title"

res <- data.table(recordedByID = recorderID,
                  recordedBy = "")

for (i in 1:nrow(res)) {
  print(i)
  page <- res[i, recordedByID]
  txt <- tryCatch(read_html(page),
                  error = function(e) e)

  if ("xml_document" %in% class(txt)) {
    observer_name <- txt |>
      html_node(css = recorder_css_class) |>
      html_text(trim = TRUE)

    # dat$Netherlands[recordedByID == page, recordedBy := observer_name]
    res[i, recordedBy := observer_name]
  } else {
    message("Error: ", txt)
  }
  closeAllConnections()

}

# write.table(res,
#             file = here("data/03_data_clean/observer/ned_names.csv"),
#             row.names = FALSE,
#             qmethod = "double",
#             sep = ",")

# Fiddle with partial records ---------------------------------------------

# ned_names1 <- fread(here("data/03_data_clean/observer/ned_names1.csv"),
#                     header = TRUE,
#                     na.strings = c("", "NA"),
#                     sep = ",")
# ned_names2 <- fread(here("data/03_data_clean/observer/ned_names2.csv"),
#                     header = TRUE,
#                     na.strings = c("", "NA"),
#                     sep = ",")
#
# # Merge
# res <- ned_names2[ned_names1,
#                   on = "recordedByID"]
#
# # Replace NA values in recordedBy
# res[is.na(recordedBy), recordedBy := i.recordedBy]
#
# # Delete useless columns
# res[, i.recordedBy := NULL]
#
# # Check for missed NAs
# recorderID_na <- unique(res[is.na(recordedBy), recordedByID])
# recorderID_na <- grep("waarneming.nl",
#                       recorderID_na, value = TRUE)
#
# i <- 1
# for (page in recorderID_na) {
#   print(paste(i, "---------"))
#   print(page)
#
#   txt <- tryCatch(read_html(page),
#                   error = function(e) e)
#
#   if ("xml_document" %in% class(txt)) {
#     observer_name <- txt |>
#       html_node(css = recorder_css_class) |>
#       html_text(trim = TRUE)
#
#     print(observer_name)
#
#     res[recordedByID == page, recordedBy := observer_name]
#   } else {
#     message("Error: ", txt)
#   }
#
#   closeAllConnections()
#
#   i <- i + 1
# }
#
# write.table(res,
#             file = here("data/03_data_clean/observer/ned_names.csv"),
#             row.names = FALSE,
#             qmethod = "double",
#             sep = ",")

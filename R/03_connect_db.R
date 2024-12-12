library(here)
library(data.table)

library(RPostgres) # library(RPostgreSQL)



# Read CSV ----------------------------------------------------------------
ls <- list.files(here("data/data_std"),
                 full.names = TRUE)
nam <- gsub("\\.csv$", "", basename(ls))

dat <- lapply(ls,
              fread, 
              sep = ",",
              nrows = 15)
names(dat) <- nam

df <- do.call("rbind", 
              c(dat, fill = TRUE))

# Species table
sptab <- df[, c("scientificName")]
sptab[, "speciesID" := seq(1:nrow(df))]
sptab[, "genus" := gsub("\\s[a-z]+$", "", df$scientificName)]
sptab[, "family" := NA]

setcolorder(sptab, 
            c("speciesID", "scientificName", "genus", "family"))
# write.table(sptab,
#             file = "C:/Users/CESAB/Documents/Postdoc/DB/sp.csv",
#             row.names = FALSE,
#             sep = ",")


# Connect to "local" DB
con <- dbConnect(
  drv       = RPostgres::Postgres(), # dbDriver("PostgreSQL"),
  dbname    = "dragon",
  host      = "localhost", # "192.168.0.75",
  port      = 5432,
  user      = Sys.getenv('USERNAME'),
  password  = Sys.getenv('PASSWORD')
)
dbListTables(con)

dbDisconnect(con)

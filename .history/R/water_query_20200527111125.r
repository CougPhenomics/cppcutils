library(DBI)
library(RPostgres)
library(pool)
library(jsonlite)
library(tidyverse)

config <- read_json("../cppcserver.config") # load config file with database info.

conn <- dbConnect(
    drv = RPostgres::Postgres(),
    dbname = config$dbname,
    host = config$hostname,
    user = config$dbusername,
    password = config$dbpassword,
    port = 5432
)

#'
## -----------------------------------------------------------------------------
waterquery <- "SELECT time_stamp, snapshot.weight_before, snapshot.weight_after, snapshot.water_amount_g,
snapshot.comment as Job, snapshot.id_tag, snapshot.measurement_label FROM snapshot WHERE measurement_label = 'evaporation2' and comment = 'watering'"

# waterquery = readr::read_file('query_waterdata.sql')

dbevap <- dbSendQuery(conn, waterquery)
# dbBind(airport, list("GPT", "MSY"))
evapdata <- dbFetch(dbevap) %>% as_tibble()
saveRDS(evapdata, file = "evaporation2-raw.rds")

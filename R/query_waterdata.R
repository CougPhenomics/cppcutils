#' Fetch watering data from LT database
#' @param config configuration for database as json file
#' @param expername character or vector of characters; experiment name(s) in LT database
#' @param jobname character or vector of characters; job name(s) (internally stored as `comment`) in LT database
#' @export
#' @details The % character is supported as a multicharacter wildcard and
#'  _ is supported as a single character wildcard for experiment and job names.
#'  names are case-sensitive!


query_waterdata <- function(config, expername, jobname){
    
    serverconfig <- jsonlite::read_json(config) # load config file with database info.
    
    conn <- DBI::dbConnect(
        drv = RPostgres::Postgres(),
        dbname = serverconfig$dbname,
        host = serverconfig$hostname,
        user = serverconfig$dbusername,
        password = serverconfig$dbpassword,
        port = 5432
    )
    
    
    waterquery <- glue::glue_sql("SELECT time_stamp, 
                    snapshot.weight_before, 
                    snapshot.weight_after, 
                    snapshot.water_amount_g, 
                    snapshot.comment as Job, 
                    snapshot.id_tag,
                    snapshot.measurement_label 
                FROM snapshot WHERE measurement_label LIKE ({experiment*}) AND comment LIKE ({jobname*})",
                                 .con = conn,
                                 experiment = expername,
                                 jobname = jobname)
    
    
    # waterquery = readr::read_file('query_waterdata.sql')
    dbdata <- RPostgres::dbGetQuery(conn, waterquery) %>% 
        dplyr::as_tibble()
    
    
    return(dbdata)
    
}
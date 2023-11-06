
# Gets corresponding table number from series ID
series_table_no <- function(series_ids, conn = NULL){
  if(is.null(conn) & !exists("con")){
    stop("Please provide a connection object")
  } else if(is.null(conn)) {
    conn <- con
  } 
  
  dbGetQuery(
    conn = conn,
    statement = 
      paste0(
        'SELECT DISTINCT "series_id", "table_no"',
        'FROM "abs_labour_force"',
        "WHERE series_id IN ('",
        paste0(series_ids, collapse = "', '"),
        "')"
      )
  )
}


# Determine whether a particular collection of series requires an update
series_req_update <- function(series_ids, update_tbls = NULL, conn = NULL){
  if(is.null(conn) & !exists("con")){
    stop("Please provide a connection object")
  } else if(is.null(conn)) {
    conn <- con
  } 
  
  if(is.null(update_tbls) & !exists("tables_to_update")){
    stop("Please provide update_tbls")
  } else if(is.null(update_tbls)) {
    update_tbls <- tables_to_update
  } 
  
  lookup <- series_table_no(series_ids, conn)
  any(lookup$table_no %in% update_tbls)
}

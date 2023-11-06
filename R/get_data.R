
# Gets data based on `column name` = filter_vector pairs.
get_data <- function(..., conn = NULL, where = NULL){
  
  # Find db connection if not explicitly passed
  if(is.null(conn) & !exists("con")){
    stop("Please provide a connection object")
  } else if(inherits(con, "PqConnection")) {
    conn <- con
  } else {
    stop("Please provide a connection object")
  }
  
  # clean input filter pairs
  i <- list(...)
  if(is.null(names(i)) && length(i) > 0) stop(
    "get_data inputs must have named inputs"
  )
  
  i <- i[names(i) != ""]
  i <- mapply(
    function(n, v){
      paste0(n, " IN (", paste0("'", v, "'", collapse = ","), ")")
    }, 
    n = names(i),
    v = i
  )
  
  # Add manual 'where' and collapse 
  i <- c(i, where)
  i <- paste0(i, collapse = " AND ")
  
  # Download data
  out <- dbGetQuery(
    conn = conn,
    statement = 
      paste(
        'SELECT *',
        'FROM "abs_labour_force"',
        "WHERE",
        i
      )
  )
  
  # reclass and return
  setDT(out)
  return(out)
  
}
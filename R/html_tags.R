# HTML funtions
tagfun <- function(..., tag, selfclosing = FALSE){
  i <- list(...)
  n <- names(i)
  
  
  if(!is.null(n)){
    attrs <- i[n != ""]
    attrs <- unlist(attrs)
    i     <- i[n == ""]
    n     <- n[n != ""]
    attrs <- paste0(" ", paste0(n , ' = "', attrs, '"'), collapse = " ")
  } else {
    attrs <- ""
  }
  
  if(selfclosing){
    open_tag <- paste0("<", tag, " ", attrs, "/>")
    return(open_tag)
  } else {
    open_tag <- paste0("<", tag, " ", attrs, ">")
    close_tag <- paste0("</", tag, ">")
    return(c(open_tag, unlist(i), close_tag))
  }
}

div <- function(...){
  tagfun(..., tag = "div")
}

span <- function(...){
  tagfun(..., tag = "span")
}

small <- function(...){
  tagfun(..., tag = "small")
}

th <- function(...){
  tagfun(..., tag = "th")
}

td <- function(...){
  tagfun(..., tag = "td")
}

tr <- function(...){
  tagfun(..., tag = "tr")
}

table_tag <- function(...){
  tagfun(..., tag = "table")
}

thead <- function(...){
  tagfun(..., tag = "thead")
}

tbody <- function(...){
  tagfun(..., tag = "tbody")
}

caption <- function(...){
  tagfun(..., tag = "caption")
}

svg_tag <- function(...){
  tagfun(..., tag = "svg")
}

br <- function(){"<br/>"}

img <- function(...){
  tagfun(..., tag = "img", selfclosing = TRUE)
}

abbr <- function(...){
  tagfun(..., tag = "abbr")
}
# Number formatters
comma <- function(val, suffix = NULL, add_sign = FALSE, decimal = 2){
  
  if(add_sign){
    signs <- ifelse(sign(val) == -1L, "-", "+")
    val <- abs(val)
  } else {
    signs <- NULL
  }
  
  
  out <- format(
    round(val, decimal), 
    justify = "none", 
    big.mark = ",", 
    width = 0
  )
  
  paste0(
    signs, 
    gsub(" ", "", out), 
    suffix
  )
  
}

format_percent <- function(val, add_sign = FALSE){
  comma(val, "%", add_sign, decimal = 1)
}

format_number <- function(val, add_sign = FALSE){
  val_abs <- abs(val)
  fcase(
    val_abs >= 3e09, comma(val / 1e09, "b", add_sign, 0),
    val_abs >= 1e09, comma(val / 1e09, "b", add_sign, 1),
    val_abs >= 2e06, comma(val / 1e06, "m", add_sign, 0),
    val_abs >= 1e05, comma(val / 1e06, "m", add_sign, 1),
    val_abs >= 2e03, comma(val / 1e03, "k", add_sign, 0),
    val_abs <  2e03, comma(val,       NULL, add_sign, 0)
  )
}

format_any <- function(val, form = "number", add_sign = FALSE){
  fcase(
    form == "PERCENT", format_percent(val, add_sign),
    form == "STOCK"  , format_number(val, add_sign),
    form == "FLOW"   , format_number(val, add_sign),
    form == "PPT"    , comma(val, ppt_abbr, add_sign, 1),
    TRUE             , comma(val, NULL, add_sign)
  )
}

ppt_abbr <- paste(
  abbr(
    class = "text-decoration-none", 
    title="Percentage points",
    "&nbsp;ppt"
  ),
  collapse = " "
)
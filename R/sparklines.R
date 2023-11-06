rescale <- function(x, newmax, newmin = 0, decimal = 2){
  x <- (x-min(x))/(max(x)-min(x))
  round(x * (newmax - newmin) + newmin, decimal)
}

sparkline <- function(
    y, 
    width = 150, 
    height = 50, 
    pad = 6, 
    colour = "#201547"
){
  n <- length(y)
  x <- rescale(seq_len(n), newmax = width - pad,  newmin = pad)
  y <- rescale(y, newmax = pad, newmin = height - pad)
  
  points <- paste(paste0(x, ",", y), collapse = " ")
  
  svg_tag(
    height = height,
    width = width,
    version = 1.1,
    xmlns = "http://www.w3.org/2000/svg",
    paste0(
      '<polyline points="',
      points,
      '" style="fill:none;stroke:',
      colour,
      ';stroke-width:3" />'
    ),
    sprintf(
      '<circle cx="%s" cy="%s" r="4" fill="white" stroke = "%s" stroke-width="3"/>',
      last(x), 
      last(y),
      colour
    )
  )
}
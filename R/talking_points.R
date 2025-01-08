make_talking_points <- function(jobs_data){

  # Define key variables which uniquely define talking points
  keyvars <- c("name", "series_id", "smoothing_months")

  # Aquire talking point data & handle missing valus
  tp <- fread("talking_points.csv")
  tp[is.na(smoothing_months), smoothing_months := 1]

  # Aquire jobs data filtered for relevant series
  data <- jobs_data[tp[, keyvars, with = F], on = "series_id"]

  # Add ordering column to ensure ordering is preserved in presentation
  tp[, order := 1:.N]

  # Remove existing data rounding
  data[unit == "000",       value := value * 1000]
  data[unit == "000 Hours", value := value * 1000]
  data[, unit := NULL]

  # Smooth data if required
  setkeyv(data, c(keyvars, "date"))
  data[, value := frollmean(value, smoothing_months[1]), keyvars]


  #  get last two observations per series
  data[
    ,
    keep := date == max(date) | date == max(date[date != max(date)]),
    keyvars
  ]

  # Check every talking point series is uniquely defined at each date
  not_unique <- !all(data[keep == T, .N, keyvars][, unique(N)] == 2)
  if(not_unique){
    stop(
      "A talking note series is not uniquely defined at each date or does not have two periods of data"
      )
  }

  # Filter for latest values and pivot
  data <- data[keep == TRUE]
  data[, to_pivot := c("old", "new"), keyvars]
  pivot_formula <- paste0(paste0(keyvars, collapse = " + "), " ~ to_pivot")
  data <- dcast(data, as.formula(pivot_formula), value.var = "value")

  # Join data to talking points
  tp <- merge(data, tp, all = T)

  # Define delta & dot point type (state)
  tp[, delta := new - old]
  tp[
    ,
    state := fcase(
      abs(delta) < stable_if_delta_less_than, "Stable"       ,
      up_is_good & delta > 0                , "Good increase",
      !up_is_good & delta > 0               , "Bad increase" ,
      up_is_good & delta < 0                , "Bad decrease" ,
      !up_is_good & delta < 0               , "Good decrease"
    )
  ]

  # Define relative delta
  tp[, relative_delta := (new / old - 1) * 100]

  # Round data
  tp[, delta := round(delta, digits = rounding_digits_delta)]
  tp[, relative_delta := round(relative_delta, digits = 1)]
  tp[, old := round(old, digits = rounding_digits)]
  tp[, new := round(new, digits = rounding_digits)]


  # Choose sentence format type (normal, rounding error or stable)
  # exception to rounding error for deltas on different scale to level values
  tp[
    ,
    format := fcase(
      state == "Stable"                        , format_stable        ,
      delta != new - old & scale == scale_delta, format_rounding_error,
      default = format_normal
    )
  ]

  # Define directional words
  tp[, direction := ifelse(delta > 0, format_increase, format_decrease)]
  tp[
    ,
    delta_direction := ifelse(
      delta > 0,
      format_delta_increase,
      format_delta_decrease
    )
  ]

  # Format numbers
  fmt <- \(x, suffix){
    formatted <- sapply(x, \(y) format(y, big.mark = ",", scientific = FALSE))
    if(length(suffix) == 1) suffix <- rep(suffix, length(x))
    ifelse(is.na(suffix), formatted, paste0(formatted, suffix))
  }

  tp[
    ,
    `:=`(
      old = fmt(old / scale, suffix),
      new = fmt(old / scale, suffix),
      delta = fmt(delta / scale_delta, suffix_delta),
      relative_delta = fmt(relative_delta, "%")
    )
  ]

  # Define final talking point sentences and order
  tp[
    ,
    sentance := str_glue(
      format,
      old = old,
      new = new,
      delta = delta,
      direction = direction,
      delta_direction = delta_direction,
      relative_delta = relative_delta
    ),
    keyvars
  ]

  setkey(tp, order)


  # identify indent breaks
  tp[, indent_break := indent != shift(indent, fill = FALSE)]
  tp[indent_break == T, indent_end := 1:.N %% 2 == 0]
  tp[is.na(indent_end), indent_end := F]
  tp[, indent_start := indent_end == F & indent_break == T]

  if(last(tp$indent) == T || first(tp$indent) == T){
    stop("Talking points are not set up for indent at start or end of list, pease modify talking_points.R if required")
  }

  # Generate latex
  # tp[
  #   ,
  #   bullet_latex := fcase(
  #     state == "Stable"       , "\\item[\\textcolor{coolgrey}{{\\symbolfont ▬}}]",
  #     state == "Good increase", "\\item[\\textcolor{forestgreen}{{\\symbolfont ▲}}]" ,
  #     state == "Bad increase" , "\\item[\\textcolor{burntorange}{{\\symbolfont ▲}}]"   ,
  #     state == "Bad decrease" , "\\item[\\textcolor{burntorange}{{\\symbolfont ▼}}]"   ,
  #     state == "Good decrease", "\\item[\\textcolor{forestgreen}{\\symbolfont{ ▼}}]"
  #   )
  # ]

  tp[, bullet := ifelse(indent, "\\bullet", "{\\symbolfont ▸}")]

  tp[
    ,
    bullet_latex := fcase(
      state == "Stable"       , sprintf("\\item[\\textcolor{coolgrey}{%s}]", bullet),
      state == "Good increase", sprintf("\\item[\\textcolor{forestgreen}{%s}]", bullet),
      state == "Bad increase" , sprintf("\\item[\\textcolor{burntorange}{%s}]", bullet),
      state == "Bad decrease" , sprintf("\\item[\\textcolor{burntorange}{%s}]", bullet),
      state == "Good decrease", sprintf("\\item[\\textcolor{forestgreen}{%s}]", bullet)
    )
  ]
  tp[indent_start == T, bullet_latex := paste0("\\begin{enumerate}  ", bullet_latex)]
  tp[indent_end == T, bullet_latex := paste0("\\end{enumerate}  ", bullet_latex)]


  # save latex
  latex_out <- c(
    # "{\\large",
    "\\begin{itemize}",
    "\\setlength\\itemsep{3pt}",
    tp[, paste(bullet_latex, str_replace_all(sentance, "%", "\\\\%"))],
    "\\end{itemize}"#,
    # "}"
  )
  writeLines(latex_out, "assets/talking_points/talking_points.tex")


}

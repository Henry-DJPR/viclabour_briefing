
# Generate html table
make_table <- function(
    table_name,
    series_ids,
    row_headers,
    highlight_rows,
    up_is_good,
    smoothing = NULL,
    notes = NULL,
    positive_colour = "#ccefd9",
    negative_colour = "#f7ccd6"
){

  # Collapse caption
  notes <- paste(unique(notes), collapse = " ")


  # Get data
  df <- jobs_data[series_id %in% series_ids]


  # Ensure correct data ordering
  setDT(df)
  df[, series_id := factor(series_id, levels = series_ids)]
  setkey(df, series_id, date)

  # God I HATE it when databased information is in thousands
  df[unit == "000",       value := value * 1000]
  df[unit == "000 Hours", value := value * 1000]
  df[, unit := NULL]

  # Smooth data
  smoothing[is.na(smoothing)] <- 1
  df[, smoothing := smoothing[match(series_id, series_ids)]]
  df[, value := frollmean(value, smoothing[1]), series_id]

  # Check time interval
  interval <- df[, unique(frequency)]
  if(length(interval) > 1) stop(
    "Tables can only have one frequency (e.g. not monthly AND quarterly)"
  )
  if(any(!(interval %in% c("Month", "Quarter")))) stop(
    "make_table only supports monthly or quarterly data"
  )

  # Generate sparkline colours
  colourgroup <- data.table(
    series_id = factor(series_ids, levels = series_ids),
    colour = cumsum(as.integer(highlight_rows))
  )

  if(any(colourgroup$colour == 0)) colourgroup[, colour := colour + 1]

  sparkcolours <- rep(
    pal,
    ceiling(
      max(colourgroup$colour) / length(pal)
    )
  )

  colourgroup[, colour := sparkcolours[colour]]
  df <- colourgroup[df, on = "series_id"]

  # Generate sparklines and save
  sparkdata <- df[
    date >= years_ago(max(date), 3),
    .(sparkline = sparkline(value, colour = first(colour))),
    series_id
  ]

  sparkdata[
    ,
    writeLines(sparkline, path_sparkline(table_name, unique(series_id))),
    series_id
  ]

  # Generate sparkline alt text
  sparktext <- df[
    date > years_ago(max(date), 3),
    .(
      current = last(value),
      average = mean(value, na.rm = TRUE),
      name = tolower(row_headers[match(series_id[1], series_ids)])
    ),
    series_id
  ]

  sparktext[
    , `:=`(
      name = fcase(
        grepl("rate|ratio", name), paste("The", name, "is"),
        grepl("persons", name), paste("The number of", name, "is"),
        grepl("employed ", name), paste("The number of people", name, "is"),
        rep(TRUE, .N), paste0(toupper(substring(name, 1,1)), substring(name, 2)," is")
      ),
      movement = fifelse(
        current >= average,
        "higher than average",
        "lower than average"
      )
    )
  ]

  sparktext[, alt := paste(name, movement)]


  # Detect series which are not up to date, note and correct for reporting
  not_latest <- df[, .(date = max(date)), series_id]
  not_latest <- not_latest[date != max(date)]
  not_latest[, lag_months := month(max(df$date)) - month(date)]

  df[
    series_id %in% not_latest$series_id,
    date := date +
      months(not_latest$lag_months[match(series_id, not_latest$series_id)])
  ]

  # Get interval dates
  breaks <- c(
    current = df[, max(date)],
    last    = df[, max(date[date != max(date)])],
    year    = df[, years_ago(max(date), 1)],
    covid   = as.Date("2019-03-01"),
    govt    = as.Date("2014-11-01")
  )

  row_header_1 <- c(
    current = "Current",
    last = sprintf("One %s change", tolower(interval)),
    year = "One year change",
    covid = "Change since COVID-19",
    govt = "Change during government"
  )

  row_header_2 <-  format(breaks, "%b %Y")


  if(!all(names(breaks) %in% names(row_header_1))){
    stop("Please ensure all breaks have a label in row_header")
  }


  # Generate deltas
  deltas <- df[date %in% breaks]
  deltas[, date := names(breaks)[match(date, breaks)]]
  deltas[, date := factor(date, levels = names(breaks))]
  deltas <- dcast(deltas, series_id + data_type ~ date, value.var = "value")

  diff_cols <- names(breaks)[names(breaks) != "current"]
  deltas[, (diff_cols) := lapply(.SD, \(x) current - x), .SDcols = diff_cols]


  # generate percentage change for stocks and flows
  if(any(unique(df$data_type) %in% c("STOCK", "FLOW"))){

    # filter out releveant series and generate relative growth
    relative_grow <- df[data_type %in% c("STOCK", "FLOW")]
    setkey(relative_grow, series_id, date)
    relative_grow[, value := last(value) / value - 1, series_id]

    # Pull out relevant dates and pivot
    relative_grow <- relative_grow[date %in% breaks]
    relative_grow[, date := names(breaks)[match(date, breaks)]]
    relative_grow[, date := factor(date, levels = names(breaks))]
    relative_grow <- dcast(
      relative_grow,
      series_id + data_type ~ date,
      value.var = "value"
    )
    relative_grow[, `:=`(data_type = "RELATIVE", current = as.numeric(NA))]


    # Bind to deltas, correct order and note relative growth rows
    deltas <- rbind(deltas, relative_grow)
    deltas[
      ,
      data_type := factor(
        data_type,
        levels = c("PERCENT", "STOCK", "FLOW", "RELATIVE")
        )
    ]
    setkey(deltas, series_id, data_type)

    # Correct highlight row index given new rows
    highlight_series <- series_ids[highlight_rows]
    highlight_rows <- deltas$series_id %in% highlight_series
  }

  relative_rows <- which(deltas$data_type == "RELATIVE")



  # Generate current backgrounds
  avg_3_year <- df[
    date >= years_ago(max(date), 3),
    .(avg  = mean(value)),
    keyby = series_id
  ]

  backgrounds <- deltas[avg_3_year][,
    `:=`(
      up_is_good = up_is_good[match(series_id, series_ids)],
      current = current - avg,
      avg = NULL,
      data_type = NULL
    )
  ][,
    (names(breaks)) := lapply(.SD, \(x){
      fifelse(
        (x >= 0 & up_is_good) | (x <= 0 & !up_is_good),
        positive_colour,
        negative_colour
        )
    }),
    .SDcols = names(breaks)
  ][,
    up_is_good := NULL
  ]

  backgrounds <- melt.data.table(
    data = backgrounds,
    id.vars = "series_id",
    measure.vars = names(breaks),
    variable.name = "date",
    value.name = "bg"
    )


  # format deltas
  deltas[, current := format_any(current, data_type)]
  deltas[data_type == "PERCENT", data_type := "PPT"]
  deltas[,
    (diff_cols) := lapply(.SD, format_any, form = data_type, add_sign = FALSE),
    .SDcols = diff_cols
  ]
  deltas[, data_type := NULL]


  # Add sparkline column
  deltas[, trend := path_sparkline(table_name, series_id)]
  setcolorder(deltas, after = "series_id", "trend")

  # Generate footnote content
  fnote_smoothing <-  as.data.table(
    cbind(series_id = series_ids, smooth = smoothing)
  )
  fnote_smoothing <- fnote_smoothing[
    smooth != 1,
    .(
      series_id,
      text = sprintf(
        "Smoothed using %s %s rolling average",
        smooth,
        tolower(interval)
      )
    )
  ]

  fnote_not_latest <- not_latest[
    ,
    .(
      series_id,
      text = sprintf("Latest data is from %s", format(date, "%B %Y"))
    )
  ]

  fnotes <- rbind(fnote_not_latest, fnote_smoothing)
  fnotes <- fnotes[
    ,
    .(row = which(deltas$series_id %in% series_id)),
    .(series_id, text)
  ]
  fnotes <- split(fnotes, by = "text")


  # Generate footnote function
  attach_fnotes <- function(x){

    l <- length(fnotes)
    symb <- letters[1:l]

    for(i in seq_len(l)){
      x <- footnote(
        x = x,
        i = fnotes[[i]]$row,
        j = 1,
        value = as_paragraph(fnotes[[i]]$text[1]),
        ref_symbols = symb[i],
        inline = T,
        sep = ". "
      )
    }

    return (x)
  }


  # Replace row labels
  deltas[, series_id := row_headers[match(series_id, series_ids)]]


  # flextable

  fp_border_default(color = "#222222")


  flex <- deltas |>
    flextable() |>
    # Header
    set_header_labels(values = c("", "Past three years", unname(row_header_2))) |>
    add_header_row(values = c("", "Trend", row_header_1)) |>
    # set_header_labels(values = c("", unname(row_header_2))) |>
    # add_header_row(values = c("", row_header_1)) |>
    bold(i = 1, part = "header") |>
    color(color = "#FFFFFF", part = "header") |>
    fontsize(i = 2, size = 8, part = "header") |>
    bg(bg = "#222222", part = "header") |>
    hline(1, border = officer::fp_border("#222222", style = "none"), part = "header") |>
    align(j = 2:7, align = "right", part = "header") |>
    # align(j = 2:6, align = "right", part = "header") |>
    padding(1, padding.bottom = 0, padding.top = 8, part = "header") |>
    padding(2, padding.bottom = 8, padding.top = 3, part = "header") |>
    valign(i = 1, valign = "bottom", part = "header") |>
    valign(i = 2, valign = "top", part = "header") |>
    fontsize(size = 10, part = "all") |>
    # Body
    align(j = 3:7, align = "right", part = "body") |>
    fontsize(size = 10) |>
    # align(j = 2:6, align = "right", part = "body") |>
    valign(valign = "top", part = "body") |>
    # Header rows
    bold(which(highlight_rows)) |>
    padding(which(!highlight_rows), 1, padding.left = 25) |>
    padding(
      i = which(highlight_rows),
      j = NULL,
      padding.top = 20
    ) |>
    # Relative rows
    italic(relative_rows, -1) |>
    color(relative_rows, -1, color = "#71797E") |>
    fontsize(relative_rows, -1, size = 8) |>
    padding(relative_rows, padding.top = 3) |>
    padding(relative_rows - 1, padding.bottom = 0) |>
    # Footnotes
    attach_fnotes() |>
    # Cell merging
    merge_v(1) |>
    merge_v(2) |>
    # Image column
    colformat_image(j= 2, width = 2 / 2.54, height = 0.6 / 2.54) #|>
    # Page fit
    # autofit()
    # fit_to_width(max_width = 50, unit = "cm")
    # fit_to_width(max_width = 210 - 24 * 2, unit = "mm")


  # Write table
  saveRDS(flex, path_flextable(table_name))
  return(NULL)

}



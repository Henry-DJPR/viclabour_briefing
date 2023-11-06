
# Generate html table
make_table <- function(
    table_name,
    series_ids,
    row_headers,
    highlight_rows,
    up_is_good,
    smoothing = NULL,
    notes = NULL
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

  # Get interval dates
  breaks <- c(
    current = df[, max(date)],
    last    = df[, max(date[date != max(date)])],
    year    = df[, years_ago(max(date), 1)],
    covid   = as.Date("2019-03-01"),
    govt    = as.Date("2014-11-01")
  )

  # Generate deltas
  deltas <- df[date %in% breaks]
  deltas[, date := names(breaks)[match(date, breaks)]]
  deltas[, date := factor(date, levels = names(breaks))]
  deltas <- dcast(deltas, series_id + data_type ~ date, value.var = "value")

  diff_cols <- names(breaks)[names(breaks) != "current"]
  deltas[, (diff_cols) := lapply(.SD, \(x) current - x), .SDcols = diff_cols]


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
        "table-success",
        "table-danger"
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
    (diff_cols) := lapply(.SD, format_any, form = data_type, add_sign = TRUE),
    .SDcols = diff_cols
  ]
  deltas[, data_type := NULL]


  # Define top and bottom rows
  top_row <- series_ids[1]
  bottom_row <- series_ids[length(series_ids)]


  # Convert to HTML table
  tdeltas <- as.data.table(t(deltas))

  rows <- sapply(tdeltas, function(x){
    row_title     <- row_headers[match(x[1], series_ids)]
    row_content   <- x[2:length(x)]
    row_highlight <- highlight_rows[match(row_title, row_headers)]
    alt_text      <- sparktext$alt[match(x[1], sparktext$series_id)]
    row_classes   <- backgrounds[series_id == x[1], bg]
    is_top        <- x[1] == top_row
    is_bottom     <- x[1] == bottom_row

    if(row_highlight & is_bottom){
      tr(
        class = "border-top border-bottom",
        th(row_title, scope = "row"),
        td(
          img(
            src = paste0("./sparklines/", table_name, "/", x[1], ".svg"),
            alt = alt_text,
            class = "img-fluid sparkline float-start"
            )
          ),
        mapply(td, row_content, class = row_classes)
      )
    } else if(row_highlight){
      tr(
        class = "border-top",
        th(row_title, scope = "row"),
        td(
          img(
            src = paste0("./sparklines/", table_name, "/", x[1], ".svg"),
            alt = alt_text,
            class = "img-fluid sparkline float-start"
          )
        ),
        mapply(td, row_content, class = row_classes)
      )
    } else if(is_top){
      tr(
        class = "subrow border-top",
        th(row_title, scope = "row"),
        td(
          img(
            src = paste0("./sparklines/", table_name, "/", x[1], ".svg"),
            alt = alt_text,
            class = "img-fluid sparkline float-start"
          )
        ),
        mapply(td, row_content, class = row_classes)
      )
    } else if(is_bottom){
      tr(
        class = "subrow border-bottom",
        th(row_title, scope = "row"),
        td(
          img(
            src = paste0("./sparklines/", table_name, "/", x[1], ".svg"),
            alt = alt_text,
            class = "img-fluid sparkline float-start"
          )
        ),
        mapply(td, row_content, class = row_classes)
      )
    } else {
      tr(
        class = "subrow",
        th(row_title, scope = "row"),
        td(
          img(
            src = paste0("./sparklines/", table_name, "/", x[1], ".svg"),
            alt = alt_text,
            class = "img-fluid sparkline float-start"
          )
        ),
        mapply(td, row_content, class = row_classes)
      )
    }

  })

  rows <- tbody(rows)

  header <- thead(
    td(""),
    th(
      class = "text-start",
      scope = "col",
      "Recent trend",
      br(),
      small(
        class = "text-muted p-0",
        "Last 3 years"
      )
    ),
    th(
      scope = "col",
      "Latest figures",
      br(),
      small(
        class = "text-muted p-0",
        format(breaks["current"], "%b&nbsp;%Y")
        )
    ),
    th(
      scope = "col",
      paste("One", tolower(interval), "change"),
      br(),
      small(
        class = "text-muted p-0",
        format(breaks["last"], "%b&nbsp;%Y")
      )
    ),
    th(
      scope = "col",
      "One year change",
      br(),
      small(
        class = "text-muted p-0",
        format(breaks["year"], "%b&nbsp;%Y")
      )
    ),
    th(
      scope = "col",
      "Change since COVID&#8209;19",
      br(),
      small(
        class = "text-muted p-0",
        format(breaks["covid"], "%b&nbsp;%Y")
      )
    )
  )

  # Return table
  return(
    table_tag(
      caption(notes),
      header,
      rows
    )
  )

}



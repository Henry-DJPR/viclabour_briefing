
# Generate html table
make_table_latex <- function(
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


  # Generate pdf sparklines
  lapply(
    path_sparkline(table_name, series_ids),
    \(x){
      out_path <- sub("svg$", "pdf", x)
      rsvg::rsvg_pdf(x, out_path)
    }
  )


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



  # format deltas
  deltas[, current := format_any(current, data_type)]
  deltas[data_type == "PERCENT", data_type := "PPT"]
  deltas[,
         (diff_cols) := lapply(.SD, format_any, form = data_type, add_sign = FALSE),
         .SDcols = diff_cols
  ]
  deltas[, data_type := NULL]


  # Add sparkline column
  deltas[, trend := path_sparkline_pdf(table_name, series_id)]
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


  # Replace row labels
  deltas[, series_id := row_headers[match(series_id, series_ids)]]


  # latex table header
  ## Split long header on last space and break/resume bold text
  latex_header_1 <- sub("^(.*)\\s(.*)$", "\\1} \\\\\\\\ \\\\textbf{\\2", row_header_1)
  ## Format headers as bold and small
  latex_header_1 <- sprintf("\\textbf{%s}", c("Trend", latex_header_1))
  latex_header_2 <- sprintf("\\small{%s}", c("Last 3 years", row_header_2))
  ## Combine with linebreak and concatenate with \thead and &
  latex_header <- paste(latex_header_1, latex_header_2, sep = " \\\\ ")
  latex_header <- paste0(
    "  & \\makecell[r]{",
    paste0(latex_header, collapse = "} & \\makecell[r]{"),
    "} \\\\"
  )


  # Wrap first column
  delta_matrix <- as.matrix(deltas)
  delta_matrix[, 1] <- stringr::str_wrap(delta_matrix[, 1], width = 20)
  delta_matrix[, 1] <- stringr::str_replace_all(delta_matrix[, 1], "\\n", "\\\\\\\\")


  # Latex header / nonheader rows
  if(any(highlight_rows)){
    delta_matrix[highlight_rows, 3:ncol(deltas)] <-
      sprintf("\\textbf{%s}", delta_matrix[highlight_rows, 3:ncol(deltas)])
    delta_matrix[highlight_rows, 1] <-
      sprintf("\\makecell[l]{\\textbf{%s}}", delta_matrix[highlight_rows, 1])
    delta_matrix[!highlight_rows, 1] <-
      sprintf("\\hspace{3mm}\\makecell[l]{%s}", delta_matrix[!highlight_rows, 1])
  } else {
    # Wrap first column in \makecell
    delta_matrix[, 1] <- sprintf("\\makecell[l]{%s}", delta_matrix[, 1])
  }

  delta_matrix <- gsub("\\%", "\\\\%", delta_matrix)

  # collapse relative rows
  if(length(relative_rows) > 0){

    delta_matrix[relative_rows, ] <-
      sprintf("\\small{%s}", delta_matrix[relative_rows, ])

    for(i in relative_rows){
      delta_matrix[i - 1, 4:ncol(deltas)] <- paste0(
        "\\makecell[r]{",
        delta_matrix[i - 1, 4:ncol(deltas)],
        " \\\\ ",
        delta_matrix[i, 4:ncol(deltas)],
        "}"
      )
    }

    delta_matrix <- delta_matrix[-relative_rows, ]

  }


  # Latex images
  delta_matrix[, 2] <- sprintf(
    "\\makecell[r]{\\includegraphics[width=2.5cm]{%s}}",
    delta_matrix[, 2]
    )



  # Generate latex output table
  tab_out <- c(

    # Table start
    r"--(\resizebox{\textwidth}{!}{)--",
    r"--(\begin{tabular}{l r r r r r r})--",

    # Header
    r"--(\hline)--",
    latex_header,
    r"--(\hline)--",

    # Rows
    apply(
      X      = delta_matrix,
      MARGIN = 1,
      FUN    = \(x) paste(c(x), collapse = " & ")
    ) |>
      paste0(" \\\\"),


    # Fin
    r"--(\hline)--",
    r"--(\end{tabular})--",
    r"--(})--"
    # r"--()--",
    # r"--()--",
    # r"--()--"
  )


  # save
  writeLines(tab_out, path_latex_table(table_name))

}



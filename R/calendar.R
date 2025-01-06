
# Applies mapper to some elements of `.x`. By default, it skips NA elements.
# This is necessary because most functions treat NA_character_ as "NA".
map_present <- function(.x, .f, .ignore = is.na, ...) {
  valid <- !.ignore(.x)
  .x[valid] <- .f(.x[valid], ...)
  .x
}


read_abs_calendar <- function(
    url = "https://www.abs.gov.au/release-calendar/future-releases/all",
    verbose = T,
    read_html = xml2::read_html
) {
  paged_releases <- list()
  next_page <- url

  while(!is.na(next_page)) {
    ## Structure:
    ##
    ## *.views-row (calendar entry)
    ## |- * time[@datetime=...] (release time)
    ## |- * h3 (title)
    ## |- *.views-field-body (description)
    ## |- *.views-field-field-rs-reference-period (reference period)
    ## |- *.rs-product-link-latest a (link to current release)

    pg <- read_html(next_page)
    entries <- pg |>
      rvest::html_elements(".views-row")

    e_times <- entries |>
      rvest::html_elements("time") |>
      rvest::html_attr("datetime") |>
      lubridate::ymd_hms() |>
      lubridate:::update.POSIXt(hour = 11, minute = 30)

    e_titles <- entries |>
      rvest::html_element("h3") |>
      rvest::html_text2()

    e_descriptions <- entries |>
      rvest::html_element(".views-field-body") |>
      rvest::html_text2()

    e_ref_periods <- entries |>
      rvest::html_element(".views-field-field-rs-reference-period") |>
      rvest::html_text2() |>
      stringr::str_replace("^Reference period ", "") |>
      stringr::str_replace(" financial year$", "")

    e_current_urls <- entries |>
      rvest::html_element("a[href]") |>
      rvest::html_attr("href") |>
      # url_absolute is not NA-safe
      map_present(xml2::url_absolute, base = next_page)

    releases_on_page <- data.table::data.table(
      time = e_times
      , title = e_titles
      , description = e_descriptions
      , ref_period = e_ref_periods
      , current_url = e_current_urls
      , latest_url = map_present(
        e_current_urls
        , \(.x) file.path(dirname(.x), "latest-release")
      )
      , calendar_url = next_page
    )
    paged_releases <- c(paged_releases, list(releases_on_page))
    if(verbose) {
      message(sprintf("[%d] %s", nrow(releases_on_page), next_page))
    }

    next_page <- pg |>
      rvest::html_element("a[rel=next]") |>
      rvest::html_attr("href") |>
      lapply(
        \(.x) if(is.na(.x)) .x else xml2::url_absolute(.x, base = next_page)
      ) |>
      unlist() |>
      as.character()
  }

  data.table::rbindlist(paged_releases)[
    title %in% c("Labour Force, Australia", "Labour Force, Australia, Detailed")
  ]
}

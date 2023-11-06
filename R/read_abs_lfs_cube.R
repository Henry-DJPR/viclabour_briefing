# read ABS LFS cube
read_abs_lfs_cube <- function(tables){
  
  urls <- abs_lfs_cube_urls[tables]
  if(length(urls) != length(tables)) stop("Unknown LFS pivot tables found")
  
  paths <- sapply(
    urls,
    \(x){
      tmp <- file.path(tempdir(), basename(x))
      download.file(x, tmp, mode = "wb")
      tmp
    }
  )
  
  dt <- mapply(
    SIMPLIFY = F,
    path = paths,
    tab = tables,
    FUN = \(path, tab){
      sheets <- openxlsx::getSheetNames(path)
      sheets <- sheets[grepl("Data|data", sheets)]
      df <- mapply(
        read_abs_lfs_cube_sheet, 
        sheet = sheets, 
        path = path, 
        SIMPLIFY = F
        )
      df <- rbindlist(df)
      df[, table_no := tab]
      df[, series_id := paste0(tab, ";", series_id)]
      df
    }
  )
  
  rbindlist(dt)
}


read_abs_lfs_cube_sheet <- function(path, sheet){
  # Parse and class
  df <- read.xlsx(path, sheet, startRow = 4, sep.names = " ")
  setDT(df)
  
  # Determine date column & frequency
  date_col <- intersect(names(df), abs_lfs_cube_freq)
  new_freq <- names(abs_lfs_cube_freq)[match(date_col, abs_lfs_cube_freq)]
  setnames(df, date_col, "date")

  # Fix dates
  df[, date := as.Date(date, origin = "1899-12-30")]
  
  # Pivot numeric columns
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  non_numeric_cols <- names(df)[!(names(df) %in% numeric_cols)]
  
  df <- melt(
    data = df, 
    id.vars = non_numeric_cols, 
    measure.vars = numeric_cols, 
    variable.name = "measure",
    value.name = "value"
    )
  
  # add unit
  df[
    , 
    unit := names(abs_lfs_cube_units)[
      match(measure[1], abs_lfs_cube_units)[1]
      ],
    measure
    ]
  
  # concatenate into series and series id
  concat_names <- names(df)[!(names(df) %in% c("date", "value", "unit"))]
  df[, series := do.call(paste, c(.SD, sep=";")), .SDcols = concat_names]
  df[, series_id := series]
  df[, (concat_names) := NULL]
  
  # add frequency and series_type
  df[, freq := new_freq]
  df[, series_type := "Original"]
  
  df
  
}


# ABS LFS pivot urls
abs_lfs_cube_urls <- c(
  EM2a = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/latest-release/EM2a.xlsx",
  EM2b = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/latest-release/EM2b.xlsx",
  GM1 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia/latest-release/GM1.xlsx",
  LM1 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/LM1.xlsx",
  LM3 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/LM3.xlsx",
  LM3a = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/LM3a.xlsx",
  LM4 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/LM4.xlsx",
  LM5 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/LM5.xlsx",
  LM7 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/LM7.xlsx",
  LM9 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/LM9.xlsx",
  LQ1 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/LQ1.xlsx",
  LQ2 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/LQ2.xlsx",
  EQ03 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EQ03.xlsx",
  EQ06 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EQ06.xlsx",
  EQ07a = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EQ07a.xlsx",
  EQ07b = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EQ07b.xlsx",
  EQ08 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EQ08.xlsx",
  EQ09 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EQ09.xlsx",
  EQ10 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EQ10.xlsx",
  EQ11 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EQ11.xlsx",
  EQ12 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EQ12.xlsx",
  EQ13 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EQ13.xlsx",
  EQ14 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EQ14.xlsx",
  EM1a = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EM1a.xlsx",
  EM1b = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EM1b.xlsx",
  EM3a = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EM3a.xlsx",
  EM3b = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EM3b.xlsx",
  EM4a = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EM4a.xlsx",
  EM4b = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EM4b.xlsx",
  EM5a = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EM5a.xlsx",
  EM5b = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EM5b.xlsx",
  UM2 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/UM2.xlsx",
  UM3 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/UM3.xlsx",
  UQ2a = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/UQ2a.xlsx",
  UQ2b = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/UQ2b.xlsx",
  UQ3a = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/UQ3a.xlsx",
  UQ3b = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/UQ3b.xlsx",
  EM6 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EM6.xlsx",
  EM6a = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EM6a.xlsx",
  EQ04 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EQ04.xlsx",
  EQ05 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EQ05.xlsx",
  RM1 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/RM1.xlsx",
  RM3 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/RM3.xlsx",
  RQ1 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/RQ1.xlsx",
  RQ2 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/RQ2.xlsx",
  FM1 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/FM1.xlsx",
  FM2 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/FM2.xlsx",
  FM3 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/FM3.xlsx",
  FM4 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/FM4.xlsx",
  EQ02 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/EQ02.xlsx",
  NM1 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/NM1.xlsx",
  NM2 = "https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release/NM2.xlsx"
)

# metadata

abs_lfs_cube_freq <- c(
  Month = "Month",
  Quarter = "Mid-quarter month",
  Annual = "Annual average of the preceding 4 quarters"
)

abs_lfs_cube_units <- c(
  "Hours" = "Hours actually worked in all jobs",
  "000" = "Employed full-time ('000)",
  "000" = "Employed part-time ('000)",
  "000 Hours" = "Number of hours actually worked in all jobs (employed full-time) ('000 Hours)",
  "000 Hours" = "Number of hours actually worked in all jobs (employed part-time) ('000 Hours)",
  "000" = "Persons - current month ('000)",
  "000" = "Persons - previous month ('000)",
  "000" = "Unemployed looked for full-time work ('000)",
  "000" = "Unemployed looked for only part-time work ('000)",
  "000" = "Not in the labour force (NILF) ('000)",
  "000" = "Unemployed total ('000)",
  "000" = "Employed total ('000)",
  "000 Hours" = "Number of hours actually worked in all jobs ('000 Hours)",
  "Hours" = "Hours usually worked in all jobs",
  "000 Hours" = "Number of hours usually worked in all jobs (employed full-time) ('000 Hours)",
  "000 Hours" = "Number of hours usually worked in all jobs (employed part-time) ('000 Hours)",
  "Hours" = "Hours actually worked in main job",
  "000 Hours" = "Number of hours actually worked in main job (employed full-time) ('000 Hours)",
  "000 Hours" = "Number of hours actually worked in main job (employed part-time) ('000 Hours)",
  "Hours" = "Hours usually worked in main job",
  "000 Hours" = "Number of hours usually worked in main job (employed full-time) ('000 Hours)",
  "000 Hours" = "Number of hours usually worked in main job (employed part-time) ('000 Hours)",
  "000 Weeks" = "Number of weeks searching for job ('000 Weeks)"
)


read_abs_lfs_youth <- function(...){
  # acquire data and set up columns
  lm1 <- read_abs_lfs_cube("LM1") 
  lm1[
    , 
    c(
      "sex", 
      "age", 
      "marital_status", 
      "gcc_restofstate", 
      "indicator"
      ) := tstrsplit_factor(series, split = ";")
    ]
  
  # Filter for Vic
  lm1 <- lm1[gcc_restofstate %in% c("Greater Melbourne", "Rest of Vic.")]
  
  # Aggregate years
  year_lookup <- c(
    "15-19 years" = "15-24",
    "20-24 years" = "15-24",
    "25-29 years" = "25-54",
    "30-34 years" = "25-54",
    "35-39 years" = "25-54",
    "40-44 years" = "25-54",
    "45-49 years" = "25-54",
    "50-54 years" = "25-54",
    "55-59 years" = "55+",
    "60-64 years" = "55+",
    "65 years and over" = "55+"
  )
  
  lm1[, age := as.integer(factor(age, levels = names(year_lookup)))]
  lm1[, age := year_lookup[age]]
  
  
  # rename indicators
  lm1[
    indicator %in% c("Employed full-time ('000)", "Employed part-time ('000)"),
    indicator := "Employed"
    ]
  
  lm1[
    indicator %in% c(
      "Unemployed looked for full-time work ('000)",
      "Unemployed looked for only part-time work ('000)"
      ),
    indicator := "Unemployed"
    ]
  
  lm1[
    indicator == "Not in the labour force (NILF) ('000)",
    indicator := "NILF"
  ]
 
  # Aggregate over simplified categories
  lm1_gcc <- lm1[
    , 
    .(value = sum(value)), 
    .(date, age, gcc_restofstate, indicator)
    ]
  
  lm1_sex <- lm1[, .(value = sum(value)), .(date, age, sex, indicator)]
 
  
  # Calculate unemployment rate for GCC split
  lm1_gcc_unemp <- dcast(
    data = lm1_gcc, 
    formula = date + age + gcc_restofstate ~ indicator, 
    value.var = "value"
    )
  
  lm1_gcc_unemp[, value := Employed / (Employed + Unemployed) * 100]
  lm1_gcc_unemp[, indicator := "Unemployment rate"]
  lm1_gcc_unemp[, c("Employed", "Unemployed", "NILF") := NULL]
  
  lm1_gcc <- rbind(lm1_gcc, lm1_gcc_unemp, fill = T)
  
  
  # Calculate unemployment rate for sex split
  lm1_sex_unemp <- dcast(
    data = lm1_sex, 
    formula = date + age + sex ~ indicator, 
    value.var = "value"
  )
  
  lm1_sex_unemp[, value := Employed / (Employed + Unemployed) * 100]
  lm1_sex_unemp[, indicator := "Unemployment rate"]
  lm1_sex_unemp[, c("Employed", "Unemployed", "NILF") := NULL]
  
  lm1_sex <- rbind(lm1_gcc, lm1_gcc_unemp, fill = T)
  
  
  # generate series ids
  concat_names_gcc <- names(lm1_gcc)[!(names(lm1_gcc) %in% c("date", "value"))]
  lm1_gcc[, series := do.call(paste, c(.SD, sep="_")), .SDcols = concat_names_gcc]
  lm1_gcc[, series_id := series]
  lm1_gcc[, (concat_names_gcc) := NULL]
  
  concat_names_sex <- names(lm1_sex)[!(names(lm1_sex) %in% c("date", "value"))]
  lm1_sex[, series := do.call(paste, c(.SD, sep="_")), .SDcols = concat_names_sex]
  lm1_sex[, series_id := series]
  lm1_sex[, (concat_names_sex) := NULL]
  
  
  # bind and add in supplementary data 
  lm1 <- rbind(lm1_gcc, lm1_sex)
  lm1[
    ,
    `:=`(
      series = tolower(series),
      series_id = tolower(series_id),
      series_type = "Original",
      table_no = "LM1",
      data_type = "STOCK",
      frequency = "Month",
      unit = "000",
      cat_no = "6291.0.55.001"
    )
  ]
  
  return(lm1)
}


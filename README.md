# Talking points

For each talking point there can be three possible variants:

-   There is a significant change and the difference in rounded old/new values match the rounded delta:

    -   *Old, new and delta are presented*

-   There is an significant change and he difference in rounded old/new values *do not match* the rounded delta:

    -   *New and delta are presented*

-   There is no significant change:

    -   *Only new value presented*

All talking points are generated by the file `talking_points.csv` where each unique row is a new talking point and the columns are as follows:

| Column name | Description |
|----|----|
| name | Name for the talking point - unused and just for readability in the file |
| series_id | data series as found in the project jobs_data |
| parse_method | how to download the series (see: updating) |
| smoothing_months | rolling average of n months applied to data. No rolling average if blank. |
| rounding_digits | How many digits the series should be rounded to for presentation. Negative values will round to the corresponding power of ten (as per `base::round`). |
|  |  |
|  |  |
|  |  |
|  |  |
|  |  |
|  |  |
|  |  |
|  |  |
|  |  |
|  |  |
|  |  |
|  |  |
|  |  |
|  |  |

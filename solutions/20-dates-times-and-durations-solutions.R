# ==============================================================================
# Learning R
# Part III: Importing, Cleaning and Transforming Data
# Lesson 20: Dates, Times and Durations
# Suggested Solutions
# ==============================================================================

# Exercise 1: Create and inspect Date objects.
study_date <- as.Date(
  "2026-07-14"
)

study_date
class(study_date)
typeof(study_date)
unclass(study_date)

# Exercise 2: Parse day-month-year and month-day-year.
date_dmy <- as.Date(
  "14/07/2026",
  format = "%d/%m/%Y"
)

date_mdy <- as.Date(
  "07/14/2026",
  format = "%m/%d/%Y"
)

date_dmy
date_mdy

# Exercise 3: Identify invalid dates.
raw_dates <- c(
  "01/01/2026",
  "31/02/2026",
  "15/03/2026"
)

parsed_dates <- as.Date(
  raw_dates,
  format = "%d/%m/%Y"
)

date_conversion_report <- data.frame(
  raw = raw_dates,
  parsed = parsed_dates,
  failed =
    is.na(parsed_dates) &
    !is.na(raw_dates),
  stringsAsFactors = FALSE
)

date_conversion_report

# Exercise 4: Format dates for reporting.
format(
  study_date,
  "%d %B %Y"
)

format(
  study_date,
  "%A, %d %B %Y"
)

# Exercise 5: Extract components.
date_components <- data.frame(
  year = as.integer(
    format(
      study_date,
      "%Y"
    )
  ),
  month_number = as.integer(
    format(
      study_date,
      "%m"
    )
  ),
  month_name = format(
    study_date,
    "%B"
  ),
  day = as.integer(
    format(
      study_date,
      "%d"
    )
  ),
  weekday = format(
    study_date,
    "%A"
  ),
  stringsAsFactors = FALSE
)

date_components

# Exercise 6: Compare and sort dates.
event_dates <- as.Date(
  c(
    "2026-03-10",
    "2026-01-15",
    "2026-02-20"
  )
)

sort(event_dates)

event_dates > as.Date(
  "2026-02-01"
)

# Exercise 7: Date differences.
start_date <- as.Date(
  "2026-01-15"
)

end_date <- as.Date(
  "2026-03-20"
)

end_date - start_date

difftime(
  end_date,
  start_date,
  units = "days"
)

# Exercise 8: Date sequences.
daily_sequence <- seq(
  from = as.Date(
    "2026-01-01"
  ),
  to = as.Date(
    "2026-01-10"
  ),
  by = "day"
)

weekly_sequence <- seq(
  from = as.Date(
    "2026-01-01"
  ),
  by = "week",
  length.out = 6
)

monthly_sequence <- seq(
  from = as.Date(
    "2026-01-01"
  ),
  by = "month",
  length.out = 12
)

daily_sequence
weekly_sequence
monthly_sequence

# Exercise 9: POSIXct date-times.
appointment_time <- as.POSIXct(
  "2026-07-14 09:30:00",
  format = "%Y-%m-%d %H:%M:%S",
  tz = "Africa/Accra"
)

appointment_time
class(appointment_time)
typeof(appointment_time)

# Exercise 10: Time-zone display.
format(
  appointment_time,
  tz = "Africa/Accra",
  usetz = TRUE
)

format(
  appointment_time,
  tz = "Europe/London",
  usetz = TRUE
)

# Exercise 11: Elapsed minutes.
start_time <- as.POSIXct(
  "2026-07-14 09:15:00",
  tz = "Africa/Accra"
)

end_time <- as.POSIXct(
  "2026-07-14 11:05:00",
  tz = "Africa/Accra"
)

elapsed_minutes <- difftime(
  end_time,
  start_time,
  units = "mins"
)

elapsed_minutes

# Exercise 12: Validate chronological order.
registration_dates <- as.Date(
  c(
    "2026-01-10",
    "2026-03-20",
    NA
  )
)

completion_dates <- as.Date(
  c(
    "2026-03-15",
    "2026-02-15",
    "2026-04-01"
  )
)

chronology_valid <- is.na(
  registration_dates
) |
  is.na(
    completion_dates
  ) |
  completion_dates >=
  registration_dates

data.frame(
  registration_date =
    registration_dates,
  completion_date =
    completion_dates,
  valid =
    chronology_valid
)

# Exercise 13: Detect future dates.
dates_to_check <- as.Date(
  c(
    "2025-12-01",
    "2027-01-01",
    NA
  )
)

future_status <- dates_to_check >
  Sys.Date()

data.frame(
  date = dates_to_check,
  future = future_status
)

# Exercise 14: Safe date-parsing function.
parse_date_safely <- function(
  x,
  format
) {
  if (!is.character(x)) {
    stop(
      "`x` must be character.",
      call. = FALSE
    )
  }

  cleaned <- trimws(x)

  cleaned[
    cleaned == ""
  ] <- NA

  parsed <- as.Date(
    cleaned,
    format = format
  )

  data.frame(
    raw = x,
    parsed = parsed,
    failed =
      is.na(parsed) &
      !is.na(cleaned),
    stringsAsFactors = FALSE
  )
}

parse_date_safely(
  c(
    "14/07/2026",
    "31/02/2026",
    ""
  ),
  format = "%d/%m/%Y"
)

# Exercise 15: Monthly reporting schedule.
reporting_dates <- seq(
  from = as.Date(
    "2026-01-01"
  ),
  by = "month",
  length.out = 12
)

reporting_schedule <- data.frame(
  reporting_date = reporting_dates,
  year = format(
    reporting_dates,
    "%Y"
  ),
  month_number = format(
    reporting_dates,
    "%m"
  ),
  month_name = format(
    reporting_dates,
    "%B"
  ),
  weekday = format(
    reporting_dates,
    "%A"
  ),
  stringsAsFactors = FALSE
)

reporting_schedule

# Optional lubridate demonstrations.
if (requireNamespace(
  "lubridate",
  quietly = TRUE
)) {
  print(
    lubridate::ymd(
      "2026-07-14"
    )
  )

  print(
    lubridate::dmy(
      "14/07/2026"
    )
  )

  print(
    lubridate::ymd_hms(
      "2026-07-14 09:30:00",
      tz = "Africa/Accra"
    )
  )
}

# Practical assessment workflow.
raw_appointment_data <- data.frame(
  appointment_id = c(
    "A001",
    "A002",
    "A003"
  ),
  start_text = c(
    "14/07/2026 09:00",
    "15/07/2026 14:30",
    "31/02/2026 10:00"
  ),
  end_text = c(
    "14/07/2026 10:15",
    "15/07/2026 16:00",
    "31/02/2026 11:00"
  ),
  stringsAsFactors = FALSE
)

raw_appointment_data$start_time <- as.POSIXct(
  raw_appointment_data$start_text,
  format = "%d/%m/%Y %H:%M",
  tz = "Africa/Accra"
)

raw_appointment_data$end_time <- as.POSIXct(
  raw_appointment_data$end_text,
  format = "%d/%m/%Y %H:%M",
  tz = "Africa/Accra"
)

raw_appointment_data$parse_failed <-
  is.na(
    raw_appointment_data$start_time
  ) |
  is.na(
    raw_appointment_data$end_time
  )

raw_appointment_data$chronology_valid <-
  raw_appointment_data$parse_failed |
  raw_appointment_data$end_time >=
    raw_appointment_data$start_time

raw_appointment_data$duration_minutes <- as.numeric(
  difftime(
    raw_appointment_data$end_time,
    raw_appointment_data$start_time,
    units = "mins"
  )
)

raw_appointment_data

# ==============================================================================
# End of Lesson 20 Solutions
# ==============================================================================

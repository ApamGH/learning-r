# ==============================================================================
# Learning R
# Part III: Importing, Cleaning and Transforming Data
# Lesson 18: Missing Values and Incomplete Data
# Suggested Solutions
# ==============================================================================

# Exercise 1: Create and inspect missing values.
scores <- c(
  65,
  NA,
  72,
  81,
  NA
)

scores
is.na(scores)
anyNA(scores)

# Exercise 2: Count and locate missing values.
missing_count <- sum(
  is.na(scores)
)

observed_count <- sum(
  !is.na(scores)
)

missing_positions <- which(
  is.na(scores)
)

missing_count
observed_count
missing_positions

# Exercise 3: Effective sample size and missing percentage.
total_positions <- length(scores)

missing_percentage <- mean(
  is.na(scores)
) * 100

data.frame(
  total = total_positions,
  observed = observed_count,
  missing = missing_count,
  missing_percentage = missing_percentage
)

# Exercise 4: Create a missingness report.
create_missingness_report <- function(
  data
) {
  data.frame(
    variable = names(data),
    total = nrow(data),
    observed = vapply(
      data,
      function(x) {
        sum(
          !is.na(x)
        )
      },
      FUN.VALUE = integer(1)
    ),
    missing = vapply(
      data,
      function(x) {
        sum(
          is.na(x)
        )
      },
      FUN.VALUE = integer(1)
    ),
    missing_percentage = vapply(
      data,
      function(x) {
        mean(
          is.na(x)
        ) * 100
      },
      FUN.VALUE = numeric(1)
    ),
    stringsAsFactors = FALSE
  )
}

rainfall_data <- read.csv(
  "data/rainfall_records.csv",
  stringsAsFactors = FALSE
)

create_missingness_report(
  rainfall_data
)

# Exercise 5: Complete and incomplete rows.
complete_status <- complete.cases(
  rainfall_data
)

complete_records <- rainfall_data[
  complete_status,
]

incomplete_records <- rainfall_data[
  !complete_status,
]

complete_records
incomplete_records

# Exercise 6: Row-level missing counts.
rainfall_data$missing_count <- rowSums(
  is.na(
    rainfall_data
  )
)

rainfall_data

# Exercise 7: Replace documented special codes.
raw_values <- c(
  10,
  20,
  999,
  -99,
  30
)

clean_values <- raw_values

clean_values[
  clean_values %in%
    c(
      999,
      -99
    )
] <- NA

data.frame(
  raw = raw_values,
  clean = clean_values
)

# Exercise 8: Standardise blanks to NA.
raw_text <- c(
  "Statistics",
  "",
  " ",
  "N/A",
  NA,
  "Procurement"
)

clean_text <- trimws(
  raw_text
)

clean_text[
  clean_text %in%
    c(
      "",
      "N/A"
    )
] <- NA

clean_text

# Exercise 9: Missingness by group.
clinic_data <- read.csv(
  "data/clinic_visits.csv",
  stringsAsFactors = FALSE
)

missing_by_sex <- tapply(
  is.na(
    clinic_data$waiting_time
  ),
  clinic_data$sex,
  mean
) * 100

missing_by_visit_type <- tapply(
  is.na(
    clinic_data$waiting_time
  ),
  clinic_data$visit_type,
  mean
) * 100

missing_by_sex
missing_by_visit_type

table(
  clinic_data$sex,
  is.na(
    clinic_data$waiting_time
  ),
  useNA = "ifany"
)

# Exercise 10: Listwise and pairwise deletion.
analysis_variables <- clinic_data[
  c(
    "age",
    "waiting_time",
    "consultation_duration"
  )
]

complete_analysis_data <- analysis_variables[
  complete.cases(
    analysis_variables
  ),
]

complete_case_correlation <- cor(
  analysis_variables,
  use = "complete.obs"
)

pairwise_correlation <- cor(
  analysis_variables,
  use = "pairwise.complete.obs"
)

nrow(analysis_variables)
nrow(complete_analysis_data)
complete_case_correlation
pairwise_correlation

# Exercise 11: Simple replacement methods.
waiting_values <- clinic_data$waiting_time

mean_waiting <- mean(
  waiting_values,
  na.rm = TRUE
)

median_waiting <- median(
  waiting_values,
  na.rm = TRUE
)

mean_imputed <- waiting_values
median_imputed <- waiting_values

mean_imputed[
  is.na(mean_imputed)
] <- mean_waiting

median_imputed[
  is.na(median_imputed)
] <- median_waiting

replacement_comparison <- data.frame(
  method = c(
    "Observed only",
    "Mean replacement",
    "Median replacement"
  ),
  mean = c(
    mean(
      waiting_values,
      na.rm = TRUE
    ),
    mean(mean_imputed),
    mean(median_imputed)
  ),
  standard_deviation = c(
    sd(
      waiting_values,
      na.rm = TRUE
    ),
    sd(mean_imputed),
    sd(median_imputed)
  ),
  stringsAsFactors = FALSE
)

replacement_comparison

# Exercise 12: Missing-data decision log.
missing_data_log <- data.frame(
  variable = c(
    "rainfall_mm",
    "waiting_time"
  ),
  missing_count = c(
    sum(
      is.na(
        rainfall_data$rainfall_mm
      )
    ),
    sum(
      is.na(
        clinic_data$waiting_time
      )
    )
  ),
  decision = c(
    "Use observed values for descriptive summaries",
    "Retain missing values pending further investigation"
  ),
  justification = c(
    "Descriptive objective and limited missingness",
    "Cause and pattern of missingness require review"
  ),
  stringsAsFactors = FALSE
)

missing_data_log

# Exercise 13: Visualise missing counts.
clinic_missing_counts <- colSums(
  is.na(
    clinic_data
  )
)

barplot(
  clinic_missing_counts,
  main = "Missing Values by Variable",
  xlab = "Variable",
  ylab = "Number missing",
  las = 2
)

# Exercise 14: Audit missingness.
audit_missing_data <- function(
  data
) {
  row_missing_count <- rowSums(
    is.na(data)
  )

  list(
    variable_report =
      create_missingness_report(
        data
      ),
    total_records =
      nrow(data),
    complete_records =
      sum(
        complete.cases(data)
      ),
    incomplete_records =
      sum(
        !complete.cases(data)
      ),
    complete_percentage =
      mean(
        complete.cases(data)
      ) * 100,
    rows_with_two_or_more_missing =
      sum(
        row_missing_count >= 2
      )
  )
}

clinic_missing_audit <- audit_missing_data(
  clinic_data
)

clinic_missing_audit

# Practical assessment workflow.
assessment_data <- clinic_data

assessment_data$waiting_time_missing <- is.na(
  assessment_data$waiting_time
)

assessment_data$row_missing_count <- rowSums(
  is.na(
    assessment_data
  )
)

variable_missingness <- create_missingness_report(
  assessment_data
)

group_missingness <- aggregate(
  waiting_time_missing ~ sex,
  data = assessment_data,
  FUN = mean
)

group_missingness$waiting_time_missing <-
  group_missingness$waiting_time_missing *
  100

variable_missingness
group_missingness

assessment_decision <- data.frame(
  method = c(
    "Complete-case analysis",
    "Pairwise deletion",
    "Mean replacement",
    "Median replacement"
  ),
  main_limitation = c(
    "Reduces sample size and may bias results",
    "Uses different samples across statistics",
    "Reduces variance and can distort associations",
    "Can distort distribution and relationships"
  ),
  stringsAsFactors = FALSE
)

assessment_decision

# ==============================================================================
# End of Lesson 18 Solutions
# ==============================================================================

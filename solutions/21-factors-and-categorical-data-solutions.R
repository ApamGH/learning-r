# ==============================================================================
# Learning R
# Part III: Importing, Cleaning and Transforming Data
# Lesson 21: Factors and Categorical Data
# Suggested Solutions
# ==============================================================================

# Exercise 1: Create an unordered factor.
programme <- c(
  "Statistics",
  "Procurement",
  "Hospitality",
  "Statistics",
  "Agriculture"
)

programme_factor <- factor(
  programme
)

programme_factor
class(programme_factor)
typeof(programme_factor)

# Exercise 2: Inspect levels and internal codes.
levels(programme_factor)
nlevels(programme_factor)
unclass(programme_factor)

# Exercise 3: Specify a non-alphabetical level order.
programme_factor_ordered_levels <- factor(
  programme,
  levels = c(
    "Statistics",
    "Procurement",
    "Hospitality",
    "Agriculture"
  )
)

levels(programme_factor_ordered_levels)

# Exercise 4: Validate categories before conversion.
raw_programme <- c(
  "Statistics",
  "Procurement",
  "Engineering"
)

allowed_programmes <- c(
  "Statistics",
  "Procurement",
  "Hospitality",
  "Agriculture"
)

invalid_programmes <- setdiff(
  unique(raw_programme),
  allowed_programmes
)

invalid_programmes

# Exercise 5: Create an ordered factor.
satisfaction <- factor(
  c(
    "Low",
    "Moderate",
    "High",
    "Moderate",
    "Very high"
  ),
  levels = c(
    "Low",
    "Moderate",
    "High",
    "Very high"
  ),
  ordered = TRUE
)

satisfaction
satisfaction >= "High"

# Exercise 6: Relabel factor levels.
employment_status <- factor(
  c(
    "FT",
    "PT",
    "UN",
    "FT"
  ),
  levels = c(
    "FT",
    "PT",
    "UN"
  )
)

levels(employment_status) <- c(
  "Full-time",
  "Part-time",
  "Unemployed"
)

employment_status

# Exercise 7: Collapse categories.
programme_group <- as.character(
  programme_factor_ordered_levels
)

programme_group[
  programme_group %in%
    c(
      "Statistics",
      "Procurement"
    )
] <- "Quantitative and Business"

programme_group[
  programme_group %in%
    c(
      "Hospitality",
      "Agriculture"
    )
] <- "Applied and Service"

programme_group <- factor(
  programme_group
)

programme_group
table(programme_group)

# Exercise 8: Unused levels and droplevels().
region_factor <- factor(
  c(
    "Upper East",
    "Northern",
    "Upper West"
  ),
  levels = c(
    "Upper East",
    "Northern",
    "Upper West",
    "Savannah"
  )
)

selected_regions <- region_factor[
  region_factor != "Northern"
]

levels(selected_regions)

selected_regions <- droplevels(
  selected_regions
)

levels(selected_regions)

# Exercise 9: Change a reference category.
programme_reference <- relevel(
  programme_factor_ordered_levels,
  ref = "Statistics"
)

levels(programme_reference)

# Exercise 10: Safe numeric conversion.
numeric_factor <- factor(
  c(
    "10",
    "20",
    "30"
  )
)

unsafe_numeric <- as.numeric(
  numeric_factor
)

safe_numeric <- as.numeric(
  as.character(
    numeric_factor
  )
)

unsafe_numeric
safe_numeric

# Exercise 11: Handle missing factor values.
response <- factor(
  c(
    "Yes",
    "No",
    NA,
    "Yes"
  ),
  levels = c(
    "No",
    "Yes"
  )
)

response
is.na(response)
summary(response)

response_explicit <- addNA(
  response,
  ifany = TRUE
)

response_explicit
levels(response_explicit)

# Exercise 12: One-way and two-way tables.
sex <- factor(
  c(
    "Female",
    "Male",
    "Female",
    "Male",
    "Female"
  )
)

programme_table <- table(
  programme_factor_ordered_levels
)

programme_sex_table <- table(
  programme_factor_ordered_levels,
  sex
)

programme_table
programme_sex_table

# Exercise 13: Row and column percentages.
overall_percentages <- prop.table(
  programme_table
) * 100

row_percentages <- prop.table(
  programme_sex_table,
  margin = 1
) * 100

column_percentages <- prop.table(
  programme_sex_table,
  margin = 2
) * 100

overall_percentages
row_percentages
column_percentages

# Exercise 14: Factor-validation function.
validate_categories <- function(
  x,
  allowed_levels
) {
  if (!is.character(x) &&
      !is.factor(x)) {
    stop(
      "`x` must be character or factor.",
      call. = FALSE
    )
  }

  raw_values <- as.character(x)

  invalid_values <- setdiff(
    unique(
      raw_values[
        !is.na(raw_values)
      ]
    ),
    allowed_levels
  )

  list(
    valid =
      length(
        invalid_values
      ) == 0L,
    invalid_values =
      invalid_values,
    missing_count =
      sum(
        is.na(raw_values)
      )
  )
}

validate_categories(
  raw_programme,
  allowed_programmes
)

# Exercise 15: Categorical-summary function.
summarise_categorical <- function(
  x,
  include_missing = TRUE
) {
  if (!is.factor(x)) {
    x <- factor(x)
  }

  counts <- table(
    x,
    useNA = if (
      include_missing
    ) {
      "ifany"
    } else {
      "no"
    }
  )

  data.frame(
    category = names(counts),
    frequency =
      as.integer(counts),
    percentage =
      as.numeric(
        counts /
          sum(counts) *
          100
      ),
    stringsAsFactors = FALSE
  )
}

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

summarise_categorical(
  student_data$programme
)

# Model-matrix demonstration.
student_data$programme_factor <- factor(
  student_data$programme,
  levels = c(
    "Statistics",
    "Procurement",
    "Hospitality",
    "Agriculture"
  )
)

model_matrix <- model.matrix(
  ~ programme_factor,
  data = student_data
)

head(model_matrix)

# Practical assessment workflow.
raw_categories <- c(
  "statistics",
  "Statistics ",
  "PROCUREMENT",
  "Hospitality",
  "Agriculture",
  NA
)

clean_categories <- trimws(
  tools::toTitleCase(
    tolower(
      raw_categories
    )
  )
)

validation_result <- validate_categories(
  clean_categories,
  allowed_programmes
)

validation_result

clean_factor <- factor(
  clean_categories,
  levels = allowed_programmes
)

clean_factor

categorical_report <- summarise_categorical(
  clean_factor,
  include_missing = TRUE
)

categorical_report

ordered_performance <- factor(
  c(
    "Poor",
    "Good",
    "Excellent",
    "Good"
  ),
  levels = c(
    "Poor",
    "Good",
    "Excellent"
  ),
  ordered = TRUE
)

ordered_performance
ordered_performance >= "Good"

# ==============================================================================
# End of Lesson 21 Solutions
# ==============================================================================

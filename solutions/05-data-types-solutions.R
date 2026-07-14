# ==============================================================================
# Learning R
# Part I: R Foundations
# Lesson 5: Atomic Data Types and Special Values
# Suggested Solutions
# ==============================================================================

# Run this file from the root of the learning-r project.


# ==============================================================================
# Exercise 1: Create one object of each principal atomic type
# ==============================================================================

logical_value <- TRUE
integer_value <- 25L
double_value <- 25.5
complex_value <- 3 + 4i
character_value <- "Statistics"
raw_value <- charToRaw("R")

typeof(logical_value)
typeof(integer_value)
typeof(double_value)
typeof(complex_value)
typeof(character_value)
typeof(raw_value)

class(logical_value)
class(integer_value)
class(double_value)
class(complex_value)
class(character_value)
class(raw_value)


# ==============================================================================
# Exercise 2: Difference between 25 and 25L
# ==============================================================================

ordinary_number <- 25
integer_number <- 25L

ordinary_number
integer_number

typeof(ordinary_number)
typeof(integer_number)

ordinary_number == integer_number

# Both values are numerically equal, but they have different internal storage
# types. The first is a double and the second is an integer.


# ==============================================================================
# Exercise 3: Identifier preserving leading zeros
# ==============================================================================

student_identifier <- "00125"

student_identifier
typeof(student_identifier)

# An identifier should be stored as character because arithmetic is not
# meaningful and leading zeros must be preserved.


# ==============================================================================
# Exercise 4: Logical-to-numeric coercion
# ==============================================================================

logical_numeric <- c(
  TRUE,
  FALSE,
  TRUE,
  5
)

logical_numeric
typeof(logical_numeric)

# TRUE becomes 1 and FALSE becomes 0 when combined with numeric values.


# ==============================================================================
# Exercise 5: Numeric-to-character coercion
# ==============================================================================

numeric_character <- c(
  10,
  20,
  "Thirty"
)

numeric_character
typeof(numeric_character)

# The entire vector becomes character because one element is character.


# ==============================================================================
# Exercise 6: Character numbers to numeric
# ==============================================================================

raw_character_values <- c(
  "10",
  "20",
  "Thirty",
  "40"
)

converted_numeric_values <- suppressWarnings(
  as.numeric(
    raw_character_values
  )
)

conversion_failed <-
  is.na(converted_numeric_values) &
  !is.na(raw_character_values)

conversion_report <- data.frame(
  raw_value = raw_character_values,
  converted_value = converted_numeric_values,
  failed = conversion_failed,
  stringsAsFactors = FALSE
)

conversion_report

# "Thirty" cannot be converted to a number, so R produces NA.


# ==============================================================================
# Exercise 7: Special values
# ==============================================================================

special_values <- c(
  10,
  NA,
  NaN,
  Inf,
  -Inf,
  25
)

special_values


# ==============================================================================
# Exercise 8: Test special values
# ==============================================================================

is.na(special_values)
is.nan(special_values)
is.infinite(special_values)
is.finite(special_values)

sum(is.na(special_values))
sum(is.nan(special_values))
sum(is.infinite(special_values))
sum(is.finite(special_values))


# ==============================================================================
# Exercise 9: NULL and NA
# ==============================================================================

missing_value <- NA
absent_object <- NULL

missing_value
absent_object

length(missing_value)
length(absent_object)

is.na(missing_value)
is.null(absent_object)

# NA is a missing value occupying a position.
# NULL ordinarily represents the absence of a value or component.


# ==============================================================================
# Exercise 10: Rainfall missingness
# ==============================================================================

rainfall_data <- read.csv(
  "data/rainfall_records.csv",
  stringsAsFactors = FALSE
)

head(rainfall_data)
str(rainfall_data)

rainfall_missing_by_column <- colSums(
  is.na(rainfall_data)
)

rainfall_missing_by_column


# ==============================================================================
# Exercise 11: Observed rainfall summaries
# ==============================================================================

rainfall_mm <- rainfall_data$rainfall_mm

observed_rainfall_count <- sum(
  !is.na(rainfall_mm)
)

missing_rainfall_count <- sum(
  is.na(rainfall_mm)
)

mean_rainfall <- mean(
  rainfall_mm,
  na.rm = TRUE
)

median_rainfall <- median(
  rainfall_mm,
  na.rm = TRUE
)

minimum_rainfall <- min(
  rainfall_mm,
  na.rm = TRUE
)

maximum_rainfall <- max(
  rainfall_mm,
  na.rm = TRUE
)

observed_rainfall_count
missing_rainfall_count
mean_rainfall
median_rainfall
minimum_rainfall
maximum_rainfall


# ==============================================================================
# Exercise 12: Percentage variable validation
# ==============================================================================

percentage_values <- c(
  45,
  72,
  81,
  100,
  0
)

percentage_is_numeric <- is.numeric(
  percentage_values
)

percentage_has_no_missing_values <- !anyNA(
  percentage_values
)

percentage_is_in_range <- all(
  percentage_values >= 0 &
    percentage_values <= 100
)

percentage_is_numeric
percentage_has_no_missing_values
percentage_is_in_range

if (!percentage_is_numeric) {
  stop(
    "`percentage_values` must be numeric.",
    call. = FALSE
  )
}

if (!percentage_has_no_missing_values) {
  stop(
    "`percentage_values` contains missing values.",
    call. = FALSE
  )
}

if (!percentage_is_in_range) {
  stop(
    "Percentage values must lie between 0 and 100.",
    call. = FALSE
  )
}


# ==============================================================================
# Practical assessment: Complete suggested workflow
# ==============================================================================

# Atomic values.
example_double <- 20.5
example_integer <- 20L
example_character <- "20"
example_logical <- TRUE
example_complex <- 2 + 3i
example_raw <- charToRaw("20")

typeof(example_double)
typeof(example_integer)
typeof(example_character)
typeof(example_logical)
typeof(example_complex)
typeof(example_raw)

# Implicit coercion.
implicit_coercion <- c(
  TRUE,
  2L,
  4.5
)

implicit_coercion
typeof(implicit_coercion)

# Explicit conversion.
character_numbers <- c(
  "12",
  "18",
  "25"
)

numeric_numbers <- as.numeric(
  character_numbers
)

numeric_numbers

# Invalid conversion.
invalid_conversion <- suppressWarnings(
  as.numeric(
    c(
      "12",
      "Missing",
      "25"
    )
  )
)

invalid_conversion

# Special values.
assessment_special_values <- list(
  missing = NA,
  undefined = NaN,
  positive_infinity = Inf,
  negative_infinity = -Inf,
  absent = NULL
)

assessment_special_values

# Import rainfall data.
assessment_rainfall <- read.csv(
  "data/rainfall_records.csv",
  stringsAsFactors = FALSE
)

str(assessment_rainfall)

vapply(
  assessment_rainfall,
  typeof,
  FUN.VALUE = character(1)
)

colSums(
  is.na(assessment_rainfall)
)

assessment_rainfall_summary <- c(
  Observed = sum(
    !is.na(
      assessment_rainfall$rainfall_mm
    )
  ),
  Missing = sum(
    is.na(
      assessment_rainfall$rainfall_mm
    )
  ),
  Mean = mean(
    assessment_rainfall$rainfall_mm,
    na.rm = TRUE
  ),
  Median = median(
    assessment_rainfall$rainfall_mm,
    na.rm = TRUE
  ),
  Minimum = min(
    assessment_rainfall$rainfall_mm,
    na.rm = TRUE
  ),
  Maximum = max(
    assessment_rainfall$rainfall_mm,
    na.rm = TRUE
  )
)

assessment_rainfall_summary

# Validate rainfall values.
rainfall_is_numeric <- is.numeric(
  assessment_rainfall$rainfall_mm
)

rainfall_nonnegative <- all(
  assessment_rainfall$rainfall_mm >= 0,
  na.rm = TRUE
)

rainfall_is_numeric
rainfall_nonnegative


# ==============================================================================
# End of Lesson 5 Solutions
# ==============================================================================

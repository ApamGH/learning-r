# ==============================================================================
# Learning R
# Part I: R Foundations
# Lesson 3: The First R Session and Systematic Command Execution
# Suggested Solutions
# ==============================================================================


# ==============================================================================
# Exercise 1: Console, Source pane and rendered output
# ==============================================================================

# Console:
# Executes commands immediately and displays results, warnings and errors.
#
# Source pane:
# Stores saved R or R Markdown code for review, correction and reproducibility.
#
# Rendered output:
# The document produced by knitting an R Markdown source file.


# ==============================================================================
# Exercise 2: Incomplete expressions
# ==============================================================================

# Example 1: Missing closing parenthesis.
#
# mean(c(10, 20, 30)
#
# Example 2: Unclosed quotation mark.
#
# institution <- "BTU
#
# Example 3: Trailing arithmetic operator.
#
# total <- 10 +
#
# Each produces the continuation prompt because R expects more input.


# ==============================================================================
# Exercise 3: Total cost
# ==============================================================================

number_of_items <- 45
unit_cost <- 18.50

total_cost <- number_of_items * unit_cost

number_of_items
unit_cost
total_cost


# ==============================================================================
# Exercise 4: Convert minutes to hours and minutes
# ==============================================================================

total_minutes <- 289

complete_hours <- total_minutes %/% 60
remaining_minutes <- total_minutes %% 60

complete_hours
remaining_minutes

# 289 minutes equals 4 complete hours and 49 minutes.


# ==============================================================================
# Exercise 5: Operator precedence
# ==============================================================================

without_parentheses <- 8 + 2 * 5
with_parentheses <- (8 + 2) * 5

without_parentheses
with_parentheses

# Multiplication occurs before addition in the first expression.
# Parentheses force addition to occur first in the second expression.


# ==============================================================================
# Exercise 6: Sequence
# ==============================================================================

sequence_by_five <- seq(
  from = 5,
  to = 50,
  by = 5
)

sequence_by_five


# ==============================================================================
# Exercise 7: Repeated pattern
# ==============================================================================

repeated_pattern <- rep(
  c("A", "B", "C"),
  each = 3
)

repeated_pattern


# ==============================================================================
# Exercise 8: Coursework and examination vectors
# ==============================================================================

coursework <- c(
  28,
  31,
  24,
  36,
  30,
  27
)

examination <- c(
  45,
  52,
  39,
  50,
  42,
  48
)

final_scores <- coursework + examination

pass_mark <- 50

pass_status <- final_scores >= pass_mark
pass_percentage <- mean(pass_status) * 100

final_scores
pass_status
pass_percentage


# ==============================================================================
# Exercise 9: Error, warning and unexpected output
# ==============================================================================

# Syntax error example:
#
# mean(c(10, 20, 30)
#
# Runtime error example:
#
# mean(object_that_does_not_exist)
#
# Warning example:
#
# 1:5 + 1:2
#
# Unexpected but valid output:
#
# 18 / 25
#
# This returns 0.72, which is a proportion. Multiply by 100 for 72 per cent.


# ==============================================================================
# Exercise 10: Rainfall data
# ==============================================================================

rainfall <- read.csv(
  "data/rainfall_records.csv",
  stringsAsFactors = FALSE
)

head(rainfall)
str(rainfall)
dim(rainfall)
names(rainfall)

number_missing_rainfall <- sum(
  is.na(rainfall$rainfall_mm)
)

observed_mean_rainfall <- mean(
  rainfall$rainfall_mm,
  na.rm = TRUE
)

number_missing_rainfall
observed_mean_rainfall


# ==============================================================================
# Exercise 11: Monthly sales script
# ==============================================================================

sales_path <- "data/monthly_sales.csv"

if (!file.exists(sales_path)) {
  stop(
    paste(
      "The sales dataset was not found:",
      sales_path
    ),
    call. = FALSE
  )
}

sales <- read.csv(
  sales_path,
  stringsAsFactors = FALSE
)

revenue_is_valid <- all(
  sales$revenue > 0
)

if (!revenue_is_valid) {
  stop(
    "Revenue must be positive.",
    call. = FALSE
  )
}

total_revenue <- sum(
  sales$revenue
)

mean_revenue <- mean(
  sales$revenue
)

above_mean_revenue <-
  sales$revenue >
  mean_revenue

percentage_above_mean <- mean(
  above_mean_revenue
) * 100

total_revenue
mean_revenue
percentage_above_mean


# ==============================================================================
# Exercise 12: Clean-session test
# ==============================================================================

# Save this file, restart R, open learning-r.Rproj and run the file from the
# first line. If an object or package is missing, add the required creation or
# namespace instruction to the script.


# ==============================================================================
# Practical assessment: Clinic analysis
# ==============================================================================

clinic <- read.csv(
  "data/clinic_visits.csv",
  stringsAsFactors = FALSE
)

head(clinic)
str(clinic)
dim(clinic)
names(clinic)

mean_waiting_time <- mean(
  clinic$waiting_time,
  na.rm = TRUE
)

median_waiting_time <- median(
  clinic$waiting_time,
  na.rm = TRUE
)

minimum_waiting_time <- min(
  clinic$waiting_time,
  na.rm = TRUE
)

maximum_waiting_time <- max(
  clinic$waiting_time,
  na.rm = TRUE
)

waiting_target <- 30

target_met <-
  clinic$waiting_time <=
  waiting_target

number_meeting_target <- sum(
  target_met,
  na.rm = TRUE
)

percentage_meeting_target <- mean(
  target_met,
  na.rm = TRUE
) * 100

mean_waiting_time
median_waiting_time
minimum_waiting_time
maximum_waiting_time
number_meeting_target
percentage_meeting_target

# Arithmetic operators.
addition_example <- 10 + 5
subtraction_example <- 10 - 5
multiplication_example <- 10 * 5
division_example <- 10 / 5
exponent_example <- 10 ^ 2
integer_division_example <- 10 %/% 3
remainder_example <- 10 %% 3

addition_example
subtraction_example
multiplication_example
division_example
exponent_example
integer_division_example
remainder_example

# Comparison and logical condition.
long_wait <- clinic$waiting_time > 30

priority_follow_up <-
  (clinic$waiting_time > 30) |
  (clinic$consultation_duration > 25)

long_wait
priority_follow_up

# Sequence and repeated pattern.
visit_sequence <- 1:nrow(clinic)

service_group <- rep(
  c("Morning", "Afternoon"),
  length.out = nrow(clinic)
)

visit_sequence
service_group

# Intentional warning example:
#
# 1:5 + 1:2
#
# Explanation:
# The longer vector length is not an exact multiple of the shorter vector
# length, so R warns about partial recycling.


# ==============================================================================
# End of Lesson 3 Solutions
# ==============================================================================

# ==============================================================================
# Learning R
# Part I: R Foundations
# Lesson 4: Objects, Assignment and Naming Conventions
# Suggested Solutions
# ==============================================================================


# ==============================================================================
# Exercise 1: Student objects
# ==============================================================================

student_name <- "Akua"
student_age <- 22L
student_programme <- "Applied Statistics"
student_final_score <- 78.5
is_registered <- TRUE

student_name
student_age
student_programme
student_final_score
is_registered


# ==============================================================================
# Exercise 2: Assignment, equality and named argument
# ==============================================================================

score <- 75

# Equality test.
score == 75

# Named argument.
round(
  x = score,
  digits = 1
)


# ==============================================================================
# Exercise 3: Case-sensitive names
# ==============================================================================

score <- 60
Score <- 70
SCORE <- 80

score
Score
SCORE

# This is poor practice because the names are visually similar and can easily
# be confused.


# ==============================================================================
# Exercise 4: Inspect a numeric vector
# ==============================================================================

scores <- c(
  65,
  72,
  81,
  59,
  90,
  76
)

scores
typeof(scores)
class(scores)
length(scores)
str(scores)
attributes(scores)


# ==============================================================================
# Exercise 5: Copy and modify
# ==============================================================================

original_scores <- scores
copied_scores <- original_scores

copied_scores[1] <- 95

original_scores
copied_scores


# ==============================================================================
# Exercise 6: Named budget vector
# ==============================================================================

department_budgets <- c(
  Statistics = 50000,
  Procurement = 62000,
  Hospitality = 58000,
  Agriculture = 71000
)

department_budgets

statistics_budget <- department_budgets[
  "Statistics"
]

statistics_budget


# ==============================================================================
# Exercise 7: Coercion
# ==============================================================================

logical_numeric <- c(
  TRUE,
  FALSE,
  5
)

logical_numeric
typeof(logical_numeric)

integer_double <- c(
  1L,
  2.5
)

integer_double
typeof(integer_double)

numeric_character <- c(
  10,
  20,
  "Thirty"
)

numeric_character
typeof(numeric_character)


# ==============================================================================
# Exercise 8: Character-to-numeric conversion
# ==============================================================================

raw_values <- c(
  "10",
  "20",
  "Thirty",
  "40"
)

converted_values <- suppressWarnings(
  as.numeric(raw_values)
)

conversion_failed <-
  is.na(converted_values) &
  !is.na(raw_values)

conversion_report <- data.frame(
  raw_value = raw_values,
  converted_value = converted_values,
  conversion_failed = conversion_failed,
  stringsAsFactors = FALSE
)

conversion_report


# ==============================================================================
# Exercise 9: Clinic objects and validation
# ==============================================================================

clinic <- read.csv(
  "data/clinic_visits.csv",
  stringsAsFactors = FALSE
)

waiting_time_minutes <- clinic$waiting_time
consultation_duration_minutes <-
  clinic$consultation_duration

waiting_is_numeric <- is.numeric(
  waiting_time_minutes
)

consultation_is_numeric <- is.numeric(
  consultation_duration_minutes
)

waiting_is_nonnegative <- all(
  waiting_time_minutes >= 0,
  na.rm = TRUE
)

consultation_is_nonnegative <- all(
  consultation_duration_minutes >= 0,
  na.rm = TRUE
)

waiting_is_numeric
consultation_is_numeric
waiting_is_nonnegative
consultation_is_nonnegative


# ==============================================================================
# Exercise 10: Procurement dependencies
# ==============================================================================

unit_price <- 125
quantity <- 40
subtotal <- unit_price * quantity
tax_rate <- 0.15
tax_amount <- subtotal * tax_rate
total_cost <- subtotal + tax_amount

unit_price
quantity
subtotal
tax_rate
tax_amount
total_cost

# Change the unit price.
unit_price <- 140

# The dependent values do not update automatically.
subtotal
tax_amount
total_cost

# Recalculate.
subtotal <- unit_price * quantity
tax_amount <- subtotal * tax_rate
total_cost <- subtotal + tax_amount

subtotal
tax_amount
total_cost


# ==============================================================================
# Exercise 11: Object registry
# ==============================================================================

object_registry <- data.frame(
  object_name = c(
    "unit_price",
    "quantity",
    "subtotal",
    "tax_rate",
    "tax_amount",
    "total_cost"
  ),
  purpose = c(
    "Price per unit",
    "Number of units",
    "Cost before tax",
    "Tax expressed as a proportion",
    "Tax charged",
    "Final cost including tax"
  ),
  unit = c(
    "Ghana cedis",
    "Items",
    "Ghana cedis",
    "Proportion",
    "Ghana cedis",
    "Ghana cedis"
  ),
  stringsAsFactors = FALSE
)

object_registry


# ==============================================================================
# Exercise 12: Student status script
# ==============================================================================

students <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

final_scores <- students$final_score

scores_are_numeric <- is.numeric(
  final_scores
)

scores_are_in_range <- all(
  final_scores >= 0 &
    final_scores <= 100
)

scores_have_no_missing_values <- !anyNA(
  final_scores
)

pass_mark <- 50
distinction_cutoff <- 80

pass_status <- final_scores >= pass_mark

distinction_status <-
  final_scores >= distinction_cutoff

pass_percentage <- mean(
  pass_status
) * 100

distinction_percentage <- mean(
  distinction_status
) * 100

scores_are_numeric
scores_are_in_range
scores_have_no_missing_values
pass_percentage
distinction_percentage


# ==============================================================================
# Practical assessment: Monthly sales objects
# ==============================================================================

raw_sales_data <- read.csv(
  "data/monthly_sales.csv",
  stringsAsFactors = FALSE
)

units_sold <- raw_sales_data$units_sold
sales_revenue <- raw_sales_data$revenue

# Inspect.
typeof(units_sold)
class(units_sold)
length(units_sold)

typeof(sales_revenue)
class(sales_revenue)
length(sales_revenue)

# Validate.
units_are_nonnegative <- all(
  units_sold >= 0
)

revenue_is_positive <- all(
  sales_revenue > 0
)

units_are_nonnegative
revenue_is_positive

# Summaries.
total_revenue <- sum(
  sales_revenue
)

mean_revenue <- mean(
  sales_revenue
)

minimum_revenue <- min(
  sales_revenue
)

maximum_revenue <- max(
  sales_revenue
)

total_revenue
mean_revenue
minimum_revenue
maximum_revenue

# Logical object.
above_average_revenue <-
  sales_revenue >
  mean_revenue

number_above_average <- sum(
  above_average_revenue
)

percentage_above_average <- mean(
  above_average_revenue
) * 100

number_above_average
percentage_above_average

# Named summary vector.
revenue_summary <- c(
  Total = total_revenue,
  Mean = mean_revenue,
  Minimum = minimum_revenue,
  Maximum = maximum_revenue,
  Percentage_above_average =
    percentage_above_average
)

revenue_summary

# Copying and updating.
copied_revenue <- sales_revenue
copied_revenue[1] <-
  copied_revenue[1] + 500

sales_revenue[1]
copied_revenue[1]

# Coercion demonstration.
mixed_vector <- c(
  sales_revenue[1],
  "Unavailable"
)

mixed_vector
typeof(mixed_vector)

# Invalid conversion.
invalid_conversion <- suppressWarnings(
  as.numeric(
    c(
      "100",
      "Unavailable",
      "250"
    )
  )
)

invalid_conversion

# Object registry.
sales_object_registry <- data.frame(
  object_name = c(
    "raw_sales_data",
    "units_sold",
    "sales_revenue",
    "above_average_revenue",
    "revenue_summary"
  ),
  purpose = c(
    "Imported monthly sales data",
    "Units-sold vector",
    "Revenue vector",
    "Logical above-average indicator",
    "Named summary vector"
  ),
  stringsAsFactors = FALSE
)

sales_object_registry


# ==============================================================================
# End of Lesson 4 Solutions
# ==============================================================================

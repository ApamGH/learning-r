# ==============================================================================
# Learning R
# Part I: R Foundations
# Lesson 1: Understanding R and Statistical Computing
# Suggested Solutions
# ==============================================================================

# This file contains suggested solutions to the independent exercises and
# practical assessment in Lesson 1.
#
# Run the file from the root of the learning-r project.
#
# Important:
# 1. Attempt every exercise before consulting the solution.
# 2. Read the comments as carefully as the code.
# 3. Restart R and rerun the file to confirm that it is self-contained.


# ==============================================================================
# Exercise 1: Temperature object
# ==============================================================================

temperature <- 31.5

temperature
typeof(temperature)
class(temperature)

# Interpretation:
# The value is stored as a double because ordinary numeric literals in R are
# usually represented internally as doubles.


# ==============================================================================
# Exercise 2: Ages vector
# ==============================================================================

ages <- c(
  18,
  21,
  20,
  23,
  19,
  22
)

length(ages)
sum(ages)
mean(ages)
min(ages)
max(ages)

# Manual verification of the mean:
#
# 18 + 21 + 20 + 23 + 19 + 22 = 123
#
# 123 / 6 = 20.5


# ==============================================================================
# Exercise 3: Arithmetic mean
# ==============================================================================

manual_total <- 12 + 18 + 25 + 30
manual_count <- 4

manual_mean <- manual_total / manual_count
manual_mean

r_mean <- mean(
  c(
    12,
    18,
    25,
    30
  )
)

r_mean

identical(
  manual_mean,
  r_mean
)

# The result is 21.25 by both methods.


# ==============================================================================
# Exercise 4: Coursework and examination marks
# ==============================================================================

coursework_mark <- 34
examination_mark <- 48

total_mark <- coursework_mark + examination_mark
pass_mark <- 50

passed <- total_mark >= pass_mark

total_mark
passed

# The student obtained 82 and therefore passed.


# ==============================================================================
# Exercise 5: Missing rainfall observation
# ==============================================================================

rainfall <- c(
  115.2,
  98.4,
  NA,
  121.7,
  110.5
)

mean(rainfall)

observed_mean_rainfall <- mean(
  rainfall,
  na.rm = TRUE
)

number_missing <- sum(
  is.na(rainfall)
)

observed_mean_rainfall
number_missing

# Interpretation:
# mean(rainfall) returns NA because one value is missing.
# na.rm = TRUE instructs R to calculate the mean using observed values only.


# ==============================================================================
# Exercise 6: Correcting code errors
# ==============================================================================

# Incorrect:
#
# student scores <- c(65, 70, 80)
# Mean(student_scores)
#
# Problems:
# 1. Object names cannot ordinarily contain spaces.
# 2. R is case-sensitive, so Mean() is not the same as mean().
# 3. The name used in the second line must match the created object.

student_scores <- c(
  65,
  70,
  80
)

mean(student_scores)


# ==============================================================================
# Exercise 7: Conceptual distinctions
# ==============================================================================

# R:
# The programming language and computational engine that evaluates commands.
#
# RStudio:
# An integrated development environment that provides tools for writing,
# running, organising and debugging R code.
#
# Console:
# The interface where commands are sent directly to R and results are printed.
#
# Script:
# A saved .R file containing executable commands and comments.
#
# R Markdown:
# A saved .Rmd document combining explanatory text, code, output, tables,
# figures and equations.


# ==============================================================================
# Guided practice: Monthly sales
# ==============================================================================

monthly_sales <- c(
  1250,
  1480,
  1325,
  1610,
  1540,
  1725
)

number_of_months <- length(
  monthly_sales
)

total_sales <- sum(
  monthly_sales
)

mean_monthly_sales <- mean(
  monthly_sales
)

minimum_monthly_sales <- min(
  monthly_sales
)

maximum_monthly_sales <- max(
  monthly_sales
)

months_above_1500 <- sum(
  monthly_sales > 1500
)

percentage_above_1500 <- mean(
  monthly_sales > 1500
) * 100

number_of_months
total_sales
mean_monthly_sales
minimum_monthly_sales
maximum_monthly_sales
months_above_1500
percentage_above_1500


# ==============================================================================
# Additional multidisciplinary practice
# ==============================================================================

# Agriculture: maize yields in kilograms.
maize_yield_kg <- c(
  1840,
  1925,
  1760,
  2050,
  1985
)

mean_maize_yield <- mean(
  maize_yield_kg
)

plots_above_1900 <- sum(
  maize_yield_kg > 1900
)

mean_maize_yield
plots_above_1900


# Procurement: delivery compliance.
delivery_days <- c(
  4,
  6,
  5,
  8,
  3,
  5
)

agreed_delivery_days <- 5

on_time_delivery <- delivery_days <= agreed_delivery_days

number_on_time <- sum(
  on_time_delivery
)

percentage_on_time <- mean(
  on_time_delivery
) * 100

number_on_time
percentage_on_time


# Health: waiting-time target.
waiting_time_minutes <- c(
  18,
  42,
  27,
  35,
  22,
  51
)

target_minutes <- 30

percentage_meeting_target <- mean(
  waiting_time_minutes <= target_minutes
) * 100

percentage_meeting_target


# Hospitality: occupancy and room revenue.
available_rooms <- 80
occupied_rooms <- 62
average_room_rate <- 420

occupancy_rate <- occupied_rooms / available_rooms
occupancy_percentage <- occupancy_rate * 100
estimated_room_revenue <- occupied_rooms * average_room_rate

occupancy_percentage
estimated_room_revenue


# ==============================================================================
# Practical assessment: Complete suggested workflow
# ==============================================================================

# Import the supplied student data.
student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

# Inspect the dataset.
head(
  student_data
)

str(
  student_data
)

dim(
  student_data
)

names(
  student_data
)

# Validate score range.
score_range_is_valid <- all(
  student_data$final_score >= 0 &
    student_data$final_score <= 100
)

score_range_is_valid

# Validate arithmetic.
score_arithmetic_is_valid <- all(
  student_data$final_score ==
    student_data$coursework_score +
    student_data$examination_score
)

score_arithmetic_is_valid

# Number of records.
number_of_students <- nrow(
  student_data
)

# Centre and extremes.
mean_final_score <- mean(
  student_data$final_score
)

median_final_score <- median(
  student_data$final_score
)

minimum_final_score <- min(
  student_data$final_score
)

maximum_final_score <- max(
  student_data$final_score
)

# Pass status.
pass_mark <- 50

pass_status <- student_data$final_score >= pass_mark

number_passed <- sum(
  pass_status
)

pass_percentage <- mean(
  pass_status
) * 100

# Distinction status.
distinction_cutoff <- 80

distinction_status <-
  student_data$final_score >= distinction_cutoff

number_with_distinction <- sum(
  distinction_status
)

distinction_percentage <- mean(
  distinction_status
) * 100

# Programme distribution.
programme_counts <- table(
  student_data$programme
)

programme_percentages <- prop.table(
  programme_counts
) * 100

# Display results.
number_of_students
mean_final_score
median_final_score
minimum_final_score
maximum_final_score
number_passed
pass_percentage
number_with_distinction
distinction_percentage
programme_counts
programme_percentages

# Example histogram.
hist(
  student_data$final_score,
  main = "Distribution of Final Scores",
  xlab = "Final score",
  ylab = "Frequency"
)

# Example programme bar chart.
barplot(
  programme_counts,
  main = "Students by Programme",
  xlab = "Programme",
  ylab = "Number of students",
  las = 2
)

# Interpretation template:
#
# The dataset contained `number_of_students` student records. The mean final
# score was `mean_final_score`, while the median was `median_final_score`.
# Using a pass mark of 50, `number_passed` students passed, corresponding to
# `pass_percentage` per cent. The data-validation checks confirmed that final
# scores were within the permitted range and matched the sum of coursework and
# examination scores.


# ==============================================================================
# End of Lesson 1 Solutions
# ==============================================================================

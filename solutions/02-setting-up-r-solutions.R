# ==============================================================================
# Learning R
# Part I: R Foundations
# Lesson 2: Setting Up and Managing the R Working Environment
# Suggested Solutions
# ==============================================================================


# ==============================================================================
# Exercise 1: Software roles
# ==============================================================================

# R:
# The language and computational engine.
#
# RStudio:
# The integrated development environment used to write, run and organise code.
#
# R package:
# A collection of functions, datasets and documentation extending R.
#
# Pandoc:
# The document converter used by R Markdown.
#
# XeLaTeX:
# A Unicode-capable LaTeX engine used to produce PDF output.


# ==============================================================================
# Exercise 2: R version and system information
# ==============================================================================

R.version.string

Sys.info()[
  c(
    "sysname",
    "release",
    "machine"
  )
]

R.version


# ==============================================================================
# Exercise 3: Working directory
# ==============================================================================

current_directory <- getwd()
current_directory

project_file_exists <- file.exists(
  "learning-r.Rproj"
)

project_file_exists

if (!project_file_exists) {
  warning(
    paste(
      "The project file was not found.",
      "Open learning-r.Rproj and run the code again."
    ),
    call. = FALSE
  )
}


# ==============================================================================
# Exercise 4: File listing
# ==============================================================================

# All project-root entries.
list.files()

# All R Markdown files.
list.files(
  pattern = "\\.Rmd$"
)

# All CSV files in data/.
list.files(
  path = "data",
  pattern = "\\.csv$",
  full.names = TRUE
)

# All project files recursively.
list.files(
  recursive = TRUE
)


# ==============================================================================
# Exercise 5: Dataset validation
# ==============================================================================

dataset_paths <- c(
  "data/student_scores.csv",
  "data/monthly_sales.csv",
  "data/rainfall_records.csv",
  "data/clinic_visits.csv"
)

dataset_status <- file.exists(
  dataset_paths
)

names(dataset_status) <- dataset_paths

dataset_status

all_datasets_exist <- all(
  dataset_status
)

all_datasets_exist

missing_datasets <- dataset_paths[
  !dataset_status
]

missing_datasets


# ==============================================================================
# Exercise 6: File information
# ==============================================================================

student_file_info <- file.info(
  "data/student_scores.csv"
)

student_file_info

student_file_info$size
student_file_info$isdir
student_file_info$mtime


# ==============================================================================
# Exercise 7: Directory creation
# ==============================================================================

output_root <- "student_output"

figure_directory <- file.path(
  output_root,
  "figures"
)

table_directory <- file.path(
  output_root,
  "tables"
)

if (!dir.exists(figure_directory)) {
  dir.create(
    figure_directory,
    recursive = TRUE
  )
}

if (!dir.exists(table_directory)) {
  dir.create(
    table_directory,
    recursive = TRUE
  )
}

dir.exists(output_root)
dir.exists(figure_directory)
dir.exists(table_directory)


# ==============================================================================
# Exercise 8: Exporting a table
# ==============================================================================

students <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

student_summary <- data.frame(
  statistic = c(
    "Number of students",
    "Mean final score",
    "Median final score",
    "Minimum final score",
    "Maximum final score"
  ),
  value = c(
    nrow(students),
    mean(students$final_score),
    median(students$final_score),
    min(students$final_score),
    max(students$final_score)
  ),
  stringsAsFactors = FALSE
)

student_summary

summary_path <- file.path(
  table_directory,
  "student_summary.csv"
)

write.csv(
  student_summary,
  summary_path,
  row.names = FALSE
)

file.exists(summary_path)

verified_summary <- read.csv(
  summary_path,
  stringsAsFactors = FALSE
)

verified_summary


# ==============================================================================
# Exercise 9: Package audit
# ==============================================================================

packages_to_check <- c(
  "rmarkdown",
  "knitr",
  "tinytex"
)

package_status <- vapply(
  packages_to_check,
  requireNamespace,
  quietly = TRUE,
  FUN.VALUE = logical(1)
)

package_status

missing_packages <- packages_to_check[
  !package_status
]

missing_packages


# ==============================================================================
# Exercise 10: Session information
# ==============================================================================

sessionInfo()


# ==============================================================================
# Exercise 11: Complete script solution
# ==============================================================================

student_path <- "data/student_scores.csv"

if (!file.exists(student_path)) {
  stop(
    paste(
      "The required student dataset was not found:",
      student_path
    ),
    call. = FALSE
  )
}

student_data <- read.csv(
  student_path,
  stringsAsFactors = FALSE
)

number_of_students <- nrow(
  student_data
)

mean_final_score <- mean(
  student_data$final_score
)

programme_distribution <- table(
  student_data$programme
)

number_of_students
mean_final_score
programme_distribution


# ==============================================================================
# Exercise 12: R Markdown YAML example
# ==============================================================================

# Copy the following into a new .Rmd file:
#
# ---
# title: "Monthly Sales Summary"
# author: "Student Name"
# date: "`r format(Sys.Date(), '%d %B %Y')`"
# output:
#   html_document:
#     toc: true
#     number_sections: true
#   word_document:
#     toc: true
#     number_sections: true
#   pdf_document:
#     toc: true
#     number_sections: true
#     latex_engine: xelatex
# ---


# ==============================================================================
# Exercise 13: Hidden state
# ==============================================================================

# Incomplete code:
#
# mean(scores)
#
# Correct self-contained code:

scores <- c(
  65,
  72,
  80
)

mean(scores)


# ==============================================================================
# Exercise 14: Cleanup
# ==============================================================================

file.exists(summary_path)

final_check <- read.csv(
  summary_path,
  stringsAsFactors = FALSE
)

final_check

unlink(
  output_root,
  recursive = TRUE,
  force = TRUE
)

dir.exists(output_root)


# ==============================================================================
# Practical assessment: Complete suggested solution
# ==============================================================================

# Working directory.
getwd()

# Project file.
project_status <- file.exists(
  "learning-r.Rproj"
)

project_status

# Dataset audit.
assessment_dataset_paths <- c(
  "data/student_scores.csv",
  "data/monthly_sales.csv",
  "data/rainfall_records.csv",
  "data/clinic_visits.csv"
)

assessment_dataset_status <- file.exists(
  assessment_dataset_paths
)

names(assessment_dataset_status) <-
  assessment_dataset_paths

assessment_dataset_status

# Package audit.
assessment_packages <- c(
  "rmarkdown",
  "knitr",
  "tinytex"
)

assessment_package_status <- vapply(
  assessment_packages,
  requireNamespace,
  quietly = TRUE,
  FUN.VALUE = logical(1)
)

assessment_package_status

# R version.
R.version.string

# Pandoc.
rmarkdown::pandoc_version()

# XeLaTeX.
xelatex_path <- Sys.which(
  "xelatex"
)

xelatex_available <- nzchar(
  xelatex_path
)

xelatex_path
xelatex_available

# Import sales data.
sales_path <- "data/monthly_sales.csv"

if (!file.exists(sales_path)) {
  stop(
    paste(
      "The monthly sales file was not found:",
      sales_path
    ),
    call. = FALSE
  )
}

sales <- read.csv(
  sales_path,
  stringsAsFactors = FALSE
)

head(sales)
str(sales)
dim(sales)
names(sales)

# Validate.
units_are_valid <- all(
  sales$units_sold >= 0
)

revenue_is_valid <- all(
  sales$revenue > 0
)

units_are_valid
revenue_is_valid

# Summary.
sales_summary <- data.frame(
  statistic = c(
    "Number of records",
    "Total units sold",
    "Mean units sold",
    "Minimum units sold",
    "Maximum units sold",
    "Total revenue",
    "Mean revenue",
    "Minimum revenue",
    "Maximum revenue"
  ),
  value = c(
    nrow(sales),
    sum(sales$units_sold),
    mean(sales$units_sold),
    min(sales$units_sold),
    max(sales$units_sold),
    sum(sales$revenue),
    mean(sales$revenue),
    min(sales$revenue),
    max(sales$revenue)
  ),
  stringsAsFactors = FALSE
)

sales_summary

# Export.
assessment_output_directory <- "lesson_2_output"

if (!dir.exists(assessment_output_directory)) {
  dir.create(
    assessment_output_directory
  )
}

assessment_output_path <- file.path(
  assessment_output_directory,
  "monthly_sales_summary.csv"
)

write.csv(
  sales_summary,
  assessment_output_path,
  row.names = FALSE
)

file.exists(assessment_output_path)

verified_output <- read.csv(
  assessment_output_path,
  stringsAsFactors = FALSE
)

verified_output

# Cleanup.
unlink(
  assessment_output_directory,
  recursive = TRUE,
  force = TRUE
)

dir.exists(assessment_output_directory)


# ==============================================================================
# End of Lesson 2 Solutions
# ==============================================================================

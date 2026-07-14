# ==============================================================================
# Learning R
# Part III: Importing, Cleaning and Transforming Data
# Lesson 15: Importing CSV, Text and Delimited Files
# Suggested Solutions
# ==============================================================================

student_path <- "data/student_scores.csv"

if (!file.exists(student_path)) {
  stop(
    paste(
      "Required file not found:",
      student_path
    ),
    call. = FALSE
  )
}

# Exercise 1: Import with read.csv().
student_data <- read.csv(
  student_path,
  stringsAsFactors = FALSE
)

head(student_data)
str(student_data)
dim(student_data)
names(student_data)

# Exercise 2: Re-import with read.table().
student_data_table <- read.table(
  file = student_path,
  header = TRUE,
  sep = ",",
  stringsAsFactors = FALSE
)

head(student_data_table)

# Exercise 3: Compare imported objects.
identical(
  student_data,
  student_data_table
)

# Exercise 4: Import only the first five rows.
student_preview <- read.csv(
  student_path,
  nrows = 5,
  stringsAsFactors = FALSE
)

student_preview

# Exercise 5: Inspect classes and missingness.
column_classes <- vapply(
  student_data,
  class,
  FUN.VALUE = character(1)
)

missing_counts <- colSums(
  is.na(student_data)
)

column_classes
missing_counts

# Exercise 6: Validate required variables and identifiers.
required_columns <- c(
  "student_id",
  "programme",
  "coursework_score",
  "examination_score",
  "final_score"
)

required_columns_exist <- all(
  required_columns %in%
    names(student_data)
)

identifiers_are_unique <- !anyDuplicated(
  student_data$student_id
)

required_columns_exist
identifiers_are_unique

# Exercise 7: Demonstrate custom missing-value codes.
temporary_missing_file <- tempfile(
  fileext = ".csv"
)

writeLines(
  c(
    "id,score",
    "A,65",
    "B,Missing",
    "C,N/A",
    "D,72"
  ),
  con = temporary_missing_file
)

custom_missing_data <- read.csv(
  temporary_missing_file,
  na.strings = c(
    "",
    "NA",
    "N/A",
    "Missing"
  ),
  stringsAsFactors = FALSE
)

custom_missing_data
colSums(is.na(custom_missing_data))

unlink(temporary_missing_file)

# Exercise 8: Template code for other delimiters.
tab_template <- '
tab_data <- read.delim(
  "data/example_data.tsv",
  stringsAsFactors = FALSE
)
'

semicolon_template <- '
semicolon_data <- read.table(
  "data/example_semicolon.txt",
  header = TRUE,
  sep = ";",
  stringsAsFactors = FALSE
)
'

pipe_template <- '
pipe_data <- read.table(
  "data/example_pipe.txt",
  header = TRUE,
  sep = "|",
  stringsAsFactors = FALSE
)
'

cat(tab_template)
cat(semicolon_template)
cat(pipe_template)

# Exercise 9: Safe character-to-numeric conversion.
raw_values <- c(
  "10",
  "20",
  "Thirty",
  "40"
)

converted_values <- suppressWarnings(
  as.numeric(raw_values)
)

conversion_report <- data.frame(
  raw = raw_values,
  converted = converted_values,
  failed =
    is.na(converted_values) &
    !is.na(raw_values),
  stringsAsFactors = FALSE
)

conversion_report

# Exercise 10: Clean non-standard variable names.
raw_names <- c(
  "Student ID",
  "Final Score",
  "Programme Name",
  "Final Score"
)

clean_names_base <- function(
  names_vector
) {
  cleaned <- tolower(
    names_vector
  )

  cleaned <- gsub(
    "[^a-z0-9]+",
    "_",
    cleaned
  )

  cleaned <- gsub(
    "^_|_$",
    "",
    cleaned
  )

  make.unique(cleaned)
}

clean_names_base(raw_names)

# Exercise 11: Export and verify a CSV copy.
output_directory <- "lesson_15_solution_output"

if (!dir.exists(output_directory)) {
  dir.create(output_directory)
}

output_path <- file.path(
  output_directory,
  "student_scores_copy.csv"
)

write.csv(
  student_data,
  output_path,
  row.names = FALSE
)

verified_export <- read.csv(
  output_path,
  stringsAsFactors = FALSE
)

file.exists(output_path)
identical(student_data, verified_export)

unlink(
  output_directory,
  recursive = TRUE,
  force = TRUE
)

# Exercise 12: Import and validate rainfall data.
rainfall_data <- read.csv(
  "data/rainfall_records.csv",
  na.strings = c(
    "",
    "NA",
    "N/A",
    "."
  ),
  stringsAsFactors = FALSE
)

head(rainfall_data)
str(rainfall_data)
colSums(is.na(rainfall_data))

rainfall_is_numeric <- is.numeric(
  rainfall_data$rainfall_mm
)

rainfall_is_nonnegative <- all(
  rainfall_data$rainfall_mm >= 0,
  na.rm = TRUE
)

rainfall_is_numeric
rainfall_is_nonnegative

# Exercise 13: Compare base R and readr code.
readr_available <- requireNamespace(
  "readr",
  quietly = TRUE
)

readr_available

if (readr_available) {
  student_readr <- readr::read_csv(
    student_path,
    show_col_types = FALSE
  )

  print(student_readr)
  print(readr::problems(student_readr))
}

# Exercise 14: Common import failures.
common_import_failures <- data.frame(
  problem = c(
    "Wrong working directory",
    "Wrong delimiter",
    "Wrong header setting",
    "Unexpected column types",
    "Encoding problems"
  ),
  diagnostic = c(
    "Check getwd() and file.exists().",
    "Inspect the first lines and specify sep correctly.",
    "Verify whether the first row contains variable names.",
    "Inspect str() and conversion failures.",
    "Specify the correct fileEncoding."
  ),
  stringsAsFactors = FALSE
)

common_import_failures

# Practical assessment workflow.
assessment_path <- "data/monthly_sales.csv"

if (!file.exists(assessment_path)) {
  stop(
    paste(
      "Required file not found:",
      assessment_path
    ),
    call. = FALSE
  )
}

sales_data <- read.csv(
  assessment_path,
  stringsAsFactors = FALSE
)

sales_profile <- data.frame(
  variable = names(sales_data),
  class = vapply(
    sales_data,
    class,
    FUN.VALUE = character(1)
  ),
  missing = vapply(
    sales_data,
    function(x) {
      sum(is.na(x))
    },
    FUN.VALUE = integer(1)
  ),
  stringsAsFactors = FALSE
)

sales_profile

sales_validation <- list(
  rows = nrow(sales_data),
  columns = ncol(sales_data),
  units_nonnegative = all(
    sales_data$units_sold >= 0,
    na.rm = TRUE
  ),
  revenue_positive = all(
    sales_data$revenue > 0,
    na.rm = TRUE
  )
)

sales_validation

assessment_output <- tempfile(
  fileext = ".csv"
)

write.csv(
  sales_data,
  assessment_output,
  row.names = FALSE
)

verified_sales <- read.csv(
  assessment_output,
  stringsAsFactors = FALSE
)

identical(
  sales_data,
  verified_sales
)

unlink(assessment_output)

# ==============================================================================
# End of Lesson 15 Solutions
# ==============================================================================

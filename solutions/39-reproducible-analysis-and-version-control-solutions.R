# ==============================================================================
# Learning R
# Part VI: Regression Modelling and Reproducible Statistical Workflows
# Lesson 39: Reproducible Statistical Analysis with R Markdown, Projects and
# Version Control
# Suggested Solutions
# ==============================================================================

set.seed(20260714)

# Exercise 1: Create a project folder structure.
project_root <- "lesson_39_reproducible_project"

required_directories <- c(
  file.path(
    project_root,
    "data",
    "raw"
  ),
  file.path(
    project_root,
    "data",
    "processed"
  ),
  file.path(
    project_root,
    "R"
  ),
  file.path(
    project_root,
    "scripts"
  ),
  file.path(
    project_root,
    "reports"
  ),
  file.path(
    project_root,
    "outputs",
    "figures"
  ),
  file.path(
    project_root,
    "outputs",
    "tables"
  )
)

for (directory in required_directories) {
  if (!dir.exists(directory)) {
    dir.create(
      directory,
      recursive = TRUE
    )
  }
}

vapply(
  required_directories,
  dir.exists,
  FUN.VALUE = logical(1)
)

# Exercise 2: Use relative paths.
source_data_path <- file.path(
  "data",
  "student_scores.csv"
)

file.exists(
  source_data_path
)

# Exercise 3: Import data from the project root.
student_data <- read.csv(
  source_data_path,
  stringsAsFactors = FALSE
)

head(student_data)
str(student_data)

# Exercise 4: Preserve raw data.
raw_copy_path <- file.path(
  project_root,
  "data",
  "raw",
  "student_scores.csv"
)

file.copy(
  source_data_path,
  raw_copy_path,
  overwrite = TRUE
)

raw_student_data <- read.csv(
  raw_copy_path,
  stringsAsFactors = FALSE
)

identical(
  student_data,
  raw_student_data
)

# Exercise 5: Document transformations.
processed_student_data <- raw_student_data

processed_student_data$programme <- trimws(
  processed_student_data$programme
)

processed_student_data$passed <-
  processed_student_data$final_score >= 50

transformation_log <- data.frame(
  step = c(
    1,
    2
  ),
  variable = c(
    "programme",
    "passed"
  ),
  action = c(
    "Trim surrounding whitespace",
    "Create pass indicator"
  ),
  rule = c(
    "trimws(programme)",
    "final_score >= 50"
  ),
  stringsAsFactors = FALSE
)

transformation_log

transformation_log_path <- file.path(
  project_root,
  "data",
  "processed",
  "transformation_log.csv"
)

write.csv(
  transformation_log,
  transformation_log_path,
  row.names = FALSE
)

# Exercise 6: Export processed data.
processed_data_path <- file.path(
  project_root,
  "data",
  "processed",
  "student_scores_processed.csv"
)

write.csv(
  processed_student_data,
  processed_data_path,
  row.names = FALSE
)

file.exists(
  processed_data_path
)

# Exercise 7: Create reusable functions.
summarise_numeric <- function(
  x
) {
  if (!is.numeric(x)) {
    stop(
      "`x` must be numeric.",
      call. = FALSE
    )
  }

  observed <- x[
    !is.na(x)
  ]

  if (length(observed) == 0L) {
    stop(
      "No observed values are available.",
      call. = FALSE
    )
  }

  data.frame(
    n = length(observed),
    missing = sum(
      is.na(x)
    ),
    mean = mean(observed),
    median = median(observed),
    standard_deviation = sd(observed),
    minimum = min(observed),
    maximum = max(observed)
  )
}

summary_function_path <- file.path(
  project_root,
  "R",
  "summary_functions.R"
)

summary_function_lines <- c(
  "summarise_numeric <- function(x) {",
  "  if (!is.numeric(x)) {",
  "    stop('`x` must be numeric.', call. = FALSE)",
  "  }",
  "",
  "  observed <- x[!is.na(x)]",
  "",
  "  data.frame(",
  "    n = length(observed),",
  "    missing = sum(is.na(x)),",
  "    mean = mean(observed),",
  "    median = median(observed),",
  "    standard_deviation = sd(observed),",
  "    minimum = min(observed),",
  "    maximum = max(observed)",
  "  )",
  "}"
)

writeLines(
  summary_function_lines,
  summary_function_path
)

file.exists(
  summary_function_path
)

summarise_numeric(
  processed_student_data$final_score
)

# Exercise 8: Structure an R Markdown report.
report_path <- file.path(
  project_root,
  "reports",
  "student_performance_report.Rmd"
)

report_lines <- c(
  "---",
  'title: "Student Performance Report"',
  'author: "Dr. Apam"',
  'date: "`r format(Sys.Date(), \'%d %B %Y\')`"',
  "output:",
  "  html_document:",
  "    toc: true",
  "    number_sections: true",
  "  word_document:",
  "    toc: true",
  "lang: en-GB",
  "---",
  "",
  "```{r setup, include=FALSE}",
  "knitr::opts_chunk$set(",
  "  echo = TRUE,",
  "  message = FALSE,",
  "  warning = FALSE",
  ")",
  "```",
  "",
  "# Research question",
  "",
  "What is the distribution of final student scores?",
  "",
  "# Data import",
  "",
  "```{r import-data}",
  "student_data <- read.csv(",
  '  file.path("..", "data", "processed", "student_scores_processed.csv"),',
  "  stringsAsFactors = FALSE",
  ")",
  "```",
  "",
  "# Descriptive analysis",
  "",
  "```{r descriptive-analysis}",
  "summary(student_data$final_score)",
  "```",
  "",
  "# Conclusion",
  "",
  "The report is generated directly from code and data."
)

writeLines(
  report_lines,
  report_path
)

file.exists(
  report_path
)

# Exercise 9: Use inline R code.
inline_example <- paste(
  "The mean score was",
  round(
    mean(
      processed_student_data$final_score,
      na.rm = TRUE
    ),
    2
  ),
  "."
)

inline_example

# Exercise 10: Create a parameterised analysis.
selected_programme <- "Statistics"

selected_data <- processed_student_data[
  processed_student_data$programme ==
    selected_programme,
]

parameterised_summary <- summarise_numeric(
  selected_data$final_score
)

parameterised_summary$programme <-
  selected_programme

parameterised_summary

# Exercise 11: Record package versions.
required_packages <- c(
  "knitr",
  "rmarkdown",
  "ggplot2"
)

dependency_status <- data.frame(
  package =
    required_packages,
  installed = vapply(
    required_packages,
    requireNamespace,
    FUN.VALUE = logical(1),
    quietly = TRUE
  ),
  version = vapply(
    required_packages,
    function(package_name) {
      if (requireNamespace(
        package_name,
        quietly = TRUE
      )) {
        as.character(
          packageVersion(
            package_name
          )
        )
      } else {
        NA_character_
      }
    },
    FUN.VALUE = character(1)
  ),
  stringsAsFactors = FALSE
)

dependency_status

dependency_path <- file.path(
  project_root,
  "package_dependencies.csv"
)

write.csv(
  dependency_status,
  dependency_path,
  row.names = FALSE
)

# Exercise 12: Record session information.
session_information <- capture.output(
  sessionInfo()
)

session_info_path <- file.path(
  project_root,
  "session-info.txt"
)

writeLines(
  session_information,
  session_info_path
)

file.exists(
  session_info_path
)

# Exercise 13: Set and verify random seeds.
set.seed(20260714)

first_draw <- rnorm(10)

set.seed(20260714)

second_draw <- rnorm(10)

seed_reproducible <- identical(
  first_draw,
  second_draw
)

seed_reproducible

# Exercise 14: Write a README.
readme_path <- file.path(
  project_root,
  "README.md"
)

readme_lines <- c(
  "# Student Performance Analysis",
  "",
  "## Purpose",
  "",
  "This project demonstrates a reproducible analysis of student performance.",
  "",
  "## Main input",
  "",
  "- `data/raw/student_scores.csv`",
  "",
  "## Processed data",
  "",
  "- `data/processed/student_scores_processed.csv`",
  "",
  "## Execution order",
  "",
  "1. Import the raw data.",
  "2. Clean and validate the data.",
  "3. Produce descriptive and inferential analyses.",
  "4. Render the report.",
  "",
  "## Main outputs",
  "",
  "- `outputs/tables/`",
  "- `outputs/figures/`",
  "",
  "## Reproducibility",
  "",
  "Run the project from the RStudio project root and use relative paths."
)

writeLines(
  readme_lines,
  readme_path
)

file.exists(
  readme_path
)

# Exercise 15: Write a .gitignore.
gitignore_path <- file.path(
  project_root,
  ".gitignore"
)

gitignore_lines <- c(
  ".Rhistory",
  ".RData",
  ".Ruserdata",
  ".Rproj.user/",
  "*.html",
  "*.pdf",
  "outputs/temp/",
  ".Renviron"
)

writeLines(
  gitignore_lines,
  gitignore_path
)

readLines(
  gitignore_path
)

# Exercise 16: Create a reproducibility audit.
audit_project_paths <- function(
  required_paths
) {
  data.frame(
    path = required_paths,
    exists = file.exists(
      required_paths
    ) |
      dir.exists(
        required_paths
      ),
    type = vapply(
      required_paths,
      function(path) {
        if (dir.exists(path)) {
          "directory"
        } else if (file.exists(path)) {
          "file"
        } else {
          "missing"
        }
      },
      FUN.VALUE = character(1)
    ),
    stringsAsFactors = FALSE
  )
}

audit_paths <- c(
  project_root,
  file.path(
    project_root,
    "data",
    "raw"
  ),
  file.path(
    project_root,
    "data",
    "processed"
  ),
  file.path(
    project_root,
    "R"
  ),
  file.path(
    project_root,
    "reports"
  ),
  file.path(
    project_root,
    "outputs"
  ),
  readme_path,
  gitignore_path,
  session_info_path,
  dependency_path,
  report_path
)

project_audit <- audit_project_paths(
  audit_paths
)

project_audit

stopifnot(
  all(
    project_audit$exists
  )
)

# Reproducibility checklist.
reproducibility_checklist <- data.frame(
  item = c(
    "Relative paths used",
    "Raw data preserved",
    "Processed data separated",
    "Transformations documented",
    "Reusable functions stored",
    "Random seed recorded",
    "Package versions recorded",
    "Session information recorded",
    "README available",
    ".gitignore available",
    "Outputs created by code"
  ),
  completed = c(
    TRUE,
    file.exists(
      raw_copy_path
    ),
    file.exists(
      processed_data_path
    ),
    file.exists(
      transformation_log_path
    ),
    file.exists(
      summary_function_path
    ),
    seed_reproducible,
    file.exists(
      dependency_path
    ),
    file.exists(
      session_info_path
    ),
    file.exists(
      readme_path
    ),
    file.exists(
      gitignore_path
    ),
    TRUE
  ),
  stringsAsFactors = FALSE
)

reproducibility_checklist

# Suggested Git commands.
git_workflow <- c(
  "git init",
  "git status",
  "git add .",
  'git commit -m "Create reproducible student analysis project"',
  "git branch -M main",
  "git remote add origin <repository-url>",
  "git push -u origin main"
)

git_workflow

# Suggested commit-message examples.
commit_messages <- c(
  "Add raw student dataset",
  "Create data-cleaning workflow",
  "Add descriptive analysis functions",
  "Create reproducible R Markdown report",
  "Document package dependencies",
  "Update README execution instructions"
)

commit_messages

# Practical assessment output.
lesson_39_assessment <- list(
  project_audit =
    project_audit,
  reproducibility_checklist =
    reproducibility_checklist,
  package_dependencies =
    dependency_status,
  parameterised_summary =
    parameterised_summary,
  suggested_git_workflow =
    git_workflow
)

lesson_39_assessment

# Remove the demonstration project after verification.
unlink(
  project_root,
  recursive = TRUE,
  force = TRUE
)

# ==============================================================================
# End of Lesson 39 Solutions
# ==============================================================================

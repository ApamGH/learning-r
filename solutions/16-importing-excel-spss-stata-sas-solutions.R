# ==============================================================================
# Learning R
# Part III: Importing, Cleaning and Transforming Data
# Lesson 16: Importing Excel, SPSS, Stata and SAS Files
# Suggested Solutions
# ==============================================================================

# Exercise 1: Audit package availability.
packages <- c(
  "readxl",
  "haven",
  "writexl",
  "openxlsx"
)

package_status <- vapply(
  packages,
  requireNamespace,
  quietly = TRUE,
  FUN.VALUE = logical(1)
)

package_status

# Exercise 2: Template to list Excel sheets.
excel_sheet_template <- '
readxl::excel_sheets(
  "data/example_workbook.xlsx"
)
'

cat(excel_sheet_template)

# Exercise 3: Template to import a named sheet.
excel_named_sheet_template <- '
student_data <- readxl::read_excel(
  "data/example_workbook.xlsx",
  sheet = "Students"
)
'

cat(excel_named_sheet_template)

# Exercise 4: Skip rows and select a range.
excel_skip_template <- '
student_data <- readxl::read_excel(
  "data/example_workbook.xlsx",
  sheet = "Students",
  skip = 3
)
'

excel_range_template <- '
student_data <- readxl::read_excel(
  "data/example_workbook.xlsx",
  sheet = "Students",
  range = "A4:F100"
)
'

cat(excel_skip_template)
cat(excel_range_template)

# Exercise 5: Missing values and explicit column types.
excel_typed_template <- '
student_data <- readxl::read_excel(
  "data/example_workbook.xlsx",
  na = c("", "NA", "N/A", "."),
  col_types = c(
    "text",
    "text",
    "numeric",
    "numeric",
    "numeric"
  )
)
'

cat(excel_typed_template)

# Exercise 6: SPSS, Stata and SAS templates.
spss_template <- '
spss_data <- haven::read_sav(
  "data/survey_data.sav"
)
'

stata_template <- '
stata_data <- haven::read_dta(
  "data/survey_data.dta"
)
'

sas_template <- '
sas_data <- haven::read_sas(
  "data/survey_data.sas7bdat"
)
'

xpt_template <- '
sas_transport <- haven::read_xpt(
  "data/survey_data.xpt"
)
'

cat(spss_template)
cat(stata_template)
cat(sas_template)
cat(xpt_template)

# Exercise 7: Create a labelled variable.
if (requireNamespace(
  "haven",
  quietly = TRUE
)) {
  labelled_status <- haven::labelled(
    c(
      1,
      2,
      1,
      9
    ),
    labels = c(
      Yes = 1,
      No = 2,
      Missing = 9
    )
  )

  print(labelled_status)
  print(attributes(labelled_status))
}

# Exercise 8: Convert labelled values to factors.
if (requireNamespace(
  "haven",
  quietly = TRUE
)) {
  status_factor <- haven::as_factor(
    labelled_status
  )

  print(status_factor)
  print(levels(status_factor))
}

# Exercise 9: Tagged missing values.
if (requireNamespace(
  "haven",
  quietly = TRUE
)) {
  tagged_values <- c(
    10,
    haven::tagged_na("a"),
    20,
    haven::tagged_na("b")
  )

  print(tagged_values)
  print(
    haven::is_tagged_na(
      tagged_values
    )
  )
}

# Exercise 10: Metadata-inspection function.
inspect_metadata <- function(
  data
) {
  data.frame(
    variable = names(data),
    class = vapply(
      data,
      function(x) {
        paste(
          class(x),
          collapse = ", "
        )
      },
      FUN.VALUE = character(1)
    ),
    type = vapply(
      data,
      typeof,
      FUN.VALUE = character(1)
    ),
    missing = vapply(
      data,
      function(x) {
        sum(is.na(x))
      },
      FUN.VALUE = integer(1)
    ),
    label = vapply(
      data,
      function(x) {
        variable_label <- attr(
          x,
          "label"
        )

        if (is.null(variable_label)) {
          ""
        } else {
          as.character(variable_label)
        }
      },
      FUN.VALUE = character(1)
    ),
    stringsAsFactors = FALSE
  )
}

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

inspect_metadata(student_data)

# Exercise 11: Cross-software problems.
cross_software_problems <- data.frame(
  problem = c(
    "Identifiers imported as numbers",
    "Dates imported as text or serial numbers",
    "Labelled categories treated as continuous",
    "Missing codes retained as valid values",
    "Metadata lost during export"
  ),
  consequence = c(
    "Leading zeros may disappear.",
    "Date arithmetic becomes invalid.",
    "Model interpretation may be wrong.",
    "Summaries may be biased.",
    "Labels and definitions may be unavailable."
  ),
  stringsAsFactors = FALSE
)

cross_software_problems

# Exercise 12: Validation template.
validate_imported_data <- function(
  data,
  required_variables = character(0),
  id_variable = NULL
) {
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame.",
      call. = FALSE
    )
  }

  missing_variables <- setdiff(
    required_variables,
    names(data)
  )

  report <- list(
    rows = nrow(data),
    columns = ncol(data),
    missing_required_variables =
      missing_variables,
    metadata = inspect_metadata(data)
  )

  if (!is.null(id_variable) &&
      id_variable %in%
      names(data)) {
    report$missing_identifiers <- sum(
      is.na(data[[id_variable]]) |
        data[[id_variable]] == ""
    )

    report$duplicate_identifiers <- sum(
      duplicated(
        data[[id_variable]]
      )
    )
  }

  report
}

validate_imported_data(
  student_data,
  required_variables = c(
    "student_id",
    "programme",
    "final_score"
  ),
  id_variable = "student_id"
)

# Exercise 13: Export templates.
excel_export_template <- '
writexl::write_xlsx(
  list(
    Students = student_data,
    Summary = summary_table
  ),
  path = "output/student_analysis.xlsx"
)
'

stata_export_template <- '
haven::write_dta(
  student_data,
  "output/student_data.dta"
)
'

spss_export_template <- '
haven::write_sav(
  student_data,
  "output/student_data.sav"
)
'

xpt_export_template <- '
haven::write_xpt(
  student_data,
  "output/student_data.xpt"
)
'

cat(excel_export_template)
cat(stata_export_template)
cat(spss_export_template)
cat(xpt_export_template)

# Exercise 14: Package versions.
available_packages <- names(
  package_status
)[
  package_status
]

if (length(available_packages) > 0L) {
  package_versions <- vapply(
    available_packages,
    function(package_name) {
      as.character(
        packageVersion(
          package_name
        )
      )
    },
    FUN.VALUE = character(1)
  )

  print(package_versions)
}

# Practical assessment: A complete proprietary-import plan.
proprietary_import_plan <- data.frame(
  format = c(
    "Excel",
    "SPSS",
    "Stata",
    "SAS"
  ),
  package = c(
    "readxl",
    "haven",
    "haven",
    "haven"
  ),
  import_function = c(
    "read_excel",
    "read_sav",
    "read_dta",
    "read_sas"
  ),
  validation_focus = c(
    "Sheets, title rows, column types, dates",
    "Variable labels, value labels, user missing values",
    "Value labels, tagged missing values, variable types",
    "Formats, tagged missing values, date conversion"
  ),
  stringsAsFactors = FALSE
)

proprietary_import_plan

raw_import_workflow <- c(
  "Confirm file and package availability",
  "Preserve raw imported object",
  "Inspect dimensions and names",
  "Inspect classes and labels",
  "Check missing-value metadata",
  "Validate identifiers and ranges",
  "Create analysis copy",
  "Document conversions"
)

raw_import_workflow

# ==============================================================================
# End of Lesson 16 Solutions
# ==============================================================================

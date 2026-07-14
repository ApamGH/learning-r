# ==============================================================================
# Learning R
# Part III: Importing, Cleaning and Transforming Data
# Lesson 19: Character Data, Text Cleaning and Regular Expressions
# Suggested Solutions
# ==============================================================================

# Exercise 1: Inspect character vectors.
programmes <- c(
  "Statistics",
  "Procurement",
  "Hospitality",
  "Agriculture"
)

programmes
typeof(programmes)
class(programmes)
nchar(programmes)

# Exercise 2: Change case.
tolower(programmes)
toupper(programmes)

tools::toTitleCase(
  tolower(
    c(
      "applied statistics",
      "PROCUREMENT MANAGEMENT"
    )
  )
)

# Exercise 3: Trim and normalise whitespace.
messy_text <- c(
  " Statistics",
  "Procurement ",
  "Bolgatanga   Technical University"
)

trimmed_text <- trimws(
  messy_text
)

normalised_text <- gsub(
  "[[:space:]]+",
  " ",
  trimmed_text
)

normalised_text

# Exercise 4: Combine text.
paste(
  "Student",
  1:5
)

paste(
  "Student",
  1:5,
  sep = "_"
)

paste0(
  "S",
  sprintf(
    "%04d",
    1:5
  )
)

paste(
  programmes,
  collapse = ", "
)

# Exercise 5: Split full names.
full_names <- c(
  "Ama Mensah",
  "Kojo Asante",
  "Esi Ayisi"
)

name_parts <- strsplit(
  full_names,
  split = " "
)

first_names <- vapply(
  name_parts,
  `[`,
  FUN.VALUE = character(1),
  1
)

surnames <- vapply(
  name_parts,
  function(x) {
    paste(
      x[-1],
      collapse = " "
    )
  },
  FUN.VALUE = character(1)
)

data.frame(
  full_name = full_names,
  first_name = first_names,
  surname = surnames,
  stringsAsFactors = FALSE
)

# Exercise 6: grep() and grepl().
grepl(
  "Statistics",
  programmes,
  fixed = TRUE
)

grep(
  "Statistics",
  programmes,
  fixed = TRUE
)

grep(
  "Statistics",
  programmes,
  fixed = TRUE,
  value = TRUE
)

# Exercise 7: Anchors and character classes.
codes <- c(
  "S001",
  "S002",
  "P010",
  "X999",
  "S12"
)

starts_with_s <- grepl(
  "^S",
  codes
)

ends_with_nine <- grepl(
  "9$",
  codes
)

valid_s_code <- grepl(
  "^S[0-9]{3}$",
  codes
)

data.frame(
  code = codes,
  starts_with_s = starts_with_s,
  ends_with_nine = ends_with_nine,
  valid_s_code = valid_s_code,
  stringsAsFactors = FALSE
)

# Exercise 8: Quantifiers.
sample_strings <- c(
  "A",
  "AA",
  "AAA",
  "A1",
  "A12"
)

data.frame(
  value = sample_strings,
  one_or_more_a = grepl(
    "^A+$",
    sample_strings
  ),
  a_followed_by_one_or_two_digits =
    grepl(
      "^A[0-9]{1,2}$",
      sample_strings
    ),
  stringsAsFactors = FALSE
)

# Exercise 9: Replace and remove text.
gsub(
  "[[:space:]]+",
  " ",
  "Bolgatanga   Technical   University"
)

gsub(
  "[[:punct:]]",
  "",
  c(
    "Statistics.",
    "Procurement!",
    "Hospitality,"
  )
)

sub(
  "Statistics",
  "Applied Statistics",
  c(
    "Statistics",
    "Statistics Department"
  )
)

# Exercise 10: Extract numbers.
text_values <- c(
  "Age 25",
  "Score 78",
  "Room 104"
)

numeric_text <- gsub(
  "[^0-9.-]",
  "",
  text_values
)

numeric_values <- as.numeric(
  numeric_text
)

data.frame(
  original = text_values,
  extracted = numeric_text,
  numeric = numeric_values,
  stringsAsFactors = FALSE
)

# Exercise 11: Standardise categories.
standardise_programme <- function(
  x
) {
  cleaned <- trimws(
    tolower(x)
  )

  cleaned <- gsub(
    "[[:space:]]+",
    " ",
    cleaned
  )

  cleaned[
    cleaned %in%
      c(
        "statistics",
        "applied statistics"
      )
  ] <- "Statistics"

  cleaned[
    cleaned %in%
      c(
        "procurement",
        "procurement management"
      )
  ] <- "Procurement"

  cleaned[
    cleaned %in%
      c(
        "hospitality",
        "hospitality management"
      )
  ] <- "Hospitality"

  cleaned[
    cleaned %in%
      c(
        "agriculture",
        "ecological agriculture"
      )
  ] <- "Agriculture"

  cleaned
}

messy_programmes <- c(
  "statistics",
  " Statistics ",
  "STATISTICS",
  "procurement management",
  "Hospitality"
)

standardise_programme(
  messy_programmes
)

# Exercise 12: Validate student identifiers.
student_ids <- c(
  "S0001",
  "S0012",
  "0013",
  "S14",
  "S12345"
)

valid_identifier <- grepl(
  "^S[0-9]{4}$",
  student_ids
)

data.frame(
  student_id = student_ids,
  valid = valid_identifier,
  stringsAsFactors = FALSE
)

# Exercise 13: Clean telephone numbers.
telephone_numbers <- c(
  "+233 24 123 4567",
  "024-123-4567",
  "(024) 123 4567"
)

digits_only <- gsub(
  "[^0-9]",
  "",
  telephone_numbers
)

data.frame(
  raw = telephone_numbers,
  digits_only = digits_only,
  stringsAsFactors = FALSE
)

# Exercise 14: Split a composite field.
location_values <- c(
  "Bolgatanga|Upper East",
  "Tamale|Northern",
  "Wa|Upper West"
)

location_parts <- strsplit(
  location_values,
  split = "|",
  fixed = TRUE
)

town <- vapply(
  location_parts,
  `[`,
  FUN.VALUE = character(1),
  1
)

region <- vapply(
  location_parts,
  `[`,
  FUN.VALUE = character(1),
  2
)

data.frame(
  original = location_values,
  town = town,
  region = region,
  stringsAsFactors = FALSE
)

# Exercise 15: Reusable text-cleaning function.
clean_character_vector <- function(
  x,
  missing_codes = c(
    "",
    "NA",
    "N/A",
    "."
  ),
  normalise_case = FALSE
) {
  if (!is.character(x)) {
    stop(
      "`x` must be character.",
      call. = FALSE
    )
  }

  cleaned <- trimws(x)

  cleaned <- gsub(
    "[[:space:]]+",
    " ",
    cleaned
  )

  cleaned[
    cleaned %in%
      missing_codes
  ] <- NA

  if (normalise_case) {
    cleaned <- tools::toTitleCase(
      tolower(cleaned)
    )
  }

  cleaned
}

clean_character_vector(
  c(
    " Statistics ",
    "N/A",
    "Procurement   Management"
  ),
  normalise_case = TRUE
)

# Optional stringr comparison.
if (requireNamespace(
  "stringr",
  quietly = TRUE
)) {
  print(
    stringr::str_trim(
      messy_text
    )
  )

  print(
    stringr::str_detect(
      codes,
      "^S"
    )
  )
}

# Practical assessment.
raw_assessment_text <- c(
  " statistics ",
  "PROCUREMENT MANAGEMENT",
  "",
  "N/A",
  "Hospitality,",
  "Agriculture"
)

clean_assessment_text <- clean_character_vector(
  raw_assessment_text,
  normalise_case = TRUE
)

clean_assessment_text <- gsub(
  "[[:punct:]]",
  "",
  clean_assessment_text
)

assessment_report <- data.frame(
  raw = raw_assessment_text,
  clean = clean_assessment_text,
  missing = is.na(
    clean_assessment_text
  ),
  length = nchar(
    clean_assessment_text
  ),
  stringsAsFactors = FALSE
)

assessment_report

# ==============================================================================
# End of Lesson 19 Solutions
# ==============================================================================

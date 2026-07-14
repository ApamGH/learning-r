# ==============================================================================
# Learning R
# Part VI: Regression Modelling and Reproducible Statistical Workflows
# Lesson 40: Integrated Capstone Analysis and Professional Reporting
# Suggested Solutions
# ==============================================================================

set.seed(20260714)

# Exercise 1: Define a research problem.
research_problem <- paste(
  "The university requires evidence on student performance, differences across",
  "programmes, the association between coursework and examination scores, and",
  "the extent to which coursework and programme predict examination",
  "performance."
)

research_problem

# Exercise 2: Write measurable objectives.
study_objectives <- c(
  "Describe coursework, examination and final scores.",
  "Compare final scores across programmes.",
  "Assess the association between coursework and examination scores.",
  "Model examination score using coursework score and programme."
)

study_objectives

# Exercise 3: Import data.
student_data <- read.csv(
  file.path(
    "data",
    "student_scores.csv"
  ),
  stringsAsFactors = FALSE
)

head(student_data)
str(student_data)

# Exercise 4: Create a data dictionary.
data_dictionary <- data.frame(
  variable = c(
    "student_id",
    "programme",
    "coursework_score",
    "examination_score",
    "final_score"
  ),
  role = c(
    "Identifier",
    "Grouping variable",
    "Numerical predictor",
    "Numerical outcome",
    "Derived numerical outcome"
  ),
  measurement = c(
    "Nominal",
    "Nominal",
    "Ratio",
    "Ratio",
    "Ratio"
  ),
  valid_rule = c(
    "Unique non-missing code",
    "Recognised programme category",
    "0 to 40",
    "0 to 60",
    "0 to 100 and equals the component sum"
  ),
  stringsAsFactors = FALSE
)

data_dictionary

# Exercise 5: Create an analysis plan.
analysis_plan <- data.frame(
  objective = study_objectives,
  outcome = c(
    "Coursework, examination and final scores",
    "Final score",
    "Examination score",
    "Examination score"
  ),
  predictor = c(
    "None",
    "Programme",
    "Coursework score",
    "Coursework score and programme"
  ),
  method = c(
    "Descriptive statistics and visualisation",
    "One-way ANOVA with Kruskal-Wallis sensitivity analysis",
    "Pearson and Spearman correlation with simple regression",
    "Multiple linear regression with diagnostics"
  ),
  stringsAsFactors = FALSE
)

analysis_plan

# Exercise 6: Validate structure.
required_variables <- c(
  "student_id",
  "programme",
  "coursework_score",
  "examination_score",
  "final_score"
)

missing_required_variables <- setdiff(
  required_variables,
  names(
    student_data
  )
)

missing_required_variables

stopifnot(
  length(
    missing_required_variables
  ) == 0L
)

# Exercise 7: Validate identifiers.
identifier_missing <- is.na(
  student_data$student_id
) |
  student_data$student_id == ""

identifier_duplicate <- duplicated(
  student_data$student_id
) |
  duplicated(
    student_data$student_id,
    fromLast = TRUE
  )

identifier_report <- data.frame(
  missing_identifiers =
    sum(
      identifier_missing
    ),
  duplicated_identifier_rows =
    sum(
      identifier_duplicate
    )
)

identifier_report

# Exercise 8: Validate ranges.
coursework_invalid <-
  student_data$coursework_score < 0 |
  student_data$coursework_score > 40

examination_invalid <-
  student_data$examination_score < 0 |
  student_data$examination_score > 60

final_invalid <-
  student_data$final_score < 0 |
  student_data$final_score > 100

range_report <- data.frame(
  variable = c(
    "coursework_score",
    "examination_score",
    "final_score"
  ),
  invalid_count = c(
    sum(
      coursework_invalid,
      na.rm = TRUE
    ),
    sum(
      examination_invalid,
      na.rm = TRUE
    ),
    sum(
      final_invalid,
      na.rm = TRUE
    )
  ),
  stringsAsFactors = FALSE
)

range_report

# Exercise 9: Validate arithmetic consistency.
component_total <-
  student_data$coursework_score +
  student_data$examination_score

arithmetic_invalid <-
  student_data$final_score !=
  component_total

arithmetic_report <- data.frame(
  inconsistent_rows =
    sum(
      arithmetic_invalid,
      na.rm = TRUE
    )
)

arithmetic_report

# Exercise 10: Profile missingness.
missingness_report <- data.frame(
  variable = names(
    student_data
  ),
  missing = vapply(
    student_data,
    function(x) {
      sum(
        is.na(x)
      )
    },
    FUN.VALUE = integer(1)
  ),
  missing_percentage = vapply(
    student_data,
    function(x) {
      mean(
        is.na(x)
      ) * 100
    },
    FUN.VALUE = numeric(1)
  ),
  stringsAsFactors = FALSE
)

missingness_report

# Exercise 11: Create the analysis dataset.
analysis_data <- student_data

analysis_data$programme <- factor(
  trimws(
    analysis_data$programme
  )
)

analysis_data$passed <-
  analysis_data$final_score >= 50

analysis_data$grade <- cut(
  analysis_data$final_score,
  breaks = c(
    -Inf,
    49,
    59,
    69,
    79,
    Inf
  ),
  labels = c(
    "F",
    "D",
    "C",
    "B",
    "A"
  ),
  ordered_result = TRUE
)

head(analysis_data)

# Exercise 12: Produce descriptive tables.
summarise_numeric <- function(
  x,
  variable_name
) {
  observed <- x[
    !is.na(x)
  ]

  q <- quantile(
    observed,
    c(
      0.25,
      0.50,
      0.75
    )
  )

  data.frame(
    variable = variable_name,
    n = length(observed),
    missing = sum(
      is.na(x)
    ),
    mean = mean(observed),
    median = median(observed),
    standard_deviation = sd(observed),
    minimum = min(observed),
    first_quartile = q[1],
    third_quartile = q[3],
    maximum = max(observed),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

numerical_variables <- c(
  "coursework_score",
  "examination_score",
  "final_score"
)

descriptive_table <- do.call(
  rbind,
  lapply(
    numerical_variables,
    function(variable_name) {
      summarise_numeric(
        analysis_data[[
          variable_name
        ]],
        variable_name
      )
    }
  )
)

descriptive_table

programme_counts <- table(
  analysis_data$programme,
  useNA = "ifany"
)

programme_table <- data.frame(
  programme = names(
    programme_counts
  ),
  frequency =
    as.integer(
      programme_counts
    ),
  percentage =
    as.numeric(
      programme_counts /
      sum(
        programme_counts
      ) *
      100
    ),
  stringsAsFactors = FALSE
)

programme_table

grade_counts <- table(
  analysis_data$grade,
  useNA = "ifany"
)

grade_table <- data.frame(
  grade = names(
    grade_counts
  ),
  frequency =
    as.integer(
      grade_counts
    ),
  percentage =
    as.numeric(
      grade_counts /
      sum(
        grade_counts
      ) *
      100
    ),
  stringsAsFactors = FALSE
)

grade_table

# Exercise 13: Produce three analytical graphs.
hist(
  analysis_data$final_score,
  breaks = 10,
  main = "Distribution of Final Scores",
  xlab = "Final score",
  ylab = "Frequency"
)

boxplot(
  final_score ~ programme,
  data = analysis_data,
  main = "Final Scores by Programme",
  xlab = "Programme",
  ylab = "Final score",
  las = 2
)

plot(
  analysis_data$coursework_score,
  analysis_data$examination_score,
  main = "Coursework and Examination Scores",
  xlab = "Coursework score",
  ylab = "Examination score",
  pch = 19
)

# Exercise 14: Conduct a group-comparison test.
anova_model <- aov(
  final_score ~ programme,
  data = analysis_data
)

anova_table <- summary(
  anova_model
)[[1]]

anova_table

anova_residuals <- residuals(
  anova_model
)

shapiro_anova <- shapiro.test(
  anova_residuals
)

fligner_result <- fligner.test(
  final_score ~ programme,
  data = analysis_data
)

kruskal_result <- kruskal.test(
  final_score ~ programme,
  data = analysis_data
)

shapiro_anova
fligner_result
kruskal_result

between_sum_squares <- anova_table[
  "programme",
  "Sum Sq"
]

total_sum_squares <- sum(
  anova_table[
    ,
    "Sum Sq"
  ]
)

eta_squared <-
  between_sum_squares /
  total_sum_squares

eta_squared

# Exercise 15: Conduct correlation analysis.
pearson_result <- cor.test(
  analysis_data$coursework_score,
  analysis_data$examination_score,
  method = "pearson"
)

spearman_result <- cor.test(
  analysis_data$coursework_score,
  analysis_data$examination_score,
  method = "spearman",
  exact = FALSE
)

pearson_result
spearman_result

# Exercise 16: Fit simple and multiple regression models.
simple_model <- lm(
  examination_score ~
    coursework_score,
  data = analysis_data
)

analysis_data$programme <- relevel(
  analysis_data$programme,
  ref = "Statistics"
)

multiple_model <- lm(
  examination_score ~
    coursework_score +
    programme,
  data = analysis_data
)

summary(simple_model)
summary(multiple_model)

# Exercise 17: Assess regression diagnostics.
old_parameters <- par(
  mfrow = c(
    2,
    2
  )
)

plot(multiple_model)

par(old_parameters)

studentised_residuals <- rstudent(
  multiple_model
)

leverage_values <- hatvalues(
  multiple_model
)

cooks_values <- cooks.distance(
  multiple_model
)

number_parameters <- length(
  coef(
    multiple_model
  )
)

diagnostic_report <- data.frame(
  observation =
    seq_len(
      nobs(
        multiple_model
      )
    ),
  studentised_residual =
    studentised_residuals,
  leverage =
    leverage_values,
  cooks_distance =
    cooks_values,
  unusual_residual =
    abs(
      studentised_residuals
    ) > 2,
  high_leverage =
    leverage_values >
    2 *
    number_parameters /
    nobs(
      multiple_model
    ),
  influential =
    cooks_values >
    4 /
    nobs(
      multiple_model
    )
)

diagnostic_report[
  diagnostic_report$unusual_residual |
    diagnostic_report$high_leverage |
    diagnostic_report$influential,
]

# Exercise 18: Produce adjusted predictions.
prediction_grid <- expand.grid(
  coursework_score = c(
    20,
    30,
    40
  ),
  programme =
    levels(
      analysis_data$programme
    )
)

prediction_intervals <- predict(
  multiple_model,
  newdata = prediction_grid,
  interval = "confidence",
  level = 0.95
)

prediction_output <- cbind(
  prediction_grid,
  prediction_intervals
)

prediction_output

# Exercise 19: Create a model-reporting function.
report_lm <- function(
  model
) {
  model_summary <- summary(
    model
  )

  coefficient_table <- as.data.frame(
    model_summary$coefficients
  )

  coefficient_table$term <- rownames(
    coefficient_table
  )

  rownames(
    coefficient_table
  ) <- NULL

  intervals <- as.data.frame(
    confint(model)
  )

  intervals$term <- rownames(
    intervals
  )

  rownames(
    intervals
  ) <- NULL

  names(
    intervals
  )[1:2] <- c(
    "confidence_lower",
    "confidence_upper"
  )

  coefficient_table <- merge(
    coefficient_table,
    intervals,
    by = "term",
    sort = FALSE
  )

  fit_table <- data.frame(
    n = nobs(model),
    r_squared =
      model_summary$r.squared,
    adjusted_r_squared =
      model_summary$adj.r.squared,
    residual_standard_error =
      model_summary$sigma,
    AIC = AIC(model),
    BIC = BIC(model)
  )

  list(
    coefficients =
      coefficient_table,
    fit =
      fit_table
  )
}

multiple_model_report <- report_lm(
  multiple_model
)

multiple_model_report

# Exercise 20: Write results paragraphs.
overall_mean <- mean(
  analysis_data$final_score,
  na.rm = TRUE
)

overall_sd <- sd(
  analysis_data$final_score,
  na.rm = TRUE
)

descriptive_text <- paste(
  "The mean final score was",
  round(
    overall_mean,
    2
  ),
  "with a standard deviation of",
  round(
    overall_sd,
    2
  ),
  "."
)

coursework_row <- summary(
  multiple_model
)$coefficients[
  "coursework_score",
]

regression_text <- paste(
  "After adjustment for programme, a one-mark increase in coursework score",
  "was associated with an estimated",
  round(
    coursework_row[
      "Estimate"
    ],
    3
  ),
  "mark change in examination score, 95 per cent CI",
  paste0(
    "[",
    round(
      confint(
        multiple_model
      )[
        "coursework_score",
        1
      ],
      3
    ),
    ", ",
    round(
      confint(
        multiple_model
      )[
        "coursework_score",
        2
      ],
      3
    ),
    "]"
  ),
  ", p =",
  format.pval(
    coursework_row[
      "Pr(>|t|)"
    ],
    digits = 3,
    eps = 0.001
  ),
  "."
)

descriptive_text
regression_text

# Exercise 21: State limitations.
limitations <- c(
  "The dataset is observational, so causal claims are not justified.",
  "The sample may not represent all students or institutions.",
  "Only a limited number of explanatory variables are available.",
  "Measurement quality and assessment standardisation require consideration.",
  "Small programme-specific samples may reduce precision."
)

limitations

# Exercise 22: Conduct a quality audit.
quality_audit <- data.frame(
  check = c(
    "Required variables present",
    "Identifiers checked",
    "Ranges checked",
    "Arithmetic consistency checked",
    "Missingness reported",
    "Descriptive statistics produced",
    "Visualisations produced",
    "Inferential methods justified",
    "Diagnostics assessed",
    "Effect estimates and intervals reported",
    "Limitations stated",
    "Reproducibility information recorded"
  ),
  completed = c(
    length(
      missing_required_variables
    ) == 0L,
    TRUE,
    all(
      range_report$invalid_count == 0
    ),
    arithmetic_report$inconsistent_rows == 0,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    length(
      limitations
    ) > 0,
    TRUE
  ),
  stringsAsFactors = FALSE
)

quality_audit

# Exercise 23: Package outputs for handover.
capstone_directory <- "lesson_40_capstone_project"

capstone_subdirectories <- c(
  file.path(
    capstone_directory,
    "data"
  ),
  file.path(
    capstone_directory,
    "R"
  ),
  file.path(
    capstone_directory,
    "scripts"
  ),
  file.path(
    capstone_directory,
    "reports"
  ),
  file.path(
    capstone_directory,
    "outputs",
    "tables"
  ),
  file.path(
    capstone_directory,
    "outputs",
    "figures"
  )
)

for (directory in capstone_subdirectories) {
  if (!dir.exists(directory)) {
    dir.create(
      directory,
      recursive = TRUE
    )
  }
}

write.csv(
  data_dictionary,
  file.path(
    capstone_directory,
    "data_dictionary.csv"
  ),
  row.names = FALSE
)

write.csv(
  analysis_plan,
  file.path(
    capstone_directory,
    "analysis_plan.csv"
  ),
  row.names = FALSE
)

write.csv(
  descriptive_table,
  file.path(
    capstone_directory,
    "outputs",
    "tables",
    "descriptive_statistics.csv"
  ),
  row.names = FALSE
)

write.csv(
  programme_table,
  file.path(
    capstone_directory,
    "outputs",
    "tables",
    "programme_distribution.csv"
  ),
  row.names = FALSE
)

write.csv(
  prediction_output,
  file.path(
    capstone_directory,
    "outputs",
    "tables",
    "adjusted_predictions.csv"
  ),
  row.names = FALSE
)

png(
  file.path(
    capstone_directory,
    "outputs",
    "figures",
    "programme_boxplot.png"
  ),
  width = 1200,
  height = 800,
  res = 150
)

boxplot(
  final_score ~ programme,
  data = analysis_data,
  main = "Final Scores by Programme",
  xlab = "Programme",
  ylab = "Final score",
  las = 2
)

dev.off()

writeLines(
  capture.output(
    sessionInfo()
  ),
  file.path(
    capstone_directory,
    "session-info.txt"
  )
)

readme_lines <- c(
  "# Student Performance Capstone",
  "",
  "## Purpose",
  "This project analyses student performance across programmes.",
  "",
  "## Main analyses",
  "- Descriptive statistics",
  "- Programme comparison",
  "- Correlation analysis",
  "- Simple and multiple linear regression",
  "- Regression diagnostics",
  "",
  "## Main outputs",
  "- outputs/tables/descriptive_statistics.csv",
  "- outputs/tables/programme_distribution.csv",
  "- outputs/tables/adjusted_predictions.csv",
  "- outputs/figures/programme_boxplot.png",
  "",
  "## Reproducibility",
  "Run all scripts from the project root using relative paths."
)

writeLines(
  readme_lines,
  file.path(
    capstone_directory,
    "README.md"
  )
)

handover_audit <- data.frame(
  item = c(
    "README",
    "Data dictionary",
    "Analysis plan",
    "Descriptive table",
    "Programme table",
    "Prediction table",
    "Figure",
    "Session information"
  ),
  path = c(
    file.path(
      capstone_directory,
      "README.md"
    ),
    file.path(
      capstone_directory,
      "data_dictionary.csv"
    ),
    file.path(
      capstone_directory,
      "analysis_plan.csv"
    ),
    file.path(
      capstone_directory,
      "outputs",
      "tables",
      "descriptive_statistics.csv"
    ),
    file.path(
      capstone_directory,
      "outputs",
      "tables",
      "programme_distribution.csv"
    ),
    file.path(
      capstone_directory,
      "outputs",
      "tables",
      "adjusted_predictions.csv"
    ),
    file.path(
      capstone_directory,
      "outputs",
      "figures",
      "programme_boxplot.png"
    ),
    file.path(
      capstone_directory,
      "session-info.txt"
    )
  ),
  exists = FALSE,
  stringsAsFactors = FALSE
)

handover_audit$exists <- file.exists(
  handover_audit$path
)

handover_audit

stopifnot(
  all(
    handover_audit$exists
  )
)

# Final capstone result object.
capstone_assessment <- list(
  research_problem =
    research_problem,
  objectives =
    study_objectives,
  data_dictionary =
    data_dictionary,
  analysis_plan =
    analysis_plan,
  validation = list(
    identifiers =
      identifier_report,
    ranges =
      range_report,
    arithmetic =
      arithmetic_report,
    missingness =
      missingness_report
  ),
  descriptive_results = list(
    numerical =
      descriptive_table,
    programme =
      programme_table,
    grade =
      grade_table
  ),
  inference = list(
    anova =
      anova_table,
    eta_squared =
      eta_squared,
    kruskal_wallis =
      kruskal_result,
    pearson =
      pearson_result,
    spearman =
      spearman_result
  ),
  regression =
    multiple_model_report,
  diagnostics =
    diagnostic_report,
  predictions =
    prediction_output,
  results_text = c(
    descriptive_text,
    regression_text
  ),
  limitations =
    limitations,
  quality_audit =
    quality_audit,
  handover_audit =
    handover_audit
)

capstone_assessment

# Remove the demonstration handover folder after verification.
unlink(
  capstone_directory,
  recursive = TRUE,
  force = TRUE
)

# ==============================================================================
# End of Lesson 40 Solutions
# ==============================================================================

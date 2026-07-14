# ==============================================================================
# Learning R
# Part VI: Regression Modelling and Reproducible Statistical Workflows
# Lesson 36: Simple Linear Regression
# Suggested Solutions
# ==============================================================================

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

regression_data <- student_data[
  c(
    "student_id",
    "coursework_score",
    "examination_score"
  )
]

# Exercise 1: Import and inspect the data.
head(regression_data)
str(regression_data)
summary(regression_data)

# Exercise 2: Create a scatter plot.
plot(
  regression_data$coursework_score,
  regression_data$examination_score,
  main = "Coursework and Examination Scores",
  xlab = "Coursework score",
  ylab = "Examination score",
  pch = 19
)

# Exercise 3: Fit a simple linear regression.
simple_model <- lm(
  examination_score ~ coursework_score,
  data = regression_data
)

summary(simple_model)

# Exercise 4: Interpret intercept and slope.
model_coefficients <- coef(
  simple_model
)

intercept <- unname(
  model_coefficients[1]
)

slope <- unname(
  model_coefficients[2]
)

coefficient_interpretation <- data.frame(
  term = c(
    "Intercept",
    "Coursework slope"
  ),
  estimate = c(
    intercept,
    slope
  ),
  interpretation = c(
    paste(
      "Predicted examination score when coursework score equals zero is",
      round(intercept, 3)
    ),
    paste(
      "A one-mark increase in coursework score is associated with an average",
      round(slope, 3),
      "mark change in examination score."
    )
  ),
  stringsAsFactors = FALSE
)

coefficient_interpretation

# Exercise 5: Extract fitted values and residuals.
fitted_values <- fitted(
  simple_model
)

model_residuals <- residuals(
  simple_model
)

observation_output <- data.frame(
  student_id =
    regression_data$student_id,
  observed =
    regression_data$examination_score,
  fitted =
    fitted_values,
  residual =
    model_residuals,
  stringsAsFactors = FALSE
)

head(observation_output)

# Exercise 6: Verify coefficients manually.
x <- regression_data$coursework_score
y <- regression_data$examination_score

manual_slope <- cov(
  x,
  y
) / var(
  x
)

manual_intercept <- mean(y) -
  manual_slope *
  mean(x)

data.frame(
  coefficient = c(
    "Intercept",
    "Slope"
  ),
  manual = c(
    manual_intercept,
    manual_slope
  ),
  lm = c(
    intercept,
    slope
  ),
  stringsAsFactors = FALSE
)

# Exercise 7: Conduct slope inference.
coefficient_table <- summary(
  simple_model
)$coefficients

slope_row <- coefficient_table[
  "coursework_score",
]

slope_row

# Exercise 8: Calculate confidence intervals.
coefficient_intervals <- confint(
  simple_model,
  level = 0.95
)

coefficient_intervals

# Exercise 9: Interpret R-squared.
model_summary <- summary(
  simple_model
)

r_squared <- model_summary$r.squared
adjusted_r_squared <-
  model_summary$adj.r.squared

r_squared_interpretation <- paste(
  round(
    r_squared * 100,
    2
  ),
  "per cent of the observed variation in examination scores is explained by",
  "the fitted linear relationship with coursework score."
)

r_squared_interpretation

# Exercise 10: Create point predictions.
new_coursework <- data.frame(
  coursework_score = c(
    20,
    25,
    30,
    35
  )
)

point_predictions <- predict(
  simple_model,
  newdata = new_coursework
)

data.frame(
  new_coursework,
  predicted_examination =
    point_predictions
)

# Exercise 11: Create confidence and prediction intervals.
mean_response_interval <- predict(
  simple_model,
  newdata = new_coursework,
  interval = "confidence",
  level = 0.95
)

individual_prediction_interval <- predict(
  simple_model,
  newdata = new_coursework,
  interval = "prediction",
  level = 0.95
)

confidence_output <- cbind(
  new_coursework,
  mean_response_interval
)

prediction_output <- cbind(
  new_coursework,
  individual_prediction_interval
)

confidence_output
prediction_output

# Exercise 12: Inspect diagnostic plots.
old_parameters <- par(
  mfrow = c(
    2,
    2
  )
)

plot(simple_model)

par(old_parameters)

# Exercise 13: Identify potentially influential records.
standardised_residuals <- rstandard(
  simple_model
)

studentised_residuals <- rstudent(
  simple_model
)

leverage_values <- hatvalues(
  simple_model
)

cooks_values <- cooks.distance(
  simple_model
)

number_parameters <- length(
  coef(simple_model)
)

leverage_cutoff <- 2 *
  number_parameters /
  nobs(simple_model)

cooks_cutoff <- 4 /
  nobs(simple_model)

influence_report <- data.frame(
  student_id =
    regression_data$student_id,
  standardised_residual =
    standardised_residuals,
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
    leverage_cutoff,
  influential =
    cooks_values >
    cooks_cutoff,
  stringsAsFactors = FALSE
)

influence_report[
  influence_report$unusual_residual |
    influence_report$high_leverage |
    influence_report$influential,
]

# Exercise 14: Reusable reporting function.
report_simple_regression <- function(
  data,
  outcome,
  predictor
) {
  required <- c(
    outcome,
    predictor
  )

  if (!all(
    required %in%
      names(data)
  )) {
    stop(
      "Required variables are missing.",
      call. = FALSE
    )
  }

  formula_object <- reformulate(
    predictor,
    response = outcome
  )

  model <- lm(
    formula_object,
    data = data
  )

  model_summary <- summary(
    model
  )

  coefficient_table <-
    model_summary$coefficients

  slope_row <- coefficient_table[
    predictor,
  ]

  slope_interval <- confint(
    model,
    predictor
  )

  data.frame(
    outcome = outcome,
    predictor = predictor,
    n = nobs(model),
    intercept =
      unname(
        coef(model)[1]
      ),
    slope =
      unname(
        coef(model)[2]
      ),
    slope_standard_error =
      slope_row[
        "Std. Error"
      ],
    t =
      slope_row[
        "t value"
      ],
    p_value =
      slope_row[
        "Pr(>|t|)"
      ],
    confidence_lower =
      slope_interval[1],
    confidence_upper =
      slope_interval[2],
    r_squared =
      model_summary$r.squared,
    adjusted_r_squared =
      model_summary$adj.r.squared,
    residual_standard_error =
      model_summary$sigma,
    stringsAsFactors = FALSE
  )
}

simple_regression_report <- report_simple_regression(
  regression_data,
  "examination_score",
  "coursework_score"
)

simple_regression_report

# Exercise 15: Write a non-causal interpretation.
decision <- if (
  simple_regression_report$p_value < 0.05
) {
  "statistically significant"
} else {
  "not statistically significant"
}

interpretation <- paste(
  "A simple linear regression examined the association between coursework and",
  "examination scores. The estimated slope was",
  round(
    simple_regression_report$slope,
    3
  ),
  "with a 95 per cent confidence interval from",
  round(
    simple_regression_report$confidence_lower,
    3
  ),
  "to",
  round(
    simple_regression_report$confidence_upper,
    3
  ),
  ". The association was",
  decision,
  "at the 5 per cent level. The model explained",
  round(
    simple_regression_report$r_squared * 100,
    2
  ),
  "per cent of the observed outcome variation. This describes association and",
  "does not establish that coursework performance causes examination",
  "performance."
)

interpretation

# Practical assessment workflow.
plot(
  regression_data$coursework_score,
  regression_data$examination_score,
  main = "Coursework and Examination Scores",
  xlab = "Coursework score",
  ylab = "Examination score",
  pch = 19
)

abline(
  simple_model,
  lwd = 2
)

simple_regression_assessment <- list(
  report =
    simple_regression_report,
  predictions =
    prediction_output,
  observation_output =
    observation_output,
  influence_report =
    influence_report,
  interpretation =
    interpretation
)

simple_regression_assessment

# ==============================================================================
# End of Lesson 36 Solutions
# ==============================================================================

# ==============================================================================
# Learning R
# Part VI: Regression Modelling and Reproducible Statistical Workflows
# Lesson 38: Regression Diagnostics, Model Validation and Reporting
# Suggested Solutions
# ==============================================================================

set.seed(20260714)

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

student_data$programme <- factor(
  student_data$programme
)

diagnostic_model <- lm(
  examination_score ~
    coursework_score +
    programme,
  data = student_data
)

# Exercise 1: Extract residual and influence measures.
fitted_values <- fitted(
  diagnostic_model
)

raw_residuals <- residuals(
  diagnostic_model
)

standardised_residuals <- rstandard(
  diagnostic_model
)

studentised_residuals <- rstudent(
  diagnostic_model
)

leverage_values <- hatvalues(
  diagnostic_model
)

cooks_values <- cooks.distance(
  diagnostic_model
)

dfbeta_values <- dfbetas(
  diagnostic_model
)

# Exercise 2: Create a diagnostic data frame.
diagnostic_output <- data.frame(
  observation =
    seq_len(
      nobs(
        diagnostic_model
      )
    ),
  fitted =
    fitted_values,
  residual =
    raw_residuals,
  standardised_residual =
    standardised_residuals,
  studentised_residual =
    studentised_residuals,
  leverage =
    leverage_values,
  cooks_distance =
    cooks_values
)

head(diagnostic_output)

# Exercise 3: Assess linearity.
plot(
  fitted_values,
  raw_residuals,
  main = "Residuals versus Fitted Values",
  xlab = "Fitted value",
  ylab = "Residual",
  pch = 19
)

abline(
  h = 0,
  lty = 2
)

lines(
  lowess(
    fitted_values,
    raw_residuals
  ),
  lwd = 2
)

# Exercise 4: Assess constant variance.
plot(
  fitted_values,
  sqrt(
    abs(
      standardised_residuals
    )
  ),
  main = "Scale-Location Plot",
  xlab = "Fitted value",
  ylab = "Square root of absolute standardised residual",
  pch = 19
)

lines(
  lowess(
    fitted_values,
    sqrt(
      abs(
        standardised_residuals
      )
    )
  ),
  lwd = 2
)

auxiliary_model <- lm(
  I(
    raw_residuals ^ 2
  ) ~ fitted_values
)

auxiliary_r_squared <- summary(
  auxiliary_model
)$r.squared

lagrange_multiplier <- nobs(
  diagnostic_model
) * auxiliary_r_squared

bp_p_value <- pchisq(
  lagrange_multiplier,
  df = 1,
  lower.tail = FALSE
)

data.frame(
  statistic =
    lagrange_multiplier,
  degrees_of_freedom = 1,
  p_value =
    bp_p_value
)

# Exercise 5: Assess residual normality.
hist(
  raw_residuals,
  main = "Residual Distribution",
  xlab = "Residual"
)

qqnorm(
  raw_residuals,
  main = "Normal Q-Q Plot of Residuals"
)

qqline(
  raw_residuals
)

shapiro_result <- shapiro.test(
  raw_residuals
)

shapiro_result

# Exercise 6: Identify unusual residuals.
unusual_residual_rows <- which(
  abs(
    studentised_residuals
  ) > 2
)

diagnostic_output[
  unusual_residual_rows,
]

# Exercise 7: Calculate leverage cutoffs.
number_parameters <- length(
  coef(
    diagnostic_model
  )
)

leverage_cutoff <- 2 *
  number_parameters /
  nobs(
    diagnostic_model
  )

high_leverage_rows <- which(
  leverage_values >
    leverage_cutoff
)

leverage_cutoff

diagnostic_output[
  high_leverage_rows,
]

# Exercise 8: Identify influential observations.
cooks_cutoff <- 4 /
  nobs(
    diagnostic_model
  )

influential_rows <- which(
  cooks_values >
    cooks_cutoff
)

cooks_cutoff

diagnostic_output[
  influential_rows,
]

# Exercise 9: Inspect DFBETAs.
dfbeta_cutoff <- 2 /
  sqrt(
    nobs(
      diagnostic_model
    )
  )

dfbeta_flags <- apply(
  abs(
    dfbeta_values
  ) >
    dfbeta_cutoff,
  1,
  any
)

which(
  dfbeta_flags
)

# Exercise 10: Calculate VIFs.
calculate_vif <- function(
  model
) {
  design <- model.matrix(
    model
  )

  design <- design[
    ,
    colnames(design) !=
      "(Intercept)",
    drop = FALSE
  ]

  if (ncol(design) == 1L) {
    return(
      setNames(
        1,
        colnames(design)
      )
    )
  }

  result <- vapply(
    seq_len(
      ncol(design)
    ),
    function(index) {
      auxiliary <- lm(
        design[
          ,
          index
        ] ~
          design[
            ,
            -index,
            drop = FALSE
          ]
      )

      1 / (
        1 -
          summary(
            auxiliary
          )$r.squared
      )
    },
    FUN.VALUE = numeric(1)
  )

  names(result) <- colnames(
    design
  )

  result
}

vif_values <- calculate_vif(
  diagnostic_model
)

vif_values

# Exercise 11: Conduct a transparent sensitivity analysis.
rows_to_review <- sort(
  unique(
    c(
      unusual_residual_rows,
      high_leverage_rows,
      influential_rows,
      which(
        dfbeta_flags
      )
    )
  )
)

sensitivity_data <- if (
  length(rows_to_review) > 0L
) {
  student_data[
    -rows_to_review,
  ]
} else {
  student_data
}

sensitivity_model <- lm(
  examination_score ~
    coursework_score +
    programme,
  data = sensitivity_data
)

coefficient_comparison <- merge(
  data.frame(
    term = names(
      coef(
        diagnostic_model
      )
    ),
    original =
      unname(
        coef(
          diagnostic_model
        )
      )
  ),
  data.frame(
    term = names(
      coef(
        sensitivity_model
      )
    ),
    sensitivity =
      unname(
        coef(
          sensitivity_model
        )
      )
  ),
  by = "term",
  all = TRUE,
  sort = FALSE
)

coefficient_comparison

# Exercise 12: Create a train-test split.
training_rows <- sample(
  seq_len(
    nrow(
      student_data
    )
  ),
  size = floor(
    0.70 *
      nrow(
        student_data
      )
  )
)

training_data <- student_data[
  training_rows,
]

test_data <- student_data[
  -training_rows,
]

data.frame(
  set = c(
    "Training",
    "Test"
  ),
  n = c(
    nrow(training_data),
    nrow(test_data)
  )
)

# Exercise 13: Calculate MAE, RMSE and test R-squared.
training_model <- lm(
  examination_score ~
    coursework_score +
    programme,
  data = training_data
)

test_predictions <- predict(
  training_model,
  newdata = test_data
)

prediction_errors <-
  test_data$examination_score -
  test_predictions

mean_absolute_error <- mean(
  abs(
    prediction_errors
  )
)

root_mean_squared_error <- sqrt(
  mean(
    prediction_errors ^ 2
  )
)

test_r_squared <- 1 -
  sum(
    prediction_errors ^ 2
  ) /
  sum(
    (
      test_data$examination_score -
        mean(
          test_data$examination_score
        )
    ) ^ 2
  )

validation_report <- data.frame(
  MAE =
    mean_absolute_error,
  RMSE =
    root_mean_squared_error,
  test_R_squared =
    test_r_squared
)

validation_report

# Exercise 14: Manual k-fold cross-validation.
cross_validate_lm <- function(
  data,
  formula,
  k = 5,
  seed = 20260714
) {
  if (k < 2L ||
      k > nrow(data)) {
    stop(
      "`k` must be between 2 and the number of rows.",
      call. = FALSE
    )
  }

  set.seed(seed)

  fold_id <- sample(
    rep(
      seq_len(k),
      length.out = nrow(data)
    )
  )

  outcome_name <- all.vars(
    formula
  )[1]

  fold_results <- lapply(
    seq_len(k),
    function(current_fold) {
      training <- data[
        fold_id != current_fold,
      ]

      validation <- data[
        fold_id == current_fold,
      ]

      model <- lm(
        formula,
        data = training
      )

      predictions <- predict(
        model,
        newdata = validation
      )

      errors <- validation[[
        outcome_name
      ]] - predictions

      data.frame(
        fold = current_fold,
        n = nrow(validation),
        MAE = mean(
          abs(errors)
        ),
        RMSE = sqrt(
          mean(
            errors ^ 2
          )
        )
      )
    }
  )

  do.call(
    rbind,
    fold_results
  )
}

cross_validation_results <- cross_validate_lm(
  student_data,
  examination_score ~
    coursework_score +
    programme,
  k = 5
)

cross_validation_results

# Exercise 15: Assess coefficient stability.
cross_validate_coefficients <- function(
  data,
  formula,
  k = 5,
  seed = 20260714
) {
  set.seed(seed)

  fold_id <- sample(
    rep(
      seq_len(k),
      length.out = nrow(data)
    )
  )

  coefficient_list <- lapply(
    seq_len(k),
    function(current_fold) {
      training <- data[
        fold_id != current_fold,
      ]

      model <- lm(
        formula,
        data = training
      )

      coefficients <- coef(
        model
      )

      data.frame(
        fold = current_fold,
        term = names(
          coefficients
        ),
        estimate =
          unname(
            coefficients
          ),
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(
    rbind,
    coefficient_list
  )
}

coefficient_stability <- cross_validate_coefficients(
  student_data,
  examination_score ~
    coursework_score +
    programme,
  k = 5
)

coefficient_stability

stability_summary <- aggregate(
  estimate ~ term,
  data = coefficient_stability,
  FUN = function(x) {
    c(
      mean = mean(x),
      standard_deviation = sd(x),
      minimum = min(x),
      maximum = max(x)
    )
  }
)

stability_summary

# Exercise 16: Complete diagnostic and model reports.
create_regression_diagnostic_report <- function(
  model
) {
  if (!inherits(
    model,
    "lm"
  )) {
    stop(
      "`model` must be an lm object.",
      call. = FALSE
    )
  }

  n <- nobs(model)
  p <- length(
    coef(model)
  )

  studentised <- rstudent(
    model
  )

  leverage <- hatvalues(
    model
  )

  cooks <- cooks.distance(
    model
  )

  dfbeta_matrix <- dfbetas(
    model
  )

  data.frame(
    n = n,
    parameters = p,
    maximum_absolute_studentised_residual =
      max(
        abs(
          studentised
        )
      ),
    residuals_above_two =
      sum(
        abs(
          studentised
        ) > 2
      ),
    leverage_cutoff =
      2 *
      p /
      n,
    high_leverage_count =
      sum(
        leverage >
          2 *
          p /
          n
      ),
    cooks_cutoff =
      4 /
      n,
    influential_count =
      sum(
        cooks >
          4 /
          n
      ),
    dfbeta_cutoff =
      2 /
      sqrt(n),
    dfbeta_flagged_count =
      sum(
        apply(
          abs(
            dfbeta_matrix
          ) >
            2 /
            sqrt(n),
          1,
          any
        )
      ),
    residual_shapiro_p =
      shapiro.test(
        residuals(model)
      )$p.value,
    stringsAsFactors = FALSE
  )
}

create_full_regression_report <- function(
  model
) {
  model_summary <- summary(
    model
  )

  coefficients <- as.data.frame(
    model_summary$coefficients
  )

  coefficients$term <- rownames(
    coefficients
  )

  rownames(coefficients) <- NULL

  intervals <- as.data.frame(
    confint(model)
  )

  intervals$term <- rownames(
    intervals
  )

  rownames(intervals) <- NULL

  names(intervals)[1:2] <- c(
    "confidence_lower",
    "confidence_upper"
  )

  coefficients <- merge(
    coefficients,
    intervals,
    by = "term",
    sort = FALSE
  )

  fit <- data.frame(
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
    coefficients = coefficients,
    fit = fit,
    diagnostics =
      create_regression_diagnostic_report(
        model
      )
  )
}

full_report <- create_full_regression_report(
  diagnostic_model
)

cross_validation_summary <- data.frame(
  mean_MAE =
    mean(
      cross_validation_results$MAE
    ),
  mean_RMSE =
    mean(
      cross_validation_results$RMSE
    ),
  standard_deviation_RMSE =
    sd(
      cross_validation_results$RMSE
    )
)

regression_assessment <- list(
  full_report =
    full_report,
  vif =
    vif_values,
  coefficient_sensitivity =
    coefficient_comparison,
  train_test_validation =
    validation_report,
  cross_validation =
    cross_validation_summary,
  coefficient_stability =
    stability_summary
)

regression_assessment

# Standard diagnostic panel.
old_parameters <- par(
  mfrow = c(
    2,
    2
  )
)

plot(diagnostic_model)

par(old_parameters)

# ==============================================================================
# End of Lesson 38 Solutions
# ==============================================================================

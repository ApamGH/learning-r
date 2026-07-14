# ==============================================================================
# Learning R
# Part VI: Regression Modelling and Reproducible Statistical Workflows
# Lesson 37: Multiple Linear Regression
# Suggested Solutions
# ==============================================================================

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

student_data$programme <- factor(
  student_data$programme
)

# Exercise 1: Fit a model with two numerical predictors.
structural_model <- lm(
  final_score ~
    coursework_score +
    examination_score,
  data = student_data
)

summary(structural_model)

# Exercise 2: Explain exact structural dependence.
structural_check <- all(
  student_data$final_score ==
    student_data$coursework_score +
    student_data$examination_score
)

structural_check

structural_explanation <- paste(
  "Final score equals coursework score plus examination score in this teaching",
  "dataset. Therefore, modelling final score with both components reproduces",
  "the arithmetic rule rather than discovering a substantive relationship."
)

structural_explanation

# Exercise 3: Fit a model with a categorical predictor.
adjusted_model <- lm(
  examination_score ~
    coursework_score +
    programme,
  data = student_data
)

summary(adjusted_model)

# Exercise 4: Change the reference category.
student_data$programme_statistics_reference <- relevel(
  student_data$programme,
  ref = "Statistics"
)

reference_model <- lm(
  examination_score ~
    coursework_score +
    programme_statistics_reference,
  data = student_data
)

summary(reference_model)

levels(
  student_data$programme_statistics_reference
)

# Exercise 5: Interpret adjusted coefficients.
coefficient_table <- summary(
  reference_model
)$coefficients

coefficient_table

adjusted_interpretation <- paste(
  "The coursework coefficient estimates the expected change in examination",
  "score for a one-mark increase in coursework score while holding programme",
  "constant. Programme coefficients estimate adjusted differences from the",
  "Statistics reference programme."
)

adjusted_interpretation

# Exercise 6: Compare nested models.
reduced_model <- lm(
  examination_score ~
    coursework_score,
  data = student_data
)

full_model <- reference_model

nested_comparison <- anova(
  reduced_model,
  full_model
)

nested_comparison

# Exercise 7: Fit and interpret an interaction.
interaction_model <- lm(
  examination_score ~
    coursework_score *
    programme_statistics_reference,
  data = student_data
)

summary(interaction_model)

interaction_comparison <- anova(
  full_model,
  interaction_model
)

interaction_comparison

# Exercise 8: Centre a continuous predictor.
student_data$coursework_centred <-
  student_data$coursework_score -
  mean(
    student_data$coursework_score
  )

centred_interaction_model <- lm(
  examination_score ~
    coursework_centred *
    programme_statistics_reference,
  data = student_data
)

summary(centred_interaction_model)

# Exercise 9: Create adjusted predictions.
prediction_grid <- expand.grid(
  coursework_score = c(
    20,
    30,
    40
  ),
  programme_statistics_reference =
    levels(
      student_data$programme_statistics_reference
    )
)

prediction_intervals <- predict(
  reference_model,
  newdata = prediction_grid,
  interval = "prediction",
  level = 0.95
)

prediction_output <- cbind(
  prediction_grid,
  prediction_intervals
)

prediction_output

# Exercise 10: Calculate manual VIFs.
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

  if (ncol(design) < 2L) {
    return(
      setNames(
        1,
        colnames(design)
      )
    )
  }

  vif_values <- vapply(
    seq_len(
      ncol(design)
    ),
    function(column_index) {
      response <- design[
        ,
        column_index
      ]

      predictors <- design[
        ,
        -column_index,
        drop = FALSE
      ]

      auxiliary_model <- lm(
        response ~ predictors
      )

      1 / (
        1 -
          summary(
            auxiliary_model
          )$r.squared
      )
    },
    FUN.VALUE = numeric(1)
  )

  names(vif_values) <- colnames(
    design
  )

  vif_values
}

vif_values <- calculate_vif(
  reference_model
)

vif_values

# Exercise 11: Compare model-fit criteria.
model_comparison <- data.frame(
  model = c(
    "Reduced",
    "Additive full",
    "Interaction"
  ),
  residual_degrees_of_freedom = c(
    df.residual(
      reduced_model
    ),
    df.residual(
      full_model
    ),
    df.residual(
      interaction_model
    )
  ),
  r_squared = c(
    summary(
      reduced_model
    )$r.squared,
    summary(
      full_model
    )$r.squared,
    summary(
      interaction_model
    )$r.squared
  ),
  adjusted_r_squared = c(
    summary(
      reduced_model
    )$adj.r.squared,
    summary(
      full_model
    )$adj.r.squared,
    summary(
      interaction_model
    )$adj.r.squared
  ),
  AIC = c(
    AIC(reduced_model),
    AIC(full_model),
    AIC(interaction_model)
  ),
  BIC = c(
    BIC(reduced_model),
    BIC(full_model),
    BIC(interaction_model)
  ),
  stringsAsFactors = FALSE
)

model_comparison

# Exercise 12: Explain confounding, mediation and interaction.
concept_table <- data.frame(
  concept = c(
    "Confounding",
    "Mediation",
    "Interaction"
  ),
  meaning = c(
    "A third variable creates or distorts an exposure-outcome association.",
    "A variable lies on a causal pathway between exposure and outcome.",
    "The association between one predictor and outcome varies across levels of another predictor."
  ),
  stringsAsFactors = FALSE
)

concept_table

# Exercise 13: Reusable reporting functions.
report_multiple_regression <- function(
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

  confidence_intervals <- as.data.frame(
    confint(model)
  )

  confidence_intervals$term <- rownames(
    confidence_intervals
  )

  rownames(
    confidence_intervals
  ) <- NULL

  names(
    confidence_intervals
  )[1:2] <- c(
    "confidence_lower",
    "confidence_upper"
  )

  merged <- merge(
    coefficient_table,
    confidence_intervals,
    by = "term",
    sort = FALSE
  )

  names(merged)[
    names(merged) ==
      "Estimate"
  ] <- "estimate"

  names(merged)[
    names(merged) ==
      "Std. Error"
  ] <- "standard_error"

  names(merged)[
    names(merged) ==
      "t value"
  ] <- "t"

  names(merged)[
    names(merged) ==
      "Pr(>|t|)"
  ] <- "p_value"

  merged
}

summarise_model_fit <- function(
  model
) {
  model_summary <- summary(
    model
  )

  f_statistic <- model_summary$fstatistic

  data.frame(
    n = nobs(model),
    predictors =
      length(
        coef(model)
      ) - 1,
    r_squared =
      model_summary$r.squared,
    adjusted_r_squared =
      model_summary$adj.r.squared,
    residual_standard_error =
      model_summary$sigma,
    f =
      unname(
        f_statistic[
          "value"
        ]
      ),
    df_model =
      unname(
        f_statistic[
          "numdf"
        ]
      ),
    df_residual =
      unname(
        f_statistic[
          "dendf"
        ]
      ),
    p_value = pf(
      f_statistic[
        "value"
      ],
      f_statistic[
        "numdf"
      ],
      f_statistic[
        "dendf"
      ],
      lower.tail = FALSE
    ),
    AIC = AIC(model),
    BIC = BIC(model)
  )
}

coefficient_report <- report_multiple_regression(
  reference_model
)

fit_report <- summarise_model_fit(
  reference_model
)

coefficient_report
fit_report

# Exercise 14: Complete model interpretation.
coursework_row <- coefficient_report[
  coefficient_report$term ==
    "coursework_score",
]

interpretation <- paste(
  "After adjustment for programme, a one-mark increase in coursework score",
  "was associated with an estimated",
  round(
    coursework_row$estimate,
    3
  ),
  "mark change in examination score. The 95 per cent confidence interval",
  "ranged from",
  round(
    coursework_row$confidence_lower,
    3
  ),
  "to",
  round(
    coursework_row$confidence_upper,
    3
  ),
  ". The model R-squared was",
  round(
    fit_report$r_squared,
    3
  ),
  ". These estimates are conditional associations and do not establish causal",
  "effects."
)

interpretation

# Practical assessment workflow.
multiple_regression_assessment <- list(
  coefficient_report =
    coefficient_report,
  model_fit =
    fit_report,
  nested_comparison =
    nested_comparison,
  interaction_comparison =
    interaction_comparison,
  predictions =
    prediction_output,
  vif =
    vif_values,
  interpretation =
    interpretation
)

multiple_regression_assessment

# ==============================================================================
# End of Lesson 37 Solutions
# ==============================================================================

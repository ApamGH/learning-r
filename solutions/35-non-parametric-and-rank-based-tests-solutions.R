# ==============================================================================
# Learning R
# Part V: Statistical Inference and Hypothesis Testing
# Lesson 35: Non-Parametric Tests and Rank-Based Methods
# Suggested Solutions
# ==============================================================================

# Exercise 1: Explain appropriate uses.
rank_method_reasons <- data.frame(
  situation = c(
    "Ordinal outcome",
    "Strongly skewed distribution",
    "Extreme values dominate means",
    "Small sample with implausible normality",
    "Monotonic association"
  ),
  suitable_method = c(
    "Wilcoxon or Kruskal-Wallis",
    "Rank-sum or signed-rank",
    "Rank-based location test",
    "Exact or approximate rank test",
    "Spearman or Kendall correlation"
  ),
  stringsAsFactors = FALSE
)

rank_method_reasons

# Exercise 2: One-sample signed-rank test.
scores <- c(
  62,
  68,
  70,
  74,
  75,
  80,
  82,
  66,
  71,
  77
)

one_sample_wilcoxon <- wilcox.test(
  scores,
  mu = 70,
  alternative = "two.sided",
  exact = FALSE
)

one_sample_wilcoxon

# Exercise 3: Simple sign test.
differences <- scores - 70

positive_count <- sum(
  differences > 0
)

nonzero_count <- sum(
  differences != 0
)

sign_test <- binom.test(
  positive_count,
  nonzero_count,
  p = 0.5
)

sign_test

# Exercise 4: Paired Wilcoxon test.
before_scores <- c(
  60,
  65,
  70,
  72,
  68,
  75,
  66,
  71
)

after_scores <- c(
  64,
  67,
  74,
  73,
  72,
  80,
  70,
  74
)

paired_wilcoxon <- wilcox.test(
  after_scores,
  before_scores,
  paired = TRUE,
  exact = FALSE
)

paired_wilcoxon

paired_differences <-
  after_scores -
  before_scores

summary(
  paired_differences
)

# Exercise 5: Independent rank-sum test.
student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

two_programmes <- student_data[
  student_data$programme %in%
    c(
      "Statistics",
      "Procurement"
    ),
]

rank_sum_test <- wilcox.test(
  final_score ~ programme,
  data = two_programmes,
  exact = FALSE,
  conf.int = TRUE
)

rank_sum_test

# Exercise 6: Median and IQR summaries.
rank_group_summary <- aggregate(
  final_score ~ programme,
  data = two_programmes,
  FUN = function(x) {
    c(
      n = length(x),
      median = median(x),
      iqr = IQR(x)
    )
  }
)

rank_group_summary

# Exercise 7: Kruskal-Wallis test.
kruskal_result <- kruskal.test(
  final_score ~ programme,
  data = student_data
)

kruskal_result

# Exercise 8: Adjusted pairwise Wilcoxon tests.
pairwise_rank_results <- pairwise.wilcox.test(
  student_data$final_score,
  student_data$programme,
  p.adjust.method = "holm",
  exact = FALSE
)

pairwise_rank_results

# Exercise 9: Compare ANOVA and Kruskal-Wallis.
anova_table <- summary(
  aov(
    final_score ~ programme,
    data = student_data
  )
)[[1]]

method_comparison <- data.frame(
  method = c(
    "One-way ANOVA",
    "Kruskal-Wallis"
  ),
  statistic = c(
    anova_table[
      1,
      "F value"
    ],
    unname(
      kruskal_result$statistic
    )
  ),
  degrees_of_freedom = c(
    anova_table[
      1,
      "Df"
    ],
    unname(
      kruskal_result$parameter
    )
  ),
  p_value = c(
    anova_table[
      1,
      "Pr(>F)"
    ],
    kruskal_result$p.value
  ),
  stringsAsFactors = FALSE
)

method_comparison

# Exercise 10: Calculate rank-based effect sizes.
group_levels <- unique(
  two_programmes$programme
)

group_one <- two_programmes$final_score[
  two_programmes$programme ==
    group_levels[1]
]

group_two <- two_programmes$final_score[
  two_programmes$programme ==
    group_levels[2]
]

direction <- sign(
  median(
    group_one
  ) -
  median(
    group_two
  )
)

approximate_z <- qnorm(
  rank_sum_test$p.value / 2,
  lower.tail = FALSE
) * direction

rank_effect_r <-
  approximate_z /
  sqrt(
    length(group_one) +
      length(group_two)
  )

h_value <- unname(
  kruskal_result$statistic
)

number_groups <- length(
  unique(
    student_data$programme
  )
)

total_n <- sum(
  complete.cases(
    student_data[
      c(
        "final_score",
        "programme"
      )
    ]
  )
)

epsilon_squared <- (
  h_value -
    number_groups +
    1
) / (
  total_n -
    number_groups
)

data.frame(
  effect_size = c(
    "Rank-sum r",
    "Kruskal-Wallis epsilon squared"
  ),
  value = c(
    rank_effect_r,
    epsilon_squared
  ),
  stringsAsFactors = FALSE
)

# Exercise 11: Inspect ties.
score_frequency <- table(
  student_data$final_score
)

ties <- score_frequency[
  score_frequency > 1
]

ties

# Exercise 12: Spearman and Kendall correlations.
spearman_result <- cor.test(
  student_data$coursework_score,
  student_data$examination_score,
  method = "spearman",
  exact = FALSE
)

kendall_result <- cor.test(
  student_data$coursework_score,
  student_data$examination_score,
  method = "kendall",
  exact = FALSE
)

spearman_result
kendall_result

# Exercise 13: Reusable reporting functions.
report_one_sample_wilcoxon <- function(
  x,
  null_location = 0,
  alternative = c(
    "two.sided",
    "less",
    "greater"
  )
) {
  alternative <- match.arg(
    alternative
  )

  observed <- x[
    !is.na(x)
  ]

  result <- wilcox.test(
    observed,
    mu = null_location,
    alternative = alternative,
    exact = FALSE
  )

  data.frame(
    n = length(observed),
    median = median(observed),
    null_location = null_location,
    statistic =
      unname(
        result$statistic
      ),
    p_value =
      result$p.value,
    stringsAsFactors = FALSE
  )
}

report_rank_sum <- function(
  data,
  outcome,
  group
) {
  required <- c(
    outcome,
    group
  )

  if (!all(
    required %in% names(data)
  )) {
    stop(
      "Required variables are missing.",
      call. = FALSE
    )
  }

  groups <- unique(
    data[[group]][
      !is.na(
        data[[group]]
      )
    ]
  )

  if (length(groups) != 2L) {
    stop(
      "Exactly two groups are required.",
      call. = FALSE
    )
  }

  formula_object <- reformulate(
    group,
    response = outcome
  )

  result <- wilcox.test(
    formula_object,
    data = data,
    exact = FALSE
  )

  data.frame(
    group_1 = groups[1],
    group_2 = groups[2],
    median_1 = median(
      data[[outcome]][
        data[[group]] ==
          groups[1]
      ],
      na.rm = TRUE
    ),
    median_2 = median(
      data[[outcome]][
        data[[group]] ==
          groups[2]
      ],
      na.rm = TRUE
    ),
    statistic =
      unname(
        result$statistic
      ),
    p_value =
      result$p.value,
    stringsAsFactors = FALSE
  )
}

report_kruskal_wallis <- function(
  data,
  outcome,
  group
) {
  formula_object <- reformulate(
    group,
    response = outcome
  )

  result <- kruskal.test(
    formula_object,
    data = data
  )

  complete_data <- data[
    complete.cases(
      data[
        c(
          outcome,
          group
        )
      ]
    ),
  ]

  k <- length(
    unique(
      complete_data[[group]]
    )
  )

  n <- nrow(
    complete_data
  )

  h <- unname(
    result$statistic
  )

  data.frame(
    h = h,
    degrees_of_freedom =
      unname(
        result$parameter
      ),
    p_value =
      result$p.value,
    epsilon_squared =
      (
        h -
          k +
          1
      ) / (
        n -
          k
      ),
    stringsAsFactors = FALSE
  )
}

report_one_sample_wilcoxon(
  scores,
  null_location = 70
)

report_rank_sum(
  two_programmes,
  "final_score",
  "programme"
)

report_kruskal_wallis(
  student_data,
  "final_score",
  "programme"
)

# Exercise 14: Complete interpretation.
kruskal_report <- report_kruskal_wallis(
  student_data,
  "final_score",
  "programme"
)

decision <- if (
  kruskal_report$p_value < 0.05
) {
  "reject"
} else {
  "fail to reject"
}

interpretation <- paste(
  "A Kruskal-Wallis test compared final-score distributions across",
  "programmes. The test produced H(",
  kruskal_report$degrees_of_freedom,
  ") = ",
  round(
    kruskal_report$h,
    3
  ),
  ", p = ",
  format.pval(
    kruskal_report$p_value,
    digits = 3,
    eps = 0.001
  ),
  ". We therefore ",
  decision,
  " the null hypothesis. Epsilon squared was ",
  round(
    kruskal_report$epsilon_squared,
    3
  ),
  ". Interpretation should consider group medians, IQRs and distribution",
  "shapes rather than assuming that the test concerns medians alone.",
  sep = ""
)

interpretation

# Visual assessment.
boxplot(
  final_score ~ programme,
  data = student_data,
  main = "Final Scores by Programme",
  xlab = "Programme",
  ylab = "Final score",
  las = 2
)

# Practical assessment workflow.
rank_test_assessment <- list(
  one_sample =
    report_one_sample_wilcoxon(
      scores,
      70
    ),
  paired =
    paired_wilcoxon,
  independent =
    report_rank_sum(
      two_programmes,
      "final_score",
      "programme"
    ),
  kruskal_wallis =
    kruskal_report,
  pairwise =
    pairwise_rank_results,
  method_comparison =
    method_comparison,
  spearman =
    spearman_result,
  kendall =
    kendall_result
)

rank_test_assessment

# ==============================================================================
# End of Lesson 35 Solutions
# ==============================================================================

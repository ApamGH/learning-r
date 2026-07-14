# ==============================================================================
# Learning R
# Part IV: Descriptive Analysis and Data Visualisation
# Lesson 26: Data Visualisation with Base R
# Suggested Solutions
# ==============================================================================

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

clinic_data <- read.csv(
  "data/clinic_visits.csv",
  stringsAsFactors = FALSE
)

programme_counts <- table(
  student_data$programme
)

# Exercise 1: Vertical and horizontal bar charts.
barplot(
  programme_counts,
  main = "Students by Programme",
  xlab = "Programme",
  ylab = "Number of students",
  las = 2
)

barplot(
  programme_counts,
  horiz = TRUE,
  main = "Students by Programme",
  xlab = "Number of students",
  ylab = "Programme",
  las = 1
)

# Exercise 2: Ordered frequency chart.
ordered_programme_counts <- sort(
  programme_counts,
  decreasing = TRUE
)

barplot(
  ordered_programme_counts,
  main = "Students by Programme",
  xlab = "Programme",
  ylab = "Number of students",
  las = 2
)

# Exercise 3: Grouped and stacked bar charts.
student_data$result <- ifelse(
  student_data$final_score >= 50,
  "Pass",
  "Fail"
)

programme_result_table <- table(
  student_data$result,
  student_data$programme
)

barplot(
  programme_result_table,
  beside = TRUE,
  main = "Results by Programme",
  xlab = "Programme",
  ylab = "Number of students",
  legend.text = rownames(
    programme_result_table
  ),
  args.legend = list(
    x = "topright"
  ),
  las = 2
)

barplot(
  programme_result_table,
  beside = FALSE,
  main = "Results by Programme",
  xlab = "Programme",
  ylab = "Number of students",
  legend.text = rownames(
    programme_result_table
  ),
  args.legend = list(
    x = "topright"
  ),
  las = 2
)

# Exercise 4: Histograms with different breaks.
hist(
  student_data$final_score,
  breaks = 5,
  main = "Final Scores with Five Bins",
  xlab = "Final score",
  ylab = "Frequency"
)

hist(
  student_data$final_score,
  breaks = 12,
  main = "Final Scores with Twelve Bins",
  xlab = "Final score",
  ylab = "Frequency"
)

# Exercise 5: Density-scaled histogram.
hist(
  student_data$final_score,
  probability = TRUE,
  main = "Density of Final Scores",
  xlab = "Final score",
  ylab = "Density"
)

lines(
  density(
    student_data$final_score,
    na.rm = TRUE
  )
)

# Exercise 6: Box plot and grouped box plot.
boxplot(
  student_data$final_score,
  main = "Box Plot of Final Scores",
  ylab = "Final score"
)

boxplot(
  final_score ~ programme,
  data = student_data,
  main = "Final Scores by Programme",
  xlab = "Programme",
  ylab = "Final score",
  las = 2
)

# Exercise 7: Scatter plot with fitted line.
plot(
  student_data$coursework_score,
  student_data$examination_score,
  main = "Coursework and Examination Scores",
  xlab = "Coursework score",
  ylab = "Examination score",
  pch = 19
)

fitted_model <- lm(
  examination_score ~ coursework_score,
  data = student_data
)

abline(
  fitted_model,
  lwd = 2
)

# Exercise 8: Line graph.
monthly_values <- c(
  12,
  15,
  14,
  18,
  20,
  22
)

month_number <- seq_along(
  monthly_values
)

plot(
  month_number,
  monthly_values,
  type = "o",
  main = "Monthly Trend",
  xlab = "Month",
  ylab = "Value",
  xaxt = "n"
)

axis(
  1,
  at = month_number,
  labels = month.abb[
    month_number
  ]
)

# Exercise 9: Dot chart.
programme_means <- tapply(
  student_data$final_score,
  student_data$programme,
  mean,
  na.rm = TRUE
)

dotchart(
  programme_means,
  main = "Mean Final Score by Programme",
  xlab = "Mean final score"
)

# Exercise 10: Empirical cumulative distribution.
plot(
  ecdf(
    student_data$final_score
  ),
  main = "Empirical Cumulative Distribution of Scores",
  xlab = "Final score",
  ylab = "Cumulative proportion"
)

# Exercise 11: Multi-panel display.
old_parameters <- par(
  mfrow = c(
    2,
    2
  )
)

hist(
  student_data$final_score,
  main = "Histogram",
  xlab = "Final score"
)

boxplot(
  student_data$final_score,
  main = "Box Plot",
  ylab = "Final score"
)

plot(
  student_data$coursework_score,
  student_data$examination_score,
  main = "Scatter Plot",
  xlab = "Coursework",
  ylab = "Examination",
  pch = 19
)

barplot(
  programme_counts,
  main = "Programme Counts",
  las = 2
)

par(
  old_parameters
)

# Exercise 12: Annotate an unusual point.
plot(
  student_data$coursework_score,
  student_data$examination_score,
  main = "Assessment Components",
  xlab = "Coursework score",
  ylab = "Examination score",
  pch = 19
)

highest_index <- which.max(
  student_data$final_score
)

text(
  student_data$coursework_score[
    highest_index
  ],
  student_data$examination_score[
    highest_index
  ],
  labels =
    student_data$student_id[
      highest_index
    ],
  pos = 3
)

# Exercise 13: Save PNG and PDF graphs.
output_directory <- "lesson_26_solution_output"

if (!dir.exists(output_directory)) {
  dir.create(output_directory)
}

png_path <- file.path(
  output_directory,
  "final_score_histogram.png"
)

png(
  filename = png_path,
  width = 1200,
  height = 800,
  res = 150
)

hist(
  student_data$final_score,
  main = "Distribution of Final Scores",
  xlab = "Final score",
  ylab = "Frequency"
)

dev.off()

pdf_path <- file.path(
  output_directory,
  "final_score_boxplot.pdf"
)

pdf(
  file = pdf_path,
  width = 7,
  height = 5
)

boxplot(
  final_score ~ programme,
  data = student_data,
  main = "Final Scores by Programme",
  xlab = "Programme",
  ylab = "Final score",
  las = 2
)

dev.off()

file.exists(png_path)
file.exists(pdf_path)

# Exercise 14: Reusable plotting functions.
plot_histogram <- function(
  x,
  title,
  x_label,
  breaks = "Sturges"
) {
  if (!is.numeric(x)) {
    stop(
      "`x` must be numeric.",
      call. = FALSE
    )
  }

  hist(
    x,
    breaks = breaks,
    main = title,
    xlab = x_label,
    ylab = "Frequency"
  )

  invisible(NULL)
}

plot_grouped_boxplot <- function(
  data,
  numeric_variable,
  group_variable,
  title = NULL
) {
  if (!all(
    c(
      numeric_variable,
      group_variable
    ) %in%
      names(data)
  )) {
    stop(
      "Required variables are missing.",
      call. = FALSE
    )
  }

  formula_object <- reformulate(
    group_variable,
    response =
      numeric_variable
  )

  boxplot(
    formula_object,
    data = data,
    main = title,
    xlab = group_variable,
    ylab = numeric_variable,
    las = 2
  )

  invisible(NULL)
}

plot_histogram(
  student_data$final_score,
  title = "Distribution of Final Scores",
  x_label = "Final score",
  breaks = 10
)

plot_grouped_boxplot(
  student_data,
  "final_score",
  "programme",
  "Final Scores by Programme"
)

# Exercise 15: Critique misleading graph practices.
graph_critique <- data.frame(
  practice = c(
    "Truncated axis",
    "Three-dimensional effects",
    "Line graph for unordered categories",
    "Unlabelled axes",
    "Too many visual elements"
  ),
  problem = c(
    "Can exaggerate apparent differences.",
    "Distorts size and position.",
    "Implies continuity or order that does not exist.",
    "Prevents clear interpretation.",
    "Distracts from the analytical pattern."
  ),
  stringsAsFactors = FALSE
)

graph_critique

# Practical assessment workflow.
missing_counts <- colSums(
  is.na(
    clinic_data
  )
)

barplot(
  missing_counts,
  main = "Missing Values by Clinic Variable",
  xlab = "Variable",
  ylab = "Number missing",
  las = 2
)

plot(
  clinic_data$waiting_time,
  clinic_data$consultation_duration,
  main = "Waiting Time and Consultation Duration",
  xlab = "Waiting time",
  ylab = "Consultation duration",
  pch = 19
)

abline(
  lm(
    consultation_duration ~ waiting_time,
    data = clinic_data
  ),
  lwd = 2
)

unlink(
  output_directory,
  recursive = TRUE,
  force = TRUE
)

# ==============================================================================
# End of Lesson 26 Solutions
# ==============================================================================

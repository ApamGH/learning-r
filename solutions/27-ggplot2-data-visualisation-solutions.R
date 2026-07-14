# ==============================================================================
# Learning R
# Part IV: Descriptive Analysis and Data Visualisation
# Lesson 27: Data Visualisation with ggplot2
# Suggested Solutions
# ==============================================================================

ggplot2_available <- requireNamespace(
  "ggplot2",
  quietly = TRUE
)

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

student_data$result <- ifelse(
  student_data$final_score >= 50,
  "Pass",
  "Fail"
)

# Exercise 1: Check package availability.
ggplot2_available

# Exercise 2: Basic scatter plot.
if (ggplot2_available) {
  basic_scatter <- ggplot2::ggplot(
    student_data,
    ggplot2::aes(
      x = coursework_score,
      y = examination_score
    )
  ) +
    ggplot2::geom_point()

  print(basic_scatter)
}

# Exercise 3: Mapped and fixed aesthetics.
if (ggplot2_available) {
  mapped_plot <- ggplot2::ggplot(
    student_data,
    ggplot2::aes(
      x = coursework_score,
      y = examination_score,
      shape = programme
    )
  ) +
    ggplot2::geom_point(
      size = 3
    )

  fixed_plot <- ggplot2::ggplot(
    student_data,
    ggplot2::aes(
      x = coursework_score,
      y = examination_score
    )
  ) +
    ggplot2::geom_point(
      size = 3
    )

  print(mapped_plot)
  print(fixed_plot)
}

# Exercise 4: Count and pre-computed bar charts.
programme_summary <- as.data.frame(
  table(
    student_data$programme
  ),
  stringsAsFactors = FALSE
)

names(programme_summary) <- c(
  "programme",
  "frequency"
)

if (ggplot2_available) {
  count_bar <- ggplot2::ggplot(
    student_data,
    ggplot2::aes(
      x = programme
    )
  ) +
    ggplot2::geom_bar() +
    ggplot2::labs(
      title = "Students by Programme",
      x = "Programme",
      y = "Number of students"
    )

  precomputed_bar <- ggplot2::ggplot(
    programme_summary,
    ggplot2::aes(
      x = programme,
      y = frequency
    )
  ) +
    ggplot2::geom_col() +
    ggplot2::coord_flip()

  print(count_bar)
  print(precomputed_bar)
}

# Exercise 5: Histograms and density plots.
if (ggplot2_available) {
  score_histogram <- ggplot2::ggplot(
    student_data,
    ggplot2::aes(
      x = final_score
    )
  ) +
    ggplot2::geom_histogram(
      bins = 10
    ) +
    ggplot2::labs(
      title = "Distribution of Final Scores",
      x = "Final score",
      y = "Frequency"
    )

  score_density <- ggplot2::ggplot(
    student_data,
    ggplot2::aes(
      x = final_score
    )
  ) +
    ggplot2::geom_density() +
    ggplot2::labs(
      title = "Density of Final Scores",
      x = "Final score",
      y = "Density"
    )

  print(score_histogram)
  print(score_density)
}

# Exercise 6: Grouped box plot.
if (ggplot2_available) {
  grouped_boxplot <- ggplot2::ggplot(
    student_data,
    ggplot2::aes(
      x = programme,
      y = final_score
    )
  ) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(
      title = "Final Scores by Programme",
      x = "Programme",
      y = "Final score"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      )
    )

  print(grouped_boxplot)
}

# Exercise 7: Scatter plot with fitted line.
if (ggplot2_available) {
  fitted_scatter <- ggplot2::ggplot(
    student_data,
    ggplot2::aes(
      x = coursework_score,
      y = examination_score
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(
      method = "lm",
      se = TRUE
    ) +
    ggplot2::labs(
      title = "Coursework and Examination Scores",
      x = "Coursework score",
      y = "Examination score"
    )

  print(fitted_scatter)
}

# Exercise 8: Grouped and proportional bar charts.
if (ggplot2_available) {
  grouped_bar <- ggplot2::ggplot(
    student_data,
    ggplot2::aes(
      x = programme,
      fill = result
    )
  ) +
    ggplot2::geom_bar(
      position = "dodge"
    ) +
    ggplot2::labs(
      title = "Results by Programme",
      x = "Programme",
      y = "Number of students"
    )

  proportional_bar <- ggplot2::ggplot(
    student_data,
    ggplot2::aes(
      x = programme,
      fill = result
    )
  ) +
    ggplot2::geom_bar(
      position = "fill"
    ) +
    ggplot2::labs(
      title = "Proportion of Results by Programme",
      x = "Programme",
      y = "Proportion"
    )

  print(grouped_bar)
  print(proportional_bar)
}

# Exercise 9: Faceting.
if (ggplot2_available) {
  faceted_histogram <- ggplot2::ggplot(
    student_data,
    ggplot2::aes(
      x = final_score
    )
  ) +
    ggplot2::geom_histogram(
      bins = 8
    ) +
    ggplot2::facet_wrap(
      ~ programme
    ) +
    ggplot2::labs(
      title = "Final-Score Distributions by Programme",
      x = "Final score",
      y = "Frequency"
    )

  print(faceted_histogram)
}

# Exercise 10: Labels and themes.
if (ggplot2_available) {
  labelled_plot <- ggplot2::ggplot(
    student_data,
    ggplot2::aes(
      x = coursework_score,
      y = examination_score
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = "Relationship between Assessment Components",
      subtitle = "Student assessment records",
      x = "Coursework score",
      y = "Examination score",
      caption = "Source: Synthetic teaching dataset"
    ) +
    ggplot2::theme_minimal()

  print(labelled_plot)
}

# Exercise 11: Scales.
if (ggplot2_available) {
  scaled_plot <- ggplot2::ggplot(
    student_data,
    ggplot2::aes(
      x = coursework_score,
      y = examination_score
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(
      limits = c(
        0,
        40
      ),
      breaks = seq(
        0,
        40,
        10
      )
    ) +
    ggplot2::scale_y_continuous(
      limits = c(
        0,
        60
      ),
      breaks = seq(
        0,
        60,
        10
      )
    )

  print(scaled_plot)
}

# Exercise 12: Annotation.
highest_index <- which.max(
  student_data$final_score
)

highest_student <- student_data[
  highest_index,
]

if (ggplot2_available) {
  annotated_plot <- ggplot2::ggplot(
    student_data,
    ggplot2::aes(
      x = coursework_score,
      y = examination_score
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::annotate(
      "text",
      x = highest_student$coursework_score,
      y = highest_student$examination_score,
      label = highest_student$student_id,
      vjust = -1
    ) +
    ggplot2::labs(
      title = "Assessment Components with Highest-Scoring Student"
    )

  print(annotated_plot)
}

# Exercise 13: Save a high-resolution plot.
output_directory <- "lesson_27_solution_output"

if (!dir.exists(output_directory)) {
  dir.create(output_directory)
}

if (ggplot2_available) {
  save_plot <- ggplot2::ggplot(
    student_data,
    ggplot2::aes(
      x = final_score
    )
  ) +
    ggplot2::geom_histogram(
      bins = 10
    ) +
    ggplot2::labs(
      title = "Distribution of Final Scores",
      x = "Final score",
      y = "Frequency"
    )

  output_path <- file.path(
    output_directory,
    "final_score_histogram.png"
  )

  ggplot2::ggsave(
    filename = output_path,
    plot = save_plot,
    width = 7,
    height = 5,
    dpi = 300
  )

  print(
    file.exists(
      output_path
    )
  )
}

# Exercise 14: Reusable plotting function.
create_scatter_plot <- function(
  data,
  x_variable,
  y_variable,
  title = NULL,
  add_line = TRUE
) {
  if (!requireNamespace(
    "ggplot2",
    quietly = TRUE
  )) {
    stop(
      "`ggplot2` is required.",
      call. = FALSE
    )
  }

  if (!all(
    c(
      x_variable,
      y_variable
    ) %in%
      names(data)
  )) {
    stop(
      "Required variables are missing.",
      call. = FALSE
    )
  }

  plot_object <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data[[
        x_variable
      ]],
      y = .data[[
        y_variable
      ]]
    )
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(
      title = title,
      x = x_variable,
      y = y_variable
    ) +
    ggplot2::theme_minimal()

  if (add_line) {
    plot_object <- plot_object +
      ggplot2::geom_smooth(
        method = "lm",
        se = TRUE
      )
  }

  plot_object
}

if (ggplot2_available) {
  print(
    create_scatter_plot(
      student_data,
      "coursework_score",
      "examination_score",
      "Assessment Components"
    )
  )
}

# Practical assessment workflow.
if (ggplot2_available) {
  practical_plots <- list(
    bar = ggplot2::ggplot(
      student_data,
      ggplot2::aes(
        x = programme
      )
    ) +
      ggplot2::geom_bar(),

    histogram = ggplot2::ggplot(
      student_data,
      ggplot2::aes(
        x = final_score
      )
    ) +
      ggplot2::geom_histogram(
        bins = 10
      ),

    boxplot = ggplot2::ggplot(
      student_data,
      ggplot2::aes(
        x = programme,
        y = final_score
      )
    ) +
      ggplot2::geom_boxplot(),

    scatter = create_scatter_plot(
      student_data,
      "coursework_score",
      "examination_score",
      "Assessment Components"
    )
  )

  lapply(
    practical_plots,
    print
  )
}

unlink(
  output_directory,
  recursive = TRUE,
  force = TRUE
)

# ==============================================================================
# End of Lesson 27 Solutions
# ==============================================================================

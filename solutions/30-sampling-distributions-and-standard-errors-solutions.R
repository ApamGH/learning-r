# ==============================================================================
# Learning R
# Part V: Statistical Inference and Hypothesis Testing
# Lesson 30: Sampling Distributions and Standard Errors
# Suggested Solutions
# ==============================================================================

set.seed(20260714)

# Exercise 1: Distinguish population, sample, parameter and statistic.
population <- rnorm(
  n = 100000,
  mean = 70,
  sd = 12
)

population_mean <- mean(population)
population_sd <- sd(population)

sample_data <- sample(
  population,
  size = 30,
  replace = FALSE
)

sample_mean <- mean(sample_data)
sample_sd <- sd(sample_data)

concept_summary <- data.frame(
  concept = c(
    "Population parameter: mean",
    "Population parameter: standard deviation",
    "Sample statistic: mean",
    "Sample statistic: standard deviation"
  ),
  value = c(
    population_mean,
    population_sd,
    sample_mean,
    sample_sd
  ),
  stringsAsFactors = FALSE
)

concept_summary

# Exercise 2: Draw one random sample.
one_sample <- sample(
  population,
  size = 30,
  replace = FALSE
)

summary(one_sample)

# Exercise 3: Simulate repeated sample means.
number_of_samples <- 2000
sample_size <- 30

sample_means <- replicate(
  number_of_samples,
  mean(
    sample(
      population,
      size = sample_size,
      replace = TRUE
    )
  )
)

head(sample_means)

# Exercise 4: Estimate sampling mean and standard error.
sampling_mean <- mean(sample_means)
sampling_standard_error <- sd(sample_means)

data.frame(
  population_mean = population_mean,
  sampling_mean = sampling_mean,
  simulated_standard_error =
    sampling_standard_error
)

# Exercise 5: Compare theoretical and simulated standard errors.
theoretical_standard_error <-
  population_sd /
  sqrt(sample_size)

data.frame(
  method = c(
    "Theoretical",
    "Simulation"
  ),
  standard_error = c(
    theoretical_standard_error,
    sampling_standard_error
  ),
  stringsAsFactors = FALSE
)

# Exercise 6: Compare several sample sizes.
sample_sizes <- c(
  5,
  30,
  100
)

sampling_results <- lapply(
  sample_sizes,
  function(current_size) {
    replicate(
      2000,
      mean(
        sample(
          population,
          size = current_size,
          replace = TRUE
        )
      )
    )
  }
)

sample_size_summary <- data.frame(
  sample_size = sample_sizes,
  sampling_mean = vapply(
    sampling_results,
    mean,
    FUN.VALUE = numeric(1)
  ),
  simulated_standard_error = vapply(
    sampling_results,
    sd,
    FUN.VALUE = numeric(1)
  ),
  theoretical_standard_error =
    population_sd /
    sqrt(sample_sizes)
)

sample_size_summary

# Exercise 7: Demonstrate the central limit theorem.
skewed_population <- rexp(
  n = 100000,
  rate = 0.10
)

skewed_small_means <- replicate(
  2000,
  mean(
    sample(
      skewed_population,
      size = 5,
      replace = TRUE
    )
  )
)

skewed_large_means <- replicate(
  2000,
  mean(
    sample(
      skewed_population,
      size = 50,
      replace = TRUE
    )
  )
)

old_parameters <- par(
  mfrow = c(
    1,
    3
  )
)

hist(
  skewed_population,
  breaks = 40,
  main = "Skewed Population",
  xlab = "Value"
)

hist(
  skewed_small_means,
  breaks = 30,
  main = "Sample Means: n = 5",
  xlab = "Sample mean"
)

hist(
  skewed_large_means,
  breaks = 30,
  main = "Sample Means: n = 50",
  xlab = "Sample mean"
)

par(old_parameters)

# Exercise 8: Compare standard deviation and standard error.
sample_standard_deviation <- sd(
  one_sample
)

estimated_standard_error <-
  sample_standard_deviation /
  sqrt(
    length(one_sample)
  )

data.frame(
  statistic = c(
    "Standard deviation",
    "Standard error"
  ),
  value = c(
    sample_standard_deviation,
    estimated_standard_error
  ),
  stringsAsFactors = FALSE
)

# Exercise 9: Simulate sample proportions.
binary_population <- rbinom(
  n = 100000,
  size = 1,
  prob = 0.60
)

binary_population_proportion <- mean(
  binary_population
)

proportion_sample_size <- 100

sample_proportions <- replicate(
  2000,
  mean(
    sample(
      binary_population,
      size = proportion_sample_size,
      replace = TRUE
    )
  )
)

simulated_proportion_se <- sd(
  sample_proportions
)

theoretical_proportion_se <- sqrt(
  binary_population_proportion *
    (
      1 -
      binary_population_proportion
    ) /
    proportion_sample_size
)

data.frame(
  population_proportion =
    binary_population_proportion,
  simulated_mean =
    mean(sample_proportions),
  simulated_standard_error =
    simulated_proportion_se,
  theoretical_standard_error =
    theoretical_proportion_se
)

# Exercise 10: Estimate bias.
mean_bias <-
  mean(sample_means) -
  population_mean

proportion_bias <-
  mean(sample_proportions) -
  binary_population_proportion

data.frame(
  estimator = c(
    "Sample mean",
    "Sample proportion"
  ),
  estimated_bias = c(
    mean_bias,
    proportion_bias
  ),
  stringsAsFactors = FALSE
)

# Exercise 11: Compare Monte Carlo replication counts.
replication_counts <- c(
  100,
  1000,
  10000
)

monte_carlo_results <- lapply(
  replication_counts,
  function(repetitions) {
    simulated_values <- replicate(
      repetitions,
      mean(
        sample(
          population,
          size = 30,
          replace = TRUE
        )
      )
    )

    data.frame(
      replications = repetitions,
      estimated_sampling_mean =
        mean(simulated_values),
      estimated_standard_error =
        sd(simulated_values)
    )
  }
)

do.call(
  rbind,
  monte_carlo_results
)

# Exercise 12: Reusable simulation functions.
simulate_sample_means <- function(
  population,
  sample_size,
  replications = 1000
) {
  if (!is.numeric(population)) {
    stop(
      "`population` must be numeric.",
      call. = FALSE
    )
  }

  if (sample_size < 1 ||
      sample_size != as.integer(
        sample_size
      )) {
    stop(
      "`sample_size` must be a positive whole number.",
      call. = FALSE
    )
  }

  if (replications < 1 ||
      replications != as.integer(
        replications
      )) {
    stop(
      "`replications` must be a positive whole number.",
      call. = FALSE
    )
  }

  replicate(
    replications,
    mean(
      sample(
        population,
        size = sample_size,
        replace = TRUE
      )
    )
  )
}

create_sampling_report <- function(
  population,
  sample_size,
  replications = 2000
) {
  simulated_statistics <- simulate_sample_means(
    population,
    sample_size,
    replications
  )

  data.frame(
    population_mean = mean(population),
    population_sd = sd(population),
    sample_size = sample_size,
    replications = replications,
    simulated_mean =
      mean(simulated_statistics),
    simulated_standard_error =
      sd(simulated_statistics),
    theoretical_standard_error =
      sd(population) /
      sqrt(sample_size),
    estimated_bias =
      mean(simulated_statistics) -
      mean(population)
  )
}

create_sampling_report(
  population,
  sample_size = 50
)

# Exercise 13: Interpretation.
interpretation <- paste(
  "The sampling distribution of the sample mean was centred close to the",
  "population mean. Increasing the sample size reduced the standard error,",
  "which indicates greater precision. The standard deviation described",
  "variation among individual observations, whereas the standard error",
  "described variation among repeated sample means."
)

interpretation

# Practical assessment workflow.
assessment_sizes <- c(
  10,
  30,
  100
)

assessment_reports <- do.call(
  rbind,
  lapply(
    assessment_sizes,
    function(current_size) {
      create_sampling_report(
        population,
        sample_size = current_size,
        replications = 3000
      )
    }
  )
)

assessment_reports

# ==============================================================================
# End of Lesson 30 Solutions
# ==============================================================================

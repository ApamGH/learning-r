# ==============================================================================
# Learning R
# Part II: Accessing and Controlling Data
# Lesson 14: Function Scope, Environments and Argument Behaviour
# Suggested Solutions
# ==============================================================================


# ==============================================================================
# Exercise 1: Local and global scope
# ==============================================================================

global_value <- 100

demonstrate_scope <- function() {
  local_value <- 25

  c(
    local_value = local_value,
    global_value = global_value
  )
}

demonstrate_scope()

exists(
  "local_value",
  envir = globalenv()
)

global_value


# ==============================================================================
# Exercise 2: Lexical scoping with an external object
# ==============================================================================

tax_rate <- 0.15

calculate_total_with_external_tax <- function(
  subtotal
) {
  subtotal +
    subtotal * tax_rate
}

calculate_total_with_external_tax(
  1000
)


# ==============================================================================
# Exercise 3: Rewrite with explicit argument
# ==============================================================================

calculate_total_with_tax <- function(
  subtotal,
  tax_rate = 0.15
) {
  subtotal +
    subtotal * tax_rate
}

calculate_total_with_tax(
  subtotal = 1000
)

calculate_total_with_tax(
  subtotal = 1000,
  tax_rate = 0.20
)


# ==============================================================================
# Exercise 4: Ordinary assignment and superassignment
# ==============================================================================

counter <- 0

increment_locally <- function() {
  counter <- counter + 1

  counter
}

increment_locally()
counter

increment_globally <- function() {
  counter <<- counter + 1

  counter
}

increment_globally()
counter

# Ordinary assignment creates or updates a local binding.
# Superassignment modifies a binding in a parent environment and therefore
# creates a side effect.


# ==============================================================================
# Exercise 5: Argument matching
# ==============================================================================

# Exact named matching.
round(
  x = 3.14159,
  digits = 2
)

# Positional matching.
round(
  3.14159,
  2
)

# Partial matching may work but should be avoided.
round(
  3.14159,
  dig = 2
)


# ==============================================================================
# Exercise 6: Default depending on earlier argument
# ==============================================================================

centre_values <- function(
  x,
  centre = mean(
    x,
    na.rm = TRUE
  )
) {
  x - centre
}

centre_values(
  c(
    10,
    20,
    30
  )
)

centre_values(
  c(
    10,
    20,
    30
  ),
  centre = 0
)


# ==============================================================================
# Exercise 7: Lazy evaluation
# ==============================================================================

ignore_second_argument <- function(
  x,
  y
) {
  x
}

ignore_second_argument(
  x = 10,
  y = stop(
    "This error is not triggered because y is unused."
  )
)

# R does not evaluate y because the function never uses it.


# ==============================================================================
# Exercise 8: missing()
# ==============================================================================

describe_input <- function(
  x
) {
  if (missing(x)) {
    return(
      "No input supplied."
    )
  }

  paste(
    "Input:",
    x
  )
}

describe_input()
describe_input(25)


# ==============================================================================
# Exercise 9: match.arg()
# ==============================================================================

summarise_value <- function(
  x,
  statistic = c(
    "mean",
    "median",
    "sum"
  )
) {
  statistic <- match.arg(
    statistic
  )

  switch(
    statistic,
    mean = mean(
      x,
      na.rm = TRUE
    ),
    median = median(
      x,
      na.rm = TRUE
    ),
    sum = sum(
      x,
      na.rm = TRUE
    )
  )
}

summarise_value(
  c(
    10,
    20,
    30
  ),
  statistic = "mean"
)

summarise_value(
  c(
    10,
    20,
    30
  ),
  statistic = "median"
)


# ==============================================================================
# Exercise 10: Forward arguments with ...
# ==============================================================================

calculate_summary <- function(
  x,
  summary_function,
  ...
) {
  summary_function(
    x,
    ...
  )
}

calculate_summary(
  c(
    10,
    NA,
    20
  ),
  summary_function = mean,
  na.rm = TRUE
)

calculate_summary(
  c(
    10,
    NA,
    20
  ),
  summary_function = median,
  na.rm = TRUE
)


# ==============================================================================
# Exercise 11: Create a closure
# ==============================================================================

create_multiplier <- function(
  multiplier
) {
  function(x) {
    x * multiplier
  }
}

double_value <- create_multiplier(
  2
)

triple_value <- create_multiplier(
  3
)

double_value(10)
triple_value(10)


# ==============================================================================
# Exercise 12: Modifying an input does not alter the original
# ==============================================================================

modify_first_value <- function(
  x
) {
  x[1] <- 999

  x
}

original_values <- c(
  10,
  20,
  30
)

modified_values <- modify_first_value(
  original_values
)

original_values
modified_values


# ==============================================================================
# Exercise 13: Remove a hidden global dependency
# ==============================================================================

# Poor version:
global_pass_mark <- 50

calculate_status_poor <- function(
  scores
) {
  scores >= global_pass_mark
}

calculate_status_poor(
  c(
    45,
    58,
    72
  )
)

# Improved version:
calculate_status <- function(
  scores,
  pass_mark = 50
) {
  scores >= pass_mark
}

calculate_status(
  c(
    45,
    58,
    72
  )
)


# ==============================================================================
# Exercise 14: Reproducible simulation function
# ==============================================================================

simulate_scores <- function(
  n,
  mean_score = 70,
  standard_deviation = 10,
  seed = NULL
) {
  if (!is.numeric(n) ||
      length(n) != 1L ||
      n <= 0) {
    stop(
      "`n` must be one positive number.",
      call. = FALSE
    )
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  rnorm(
    n = n,
    mean = mean_score,
    sd = standard_deviation
  )
}

first_simulation <- simulate_scores(
  n = 5,
  seed = 123
)

second_simulation <- simulate_scores(
  n = 5,
  seed = 123
)

first_simulation
second_simulation

identical(
  first_simulation,
  second_simulation
)


# ==============================================================================
# Practical assessment: Complete suggested workflow
# ==============================================================================

# Local and global objects.
global_rate <- 0.10

scope_demo <- function(
  value
) {
  local_rate <- 0.05

  list(
    local_result =
      value *
      local_rate,
    global_result =
      value *
      global_rate
  )
}

scope_demo(
  1000
)

exists(
  "local_rate",
  envir = globalenv()
)

# Lexical scoping.
external_multiplier <- 2

use_external_multiplier <- function(
  x
) {
  x * external_multiplier
}

use_external_multiplier(
  10
)

# Explicit dependency.
use_explicit_multiplier <- function(
  x,
  multiplier = 2
) {
  x * multiplier
}

use_explicit_multiplier(
  10
)

# Ordinary assignment versus superassignment.
assessment_counter <- 0

increment_local <- function() {
  assessment_counter <-
    assessment_counter + 1

  assessment_counter
}

increment_global <- function() {
  assessment_counter <<-
    assessment_counter + 1

  assessment_counter
}

increment_local()
assessment_counter

increment_global()
assessment_counter

# Argument matching.
calculate_growth <- function(
  initial_value,
  final_value,
  periods = 1
) {
  (
    final_value /
      initial_value
  ) ^ (
    1 / periods
  ) - 1
}

calculate_growth(
  initial_value = 100,
  final_value = 121,
  periods = 2
)

calculate_growth(
  100,
  121,
  2
)

# Lazy evaluation.
lazy_demo <- function(
  x,
  unused
) {
  x * 2
}

lazy_demo(
  5,
  stop(
    "Unused argument should not be evaluated."
  )
)

# missing().
describe_optional_input <- function(
  x
) {
  if (missing(x)) {
    "No input supplied"
  } else {
    paste(
      "Input supplied:",
      x
    )
  }
}

describe_optional_input()
describe_optional_input(25)

# match.arg().
calculate_statistic <- function(
  x,
  statistic = c(
    "mean",
    "median",
    "sum"
  )
) {
  statistic <- match.arg(
    statistic
  )

  switch(
    statistic,
    mean = mean(
      x,
      na.rm = TRUE
    ),
    median = median(
      x,
      na.rm = TRUE
    ),
    sum = sum(
      x,
      na.rm = TRUE
    )
  )
}

calculate_statistic(
  c(
    10,
    20,
    30
  ),
  "median"
)

# Forwarding ...
apply_summary_function <- function(
  x,
  FUN,
  ...
) {
  FUN(
    x,
    ...
  )
}

apply_summary_function(
  c(
    10,
    NA,
    20
  ),
  FUN = mean,
  na.rm = TRUE
)

# Closure.
create_threshold_classifier <- function(
  threshold,
  yes_label,
  no_label
) {
  function(x) {
    ifelse(
      x >= threshold,
      yes_label,
      no_label
    )
  }
}

pass_classifier <- create_threshold_classifier(
  threshold = 50,
  yes_label = "Pass",
  no_label = "Fail"
)

pass_classifier(
  c(
    45,
    58,
    72
  )
)

# Safe input modification.
add_total_score <- function(
  data
) {
  data$total_score <-
    data$coursework_score +
    data$examination_score

  data
}

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

student_with_total <- add_total_score(
  student_data
)

"total_score" %in%
  names(student_data)

"total_score" %in%
  names(student_with_total)

# No hidden global dependency.
calculate_pass_status <- function(
  scores,
  pass_mark = 50
) {
  scores >= pass_mark
}

calculate_pass_status(
  student_data$final_score
)

# Reproducible random function.
simulate_waiting_times <- function(
  n,
  mean_minutes = 30,
  sd_minutes = 8,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  rnorm(
    n = n,
    mean = mean_minutes,
    sd = sd_minutes
  )
}

waiting_simulation_one <-
  simulate_waiting_times(
    n = 10,
    seed = 2026
  )

waiting_simulation_two <-
  simulate_waiting_times(
    n = 10,
    seed = 2026
  )

identical(
  waiting_simulation_one,
  waiting_simulation_two
)


# ==============================================================================
# End of Lesson 14 Solutions
# ==============================================================================

# ==============================================================================
# Learning R
# Part I: R Foundations
# Lesson 7: Matrices, Arrays and Multidimensional Structures
# Suggested Solutions
# ==============================================================================


# ==============================================================================
# Exercise 1: Create a 4-by-3 matrix by row
# ==============================================================================

matrix_4_by_3 <- matrix(
  1:12,
  nrow = 4,
  ncol = 3,
  byrow = TRUE
)

matrix_4_by_3


# ==============================================================================
# Exercise 2: Add row and column names
# ==============================================================================

rownames(
  matrix_4_by_3
) <- paste0(
  "Row_",
  1:4
)

colnames(
  matrix_4_by_3
) <- paste0(
  "Column_",
  1:3
)

matrix_4_by_3


# ==============================================================================
# Exercise 3: Extract cells, rows and columns
# ==============================================================================

matrix_4_by_3[
  2,
  3
]

matrix_4_by_3[
  2,
]

matrix_4_by_3[
  ,
  3
]

matrix_4_by_3[
  "Row_2",
  "Column_3"
]

matrix_4_by_3[
  2,
  ,
  drop = FALSE
]


# ==============================================================================
# Exercise 4: Replace one value
# ==============================================================================

matrix_4_by_3[
  "Row_2",
  "Column_3"
] <- 99

matrix_4_by_3


# ==============================================================================
# Exercise 5: Row and column summaries
# ==============================================================================

rowSums(
  matrix_4_by_3
)

rowMeans(
  matrix_4_by_3
)

colSums(
  matrix_4_by_3
)

colMeans(
  matrix_4_by_3
)


# ==============================================================================
# Exercise 6: Element-wise and matrix multiplication
# ==============================================================================

a <- matrix(
  c(
    1,
    2,
    3,
    4
  ),
  nrow = 2,
  byrow = TRUE
)

b <- matrix(
  c(
    5,
    6,
    7,
    8
  ),
  nrow = 2,
  byrow = TRUE
)

elementwise_product <- a * b
matrix_product <- a %*% b

elementwise_product
matrix_product


# ==============================================================================
# Exercise 7: Transpose
# ==============================================================================

transpose_a <- t(
  a
)

transpose_a


# ==============================================================================
# Exercise 8: Determinant and inverse
# ==============================================================================

determinant_a <- det(
  a
)

determinant_a

inverse_a <- solve(
  a
)

inverse_a

identity_check <- a %*% inverse_a
identity_check


# ==============================================================================
# Exercise 9: Solve a linear system
# ==============================================================================

# 2x + y = 8
# x + 3y = 9

coefficient_matrix <- matrix(
  c(
    2,
    1,
    1,
    3
  ),
  nrow = 2,
  byrow = TRUE
)

constant_vector <- c(
  8,
  9
)

linear_solution <- solve(
  coefficient_matrix,
  constant_vector
)

linear_solution


# ==============================================================================
# Exercise 10: Three-dimensional array
# ==============================================================================

three_dimensional_array <- array(
  1:24,
  dim = c(
    3,
    4,
    2
  ),
  dimnames = list(
    Treatment = c(
      "A",
      "B",
      "C"
    ),
    Block = paste0(
      "B",
      1:4
    ),
    Season = c(
      "Dry",
      "Wet"
    )
  )
)

three_dimensional_array


# ==============================================================================
# Exercise 11: Array margin summaries
# ==============================================================================

treatment_means <- apply(
  three_dimensional_array,
  MARGIN = 1,
  FUN = mean
)

block_means <- apply(
  three_dimensional_array,
  MARGIN = 2,
  FUN = mean
)

season_means <- apply(
  three_dimensional_array,
  MARGIN = 3,
  FUN = mean
)

treatment_by_season_means <- apply(
  three_dimensional_array,
  MARGIN = c(
    1,
    3
  ),
  FUN = mean
)

treatment_means
block_means
season_means
treatment_by_season_means


# ==============================================================================
# Exercise 12: Convert matrix to data frame
# ==============================================================================

matrix_data_frame <- as.data.frame(
  matrix_4_by_3
)

matrix_data_frame
str(matrix_data_frame)


# ==============================================================================
# Practical assessment: Complete matrix and array workflow
# ==============================================================================

assessment_matrix <- matrix(
  c(
    28,
    48,
    30,
    44,
    35,
    51,
    24,
    39,
    32,
    54
  ),
  nrow = 5,
  byrow = TRUE,
  dimnames = list(
    paste0(
      "Student_",
      1:5
    ),
    c(
      "Coursework",
      "Examination"
    )
  )
)

assessment_matrix

# Indexing.
assessment_matrix[
  "Student_3",
  "Examination"
]

assessment_matrix[
  "Student_2",
]

assessment_matrix[
  ,
  "Coursework"
]

# Row and column summaries.
student_totals <- rowSums(
  assessment_matrix
)

student_means <- rowMeans(
  assessment_matrix
)

component_totals <- colSums(
  assessment_matrix
)

component_means <- colMeans(
  assessment_matrix
)

student_totals
student_means
component_totals
component_means

# Element-wise operation.
doubled_matrix <- assessment_matrix * 2
doubled_matrix

# Matrix multiplication for weighted results.
weights <- matrix(
  c(
    0.40,
    0.60
  ),
  ncol = 1
)

weighted_scores <- assessment_matrix %*% weights
weighted_scores

# Transpose.
t(
  assessment_matrix
)

# Linear-system example.
system_matrix <- matrix(
  c(
    3,
    1,
    2,
    4
  ),
  nrow = 2,
  byrow = TRUE
)

system_constants <- c(
  10,
  16
)

solve(
  system_matrix,
  system_constants
)

# Named array.
assessment_array <- array(
  1:24,
  dim = c(
    3,
    4,
    2
  ),
  dimnames = list(
    Programme = c(
      "Statistics",
      "Procurement",
      "Hospitality"
    ),
    Semester = paste0(
      "S",
      1:4
    ),
    Status = c(
      "Registered",
      "Completed"
    )
  )
)

assessment_array

apply(
  assessment_array,
  MARGIN = 1,
  FUN = mean
)

apply(
  assessment_array,
  MARGIN = 3,
  FUN = mean
)

# Dimension compatibility.
matrix_left <- matrix(
  1:6,
  nrow = 2
)

matrix_right <- matrix(
  1:6,
  nrow = 3
)

dim(matrix_left)
dim(matrix_right)

compatible_for_multiplication <-
  ncol(matrix_left) ==
  nrow(matrix_right)

compatible_for_multiplication

if (compatible_for_multiplication) {
  matrix_left %*% matrix_right
}


# ==============================================================================
# End of Lesson 7 Solutions
# ==============================================================================

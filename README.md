# Learning R

## From First Principles to Advanced Statistical Computing

**Learning R** is a complete, reproducible course in R programming, data
management, statistical analysis, data visualisation, simulation, regression
modelling and professional analytical practice.

The course contains **40 core lessons**, organised into six progressive parts.
Each lesson is provided as an R Markdown lecture file, with a corresponding R
solution file for guided review after the learner has attempted the exercises.

The repository is designed for:

- self-paced learning;
- lecturer-led teaching;
- computer laboratory sessions;
- statistical methods courses;
- research training;
- and professional development in data analysis.

## Course philosophy

The course develops statistical programming as a complete analytical workflow
rather than as a collection of isolated commands. Learners progress from basic
R objects and data structures to data management, visualisation, statistical
inference, regression modelling, diagnostics, reproducibility and integrated
professional reporting.

The materials emphasise:

- clear explanation before code;
- reproducible examples;
- realistic analytical scenarios;
- correct statistical interpretation;
- reusable functions;
- transparent diagnostic procedures;
- and cautious, non-causal reporting where the study design does not support
  causal inference.

## Core course structure

### Part I: R Foundations

1. Understanding R and Statistical Computing  
2. Setting Up and Managing the R Working Environment  
3. The First R Session and Command Execution  
4. Objects, Assignment and Naming Conventions  
5. Atomic Data Types and Special Values  
6. Vectors and Vectorised Operations  
7. Matrices, Arrays and Multidimensional Structures  
8. Lists, Data Frames and Tibbles  

### Part II: Accessing and Controlling Data

9. Indexing, Subsetting and Extracting Values  
10. Operators, Comparisons and Logical Conditions  
11. Conditional Execution with `if`, `else` and `ifelse()`  
12. Repetition with `for`, `while` and `repeat` Loops  
13. Writing and Using Functions  
14. Function Scope, Environments and Argument Behaviour  

### Part III: Importing, Cleaning and Transforming Data

15. Importing Delimited Text Files  
16. Importing Excel and Statistical Software Files  
17. Inspecting, Validating and Auditing Imported Data  
18. Missing Data and Incomplete Observations  
19. Character Data, Text Cleaning and Regular Expressions  
20. Dates, Times and Temporal Data  
21. Factors and Categorical Data Management  
22. Sorting, Filtering, Recoding and Transforming Variables  
23. Joining, Merging and Reshaping Data  

### Part IV: Descriptive Analysis and Data Visualisation

24. Descriptive Statistics for Numerical Data  
25. Frequency Tables, Cross-Tabulation and Grouped Summaries  
26. Data Visualisation with Base R  
27. Data Visualisation with `ggplot2`  
28. Distribution Assessment and Exploratory Data Analysis  
29. Correlation, Covariance and Visual Association  

### Part V: Statistical Inference and Hypothesis Testing

30. Sampling Distributions and Standard Errors  
31. Confidence Intervals and Estimation  
32. Hypothesis Testing and t-Tests  
33. Analysis of Variance and Post Hoc Comparisons  
34. Chi-Square Tests for Categorical Data  
35. Non-Parametric Tests and Rank-Based Methods  

### Part VI: Regression Modelling and Reproducible Statistical Workflows

36. Simple Linear Regression  
37. Multiple Linear Regression  
38. Regression Diagnostics, Model Validation and Reporting  
39. Reproducible Statistical Analysis with R Markdown, Projects and Version
    Control  
40. Integrated Capstone Analysis and Professional Reporting  

## Repository structure

```text
learning-r/
├── 01-*.Rmd
├── 02-*.Rmd
├── ...
├── 40-*.Rmd
├── data/
├── solutions/
│   ├── 01-*-solutions.R
│   ├── 02-*-solutions.R
│   ├── ...
│   └── 40-*-solutions.R
├── exercises/
├── assessments/
├── R/
├── scripts/
├── references/
├── styles/
├── tests/
├── docs/
├── learning-r.Rproj
└── README.md
```

The main lecture files are stored in the repository root so that they can be
opened and rendered directly from the RStudio project. Instructor or guided
solutions are stored separately in `solutions/`.

## Main datasets

The `data/` directory contains synthetic teaching datasets used throughout the
course. The principal files include datasets for:

- student assessment records;
- clinic visits;
- rainfall measurements;
- sales records;
- and other lesson-specific demonstrations.

The datasets are synthetic and are intended for teaching, practice and
reproducible demonstrations. They should not be interpreted as official
institutional records.

Frequently used variables include:

### Student assessment data

- `student_id`
- `programme`
- `coursework_score`
- `examination_score`
- `final_score`

### Clinic visit data

- `patient_id`
- `sex`
- `age`
- `visit_type`
- `waiting_time`
- `consultation_duration`

### Rainfall data

- `rainfall_mm`

### Sales data

- `units_sold`
- `revenue`

## Software requirements

The course requires a current installation of:

- R;
- RStudio or another suitable R development environment;
- R Markdown;
- Pandoc, normally supplied with RStudio;
- and XeLaTeX when PDF rendering is required.

Several lessons use optional packages. The most frequently used package is:

```r
install.packages("ggplot2")
```

Other optional packages are introduced only where required. Base R alternatives
are used whenever practical.

## Opening the course

1. Clone or download the repository.
2. Open `learning-r.Rproj`.
3. Confirm that the `data/` directory is available.
4. Open the required lesson `.Rmd` file.
5. Run the code chunks sequentially.
6. Complete the independent exercises before reviewing the corresponding
   solution file.
7. Render the lesson to HTML, Word or PDF where required.

## Rendering a lesson

A lesson can be rendered from R with:

```r
rmarkdown::render(
  "01-understanding-r-and-statistical-computing.Rmd"
)
```

The exact lesson filename should be substituted where necessary.

The lecture notes support:

- HTML output;
- Microsoft Word output;
- PDF output through XeLaTeX.

## Using the solution files

Solution files are located in:

```text
solutions/
```

The recommended workflow is:

1. study the lecture explanation;
2. reproduce the examples;
3. attempt the independent exercises;
4. save the learner's own solution;
5. compare it with the corresponding guided solution;
6. investigate differences rather than merely copying the supplied code.

The solution files represent suggested approaches. R often permits several
valid solutions to the same problem.

## Reproducibility

The project uses relative paths and is intended to run from the repository root.

Avoid changing the working directory with machine-specific commands such as:

```r
setwd("C:/Users/Name/Documents/learning-r")
```

Use project-relative paths instead:

```r
student_data <- read.csv(
  file.path(
    "data",
    "student_scores.csv"
  ),
  stringsAsFactors = FALSE
)
```

Simulation lessons use:

```r
set.seed(20260714)
```

This allows pseudo-random examples to be reproduced consistently.

## Recommended learning sequence

The lessons should ordinarily be completed in numerical order because later
lessons build on objects, functions, data-management methods and analytical
principles introduced earlier.

A learner with previous R experience may use individual lessons for revision,
but the capstone assumes familiarity with the full core sequence.

## Assessment approach

The course includes:

- guided code examples;
- independent exercises;
- lesson assessments;
- completion checklists;
- reusable-function tasks;
- analytical interpretation tasks;
- and a final reproducible capstone project.

The final capstone requires the learner to integrate:

- research objectives;
- a data dictionary;
- an analysis plan;
- data validation;
- data cleaning;
- descriptive statistics;
- visualisation;
- inferential analysis;
- regression modelling;
- diagnostics;
- professional reporting;
- session information;
- and a reproducibility audit.

## Version control

The repository is suitable for Git-based version control.

A basic workflow is:

```bash
git status
git add .
git commit -m "Complete lesson exercise"
git push
```

Commit messages should describe the analytical change clearly. Examples
include:

```text
Add missing-value validation
Complete Lesson 24 descriptive analysis
Correct programme recoding rule
Add regression diagnostic report
Update capstone documentation
```

## Notes on statistical interpretation

The course distinguishes carefully between:

- association and causation;
- statistical significance and practical importance;
- standard deviation and standard error;
- confidence intervals and prediction intervals;
- exploratory and confirmatory analysis;
- independent and paired designs;
- omnibus and post hoc testing;
- model fit and predictive validation;
- unusual observations and erroneous observations.

Learners should report estimates, uncertainty, effect sizes, assumptions and
limitations rather than relying only on p-values.

## File integrity and testing

The lecture files were checked structurally for:

- unique R Markdown chunk labels;
- balanced code fences;
- consistent XeLaTeX configuration;
- and coherent repository-relative paths.

The solution files were checked structurally for balanced parentheses and
braces.

Structural checking does not replace execution in a local R environment.
Package availability, operating-system differences and local versions of R may
affect rendering or execution. Each lesson should therefore be run from a clean
R session before formal deployment.

## Course completion

The 40 core lessons and their corresponding solution files are complete.

The completed core course provides a foundation for subsequent advanced modules
in areas such as:

- generalised linear models;
- logistic regression;
- mixed-effects modelling;
- time-series analysis;
- survey analysis;
- simulation studies;
- resampling methods;
- statistical learning;
- automated reporting;
- package development;
- and production analytical workflows.

## Licence

This repository is licensed under the MIT License.
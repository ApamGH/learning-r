# Teaching Datasets

The files in this directory are **synthetic teaching datasets** developed for
the Learning R course.

They are provided solely for:

- classroom demonstrations;
- guided computer laboratory exercises;
- independent practice;
- assessment activities;
- simulation and statistical-method examples;
- and reproducible analytical workflows.

The datasets do not contain official student, patient, institutional, business
or meteorological records.

## General rules

1. Do not edit the CSV files manually.
2. Preserve the original files as raw teaching data.
3. Perform all cleaning, recoding, filtering and transformation in R code.
4. Use relative paths from the project root.
5. Save altered or derived datasets under a different filename.
6. Record every transformation so that the workflow can be reproduced.
7. Do not interpret the synthetic values as evidence about any real person,
   organisation, community or institution.

A recommended import pattern is:

```r
student_data <- read.csv(
  file.path(
    "data",
    "student_scores.csv"
  ),
  stringsAsFactors = FALSE
)
```

## Dataset inventory

### `student_scores.csv`

Synthetic student-level academic records used throughout the course.

Typical variables include:

- `student_id`: unique synthetic student identifier;
- `programme`: academic programme;
- `age`: age in completed years, where included;
- `coursework_score`: continuous assessment score;
- `examination_score`: end-of-course examination score;
- `final_score`: combined final score.

Expected teaching ranges are:

- coursework score: 0 to 40;
- examination score: 0 to 60;
- final score: 0 to 100.

In the principal teaching version:

```r
final_score ==
  coursework_score +
  examination_score
```

This arithmetic relationship is deliberate. It is used to teach:

- validation rules;
- derived variables;
- descriptive analysis;
- grouped summaries;
- visualisation;
- hypothesis testing;
- correlation;
- regression;
- diagnostics;
- and reproducible reporting.

Because `final_score` is constructed from the two assessment components,
regressing it simultaneously on both components demonstrates structural
dependence rather than a substantive predictive discovery.

### `monthly_sales.csv`

Synthetic monthly sales records for multiple branches and product categories.

Typical variables include:

- month or reporting period;
- branch;
- product category;
- units sold;
- unit price, where included;
- revenue;
- and related sales indicators.

Frequently used variables include:

- `units_sold`;
- `revenue`.

The dataset supports lessons on:

- importing tabular data;
- dates and periods;
- sorting and filtering;
- grouped summaries;
- reshaping;
- branch and product comparisons;
- time-oriented visualisation;
- and reusable reporting functions.

Before analysis, learners should verify:

```r
revenue >= 0
units_sold >= 0
```

Where a unit-price variable is available, learners may also check whether:

```r
revenue ==
  units_sold *
  unit_price
```

subject to rounding or discount rules documented in the lesson.

### `rainfall_records.csv`

Synthetic monthly rainfall observations for several communities.

Typical variables include:

- community;
- month;
- year or reporting period, where included;
- `rainfall_mm`.

The dataset contains a small number of deliberately missing values. These are
included to support lessons on:

- missing-data detection;
- complete-case analysis;
- careful use of `na.rm = TRUE`;
- grouped rainfall summaries;
- temporal ordering;
- line graphs;
- distribution assessment;
- and data-quality auditing.

Learners should not replace missing values without documenting and justifying
the method.

A basic range check is:

```r
rainfall_data$rainfall_mm >= 0
```

Negative rainfall values should be treated as invalid unless a lesson
explicitly creates them for error-detection practice.

### `clinic_visits.csv`

Synthetic outpatient visit records used for data-management, descriptive and
inferential examples.

Typical variables include:

- `patient_id`: synthetic patient identifier;
- `sex`: recorded sex category;
- `age`: age in years;
- `visit_type`: category of visit;
- `waiting_time`: time spent waiting before consultation;
- `consultation_duration`: consultation time.

The dataset supports lessons on:

- identifiers and duplicate checks;
- categorical variables and factors;
- frequency tables;
- waiting-time summaries;
- cross-tabulation;
- grouped comparisons;
- box plots and histograms;
- non-parametric procedures;
- and regression examples.

Suggested validation checks include:

```r
clinic_data$age >= 0
clinic_data$waiting_time >= 0
clinic_data$consultation_duration >= 0
```

Any upper limits should be justified by the analytical context rather than
applied automatically.

## Inspecting available files

Use:

```r
list.files(
  "data"
)
```

To inspect file properties:

```r
file.info(
  list.files(
    "data",
    full.names = TRUE
  )
)
```

## Initial inspection workflow

After importing a dataset, begin with:

```r
head(data)
tail(data)
str(data)
summary(data)
names(data)
dim(data)
```

Then examine:

```r
colSums(
  is.na(data)
)

duplicated(
  data
)
```

Identifier-specific duplicate checks should use the appropriate identifier
variable rather than testing only complete rows.

## Raw and processed data

For larger projects, the recommended structure is:

```text
data/
├── raw/
├── processed/
└── README.md
```

The original CSV files should be placed in `data/raw/`.

Cleaned or transformed datasets should be saved in `data/processed/`, for
example:

```r
write.csv(
  processed_student_data,
  file.path(
    "data",
    "processed",
    "student_scores_processed.csv"
  ),
  row.names = FALSE
)
```

## Reproducibility

Avoid machine-specific absolute paths such as:

```r
read.csv(
  "C:/Users/Name/Documents/learning-r/data/student_scores.csv"
)
```

Use project-relative paths:

```r
read.csv(
  file.path(
    "data",
    "student_scores.csv"
  ),
  stringsAsFactors = FALSE
)
```

Where random sampling, simulation or artificial missingness is introduced, set
a seed:

```r
set.seed(20260714)
```

## Teaching modifications

Some exercises may require learners to create:

- missing values;
- duplicate records;
- invalid ranges;
- inconsistent category labels;
- extreme values;
- date-format errors;
- or structurally inconsistent totals.

These modifications must be made on a copy:

```r
exercise_data <- student_data
```

Do not overwrite the source file.

## Confidentiality and ethics

Although the supplied data are synthetic, learners should practise the same
standards required for real data:

- minimise unnecessary identifiers;
- do not expose confidential information;
- document exclusions;
- retain an audit trail;
- report uncertainty honestly;
- and avoid causal claims unsupported by the design.

## Data limitations

The datasets are intentionally compact and simplified. They are designed to
make programming and statistical concepts visible.

Consequently:

- sample sizes may be smaller than those required for substantive research;
- variables may not represent all relevant confounders;
- distributions may be deliberately shaped for teaching;
- some missing values and anomalies are intentional;
- and results should not be generalised beyond the exercise.

## Adding new datasets

Any new teaching dataset added to this directory should be documented here with:

1. the filename;
2. the purpose;
3. the unit of observation;
4. the main variables;
5. valid ranges or coding rules;
6. known missing values or deliberate anomalies;
7. the lessons that use it;
8. and any confidentiality restrictions.

A corresponding data dictionary is recommended for datasets with many
variables.

## Final reminder

The data directory is part of the reproducible course environment.

\[
oxed{
	ext{preserve raw data}
ightarrow
	ext{transform in code}
ightarrow
	ext{document decisions}
ightarrow
	ext{save derived data separately}
}
\]

healthimpute: Imputation Tools for Clinical and Anthropometric Data
================

The `healthimpute` R package offers two main imputation tools for
longitudinal clinical datasets, particularly useful in natural history
studies to minimise missingness.

## 🧠 Installation

``` r
# install.packages("devtools")
devtools::install_github("alicelouisejane/healthimpute")
```

## 📦 Functions

### 1. `impute_anthro()`

Imputes missing anthropometric values—`weight`, `height`, or `BMI`—by
calculating percentiles from CDC growth charts (ages 0–20 years) using
the LMS method. Flags implausible percentiles before back-calculating
imputations. Implemented using the logic from [Li Z et al
2022](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8861671/).

**Example:**

``` r
example_data <- data.frame(
  patient_id     = c(1, 1, 1, 2, 2),
  sex            = c(1,   1,   1,   2,   2),
  age_at_visit   = c(5,   6,   7,   5,   7),
  weight         = c(NA,  22,   NA,  18,   NA)
)

#Example 1
result <- impute_anthro(
  example_data,
  variable          = "weight",
  patient_id_col    = "patient_id",
  age_col           = "age_at_visit",
  sex_col           = "sex"
)

#Example 2
#Use only on those under 20 years old
result2 <- dplyr::filter(example_data,age_at_visit<=20) %>%
  impute_anthro(
  .,
  variable          = "weight",
  patient_id_col    = "patient_id",
  age_col           = "age_at_visit",
  sex_col           = "sex"
)
```

------------------------------------------------------------------------

### 2. `impute_timewindow()`

Carries the nearest available non-missing value forward or backward in
time within a customizable window (default: 30 days). Can be implemented
for anthropometric data in adults - recommended in those older than 20
years (see impute_anthro() for those 20 and under) and other clinical
measures such as HbA1c or insulin dose. It is not recommended to exceed
the imputation time window of 30 days or use for anthopometric
imputation in those under 20 years old.

The dy_col parameter indicates the name of the time column which can be
in the format of either days from some defined baseline or date. The
is_date parameter enables you to define the structure of this variable.
If TRUE, the `dy_col` is a `Date` object and will be converted to
numeric days. If FALSE, assumes `dy_col` is already numeric. Default is
FALSE.

**Example:**

``` r
example_data <- data.frame(
  id    = c(1, 1, 1, 2, 2),
  dy    = c(0, 15, 45, 0,  20),
  hba1c = c(7.2, NA, 7.4, NA, 6.9)
)

#Example 1
result <- impute_timewindow(
  example_data,
  variable          = "hba1c",
  patient_id_col    = "id",
  dy_col            = "dy",
  is_date           = FALSE
)

#Example 2
#Can be used on anthropometric data however should only be used on those over 20 years old.
# Use impute_anthro() for those 20 and under - see above. 

result2 <-  dyplr::filter(example_data,age_at_visit>20) %>%
  impute_timewindow(
  .,
  variable          = "weight",
  patient_id_col    = "id",
  dy_col            = "dy",
  is_date           = FALSE
)
```

------------------------------------------------------------------------

### 3. `flag_anthro()`

Flags biologically implausible anthropometric values—weight, height, or
BMI—across the full age spectrum. This function is intended for data
quality checks and does not impute missing values.

For individuals under 20 years of age, LMS-based z-scores and
percentiles are calculated using CDC growth chart reference data. Values
are flagged as implausible if the absolute z-score exceeds 4. For
individuals over 20 years, fixed clinical thresholds are applied: BMI is
flagged if \< 14 or \> 50, weight if \< 30 kg or \> 180 kg, and height
if \< 120 cm or \> 210 cm.

This function is intended to be used alongside impute_anthro() and
impute_timewindow() to support robust pre-processing of clinical and
anthropometric data.

**Example:**

``` r
example_data <- data.frame(
  patient_id     = c(1, 1, 2, 2),
  sex            = c("M", "M", "F", "F"),
  age_at_visit   = c(10, 25, 16, 30),
  weight         = c(28, 200, 35, 29)
)

result <- flag_anthro(
  data             = example_data,
  variable         = "weight",
  patient_id_col   = "patient_id",
  age_col          = "age_at_visit",
  sex_col          = "sex"
)
```

------------------------------------------------------------------------

## 📚 References and Acknowledgements

- Li Z, Toppari J, Lundgren M, Frohnert BI, Achenbach P, Veijola R,
  Anand V; T1DI Study Group. *Imputing Longitudinal Growth Data in
  International Pediatric Studies: Does CDC Reference Suffice?* AMIA
  Annu Symp Proc. 2021;2021:754–762. PMID: 35308906. PMCID:
  [PMC8861671](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8861671/).

- Kimberly Collins, Critical Path Institute for preliminary time window
  imputation idea.

------------------------------------------------------------------------

## 🛡️ License

MIT © 2025 Alice Carr  
Contact: <acarr1@ualberta.ca>

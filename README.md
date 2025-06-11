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

result <- impute_anthro(
  example_data,
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
for anthropometric data in adults older than 20 years (see
impute_anthro()) and other clinical measures such as HbA1c or insulin
dose. It is not recommended to exceed the imputation time window of 30
days.

The dy_col parameter indicates the name of the time column which can be
in the format of either days from some defined baseline or date. The
is_date parameter enables you to define the sruture of this variable. If
TRUE, the `dy_col` is a `Date` object and will be converted to numeric
days. If FALSE, assumes `dy_col` is already numeric. Default is FALSE.

**Example:**

``` r
example_data <- data.frame(
  id    = c(1, 1, 1, 2, 2),
  dy    = c(0, 15, 45, 0,  20),
  hba1c = c(7.2, NA, 7.4, NA, 6.9)
)

result <- impute_timewindow(
  example_data,
  variable          = "hba1c",
  patient_id_col    = "id",
  dy_col            = "dy",
  is_date           = FALSE
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

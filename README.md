# mlexperiments

<!-- badges: start -->
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R build status](https://github.com/kapsner/mlexperiments/workflows/R%20CMD%20Check%20via%20{tic}/badge.svg?branch=main)](https://github.com/kapsner/mlexperiments/actions)
[![R build status](https://github.com/kapsner/mlexperiments/workflows/lint/badge.svg?branch=main)](https://github.com/kapsner/mlexperiments/actions)
[![R build status](https://github.com/kapsner/mlexperiments/workflows/test-coverage/badge.svg?branch=main)](https://github.com/kapsner/mlexperiments/actions)
[![codecov](https://codecov.io/gh/kapsner/mlexperiments/branch/main/graph/badge.svg?branch=main)](https://app.codecov.io/gh/kapsner/mlexperiments)
<!-- badges: end -->

The goal of the package `mlexperiments` is to provide an extensible framework for reproducible machine learning experiments, namely:

* Hyperparameter tuning: with the R6 class `mlexperiments::MLTuneParameters`, to optimize the hyperparameters in a k-fold cross-validation with one of the two strategies
  + Grid search
  + Bayesian optimization (using the [`ParBayesianOptimization`](https://github.com/AnotherSamWilson/ParBayesianOptimization) R package)
* K-fold Cross-validation (CV): with the R6 class `mlexperiments::MLCrossValidation`, to validate one hyperparameter setting
* Nested k-fold cross validation: with the R6 class `mlexperiments::MLNestedCV`, which basically combines the two experiments above to perform a hyperparameter optimization on an inner CV loop, and to validate the best hyperparameter setting on an outer CV loop

The package provides a minimal shell for these experiments, and - with few adjustments - users can prepare different learner algorithms so that they can be used with `mlexperiments`.

## Installation

To install the development version, run

```r
install.packages("remotes")
remotes::install_github("kapsner/mlexperiments")
```

## Examples

### Preparations

First of all, load the data and transform it into a matrix, and define the training data and the target variable.

```{r}
library(mlexperiments)
library(mlbench)

data("DNA")
dataset <- DNA |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[1:180]

train_x <- model.matrix(
  ~ -1 + .,
  dataset[, .SD, .SDcols = feature_cols]
)
train_y <- dataset[, get("Class")]

ncores <- ifelse(
  test = parallel::detectCores() > 4,
  yes = 4L,
  no = ifelse(
    test = parallel::detectCores() < 2L,
    yes = 1L,
    no = parallel::detectCores()
  )
)
if (isTRUE(as.logical(Sys.getenv("_R_CHECK_LIMIT_CORES_")))) {
  # on cran
  ncores <- 2L
}
```

### Hyperparameter Tuning

#### Bayesian Tuning

For the Bayesian hyperparameter optimization, it is required to define a grid with some hyperparameter combinations that is used for initializing the Bayesian process. Furthermore, the borders (allowed extreme values) of the hyperparameters that are actually optimized need to be defined in a list. Finally, further arguments that are passed to the function `ParBayesianOptimization::bayesOpt()` can be defined as well.

```{r}
param_list_knn <- expand.grid(
  k = seq(4, 68, 8),
  l = 0,
  test = parse(text = "fold_test$x")
)

knn_bounds <- list(k = c(2L, 80L))

optim_args <- list(
  iters.n = ncores,
  kappa = 3.5,
  acq = "ucb"
)
```

Then, the created objects need to be assigned to the corresponding fields of the R6 class `mlexperiments::MLTuneParameters`:

```{r}
knn_tune_bayesian <- mlexperiments::MLTuneParameters$new(
  learner = LearnerKnn$new(),
  strategy = "bayesian",
  ncores = ncores,
  seed = seed
)

knn_tune_bayesian$parameter_bounds <- knn_bounds
knn_tune_bayesian$parameter_grid <- param_list_knn
knn_tune_bayesian$split_type <- "stratified"
knn_tune_bayesian$optim_args <- optim_args

# set data
knn_tune_bayesian$set_data(
  x = train_x,
  y = train_y
)

results <- knn_tune_bayesian$execute(k = 3)
head(results)
#>    Epoch setting_id  k gpUtility acqOptimum inBounds Elapsed      Score metric_optim_mean errorMessage l
#> 1:     0          1  4        NA      FALSE     TRUE   2.009 -0.2247332         0.2247332           NA 0
#> 2:     0          2 12        NA      FALSE     TRUE   2.273 -0.1600753         0.1600753           NA 0
#> 3:     0          3 20        NA      FALSE     TRUE   2.376 -0.1381042         0.1381042           NA 0
#> 4:     0          4 28        NA      FALSE     TRUE   2.323 -0.1403013         0.1403013           NA 0
#> 5:     0          5 36        NA      FALSE     TRUE   2.128 -0.1315129         0.1315129           NA 0
#> 6:     0          6 44        NA      FALSE     TRUE   2.339 -0.1258632         0.1258632           NA 0
```

#### Grid Search

To carry out the hyperparameter optimization with a grid search, only the `parameter_grid` is required:

```{r}
knn_tune_grid <- mlexperiments::MLTuneParameters$new(
  learner = LearnerKnn$new(),
  strategy = "grid",
  ncores = ncores,
  seed = seed
)

knn_tune_grid$parameter_grid <- param_list_knn
knn_tune_grid$split_type <- "stratified"

# set data
knn_tune_grid$set_data(
  x = train_x,
  y = train_y
)

results <- knn_tune_grid$execute(k = 3)
head(results)
#>    setting_id metric_optim_mean  k l
#> 1:          1         0.2187696  4 0
#> 2:          2         0.1597615 12 0
#> 3:          3         0.1349655 20 0
#> 4:          4         0.1406152 28 0
#> 5:          5         0.1318267 36 0
#> 6:          6         0.1258632 44 0
```

### Cross-Validation

For the cross-validation experiments (`mlexperiments::MLCrossValidation`, and `mlexperiments::MLNestedCV`), a named list with the in-sample row indices of the folds is required.

```{r}
fold_list <- splitTools::create_folds(
  y = train_y,
  k = 3,
  type = "stratified",
  seed = seed
)
str(fold_list)
#> List of 3
#>  $ Fold1: int [1:2124] 1 2 3 4 5 7 9 10 11 12 ...
#>  $ Fold2: int [1:2124] 1 2 3 6 8 9 11 13 16 17 ...
#>  $ Fold3: int [1:2124] 4 5 6 7 8 10 12 14 15 16 ...
```

Furthermore, a specific hyperparameter setting needs to be selected in order to validate it with the cross-validation:

```{r}
knn_cv <- mlexperiments::MLCrossValidation$new(
  learner = LearnerKnn$new(),
  fold_list = fold_list,
  seed = seed
)

best_grid_result <- knn_tune_grid$results$best.setting
best_grid_result

knn_cv$learner_args <- best_grid_result[-1]

knn_cv$predict_args <- list(type = "response")
knn_cv$performance_metric <- metric("bacc")
knn_cv$return_models <- TRUE

# set data
knn_cv$set_data(
  x = train_x,
  y = train_y
)

results <- knn_cv$execute()
head(results)
#>     fold performance  k l
#> 1: Fold1   0.8912781 68 0
#> 2: Fold2   0.8832388 68 0
#> 3: Fold3   0.8657147 68 0
```

### Nested Cross-Validation

Last but not least, the hyperparameter optimization and validation can be combined in a nested cross-validation. In each fold of the so-called "outer" cross-validation loop, the hyperparameters are optimized on the in-sample observations with one of the two strategies (Bayesian optimization or grid search, both implemented with an "inner" cross-validation), and a model with the as such identified best hyperparameter setting is then fitted on all in-sample observations of the outer cross-validation loop and validated on the respective out-sample observations.

The experiment classes must be parameterized as described above.

#### Inner Bayesian Optimization

```{r}
knn_cv_nested_bayesian <- mlexperiments::MLNestedCV$new(
  learner = LearnerKnn$new(),
  strategy = "bayesian",
  fold_list = fold_list,
  k_tuning = 3L,
  ncores = ncores,
  seed = seed
)

knn_cv_nested_bayesian$parameter_grid <- param_list_knn
knn_cv_nested_bayesian$parameter_bounds <- knn_bounds
knn_cv_nested_bayesian$split_type <- "stratified"
knn_cv_nested_bayesian$optim_args <- optim_args

knn_cv_nested_bayesian$predict_args <- list(type = "response")
knn_cv_nested_bayesian$performance_metric <- metric("bacc")

# set data
knn_cv_nested_bayesian$set_data(
  x = train_x,
  y = train_y
)

results <- knn_cv_nested_bayesian$execute()
head(results)
#>     fold performance  k l
#> 1: Fold1   0.8912781 68 0
#> 2: Fold2   0.8832388 68 0
#> 3: Fold3   0.8657147 68 0
```

#### Inner Grid Search

```{r}
knn_cv_nested_grid <- mlexperiments::MLNestedCV$new(
  learner = LearnerKnn$new(),
  strategy = "grid",
  fold_list = fold_list,
  k_tuning = 3L,
  ncores = ncores,
  seed = seed
)

knn_cv_nested_grid$parameter_grid <- param_list_knn
knn_cv_nested_grid$split_type <- "stratified"

knn_cv_nested_grid$predict_args <- list(type = "response")
knn_cv_nested_grid$performance_metric <- metric("bacc")

# set data
knn_cv_nested_grid$set_data(
  x = train_x,
  y = train_y
)

results <- knn_cv_nested_grid$execute()
head(results)
#>     fold performance  k l
#> 1: Fold1   0.8959736 52 0
#> 2: Fold2   0.8832388 68 0
#> 3: Fold3   0.8657147 68 0
```

## Purpose

The `mlexperiments` package aims at providing as much flexibility as possible while being able to perform the machine learning experiments with different learner algorithms using a common interface. The use of a common interface ensures, for example, the comparability of experiments that were performed with different learner algorithms, since they use the same underlying code for computing cross-validation folds, etc. Furthermore, the common interface also allows to quickly exchange the learner algorithms.

When developing the package, a goal was always to leave as much flexibility as possible to the users when calling the different learner algorithms. This includes, for example, the necessity to provide certain learner-specific arguments to their fitting-functions or predict-functions (for example, some `xgboost`- or `lightgbm` users prefer to use `early_stopping` during the cross-validation while others like to optimize the number of boosting iterations in a grid search).
Thus, it was decided wherever possible to not hard-code learner-specific arguments. Instead, some general fields were added to the R6 classes of the experiments to be able to pass such arguments, e.g., to the learners' fitting-functions and predict-functions, respectively.

This flexibility might come at the expense of intuitive usability as users first need to define their `mlexperiments`-specific learner functions according to their needs. However, for users who did not use the R language's well established machine learning frameworks in their experiments (e.g. [`tidymodels`](https://www.tidymodels.org/), [`caret`](https://topepo.github.io/caret/), and [`mlr3`](https://mlr3.mlr-org.com/)), this might not be such a big change at all as they previously might have been already writing code

* to perform a hyperparameter tuning (using a grid-search or even a Bayesian optimization)
* to validate a set of hyperparameters using a resampling strategy (e.g., a k-fold cross-validation)
* to fit a model with some training data
* to apply a fitted model to predict the outcome in before unseen data

The `mlexperiments` R package provides a standardized interface to define these steps inside of R functions by making some restrictions on the inputs and outputs of these functions.

Some basic learners are included into the `mlexperiments` package, mainly to provide a set of baseline learners that can be used for comparison throughout experiments (e.g., wrappers for `stats::lm()` and `stats::glm()`). Some more learners are prepared for the use with `mlexperiments` in the R package [`mllrnrs`](https://github.com/kapsner/mllrnrs). Generally, the flexibility of the `mlexperiments` package implies that users have a deeper understanding of the algorithms they use, including the hyperparameters that can be optimized.

However, `mlexperiments` aims not at providing a ready-to-use interface for many learner algorithms. Instead, users are encouraged to prepare the algorithms they want to use with `mlexperiments` according to their tasks, needs, experience, and personal preferences.

Details on how to prepare an algorithm for use with `mlexperiments` can be found in the [package vignette](vignettes/mlexperiments_starter.Rmd).
Users that want to use a new algorithm with `mlexperiments` are also encouraged to dive into the available implementations, especially [`LearnerKnn`](R/LearnerKnn.R) and [`LearnerRpart`](R/LearnerRpart.R), in order to get an understanding of the functioning and the flexibility of the framework.

## Background

The idea for this package was born when working on the project work for my Medical Data Science Certificate study program. I wanted to apply different machine learning algorithms to survival data and couldn't find a framework for machine learning experiments to analyze survival data with the algorithms `xgboost`, `glmnet` and `ranger`. While all of the three big frameworks for machine learning in R, [`tidymodels`](https://www.tidymodels.org/), [`caret`](https://topepo.github.io/caret/), and [`mlr3`](https://mlr3.mlr-org.com/), allow to perform hyperparameter tuning and (nested) cross validation, none of those frameworks had implemented stable interfaces for all of these three algorithms that could be executed on survival data at the time of starting with the project work (end of April 2022).
For [`tidymodels`](https://www.tidymodels.org/), the add-on package [`cencored`](https://censored.tidymodels.org/) addresses survival analysis, but only supported the `glmnet` algorithm in April 2022.
For [`mlr3`](https://mlr3.mlr-org.com/), the add-on package [`mlr3proba`](https://github.com/mlr-org/mlr3proba) addresses survival analysis, with lots of learners capable to conduct survival analysis available with the package [`mlr3learners`](https://mlr3extralearners.mlr-org.com/articles/learners/test_overview.html), including implementations for all of the three algorithms I wanted to use.
In contrast, the developer and maintainer of [`caret`](https://topepo.github.io/caret/) stated in a [comment on GitHub](https://github.com/topepo/caret/issues/959) that all efforts regarding survival analysis will be made in its successor framework, [`tidymodels`](https://www.tidymodels.org/).
Thus, I initially decided to implement my analysis with [`mlr3`](https://mlr3.mlr-org.com/) / [`mlr3proba`](https://github.com/mlr-org/mlr3proba).
However, when actually starting to implement things, I realized that in the meantime [`mlr3proba`](https://github.com/mlr-org/mlr3proba) has unfortunately been [archived on CRAN on 2022-05-16](https://cran.r-project.org/web/packages/mlr3proba/index.html).
For the sake of stability throughout the project work, I finally decided to implement the whole logic myself as it "just includes some for loops and summarizing results" :joy: :joy:.
In the end, implementing a common interface for the three algorithms to perform survival analysis was a very time-consuming effort.
This was even more the case when trying to make the code as generic and re-usable as possible, to generalize it to tasks other than survival analysis, as well as to allow for adding (potentially) any other learner.

The result of these efforts are:

* the [`mlexperiments`](https://github.com/kapsner/mlexperiments) R package, providing
  + R6 classes to perform the machine learning experiments (hyperparameter tuning, cross-validation, and nested cross-validation)
  + some base learners (`LearnerLm`, `LearnerGlm`, `LearnerRpart`, and `LearnerKnn`)
  + an R6 class to inherit new learners from (`MLLearnerBase`)
  + as well as functions
    - to validate the equality of folds used between different experiments (`mlexperiments::validate_fold_equality()`)
    - to apply learners to new data and predict the outcome (`mlexperiments::predictions()`)
    - to calculate performance measures with these predictions (`mlexperiments::performance()`)
    - and a utility function to select performance metrics from the [`mlr3measures`](https://cran.r-project.org/web/packages/mlr3measures/index.html) R package
* the [`mllrnrs`](https://github.com/kapsner/mllrnrs) R package, which enhances `mlexperiments` with some learner wrappers for algorithms I commonly use. They were separated into their own package in order to reduce overall maintenance load and to avoid having lots of dependencies in the [`mlexperiments`](https://github.com/kapsner/mlexperiments) R package. Implemented learners are
  + LearnerSurvCoxPHCox
  + LearnerSurvGlmnetCox
  + LearnerSurvRangerCox
  + LearnerSurvRpartCox
  + LearnerSurvXgboostCox
  + LearnerGlmnet
  + LearnerXgboost
  + LearnerLightgbm
  + LearnerRanger

## Backlog

- cat_vars

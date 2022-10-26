# mlexperiments

<!-- badges: start -->
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R build status](https://github.com/kapsner/mlexperiments/workflows/R%20CMD%20Check%20via%20{tic}/badge.svg?branch=main)](https://github.com/kapsner/mlexperiments/actions)
[![R build status](https://github.com/kapsner/mlexperiments/workflows/lint/badge.svg?branch=main)](https://github.com/kapsner/mlexperiments/actions)
[![R build status](https://github.com/kapsner/mlexperiments/workflows/test-coverage/badge.svg?branch=main)](https://github.com/kapsner/mlexperiments/actions)
[![codecov](https://codecov.io/gh/kapsner/mlexperiments/branch/main/graph/badge.svg?branch=main)](https://app.codecov.io/gh/kapsner/mlexperiments)
<!-- badges: end -->

The goal of the package `mlexperiments` is to provide a re-useable framework for reproducible machine learning experiments, namely:

* Hyperparameter tuning: with the R6 class `mlexperiments::MLTuneParameters`, to optimize hyperparameters with one of the two strategies
  + Grid search
  + Bayesian optimization (using the [`ParBayesianOptimization`](https://github.com/AnotherSamWilson/ParBayesianOptimization) R package)
* K-fold Cross-validation (CV): with the R6 class `mlexperiments::MLCrossValidation` to validate a set of hyperparameters
* Nested k-fold cross validation: with the R6 class `mlexperiments::MLNestedCV` to perform an inner CV to optimize the hyperparameters and validate them in an outer CV loop

The package follows the principle that it merely wants to provide a minimal shell for these experiments, and - with few adjustments - users can prepare different algorithms so that they can be used with `mlexperiments`. Details on how to prepare an algorithm for use with `mlexperiments` can be found in the [package vignette](vignettes/mlexperiments_starter.Rmd).

## Installation

To install the development version, run

```r
install.packages("remotes")
remotes::install_github("kapsner/mlexperiments")
```

## Example


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
  + some base learners (`LearnerLm`, `LearnerGlm`, and `LearnerKnn`)
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
  + LearnerSurvXgboostCox
  + LearnerXgboost
  + LearnerLightgbm

## Backlog

- rpart

Document features:
- learner args as expression "parse(text = )"
- bayesian_scoring_function -> if metric_optimization_higher_better = FALSE --> inversion of Score is taken care of


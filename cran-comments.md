## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.  
* Title and Description have been revised according to suggestions.  
* Wherever possible, "\dontrun"-examples have been unwrapped. Where this was not possible, comments have been added for clarity.  
* Tests have been revised to run on onlyl 2 cores when environment variable '_R_CHECK_LIMIT_CORES_' equals `TRUE` (tests/testthat/test-knn.R; tests/testthat/test-rpart_classification.R; tests/testthat/test-rpart_regression.R)

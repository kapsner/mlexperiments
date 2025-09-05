## R CMD check results

0 errors | 0 warnings | 0 notes

- this update replaces the previously suggested dependency 'mlr3measures' 
  with 'measures' as some functions have been deprecated in 'mlr3measures'
  (see also https://github.com/kapsner/mlexperiments/issues/1)
- this leads to a breaking change, affecting the downstream package 'mllrnrs'
- as I am also the maintainer of 'mllrnrs', I have already fixed the issues
  introduced by this new CRAN-submission; the revised version of 'mllrnrs' will
  be submitted to CRAN as soon as 'mlexperiments' v0.0.6 has been released on 
  CRAN

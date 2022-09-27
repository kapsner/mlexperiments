# rpkgTemplate

<!-- badges: start -->
[![R build status](https://github.com/kapsner/rpkgTemplate/workflows/R%20CMD%20Check%20via%20{tic}/badge.svg)](https://github.com/kapsner/rpkgTemplate/actions)
[![R build status](https://github.com/kapsner/rpkgTemplate/workflows/lint/badge.svg)](https://github.com/kapsner/rpkgTemplate/actions)
[![R build status](https://github.com/kapsner/rpkgTemplate/workflows/test-coverage/badge.svg)](https://github.com/kapsner/rpkgTemplate/actions)
<!-- badges: end -->

The goal of rpkgTemplate is to provide a minimal template for R package development that includes a

- setup for [GitHub actions](.github/workflows)
- [linter configuration](.lintr)
- a file for reproducible updates of the package setup: [devstuffs.R](./data-raw/devstuffs.R)

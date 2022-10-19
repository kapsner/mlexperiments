# mlexperiments NEWS

## Unreleased (2022-10-19)

#### New features

-   added validation function for fold equality
    ([54a1ec6](https://github.com/kapsner/mlexperiments/tree/54a1ec643052ce650efad85ad303c8a97708371e))
-   added functions for predictions and preformance calculations on
    cv-models
    ([c4f5287](https://github.com/kapsner/mlexperiments/tree/c4f5287512e3b6136c66145adb2682f9e975320c))
-   finalized implementation of knn
    ([70697fa](https://github.com/kapsner/mlexperiments/tree/70697fa0dd74d7043b0f8fe5dfe3c0f7be391be2))
-   implemented knn grid / nested grid
    ([5e1d910](https://github.com/kapsner/mlexperiments/tree/5e1d910d1e1f008b1dda1382d08138ec787f6908))
-   working on implementing knn
    ([5932e6c](https://github.com/kapsner/mlexperiments/tree/5932e6c18a3659dfb61e2c3c3dea9bb00ffd387a))
-   added coxph; working on unit tests
    ([03daecd](https://github.com/kapsner/mlexperiments/tree/03daecdc3d1e15ae64a25fad984b2be6c5d11149))
-   working on xgboost implementation (wip)
    ([c8db3b7](https://github.com/kapsner/mlexperiments/tree/c8db3b75e0a903cc2f9420de87349f654511172c))
-   add xgboost surv
    ([415be47](https://github.com/kapsner/mlexperiments/tree/415be47895feb361504aac3042fa9f277d87cd5b))
-   implemented nested cv
    ([47e0669](https://github.com/kapsner/mlexperiments/tree/47e066915550e1b58ac5408fe841539245de92cf))
-   initial commits
    ([07f39da](https://github.com/kapsner/mlexperiments/tree/07f39da254f6ae51248361e29b3c42888c1f2325))

#### Bug fixes

-   fixed buc in bayesian scoring function
    ([a1214e1](https://github.com/kapsner/mlexperiments/tree/a1214e1dbd4af11b8954fa8ff0d86c44ba01de03))
-   fixed general parsing of parameters
    ([015d5f6](https://github.com/kapsner/mlexperiments/tree/015d5f6bc26272b3bbd22430db4599781cc1dcc5))
-   removing duplicates by name
    ([01be901](https://github.com/kapsner/mlexperiments/tree/01be9011fb9e8437737f16d7f9e842fff892e120))
-   fixed issue when selecting parameters
    ([4b6d974](https://github.com/kapsner/mlexperiments/tree/4b6d9747f105fc509ce86348e1f5f4149f7b1e00))
-   fixed issue with prediction in ranger
    ([b71f4f8](https://github.com/kapsner/mlexperiments/tree/b71f4f809d75c32b2b740839ae9ff180fc27013c))
-   fixed some issues; finished xgboost surv implementation
    ([576066e](https://github.com/kapsner/mlexperiments/tree/576066e47895dbf9fd9f3c37378c300f3eff77e9))
-   fixed last issue with seed in grid-search
    ([cc821d6](https://github.com/kapsner/mlexperiments/tree/cc821d6f1c2b4364e19a92d7277fcda5fbfc7c95))
-   fixed issue related to parallel backend in bayesian tuning
    ([2afba3f](https://github.com/kapsner/mlexperiments/tree/2afba3ff6c1407d8b87942662bb14e48a4497366))
-   removed duplicate class names and fixed related issues
    ([9ca9385](https://github.com/kapsner/mlexperiments/tree/9ca9385167841ccfea490e05f42e94f07f757cac))

#### Refactorings

-   updated code to upstream changes
    ([54766cc](https://github.com/kapsner/mlexperiments/tree/54766cc8b63c84b74170ac3a629ddf0f82cd7da2))
-   learner to self
    ([263ae51](https://github.com/kapsner/mlexperiments/tree/263ae51b365f4db79f1b006052d2dfdf0f7d8570))
-   now relying on list.append from kdry
    ([d9f6f67](https://github.com/kapsner/mlexperiments/tree/d9f6f679139de06dce34d4d3d4f0d168030a1061))
-   metric-performance to cv-class now
    ([d4209e5](https://github.com/kapsner/mlexperiments/tree/d4209e5aad59ad46cc9297e29f9a1e5ca20be493))
-   removed metric\_perf\_h\_b
    ([e63b3da](https://github.com/kapsner/mlexperiments/tree/e63b3da8e465ff73f1a136c5a6a869789e9ff679))
-   optmizing code quality
    ([401bdf1](https://github.com/kapsner/mlexperiments/tree/401bdf14328bb4825b5030a7420559f1cb7e4e1a))
-   now using of learner\_args is also possible
    ([0ce1f6c](https://github.com/kapsner/mlexperiments/tree/0ce1f6c92f503779bf232bfd3cac5a38bee63e60))
-   working on making code more straightforward
    ([c697e5f](https://github.com/kapsner/mlexperiments/tree/c697e5f49e2641ecaad6cf5ea0498056b2a73d00))
-   learners to mllrnrs package
    ([b1ee0da](https://github.com/kapsner/mlexperiments/tree/b1ee0da24fb221e3f73359912ed6da7366309401))
-   reorgainzed code to make use of more inheritance
    ([772bba0](https://github.com/kapsner/mlexperiments/tree/772bba0cadd439f65fbf04ede5899948acc5b53c))

#### CI

-   fixed linting error
    ([35190b6](https://github.com/kapsner/mlexperiments/tree/35190b62b45ec9773efd922dd9e30485cc5dbff9))
-   solve warning for missing global variables
    ([771a3f1](https://github.com/kapsner/mlexperiments/tree/771a3f10dbf93037933c229948e7743fca52ae0b))
-   fixed test to comply with cran checks
    ([8026711](https://github.com/kapsner/mlexperiments/tree/8026711b9f6f59534cabd51fc60d8dae63c0f45e))
-   refactored unit tests
    ([d003088](https://github.com/kapsner/mlexperiments/tree/d003088c9d19dda872f9e698d9cbcb9f89cb6e95))
-   fixed linting errors
    ([a3d3eb0](https://github.com/kapsner/mlexperiments/tree/a3d3eb0887db86a97b17a3d0b07b90c587477a5a))

#### Docs

-   finished most of the package documentation
    ([21102f0](https://github.com/kapsner/mlexperiments/tree/21102f08051ce4a80f4fa5ea8d2154247fc89d50))
-   finished documenting all r6 classes
    ([d06d056](https://github.com/kapsner/mlexperiments/tree/d06d05646f8eb6e92e32cbc4935e9da1e905e2aa))
-   working on documenting r6 classes
    ([eba4a91](https://github.com/kapsner/mlexperiments/tree/eba4a91d193190450049b2e18e59a90839846dbd))
-   working on class documentation
    ([77e06c8](https://github.com/kapsner/mlexperiments/tree/77e06c89bb0f63202231bd0111de13e53ec8bbb1))
-   started working on documentation
    ([081ac26](https://github.com/kapsner/mlexperiments/tree/081ac26d999b3e809984bf0fd3d4064a4a0dc398))

#### Other changes

-   adaptions to upstream changes
    ([294e9f3](https://github.com/kapsner/mlexperiments/tree/294e9f3cd19fdfdd7e9febdd87f1421cb8d7af39))
-   updated readme, added lifecycle badge
    ([94705eb](https://github.com/kapsner/mlexperiments/tree/94705eb1dbcaf7ae32424b6fc1ad961f6c8d78d5))
-   updated news.md
    ([575266e](https://github.com/kapsner/mlexperiments/tree/575266e62417fe6f457fe05980c95f9ecf9e7f31))
-   reformating message
    ([74fed90](https://github.com/kapsner/mlexperiments/tree/74fed90ab55e229cbd77a5941c1de512a5605e11))
-   updated readme
    ([5fc4a0a](https://github.com/kapsner/mlexperiments/tree/5fc4a0a9c24e719bbe2bda0584b86e80c13b15cc))

Full set of changes:
[`edb76c3...21102f0`](https://github.com/kapsner/mlexperiments/compare/edb76c3...21102f0)

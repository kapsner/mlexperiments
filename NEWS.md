# mlexperiments NEWS

## v0.0.2 (2023-06-09)

#### Bug fixes

-   fixed issues for cran submission
    ([273c1de](https://github.com/kapsner/mlexperiments/tree/273c1de3b25777b9cf08bc2001b5ccd5099748b8))
-   removed splittools from remotes
    ([5b501cc](https://github.com/kapsner/mlexperiments/tree/5b501cc7b9eb1a8940a4c1ed3026cea43cf28b97))
-   removed splittools from remotes
    ([56fa700](https://github.com/kapsner/mlexperiments/tree/56fa700905f74010690dead9d82cf311031b6e39))
-   fixing cat\_vars stuff
    ([0a13083](https://github.com/kapsner/mlexperiments/tree/0a1308355b895e3d6032058172aa5e098ce5e4da))
-   fixed issues with cat\_vars
    ([94f60c4](https://github.com/kapsner/mlexperiments/tree/94f60c49574f51ae1a94b17997868f1c01a58a51))

#### Refactorings

-   preparing cran submission
    ([7af8517](https://github.com/kapsner/mlexperiments/tree/7af8517fcccc050ee08d8f635d163503110fc122))

#### CI

-   removed unnecessary install
    ([6da60ce](https://github.com/kapsner/mlexperiments/tree/6da60ce9ee9c9c01515a526b5d2869987b8a22e5))
-   updated ci
    ([30ba875](https://github.com/kapsner/mlexperiments/tree/30ba8758e7059e78654f9e6db8bd7ec1c1727df4))

#### Docs

-   removed dontrun from roxygen examples of mllearnerbase
    ([a9d0a77](https://github.com/kapsner/mlexperiments/tree/a9d0a776083dbc6168fcad23c37bc27ea8e2102d))
-   updated vignettes
    ([b7df96e](https://github.com/kapsner/mlexperiments/tree/b7df96ecd6c824182e75f916c367e7cad2e69891))
-   added png plots for vignettes
    ([73a66d9](https://github.com/kapsner/mlexperiments/tree/73a66d965a402448b26174ba210bfeadb902ad53))
-   fixed invalid url in vignette
    ([54546f1](https://github.com/kapsner/mlexperiments/tree/54546f13f9ab59af257293fa40fac587b53779dc))
-   vignettes now static
    ([e870ee4](https://github.com/kapsner/mlexperiments/tree/e870ee4504a791c11f6b1f9caf42d110962f9b64))

#### Other changes

-   fixed spelling and examples
    ([dd99be3](https://github.com/kapsner/mlexperiments/tree/dd99be3257992041885df17c3295449932d87500))
-   preparing v0.0.2
    ([0f7cc35](https://github.com/kapsner/mlexperiments/tree/0f7cc35f061b96681a8fcec403ca5bac96c4f328))
-   updating version
    ([d3dcadf](https://github.com/kapsner/mlexperiments/tree/d3dcadfeca949f68fdaf32a1b244b5d81744f886))
-   working on optimizations to reduce building time and unit test run
    time
    ([2346fcd](https://github.com/kapsner/mlexperiments/tree/2346fcd043f70ea2d0c52326394cdd16b8d4c882))
-   working on making vignettes static
    ([e8ddc09](https://github.com/kapsner/mlexperiments/tree/e8ddc09da12389dbb129702727f3063cc7dfef96))
-   further working on more informative stopifnot statements
    ([9e9b0c1](https://github.com/kapsner/mlexperiments/tree/9e9b0c190949c88f89e782c74bbbb268f2c7dbf7))
-   working on more informative stopifnot messages
    ([408d248](https://github.com/kapsner/mlexperiments/tree/408d248c9215d29f9da179c03b9877078c485ae3))
-   fixed issue with news.md
    ([b4bf1fb](https://github.com/kapsner/mlexperiments/tree/b4bf1fb2461a4a6216c50414ce539315376d575e))
-   removed remotes packages!
    ([b8fdc5a](https://github.com/kapsner/mlexperiments/tree/b8fdc5a9f36a6b82183c817e0128883302d8ead1))
-   catch missing ncores argument in learner’s predict function
    ([b54bd79](https://github.com/kapsner/mlexperiments/tree/b54bd79eea5239d959cc255a8c812152e3abd7a5))
-   fixed typo
    ([d0224f2](https://github.com/kapsner/mlexperiments/tree/d0224f2855f2df987affef807e0cc3e154868e8a))
-   updated docs on mlsurvlrnrs
    ([0a49a4b](https://github.com/kapsner/mlexperiments/tree/0a49a4b58e79a75fc412839b52cb500dafd2be80))
-   fixed canoncial form of cran url in vignette
    ([71818db](https://github.com/kapsner/mlexperiments/tree/71818dbbadd85ccba775f6b66ad299875acaab69))
-   updated news.md
    ([0c799d3](https://github.com/kapsner/mlexperiments/tree/0c799d323a64f1c67a53e073c20aef61c975ffac))

Full set of changes:
[`v0.0.1...v0.0.2`](https://github.com/kapsner/mlexperiments/compare/v0.0.1...v0.0.2)

## v0.0.1 (2022-11-10)

#### Breaking changes

-   removed performance metric name
    ([01e4230](https://github.com/kapsner/mlexperiments/tree/01e423043b8938dc6e39cd72ca95586eb1f824ed))
-   prepare for list of metrics
    ([db8b709](https://github.com/kapsner/mlexperiments/tree/db8b7090f943bea96fd326ad6a8487da46e9c033))

#### New features

-   added rpart learner
    ([808042e](https://github.com/kapsner/mlexperiments/tree/808042e5d5d121adaccd699b9dd9ee155715959f))
-   added support for defining multiple metrics
    ([6cc6340](https://github.com/kapsner/mlexperiments/tree/6cc634057c0d23ab294cc33a46556215b0dcbb6f))
-   metric\_types\_helper again from kdry
    ([22b6cdc](https://github.com/kapsner/mlexperiments/tree/22b6cdcd01421a196014b048d8f2dd59f1e9c6f6))
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

-   fixed typo
    ([5067f95](https://github.com/kapsner/mlexperiments/tree/5067f95707971aa948738ce1fc2662288c7e0735))
-   fixed cbind of list in predictions
    ([a81309e](https://github.com/kapsner/mlexperiments/tree/a81309e8a0c010e7955b8cb92aa16a9360c6b7af))
-   fixed performance and predictions
    ([e0174cf](https://github.com/kapsner/mlexperiments/tree/e0174cf95b7470aafd93828bf8beb2565e44cbc7))
-   fixing examples
    ([ed3a461](https://github.com/kapsner/mlexperiments/tree/ed3a46155a47341fb71e2c0fbe44e945fa32b355))
-   consolidized metrics as lists, fixed unit tests
    ([393a4d6](https://github.com/kapsner/mlexperiments/tree/393a4d642a168b501ae13c70116347fbb6a06a29))
-   updated recursive function call
    ([f30c925](https://github.com/kapsner/mlexperiments/tree/f30c92597e37e6b542c73c462f8e0b35ee078e26))
-   added recursiveness to calculating performance
    ([17f38a3](https://github.com/kapsner/mlexperiments/tree/17f38a3aac77c3f2af8b88e06a00283fc8326702))
-   fixed issues with data types when calculating performance metrics
    ([b57c53e](https://github.com/kapsner/mlexperiments/tree/b57c53e4e4702453b53b0ac8bea4ae3468d8b33a))
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

-   further optimizing rpart for reuse
    ([ce67a4f](https://github.com/kapsner/mlexperiments/tree/ce67a4fcfda8a8a0554dfc63fa4f8e624a0f0272))
-   optimized rpart for re-use
    ([fa0ca53](https://github.com/kapsner/mlexperiments/tree/fa0ca539cec775711ec2c2baec4702b7977e82ae))
-   refactored rpart, no user-visible changes
    ([207aafd](https://github.com/kapsner/mlexperiments/tree/207aafd52852f000abd0e30f261aeb8d8d599e2b))
-   metric\_from\_char
    ([8baaf3a](https://github.com/kapsner/mlexperiments/tree/8baaf3a99d8f56b3f19c4a2dec400854a7585a46))
-   switch to .compute\_performance
    ([3d8565d](https://github.com/kapsner/mlexperiments/tree/3d8565de4f1f9c707c92955b2b5d0f7f36a23a6f))
-   switch to .compute\_performance
    ([fa24034](https://github.com/kapsner/mlexperiments/tree/fa24034719f84e8c97557fece5a0f9f43764c299))
-   added .compute\_performance
    ([fffdf78](https://github.com/kapsner/mlexperiments/tree/fffdf78e10562f42b3da6ff4841697fc1030642f))
-   updated performance fun to comply with list
    ([67792f3](https://github.com/kapsner/mlexperiments/tree/67792f398f956a11bafedcf7e353d2bbd0342fad))
-   adapted metric calculation to new list logic
    ([46500f7](https://github.com/kapsner/mlexperiments/tree/46500f74a2d41c2360bf62c2766c14d2fa590f81))
-   metric as function now to list
    ([8bccfbc](https://github.com/kapsner/mlexperiments/tree/8bccfbcb90194a10e65b4a65a26d07ffc0dc5e18))
-   to internal metric\_types\_helper
    ([024adcb](https://github.com/kapsner/mlexperiments/tree/024adcb7fd765c603e94c41474d64a34791efe1a))
-   to internal metric\_types\_helper
    ([fde6e77](https://github.com/kapsner/mlexperiments/tree/fde6e770fb1e4743447edde46b0df5a7c8d69368))
-   to internal metric\_types\_helper
    ([97deeb7](https://github.com/kapsner/mlexperiments/tree/97deeb7fe20fb22b7da162de427d9935c80338e7))
-   moved fix performance types to kdry
    ([600a660](https://github.com/kapsner/mlexperiments/tree/600a6605bc53b94fd1a885fc66edf2de446e770c))
-   format\_xy to kdry
    ([322db94](https://github.com/kapsner/mlexperiments/tree/322db944f5115dbb6127ca2dab1f4962931d355d))
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

-   removed vignette building from ci
    ([97f3dbe](https://github.com/kapsner/mlexperiments/tree/97f3dbee1a0658345d0456530b0272187f663ac4))
-   explicitly installing doparallel
    ([a288058](https://github.com/kapsner/mlexperiments/tree/a288058dd65b4dbc335637394f5b010751407be5))
-   explicitly installing suggests of kdry
    ([7682912](https://github.com/kapsner/mlexperiments/tree/768291265b52153a61e9529b5dfb4cb4d48db6a5))
-   switch again to code step
    ([d3e6556](https://github.com/kapsner/mlexperiments/tree/d3e6556b99bda11e7c5963e1e19cc95d62780eac))
-   try to fix installation of kdry
    ([359de02](https://github.com/kapsner/mlexperiments/tree/359de0281a752d2674d46b5c402202e2d8a0f8ca))
-   whatever is going on with tic
    ([0ca30f0](https://github.com/kapsner/mlexperiments/tree/0ca30f0252b347fb5612bf883ca61029fd6bb97e))
-   back to previous tic
    ([e1a4541](https://github.com/kapsner/mlexperiments/tree/e1a45410a5226af057a1e0e4ff02899bbf0ecd35))
-   hopefully fixes ci
    ([17e5b28](https://github.com/kapsner/mlexperiments/tree/17e5b28f97bb43cd7769e1b15758fd545b6ae2c5))
-   try fixing
    ([5cd1579](https://github.com/kapsner/mlexperiments/tree/5cd1579681809863c536039b4ee919a65f55c19c))
-   try to fix ci
    ([7c1dad3](https://github.com/kapsner/mlexperiments/tree/7c1dad366d147362caa2a6d4bba1c7d373cfdb18))
-   try to fix ci
    ([2ba876e](https://github.com/kapsner/mlexperiments/tree/2ba876e5b7d807fd59b0bdc1b3b152bd9c4052e0))
-   update tic
    ([4ffa188](https://github.com/kapsner/mlexperiments/tree/4ffa188ef56ef11dbe352472019031f704ea41fc))
-   installing dependencies for kdry
    ([59b95e0](https://github.com/kapsner/mlexperiments/tree/59b95e0191e8b7c1d7c3b8eab55f2a577ab039bf))
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

-   added vignettes with examples
    ([b315dad](https://github.com/kapsner/mlexperiments/tree/b315dadba12a4b8d86f70a3233415d86bf392a6a))
-   started with vignettes - knn binary
    ([14c12f8](https://github.com/kapsner/mlexperiments/tree/14c12f871e6f84e266e22335b0c2b4f90611ae40))
-   fixed issues in vignette
    ([33a3951](https://github.com/kapsner/mlexperiments/tree/33a3951ebe3a5d718c82032ac15f37e5f79ff293))
-   removed commented output
    ([d92aad8](https://github.com/kapsner/mlexperiments/tree/d92aad89427e54079ad8e5c6e9a806997bb0874a))
-   updated example in vignette
    ([a958f42](https://github.com/kapsner/mlexperiments/tree/a958f4221782da6232031800452ff85f5b33d359))
-   update example in readme
    ([9663fff](https://github.com/kapsner/mlexperiments/tree/9663fffcba86f60bd3a98055c456b3f87ef226b7))
-   updated vignette accordingly
    ([4442cc5](https://github.com/kapsner/mlexperiments/tree/4442cc53c82e1ceef1c657e56c743e256b9d73b0))
-   updated readme
    ([41dfa7e](https://github.com/kapsner/mlexperiments/tree/41dfa7e7f6346185dd9a12be46f6c7b70fe4ed9f))
-   updated readme
    ([5bb27ca](https://github.com/kapsner/mlexperiments/tree/5bb27caf222018ae8d0bb23abcb285b50be087ec))
-   updated documentation
    ([c2182d4](https://github.com/kapsner/mlexperiments/tree/c2182d4abc9667d64ea366b9e0186363d45fc7cf))
-   updated metric types documentation
    ([dd07e5d](https://github.com/kapsner/mlexperiments/tree/dd07e5da61002c4b2b6a00c1a2f6bd78eb24e4f3))
-   finalizing documentation, added exaple to readme
    ([587de28](https://github.com/kapsner/mlexperiments/tree/587de2854ec002a6493eb94eafc2479aebb37c3e))
-   updated vignette intro
    ([3e0edd4](https://github.com/kapsner/mlexperiments/tree/3e0edd432cbd0a5997521bab85d3eab19dc29923))
-   finalizing vignette; working on documentation
    ([7c24878](https://github.com/kapsner/mlexperiments/tree/7c2487858aa7d56bcdd2315371f390fdf94ef035))
-   updated readme
    ([8add37f](https://github.com/kapsner/mlexperiments/tree/8add37f81a048273a3118a6d157c218e5ade2509))
-   updated readme
    ([40e38e5](https://github.com/kapsner/mlexperiments/tree/40e38e58d709116dab5856bf5500d46a8f26c4df))
-   working on vignette
    ([84ce219](https://github.com/kapsner/mlexperiments/tree/84ce219883335b6cbfa12dacc9234acd4f9710b5))
-   updated readme
    ([14cf011](https://github.com/kapsner/mlexperiments/tree/14cf0116a1fd6c1684d1ad35039630cd9fcafb70))
-   updated readme
    ([0d4e0ce](https://github.com/kapsner/mlexperiments/tree/0d4e0cecf593908a8d317f35f1f0ab1fc86c94c7))
-   working on vignette
    ([e4158f0](https://github.com/kapsner/mlexperiments/tree/e4158f0822cb36680688779087b0eafa40861d8e))
-   working on vignette
    ([66177d3](https://github.com/kapsner/mlexperiments/tree/66177d3f0579d70860b77d5b02e3e6116e8d2e71))
-   working on package vignette
    ([6fa520c](https://github.com/kapsner/mlexperiments/tree/6fa520cac5b25099744f05fe711da57f381c748a))
-   updated documentation
    ([0ff5511](https://github.com/kapsner/mlexperiments/tree/0ff5511cfc7f7f42508efed6ca295d2b351dd8ef))
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

-   version 0.0.1
    ([1354f65](https://github.com/kapsner/mlexperiments/tree/1354f6590db043b7ed05d34f787a481323f8d714))
-   fixed canonical form of cran url
    ([6dbd31a](https://github.com/kapsner/mlexperiments/tree/6dbd31aa49adb572368a4c2ff25e88df56be6525))
-   added implementation details to vignettes
    ([4401139](https://github.com/kapsner/mlexperiments/tree/4401139d83cd9d0204749044b26f06cc3ab7667a))
-   updated description and news.md
    ([49c35b6](https://github.com/kapsner/mlexperiments/tree/49c35b66941491e096465baa993439251bcb09de))
-   fixed typo in vignette title
    ([f51cad2](https://github.com/kapsner/mlexperiments/tree/f51cad2bff63f4ad0dc76a16a499de48f4f2caf3))
-   fixed rpart and optimized parsing of parameter grid
    ([74edba2](https://github.com/kapsner/mlexperiments/tree/74edba2c6ccffd240ef488a40b176e33fbbc1155))
-   updated readme example
    ([06727a0](https://github.com/kapsner/mlexperiments/tree/06727a002e8a85287dc42aa4732d9e0e340ad643))
-   finished refactoring rpart
    ([eb25888](https://github.com/kapsner/mlexperiments/tree/eb25888196d21983efbb452d905b664fef5ba168))
-   updated news.md
    ([a2b1cfb](https://github.com/kapsner/mlexperiments/tree/a2b1cfb55a32a5ff26d2c9cd70385392a0a9bb78))
-   fixed typo
    ([4d497d2](https://github.com/kapsner/mlexperiments/tree/4d497d217083e9aa65ce6f58fd1fb78628b04682))
-   updated readme
    ([c36c9ef](https://github.com/kapsner/mlexperiments/tree/c36c9efbb1d13424043cffff5f1d31cd55f7f21d))
-   fixed typo
    ([c8f7a88](https://github.com/kapsner/mlexperiments/tree/c8f7a88bef27acf1d10bc4766d3b199b28001183))
-   fixed typo
    ([26a9489](https://github.com/kapsner/mlexperiments/tree/26a9489ad1036980dc46102c77857c73274d0788))
-   updated readme and description
    ([6c0084f](https://github.com/kapsner/mlexperiments/tree/6c0084f912f11a681255368b9c64c40109e296ea))
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
[`edb76c3...v0.0.1`](https://github.com/kapsner/mlexperiments/compare/edb76c3...v0.0.1)

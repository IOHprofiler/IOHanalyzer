# IOHanalyzer

<!-- badges: start -->
[![metacran downloads](https://cranlogs.r-pkg.org/badges/IOHanalyzer)](https://cran.r-project.org/package=IOHanalyzer)
[![CRAN_Status_Badge_version_last_release](https://www.r-pkg.org/badges/version-last-release/IOHanalyzer)](https://cran.r-project.org/package=IOHanalyzer)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![R-CMD-check](https://github.com/IOHprofiler/IOHanalyzer/workflows/R-CMD-check/badge.svg)](https://github.com/IOHprofiler/IOHanalyzer/actions)
<!-- badges: end -->

The __performance analyzer__ for **I**terative **O**ptimization **H**euristics (IOHs).

* __Documentation__: [https://arxiv.org/abs/2007.03953](https://arxiv.org/abs/2007.03953)
* __Wiki page__: [https://iohprofiler.github.io/IOHanalyzer/](https://iohprofiler.github.io/IOHanalyzer/)
* __Bug reports__: [https://github.com/IOHprofiler/IOHAnalyzer/issues](https://github.com/IOHprofiler/IOHAnalyzer/issues)
* __Online service__: [http://iohanalyzer.liacs.nl](http://iohanalyzer.liacs.nl) *Due to server migrations, the webpage will be unavailable until June 16th* 
* __General Contact__: [mailto:iohprofiler@liacs.leidenuniv.nl](mailto:iohprofiler@liacs.leidenuniv.nl)
* __Mailing List__: [https://lists.leidenuniv.nl/mailman/listinfo/iohprofiler](https://lists.leidenuniv.nl/mailman/listinfo/iohprofiler)

![](./misc/demo.gif)

It _provides_:

* a web-based graphical user interface (GUI) to analyze and visualize the empirical performance of IOHs,
* interactive plotting,
* statistical evaluation,
* report generation, and
* a command-line interface (CLI) for the `R` console allowing for fine-grained controls.  

It is _built mainly on_:

* `R` packages [Shiny](https://shiny.rstudio.com/), [Plotly](https://plotly.com/) and [Rcpp](http://www.rcpp.org/).
<!-- * [scmacp](https://github.com/b0rxa/scmamp) package for Bayesian analysis. -->

It is _available through_:

* a free [online service](#server) that you can use right away, or
* the local [installation](#install) of the package.

## <a name="server"></a>Online Service

A free server [http://iohprofiler.liacs.nl](http://iohprofiler.liacs.nl) running the stable version of __IOHanalyzer__ is hosted in [Leiden Institute of Advanced Computer Science](https://liacs.leidenuniv.nl/). You're welcome to check it out!

## <a name="install"></a>Installation

### Software dependency

* [mandatory] `R` As __IOHanalyzer__ is written as a `R` package, the `R` environment has to be installed first. The binary file and installation manual for R can be found here [https://cran.r-project.org/](https://cran.r-project.org/).
* [optional] either `kaleido` (recommended) or `orca` (will be depricated) is required to download plotly figures. Please see [Kaleido](https://github.com/plotly/Kaleido) or [Orca](https://github.com/plotly/orca) for their respective installation instructions.

### Stable version

Please start up an `R` console and install the stable version as:

```r
R> install.packages('IOHanalyzer')
```

which is maintained on [CRAN](https://CRAN.R-project.org/package=IOHanalyzer) (Comprehensive R Archive Network).

### Lastest version

The lastest development is always hosted on Github. In case you'd like to try out this version, the `R` package **devtool** is needed:

```r
R> install.packages('devtools')
R> devtools::install_github('IOHprofiler/IOHanalyzer')
```

### Development version

If you want to run the version on which you develop:

```r
R> devtools::install_git("/path/to/your/IOHanalyzer/git/repo")
```

## <a name="run"></a>Runinng the Web Interface locally

The IOHanalyzer package can be loaded using the following commands:

```r
R> library('IOHanalyzer')
R> runServer()
```

It should open a browser on the `localhost` server, using a random port number. Of course, you
could also specify the port number directly:

```r
R> runServer(port = 1234)
```

Have fun! For the complete reference on usage, please check out our [Wiki page](https://iohprofiler.github.io/).

## Host it online

We provide a docker file for deploying __IOHanalyzer__ on the server. Please see [https://github.com/IOHprofiler/IOHanalyzer-docker](https://github.com/IOHprofiler/IOHanalyzer-docker) for details.

## Supported Data Format

Specific formats are required to load your benchmark data to **IOHanalyzer**. If your data sets are generated in the format of

* **COCO/BBOB** data format as regulated in [https://hal.inria.fr/inria-00362649](https://hal.inria.fr/inria-00362649),
* **Nevergrad** data format (explained in [https://github.com/facebookresearch/nevergrad](https://github.com/facebookresearch/nevergrad)), or
* **IOHprofiler** data format, which is motivated and modified from **COCO** data format,

then you just need to compress the data folder obtained from the experiment into a __zip__ file and uploaded it. However, **you are encouraged to convert your own benchmark data to the format regulated here!**. The supported data format is specified in [this page](https://iohprofiler.github.io/IOHanalyzer/data/). Please follow the instruction there to convert your data sets.

## Our Team

* [Hao Wang](https://www.lip6.fr/actualite/personnes-fiche.php?ident=D2381), Sorbonne Université, LIP6, France.
* [Diederick Vermetten](https://www.universiteitleiden.nl/en/staffmembers/diederick-vermetten), Leiden Institute of Advanced Computer Science, The Netherlands.
* [Furong Ye](https://www.universiteitleiden.nl/en/staffmembers/furong-ye), Leiden Institute of Advanced Computer Science, The Netherlands.
* [Ofer M. Shir](https://ofersh.github.io/telhai/), Tel-Hai College, Israel.
* [Carola Doerr](http://www-desir.lip6.fr/~doerr/), Sorbonne Université, CNRS, LIP6, France.
* [Thomas Bäck](https://www.universiteitleiden.nl/en/staffmembers/thomas-back), Leiden Institute of Advanced Computer Science, The Netherlands.

When using IOHprofiler and parts thereof, please kindly cite this work as

Hao Wang, Diederick Vermettern, Furong Ye, Carola Doerr and Thomas Bäck: _IOHanalyzer: Performance Analysis for Iterative Optimization Heuristic_, arXiv e-prints:2007.03953, 2020.

```bibtex
@ARTICLE{IOHprofiler,
  author = {Hao Wang and Diederick Vermettern and Furong Ye and Carola Doerr and Thomas B{\"a}ck},
  title = {{IOHanalyzer: Performance Analysis for Iterative Optimization Heuristic}},
  journal = {arXiv e-prints:2007.03953},
  archivePrefix = "arXiv",
  eprint = {2007.03953},
  year = 2020,
  month = July,
  keywords = {Computer Science - Neural and Evolutionary Computing},
  url = {https://arxiv.org/abs/2007.03953}
}
```

## License

This application is governed by the __BSD 3-Clause license__.

BSD 3-Clause License

Copyright (c) 2018,
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of the copyright holder nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

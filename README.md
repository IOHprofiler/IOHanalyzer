# IOHanalyzer

The __performance analyzer__ for **I**terative **O**ptimization **H**euristics (IOHs).

* __Documentation__: [https://arxiv.org/abs/1810.05281](https://arxiv.org/abs/1810.05281)
* __Wiki page__: [https://iohprofiler.github.io/IOHanalyzer/](https://iohprofiler.github.io/IOHanalyzer/)
* __Bug reports__: [https://github.com/IOHprofiler/IOHAnalyzer/issues](https://github.com/IOHprofiler/IOHAnalyzer/issues)
* __Online service__: [http://iohprofiler.liacs.nl](http://iohprofiler.liacs.nl)
* __General Contact__: [iohprofiler@liacs.leidenuniv.nl](iohprofiler@liacs.leidenuniv.nl)
* __Mailing List__: [https://lists.leidenuniv.nl/mailman/listinfo/iohprofiler](https://lists.leidenuniv.nl/mailman/listinfo/iohprofiler)

It _provides_:

* a web-based interface to analyze and visualize the empirical performance of IOHs,
* interactive plot,
* statistical evaluation,
* report generation, and
* `R` programming interfaces in the backend.

It is _built on_:

* `R` packages [Shiny](https://shiny.rstudio.com/), [Plotly](https://plot.ly/) and [Rcpp](http://www.rcpp.org/).
<!-- * [scmacp](https://github.com/b0rxa/scmamp) package for Bayesian analysis. -->

It is _available through_:

* a free [online service](#server) that you can use right away, or
* the local [installation](#install) of the package.

## <a name="server"></a>Online Service

A free server [http://iohprofiler.liacs.nl](http://iohprofiler.liacs.nl) running the stable version of __IOHanalyzer__ is hosted in [Leiden Institute of Advanced Computer Science](https://liacs.leidenuniv.nl/). You're welcome to check it out!

## <a name="install"></a>Installation

### Software dependency

* [mandatory] `R` As __IOHanalyzer__ is written as a `R` package, the `R` environment has to be installed first. The binary file and installation manual for R can be found here [https://cran.r-project.org/](https://cran.r-project.org/).
* [optional] `orca` required to download plotly figures. Please see [https://github.com/plotly/orca](https://github.com/plotly/orca) for the installation instruction.

### Stable version

Please start up a `R` console and install the stable version as:

```r
install.packages('IOHanalyzer')
```

which is maintained on [CRAN](https://CRAN.R-project.org/package=IOHanalyzer) (Comprehensive R Archive Network).

### Lastest version

The lastest development is always hosted on Github. In case you'd like to try out this version, the `R` package **devtool** is needed:

```r
install.packages('devtools')
devtools::install_github('IOHprofiler/IOHanalyzer')
```

## <a name="run"></a>Runinng the Web Interface locally

The IOHanalyzer package can be installed and loaded using the following commands:

```r
library('IOHanalyzer')
runServer()
```

Have fun! For the complete reference on usage, please check out our [Wiki page](https://iohprofiler.github.io/).

## Host it online?

We provide docker file for deploying __IOHanalyzer__ on the server. Please see [https://github.com/IOHprofiler/IOHanalyzer-docker](https://github.com/IOHprofiler/IOHanalyzer-docker) for details.

## Supported Data Format

Specific formats are required to load your benchmark data to **IOHanalyzer**. If your data sets are generated in the format of

* **COCO/BBOB** data format as regulated in [https://hal.inria.fr/inria-00362649](https://hal.inria.fr/inria-00362649),
* **Nevergrad** data format (explained in [https://github.com/facebookresearch/nevergrad](https://github.com/facebookresearch/nevergrad)), or
* **IOHprofiler** data format, which is motivated and modified from **COCO** data format,

then you just need to compress the data folder obtained from the experiment into a __zip__ file and uploaded it. However, **you are encouraged to convert your own benchmark data to the format regulated here!**. The supported data format is specified in [this page](https://iohprofiler.github.io/IOHanalyzer/data/). Please follow the instruction there to convert your data sets.

## Our Team

* [Hao Wang](https://www.universiteitleiden.nl/en/staffmembers/hao-wang), Leiden Institute of Advanced Computer Science.
* [Diederick Vermetten](https://www.universiteitleiden.nl/en/staffmembers/diederick-vermetten), Leiden Institute of Advanced Computer Science.
* [Furong Ye](https://www.universiteitleiden.nl/en/staffmembers/furong-ye), Leiden Institute of Advanced Computer Science.
* [Ofer M. Shir](https://ofersh.github.io/telhai), Tel-Hai College, Israel.
* [Carola Doerr](http://www-desir.lip6.fr/~doerr/), CNRS and Sorbonne University.
* [Thomas Bäck](https://www.universiteitleiden.nl/en/staffmembers/thomas-back), Leiden Institute of Advanced Computer Science.

When using IOHprofiler and parts thereof, please kindly cite this work as

Carola Doerr, Hao Wang, Furong Ye, Sander van Rijn, Thomas Bäck: _IOHprofiler: A Benchmarking and Profiling Tool for Iterative Optimization Heuristics_, arXiv e-prints:1810.05281, 2018.

```bibtex
@ARTICLE{IOHprofiler,
  author = {Carola Doerr and Hao Wang and Furong Ye and Sander van Rijn and Thomas B{\"a}ck},
  title = {{IOHprofiler: A Benchmarking and Profiling Tool for Iterative Optimization Heuristics}},
  journal = {arXiv e-prints:1810.05281},
  archivePrefix = "arXiv",
  eprint = {1810.05281},
  year = 2018,
  month = oct,
  keywords = {Computer Science - Neural and Evolutionary Computing},
  url = {https://arxiv.org/abs/1810.05281}
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

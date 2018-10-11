# IOHprofiler: Post-Processing

[This is](https://github.com/IOHprofiler/Post-Processing) the post-processing tool of the project __Iterative Optimization Heuristics Profiler__ (IOHprofiler). This tool provides a web-based interface to analyze and visualization the benchmark data, collected from previous experiments. Importantly, we __do support__ the widely used [COCO](https://github.com/numbbo/coco) data format (aka. Black-Box Optimization Benchmarking).

This tool is mainly built on R package [Shiny](https://shiny.rstudio.com/), [plotly](https://plot.ly/) and [Rcpp](http://www.rcpp.org/). To use this tool, two options are available:

1. local installation and execution (see [installation instructions](#install)) and
2. a [web-based service](#server) that you can use right away.

## <a name="install"></a>Installation

This software is mainly written in __R__. To run it directly from the source code, please install R environment first. The binary file and installation manual for R can be found here [https://cran.r-project.org/](https://cran.r-project.org/).

After R environment is correctly installed on you machine, several R packages are needed to execute the sorftware. Please start up the __R console__, which can be done (in case you're not familiar with R) by either executing command `R` in your system terminal or open the R application. Once it is done, please copy-paste and execute the following commands into the R console to install all depedencies.
  
```r
install.packages('shiny')
install.packages('shinyjs')
install.packages('shinydashboard')
install.packages('magrittr')
install.packages('dplyr')
install.packages('reshape2')
install.packages('data.table')
install.packages('markdown')
install.packages('Rcpp')
install.packages('plotly')
```

Note that it is important to check if aforementioned packages are correctly installed. The easiest method is to test if those packasges can be loaded:

```r
library('shiny')
library('shinyjs')
library('shinydashboard')
library('magrittr')
library('dplyr')
library('reshape2')
library('data.table')
library('markdown')
library('Rcpp')
library('plotly')
```

Error messages will be shown in your R console if there is any installation issue.

Then, please clone (or downlaod) this repository into your own system. To clone the repository, please execute the following command in your __system console__ (terminal):

```Shell
> git clone git@github.com:IOHprofiler/Post-Processing.git
```

```Shell
> git clone https://github.com/IOHprofiler/Post-Processing.git
```

To download, please click the green download button on this page.

To start the post-processing tool, please execute the following commands in the __R console__:

```r
> shiny::runApp('/path/to/the/clone/folder')
```

## <a name="server"></a>:construction: Online Service

Alternatively, we have built a server to put this tool online, which is currently hosted in [Leiden Institute of Advanced Computer Science](https://liacs.leidenuniv.nl/), Leiden University. The server can be accessed via [this link](https://iohprofiler.liacs.nl).

## :construction: Documentation

The details on the experimentation and post-processing tool can be found on [Carola's homepage](www-ia.lip6.fr/~doerr/IOHprofiler-v1.pdf) (and soon also on arXiv).

## Data Preparation

Data preparation is fairly easy for this tool. Just compress the data folder obtained from the experiment into a __zip__ file and uploaded it. Currently, we support two data formats:

* IOHprofiler: our own csv-based format,
* COCO: data format of the [COCO benchmark environment](https://github.com/numbbo/coco).

## Programing Interface

In addition to the graphical user interface, it is possible to directly call several procedures to analyze the data.

* To read and align all the data set in a folder
  
```Shell
> ds <- read_dir('/path/to/data/folder')
> ds
DataSetList:
1: DataSet((1+1)-Cholesky-CMA on f1 2D)
2: DataSet((1+1)-Cholesky-CMA on f1 5D)
3: DataSet((1+1)-Cholesky-CMA on f1 10D)
4: DataSet((1+1)-Cholesky-CMA on f1 20D)
5: DataSet((1+1)-Cholesky-CMA on f10 2D)
6: DataSet((1+1)-Cholesky-CMA on f10 5D)
7: DataSet((1+1)-Cholesky-CMA on f10 10D)
8: DataSet((1+1)-Cholesky-CMA on f10 20D)
9: DataSet((1+1)-Cholesky-CMA on f11 2D)
10: DataSet((1+1)-Cholesky-CMA on f11 5D)
```

The return value is a list of __DataSets__. Each data set consists of:

  1. runtime samples (aligned by target values),
  2. target values (aligned by runtime) and
  3. aligned endogenous parameter values of your optimization algorithm (aligned by target values).

* To get a summary of one data set (e.g., the runtime distribution):
  
```Shell
> summarise_runtime(ds[[1]], ftarget = 1e-1, maximization = FALSE)
             algId       f(x) runs  mean median       sd 2% 5% 10% 25% 50% 75% 90% 95% 98%
(1+1)-Cholesky-CMA 0.09986529   80 36.55   37.5 17.11236  4  5  14  22  37  49  57  67  68
```

```Shell
> summarise_target(ds[[1]], runtimes = 100, maximization = FALSE)
             algId runtime runs           mean       median           sd           2%           5%          10%          25%          50%          75%          90%         95%         98%
(1+1)-Cholesky-CMA     100   80   0.0002333208 3.797025e-05 0.0004581431 9.843261e-08 4.168509e-07 8.343177e-07 6.090179e-06 3.797025e-05 0.0001831323 0.0006597004 0.001072814 0.001900295
```

```Shell
> get_runtime_sample(ds[[1]], ftarget = 1e-1, maximization = F, format = 'long')
                algId          f(x) run RT
1  (1+1)-Cholesky-CMA 0.09986528573   1 69
2  (1+1)-Cholesky-CMA 0.09986528573   2 39
3  (1+1)-Cholesky-CMA 0.09986528573   3 38
4  (1+1)-Cholesky-CMA 0.09986528573   4 34
5  (1+1)-Cholesky-CMA 0.09986528573   5 67
6  (1+1)-Cholesky-CMA 0.09986528573   6  3
7  (1+1)-Cholesky-CMA 0.09986528573   7 36
8  (1+1)-Cholesky-CMA 0.09986528573   8 41
9  (1+1)-Cholesky-CMA 0.09986528573   9 14
10 (1+1)-Cholesky-CMA 0.09986528573  10 30
```

## :construction: TODO

The technical tasks to do are listed as follows:

* [ ] convert data processing code into a package
* [ ] add more stastistical tests
* [ ] implement the standard R `summary` method for `DataSet` and `DataSetList` classes
* [ ] add _ggplot2_ based static plotting procedures for the programming interface
* [ ] make the data analysis part as a separate R package
* [ ] to determine the data source to align the data set using runtimes

## Contact

If you have any questions, comments, suggestions or pull requests, please don't hesitate contacting us <IOHprofiler@liacs.leidenuniv.nl>!

## Cite us

The development team is:

* [Hao Wang](https://www.universiteitleiden.nl/en/staffmembers/hao-wang#tab-1), <i>Leiden Institute of Advanced Computer Science</i>, <h.wang@liacs.leidenuniv.nl>.
* [Carola Dörr](http://www-desir.lip6.fr/~doerr/), <i>CNRS and Sorbonne University</i>, <Carola.Doerr@mpi-inf.mpg.de>.
* [Furong Ye](https://www.universiteitleiden.nl/en/staffmembers/furong-ye#tab-1), <i>Leiden Institute of Advanced Computer Science</i>, <f.ye@liacs.leidenuniv.nl>.
* [Sander van Rijn](https://www.universiteitleiden.nl/en/staffmembers/sander-van-rijn#tab-1), <i>Leiden Institute of Advanced Computer Science</i>, <s.j.van.rijn@liacs.leidenuniv.nl>.
* [Thomas Bäck](https://www.universiteitleiden.nl/en/staffmembers/thomas-back#tab-1), <i>Leiden Institute of Advanced Computer Science</i>, <t.h.w.baeck@liacs.leidenuniv.nl>.

When using IOHprofiler and parts thereof, please kindly cite this work as

Hao Wang, Furong Ye, Carola Doerr, Sander van Rijn, Thomas Bäck: <i>IOHprofiler: A New Tool for Benchmarking and Profiling Iterative Optimization Heuristics</i>, arXiv, 2018

```bibtex
@article{IOHprofiler,
  author = {Hao Wang, Furong Ye, Carola Doerr, Sander van Rijn, Thomas Bäck},
  title = {IOHprofiler: A New Tool for Benchmarking and Profiling Iterative Optimization Heuristics},
  journal = {CoRR},
  volume = {tbd},  
  year = {2018},
  url = {tbd}
}
```

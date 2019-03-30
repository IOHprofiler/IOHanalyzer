# IOHprofiler: Post-Processing

[This is](https://github.com/IOHprofiler/Post-Processing) the post-processing tool of the project __Iterative Optimization Heuristics Profiler__ (IOHprofiler). This tool provides a web-based interface to analyze and visualization the benchmark data, collected from previous experiments. Importantly, we __do support__ the widely used [COCO](https://github.com/numbbo/coco) data format (aka. Black-Box Optimization Benchmarking).

This tool is mainly built on R package [Shiny](https://shiny.rstudio.com/), [plotly](https://plot.ly/) and [Rcpp](http://www.rcpp.org/). To use this tool, three options are available:

1. Install the package directely from github using devtools (see [R-package](#package))
2. A [web-based service](#server) that you can use right away.

## Documentation

The details on the experimentation and post-processing tool can be found on [arXiv.org](https://arxiv.org/abs/1810.05281).

## <a name="package"></a> Using IOHprofiler as a R-package

To install the IOHProfiler R-package, please install R environment first. The binary file and installation manual for R can be found here [https://cran.r-project.org/](https://cran.r-project.org/).

After R environment is correctly installed on you machine, 'devtools' package is needed to install the sorftware. Please start up the __R console__, which can be done (in case you're not familiar with R) by either executing command `R` in your system terminal or open the R application. Once it is done, please copy-paste and execute the following command into the R console
```r
install.packages('devtools')
```

Error messages will be shown in your R console if there is any installation issue.
Now, the IOHanalyzer package can be installed and loaded using the following commands:
```r
devtools::install_github('IOHprofiler/IOHanalyzer')
library('IOHanalyzer')
```

This will install the package and all required dependencies. The GUI can be acessed using the command:
```r
runServer()
```

## <a name="install"></a>Installation

Alternatively, you can clone the source-code directely.
This software is mainly written in __R__. To run it directly from the source code, please install R environment first. The binary file and installation manual for R can be found here [https://cran.r-project.org/](https://cran.r-project.org/).

After R environment is correctly installed on you machine, several R packages are needed to execute the sorftware. Please start up the __R console__, which can be done (in case you're not familiar with R) by either executing command `R` in your system terminal or open the R application. Once it is done, please copy-paste and execute the following commands into the R console to install all depedencies.

Error messages will be shown in your R console if there is any installation issue.

To use the repository, the following additional packages are required
```r
install.packages(c('DBI','RMariaDB'))
```

To allow for downloading of plots, orca[https://github.com/plotly/orca] and inkscape[https://inkscape.org/release/inkscape-0.92.4/] are needed.

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

## <a name="server"></a> Online Service

Alternatively, we have built a server to put this tool online, which is currently hosted in [Leiden Institute of Advanced Computer Science](https://liacs.leidenuniv.nl/), Leiden University. The server can be accessed via [http://iohprofiler.liacs.nl](http://iohprofiler.liacs.nl).

## Data Preparation

Data preparation is fairly easy for this tool. Just compress the data folder obtained from the experiment into a __zip__ file and uploaded it. Currently, we support two data formats:

* IOHprofiler: our own csv-based format,
* COCO: data format of the [COCO benchmark environment](https://github.com/numbbo/coco).

## Programing Interface

In addition to the graphical user interface, it is possible to directly call several procedures to analyze the data.

* To read and align all the data set in a folder, e.g., a COCO (BBOB) can be loaded as follows:

```shell
> dsList <- read_dir('/path/to/data/folder', format = 'COCO')
> dsList
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

  1. __runtime samples__ (aligned by target values),
  2. __function values samples__ (aligned by runtime) and
  3. __endogenous parameter samples__ of your optimization algorithm (aligned by target values).

* To get a general summary of one data set, you can use function `summary`:

```bash
> summary(dsList[[1]])
DataSet Object:
Source: COCO
Algorithm: (1+1)-Cholesky-CMA
Function ID: 1
Dimension: 2D
80 instance found: 1,2,3,4,5,6,7,...,73,74,75,76,77,78,79,80

runtime summary:
            target     mean median         sd  2%  5% 10% 25% 50% 75% 90% 95% 98%        ERT runs     ps
   1: 7.010819e+01   1.0000    1.0  0.0000000   1   1   1   1   1   1   1   1   1     1.0000   80 1.0000
   2: 6.642132e+01   1.0125    1.0  0.1118034   1   1   1   1   1   1   1   1   1     1.0125   80 1.0000
   3: 6.298712e+01   1.1125    1.0  0.8999824   1   1   1   1   1   1   1   1   1     1.1125   80 1.0000
   4: 6.254396e+01   1.1375    1.0  0.9242684   1   1   1   1   1   1   1   1   2     1.1375   80 1.0000
   5: 6.173052e+01   1.2000    1.0  1.1295793   1   1   1   1   1   1   1   1   3     1.2000   80 1.0000
  ---
1478: 9.473524e-10 182.6000  182.0 24.0894168 145 145 145 145 181 195 195 210 210  3039.6000    5 0.0625
1479: 2.759535e-10 192.0000  188.5 13.5892114 181 181 181 181 182 195 210 210 210  3799.5000    4 0.0500
1480: 2.463310e-10 195.6667  195.0 14.0118997 182 182 182 182 195 195 210 210 210  5066.0000    3 0.0375
1481: 5.223910e-11 196.0000  196.0 19.7989899 182 182 182 182 182 210 210 210 210  7599.0000    2 0.0250
1482: 1.638512e-11 210.0000  210.0         NA 210 210 210 210 210 210 210 210 210 15198.0000    1 0.0125

function value summary:
                 algId runtime runs         mean       median           sd           2%           5%          10%
 1: (1+1)-Cholesky-CMA       1   80 2.019164e+01 1.313629e+01 1.872005e+01 3.976009e-01 1.186758e+00 2.267215e+00
 2: (1+1)-Cholesky-CMA       2   80 1.672518e+01 1.171157e+01 1.626487e+01 3.412261e-01 5.649713e-01 1.313165e+00
 3: (1+1)-Cholesky-CMA       3   80 1.341813e+01 7.960940e+00 1.466877e+01 1.177243e-01 1.778454e-01 5.553242e-01
 4: (1+1)-Cholesky-CMA       4   80 1.100825e+01 6.439678e+00 1.261937e+01 9.206390e-02 1.492565e-01 5.221506e-01
 5: (1+1)-Cholesky-CMA       5   80 9.326633e+00 5.492333e+00 1.213908e+01 7.246194e-02 1.031687e-01 3.482309e-01
---
90: (1+1)-Cholesky-CMA     229   80 6.321111e-09 4.609703e-09 8.318463e-09 1.648124e-10 9.137825e-10 1.336938e-09
91: (1+1)-Cholesky-CMA     231   80 5.823366e-09 4.609703e-09 7.618201e-09 1.648124e-10 9.137825e-10 1.336938e-09
92: (1+1)-Cholesky-CMA     238   80 5.549184e-09 4.506106e-09 7.258216e-09 1.648124e-10 9.137825e-10 1.336938e-09
93: (1+1)-Cholesky-CMA     251   80 4.902827e-09 4.506106e-09 2.863671e-09 1.648124e-10 9.137825e-10 1.336938e-09
94: (1+1)-Cholesky-CMA     257   80 4.737548e-09 4.461953e-09 2.526087e-09 1.648124e-10 9.137825e-10 1.336938e-09
             25%          50%          75%          90%          95%          98%
 1: 5.166078e+00 1.313629e+01 2.922941e+01 5.004596e+01 5.895140e+01 6.442948e+01
 2: 3.506037e+00 1.171157e+01 2.470745e+01 3.956328e+01 5.012620e+01 6.002245e+01
 3: 2.786120e+00 7.960940e+00 2.103983e+01 3.251354e+01 4.030147e+01 5.690653e+01
 4: 1.284869e+00 6.439678e+00 1.636467e+01 2.453239e+01 3.043178e+01 4.718666e+01
 5: 9.886750e-01 5.492333e+00 1.317642e+01 2.216466e+01 2.913811e+01 4.718666e+01
---
90: 2.799292e-09 4.609703e-09 6.554838e-09 8.992768e-09 1.235938e-08 2.771700e-08
91: 2.799292e-09 4.609703e-09 6.321677e-09 8.916086e-09 9.326487e-09 1.785183e-08
92: 2.799292e-09 4.506106e-09 6.099065e-09 8.854852e-09 9.180278e-09 1.041480e-08
93: 2.799292e-09 4.506106e-09 6.099065e-09 8.659254e-09 8.982758e-09 9.377720e-09
94: 2.799292e-09 4.461953e-09 5.970504e-09 8.547357e-09 8.925389e-09 9.234526e-09

Attributes: names, class, funcId, DIM, Precision, algId, comment, datafile, instance, maxRT, finalFV, src, maximization
```

* To get a __summary__ of one data set at some function values / runtimes (e.g., the runtime distribution), you can use function `get_RT_summary` (RunTime) and `get_FV_summary` (FunctionValue):

```bash
> get_RT_summary(dsList[[1]], ftarget = 1e-1)
                algId     target  mean median       sd 2% 5% 10% 25% 50% 75% 90% 95% 98%   ERT runs ps
1: (1+1)-Cholesky-CMA 0.09986529 36.55   37.5 17.11236  4  5  14  22  37  49  57  67  68 36.55   80  1
```

```bash
> get_FV_summary(dsList[[1]], runtime = 100)
                algId runtime runs         mean       median          sd           2%           5%          10%          25%          50%          75%         90%         95%         98%
1: (1+1)-Cholesky-CMA     100   80 0.0005886303 8.307195e-05 0.001165899 7.233939e-07 3.708489e-06 8.776148e-06 2.409768e-05 8.307195e-05 0.0004779061 0.001991386 0.002923357 0.004207976
```

* To get the __sample__ at some function values / runtimes, you can use function `get_RT_sample` (RunTime) and `get_FV_sample` (FunctionValue):

```bash
> get_RT_sample(dsList[[1]], ftarget = 1e-1, output = 'long')
                 algId target run RT
 1: (1+1)-Cholesky-CMA    0.1   1 69
 2: (1+1)-Cholesky-CMA    0.1   2 39
 3: (1+1)-Cholesky-CMA    0.1   3 38
 4: (1+1)-Cholesky-CMA    0.1   4 34
 5: (1+1)-Cholesky-CMA    0.1   5 67
---
76: (1+1)-Cholesky-CMA    0.1  76 52
77: (1+1)-Cholesky-CMA    0.1  77 22
78: (1+1)-Cholesky-CMA    0.1  78 26
79: (1+1)-Cholesky-CMA    0.1  79 33
80: (1+1)-Cholesky-CMA    0.1  80 25
```

```bash
> get_FV_sample(dsList[[1]], runtime = 100, output = 'long')
                 algId runtime run         f(x)
 1: (1+1)-Cholesky-CMA     100   1 4.007000e-03
 2: (1+1)-Cholesky-CMA     100   2 5.381801e-04
 3: (1+1)-Cholesky-CMA     100   3 3.970844e-05
 4: (1+1)-Cholesky-CMA     100   4 5.345724e-04
 5: (1+1)-Cholesky-CMA     100   5 1.458869e-03
---
76: (1+1)-Cholesky-CMA     100  76 1.817090e-03
77: (1+1)-Cholesky-CMA     100  77 3.850201e-06
78: (1+1)-Cholesky-CMA     100  78 1.330411e-05
79: (1+1)-Cholesky-CMA     100  79 3.933669e-05
80: (1+1)-Cholesky-CMA     100  80 5.658113e-07
```

Or output a wide format...

```bash
> get_RT_sample(dsList[[1]], ftarget = 1e-1, output = 'wide')
                algId target run.1 run.2 run.3 run.4 run.5 run.6 run.7 run.8 run.9 run.10 run.11 run.12 run.13 run.14 run.15
1: (1+1)-Cholesky-CMA    0.1    69    39    38    34    67     3    36    41    14     30     41     47     31     48     53
   run.16 run.17 run.18 run.19 run.20 run.21 run.22 run.23 run.24 run.25 run.26 run.27 run.28 run.29 run.30 run.31 run.32
1:      8     19     18     57     16     28     51     53     22     47     53     17      5     48     13     63     45
   run.33 run.34 run.35 run.36 run.37 run.38 run.39 run.40 run.41 run.42 run.43 run.44 run.45 run.46 run.47 run.48 run.49
1:     15     24     46     65     44     71     52     31     17     18     45     19      5     37     50     33     24
   run.50 run.51 run.52 run.53 run.54 run.55 run.56 run.57 run.58 run.59 run.60 run.61 run.62 run.63 run.64 run.65 run.66
1:     40     49     55     48     50     33     20     33     35     49     37      4     22     68     57     19     44
   run.67 run.68 run.69 run.70 run.71 run.72 run.73 run.74 run.75 run.76 run.77 run.78 run.79 run.80
1:     38     48     31     14     41     50     67     21     43     52     22     26     33     25
```

* It is also possible to generate some diagnostic plots (using `ggplot2` or `plotly`) using the provided plotting functions. The functions currently available are: plot_RT_single_fct, plot_FV_line, plot_ERT_AGGR, plot_RT_all_fcts, plot_FCE_ECDF_PER_TARGET, plot_FCE_MULTI, plot_FCE_AGGR, plot_FV_AUC, plot_FV_ECDF_AGGR, plot_FV_HIST, plot_FV_PDF, plot_PAR_line, plot_RT_AUC, plot_RT_ECDF, plot_RT_ECDF_AGGR, plot_RT_ECDF_MULTI, plot_RT_HIST, plot_RT_PMF.

For more information on these functions, use the command:
```r
?plot_RT_single_fct.DataSetList
```

## Contact

If you have any questions, comments, suggestions or pull requests, please don't hesitate contacting us <IOHprofiler@liacs.leidenuniv.nl>!

## Cite us

The development team is:

* [Hao Wang](https://www.universiteitleiden.nl/en/staffmembers/hao-wang#tab-1), <i>Leiden Institute of Advanced Computer Science</i>,
* Diederick Vermetten, <i>Leiden Institute of Advanced Computer Science</i>,
* [Carola Doerr](http://www-desir.lip6.fr/~doerr/), <i>CNRS and Sorbonne University</i>, 
* [Furong Ye](https://www.universiteitleiden.nl/en/staffmembers/furong-ye#tab-1), <i>Leiden Institute of Advanced Computer Science</i>,
* [Sander van Rijn](https://www.universiteitleiden.nl/en/staffmembers/sander-van-rijn#tab-1), <i>Leiden Institute of Advanced Computer Science</i>,
* [Thomas Bäck](https://www.universiteitleiden.nl/en/staffmembers/thomas-back#tab-1), <i>Leiden Institute of Advanced Computer Science</i>,

When using IOHprofiler and parts thereof, please kindly cite this work as

Carola Doerr, Hao Wang, Furong Ye, Sander van Rijn, Thomas Bäck: <i>IOHprofiler: A Benchmarking and Profiling Tool for Iterative Optimization Heuristics</i>, arXiv e-prints:1810.05281, 2018.

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

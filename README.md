# Post-procesing Tool

This is the post-processing tool of the project __Iterative Optimization Heuristics Profiler__ (IOHProfiler). This tool provides a web-based interface to analyze and visualization the benchmark data, collected from previous experiments. Importantly, we __do support__ the widely used [COCO](https://github.com/numbbo/coco) data format (aka Black-Box Optimization Benchmarking).

# Installation
This software is mainly written in __R__ To run it directly from the source code, please install R environment first. The binary file and installation manual for R can be found here [https://cran.r-project.org/](https://cran.r-project.org/).

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
install.packages('devtools')
install.packages('Rcpp')
devtools::install_github("ropensci/plotly")
```
Please make sure those packages are correctly installed by monitoring the (verbose) prompting messages on the console.

Then, please clone this repository into your own system. To start the post-processing module, please execute the following commands in the R console:
```r
shiny::runApp('/path/to/the/clone/folder')
```

# Online Service
Alternatively, we have built a server to put this tool online, which is currently hosted in __Leiden University__. The server can be accessed via


# Data Preparation
Data preparation is fairly easy for this tool. Just compress the data folder obtained from the experiment and uploaded it. Currently, we support two data format:
* IOHProfiler: our own csv-based format 
* COCO: data format of the widely used COCO framework

# Programing Interface
In addition to the graphical user interface, it is possible to directly call several procedures to analyze the data.

* To read and align all the data set in a folder
```console
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
```console
> summarise_runtime(ds[[1]], ftarget = 1e-1, maximization = FALSE)
               algId       f(x) runs  mean median       sd 2% 5% 10% 25% 50% 75% 90% 95% 98%
1 (1+1)-Cholesky-CMA 0.09986529   80 36.55   37.5 17.11236  4  5  14  22  37  49  57  67  68
```

# TODO
* [ ] convert data processing code into a package
* [ ] add more stastistical tests

# Contact
If you have any questions, comments or suggestions, please don't hesitate contacting us via <wangronin@gmail.com> or <h.wang@liacs.leidenuniv.nl>.  

# Cite us

When using IOHProfiler and parts thereof, please kindly cite this work as 

Hao Wang, Furong Ye, Carola Doerr, Sander van Rijn, Thomas Bäck: <i>IOHProfiler: A New Tool for Benchmarking and Profiling Iterative Optimization Heuristics</i>, arXiv, 2018

```
@article{IOHProfiler,
  author = {Hao Wang, Furong Ye, Carola Doerr, Sander van Rijn, Thomas Bäck},
  title = {IOHProfiler: A New Tool for Benchmarking and Profiling Iterative Optimization Heuristics},
  journal = {CoRR}, 
  volume = {tbd},  
  year = {2018}, 
  url = {tbd} 
} 
```





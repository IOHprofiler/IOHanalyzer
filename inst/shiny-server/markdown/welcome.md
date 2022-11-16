This the **analyzer** for the empirical performance of _Iterative Optimization Heuristics_ (IOHs). It provides _two perspectives_ to look at the empirical performance:

* <a href="#shiny-tab-ERT_data" data-toggle="tab">Fixed-Target running time</a>: focuses on the distribution of **running time** (a.k.a. the number of function evaluations) at various given target values (thus called "fixed-target"), functions and dimensions.
* <a href="#shiny-tab-FCE_DATA" data-toggle="tab">Fixed-Budget results</a>: focuses on the distribution of the **best function value** at various given budget values (thus called "fixed-budget"), functions and dimensions.

To get started, you could

* **upload** your own dataset using the _Upload Data_ box on the bottom left. The supported format is specified in the <a href="#shiny-tab-dataformat" data-toggle="tab">data format tab</a>.
* or **choose** one of the pre-loaded datasets in the _Load Data from Repository_ box on the bottom right and directly explore the analyzer.

In details, the following functionalities are provided:

<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{font-family:Arial, sans-serif;font-size:14px;padding:10px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;border-color:black;}
.tg th{font-family:Arial, sans-serif;font-size:14px;font-weight:normal;padding:10px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;border-color:black;}
.tg .tg-0pky{border-color:inherit;text-align:left;vertical-align:top}
</style>
<table class="tg">
  <tr>
    <th class="tg-0pky"></th>
    <th class="tg-0pky">Data Summary</th>
    <th class="tg-0pky">Expected Values</th>
    <th class="tg-0pky">Probability Density/Mass Function</th>
    <th class="tg-0pky">Empirical Cumulative Distribution (ECDF)</th>
  </tr>
  <tr>
    <td class="tg-0pky">Fixed-Target running time</td>
    <td class="tg-0pky"><a href="#shiny-tab-ERT_data", data-toggle="tab">Summary of running times</a></td>
    <td class="tg-0pky"><a href="#shiny-tab-ERT_convergence", data-toggle="tab">Expected Running Time (ERT)</a></td>
    <td class="tg-0pky"><a href="#shiny-tab-RT_PMF", data-toggle="tab">Probability Mass Function of running time</a></td>
    <td class="tg-0pky"><a href="#shiny-tab-RT_ECDF", data-toggle="tab">ECDF of running times</a></td>
  </tr>
  <tr>
    <td class="tg-0pky">Fixed-Budget results</td>
    <td class="tg-0pky"><a href="#shiny-tab-FCE_DATA", data-toggle="tab">Summary of function values</a></td>
    <td class="tg-0pky"><a href="#shiny-tab-FCE_convergence", data-toggle="tab">Expected Target Value</a></td>
    <td class="tg-0pky"><a href="#shiny-tab-FCE_PDF", data-toggle="tab">Probability Density Function of target values</a></td>
    <td class="tg-0pky"><a href="#shiny-tab-FCE_ECDF", data-toggle="tab">ECDF of targets times</a></td>
  </tr>
</table>
<br/>

For more information on the features of IOHanalyzer and how to use them,
as well as a full specification of the accepted data formats, please visit <a href='https://iohprofiler.github.io/'> our wiki.</a>

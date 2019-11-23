<div class="row">
  <img src="../img/leiden.png" style="width:15%">
  <img src="../img/CNRS.gif" style="width:7%"> 
  <img src="../img/SU.jpg" style="width:15%"> 
  <img src="../img/telhai.png" style="width:15%">
</div>
<br/>

# Iterative Optimization Heuristics Profiler

The __performance analyzer__ for <b>I</b>terative <b>O</b>ptimization <b>H</b>euristics (IOHs).
It <i>provides</i>:

* a web-based interface to analyze and visualize the empirical performance of IOHs
* interactive plot
* statistical evaluation
* report generation
* `R` programming interfaces in the backend

## Development Team

* [Hao Wang](https://www.universiteitleiden.nl/en/staffmembers/hao-wang), <i>Leiden Institute of Advanced Computer Science</i>.
* [Diederick Vermetten](https://www.universiteitleiden.nl/en/staffmembers/diederick-vermetten), <i>Leiden Institute of Advanced Computer Science</i>.
* [Furong Ye](https://www.universiteitleiden.nl/en/staffmembers/furong-ye), <i>Leiden Institute of Advanced Computer Science</i>.
* [Carola Doerr](http://www-desir.lip6.fr/~doerr/), <i>CNRS and Sorbonne University</i>.
* [Ofer Shir](http://www.migal.org.il/Ofer-Shir) <i>Migal - The Galilee Research Institute, Tel-Hai College</i>.
* [Thomas Bäck](https://www.universiteitleiden.nl/en/staffmembers/thomas-back), <i>Leiden Institute of Advanced Computer Science</i>.

## <a name="install"></a>Installation

### Software dependency

* [mandatory] `R` As __IOHanalyzer__ is written as a `R` package, the `R` environment has to be installed first. The binary file and installation manual for R can be found here [https://cran.r-project.org/](https://cran.r-project.org/).
* [optional] `orca` required to download plotly figures. Please see [https://github.com/plotly/orca](https://github.com/plotly/orca) for the installation instruction.
* [optional] `inkscape` required to support pdf and eps figure format in downloading. Please visit the Inskscape Wiki page [http://wiki.inkscape.org/wiki/index.php/Installing_Inkscape](http://wiki.inkscape.org/wiki/index.php/Installing_Inkscape) for the detail.

## Host it online yourself?

We provide docker file for deploying __IOHanalyzer__ on the server. Please see [https://github.com/IOHprofiler/IOHanalyzer-docker](https://github.com/IOHprofiler/IOHanalyzer-docker) for details.

## Reference
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

## Acknowledgements
This work was supported by the Chinese scholarship council (CSC No. 201706310143), a public grant as part of the 
Investissement d’avenir project, reference ANR-11-LABX-0056-LMH, LabEx LMH, in a joint call with the Gaspard Monge 
Program for optimization, operations research, and their interactions with data sciences, by Paris Ile-de-France Region, 
and by COST Action CA15140 “Improving Applicability of Nature-Inspired Optimisation by Joining Theory and Practice (ImAppNIO)”.

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

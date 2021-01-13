# <img src="man/figures/logo.png" align="right" width="190px"/>Ramses: R package for Antimicrobial Stewardship & Surveillance

<!-- badges: start -->
[![R build status](https://github.com/ramses-antibiotics/ramses-package/workflows/R-CMD-check/badge.svg?branch=master)](https://github.com/ramses-antibiotics/ramses-package/actions)
[![Codecov test coverage](https://codecov.io/gh/ramses-antibiotics/ramses-package/branch/master/graph/badge.svg)](https://codecov.io/gh/ramses-antibiotics/ramses-package?branch=master)
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<!-- badges: end -->

<div class="lead"><code>Ramses</code> is an analytical software package enabling hospitals to protect antibiotics by monitoring their prescribing and the management of infections using routine electronic health records.</div>

## Overview

`Ramses` is a start-to-finish tool for data warehousing and analysis, supporting:

- initial data preparation and validation; 
- database loading and transformation
- analyses of antibiotic consumption, initiation, and de-escalation
- analyses of infection diagnosing and treatment.

`Ramses` supports routine electronic health records:

- admission/discharge/transfer data and ward movements
- antibiotic prescription and administration records
- microbiology order requests and results
- other clinical investigations (blood counts, vitals, point of care tests. etc.) 


## Features

- measuring antibiotic consumption
<!-- - measuring drug/bug match and rates of microbial culture sampling -->
- integration with the [AMR package](https://msberends.gitlab.io/AMR/) for classification/prediction of resistance phenotypes 
- compatible with SQLite and PostgreSQL databases

## Funders

<div style="display: table-cell;">
<a href="https://www.nihr.ac.uk/"><img height="60px" style="vertical-align: bottom;" alt="National Institute of Health Research" src="man/figures/partner-logos/NIHR.svg" ></a> &nbsp;&nbsp;
<a href="https://esrc.ukri.org/"><img height="50px" style="vertical-align: bottom; bottom;margin-bottom: -5px;" alt="Economic and Social Research Council" src="man/figures/partner-logos/ESRC.svg" ></a> &nbsp;&nbsp;
<a href="https://rosetreestrust.co.uk/"><img style="vertical-align: bottom;margin-bottom: -7px;" alt="Rosetree Trust" src="man/figures/partner-logos/rosetree.png" height="50px"></a>
</div>

## Licence 

Copyright &copy; 2020 University College London

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the [GNU General Public License](LICENCE.md) for more details.

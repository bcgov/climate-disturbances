[![img](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

# climate-disturbances


### Installation

This project leverages the [{targets} package](https://docs.ropensci.org/targets/), a pipeline toolkit for data science projects in R. You can install {targets} from CRAN:

```
install.packages{"targets"}
```

The packages used in this analysis are catalogued in the `R/setup.R`. It is likely that some of those packages won't be installed on your system. Those will need to be installed for this project to run.

### Usage
Run `targets::tar_make()` to run project.

### Environmental Data Functions

All environmental data functions have some of common parameters:

- `aoi`: a spatial boundary that is an {sf} object
- `add_aoi_attributes`: a logical argument that asks whether you want to carry over the attributes from the aoi
- `start_date`: a date of date object (via `as.Date`)
- `end_date`: a date of date object (via `as.Date`)



### Air Quality

The air quality data are sourced from the [B.C. Data Catalogue](https://catalogue.data.gov.bc.ca/dataset/air-quality-monitoring-verified-hourly-data), provided under the Open Government Licence&mdash;British Columbia (this file: ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/2009-LatestVerified/PM25.csv)

Using the method implemented in `rcaaqs::pm_24h_caaqs()`, data were aggregated to a daily average. 

### Heatwaves

Heatwave detection is implemented using Open Government Licence - Canada data retrieved via the [{weathercan} package](https://docs.ropensci.org/weathercan/) and detected using the [{heatwaveR} package](https://robwschlegel.github.io/heatwaveR/index.html).


### Floods

Water level (hydrometric) data is retrieved from the Environment & Climate Change Canada HYDAT database, provided under the Open Government Licence - Canada, using the [{tidyhydat} package](https://docs.ropensci.org/tidyhydat/). 


### Project Status

This project is in the very early stages of design. 

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/climate-disturbances/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```
Copyright 2020 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```
---
*This project was created using the [bcgovr](https://github.com/bcgov/bcgovr) package.* 

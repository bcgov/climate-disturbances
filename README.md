[![img](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

# climate-disturbances

This project identifies extreme climate events from environmental data. The current focus is on heatwave detection.

### Installation and setup

This project leverages the [{targets} package](https://docs.ropensci.org/targets/), a pipeline toolkit for data science projects in R. The packages used in this analysis are catalogued in the `DESCRIPTION` file. The project also uses the [{renv} package]() to ensure reproducibility. To initialize the project on your machine, open the project (most easily by opening the `.Rproj` file in Rstudio), install the `{renv}` package and "restore" the project to ensure all of the necessary packages are installed (with the correct versions):

```r
# install.packages("renv")
renv::restore()
```

### Usage
Run `targets::tar_make()` to run the project.

### Heatwaves

Heatwave detection is implemented using Open Government Licence - Canada data retrieved from 
[Environment Canada](https://open.canada.ca/data/en/dataset/9c4ebc00-3ea4-4fe0-8bf2-66cfe1cddd1d) and detected using the [{heatwaveR} package](https://robwschlegel.github.io/heatwaveR/index.html).

#### Methods:

1. Adjusted and Homogenized Canadian Climate Data is downloaded and stored in a [`duckdb`](https://duckdb.org/docs/api/r) database.
2. A digital elevation model is combined with station temperature data from 1990-2020 to create daily maximum temperature gridded surfaces for B.C. using Thin Plate Splines (via the [`{fields}`](https://cran.r-project.org/package=fields) R package), and combined as a 3-dimensional raster cube (x, y, time) of daily maximum temperatures using the `{stars}` R package
3. Heatwaves are detected as follows:
    1. For each pixel:
      - Daily climate normals are generated using `heatwaveR::ts2clm()` and 30 years of daily maximum temperature data (1990-2020), and heatwave thresholds are calculated at the 90th percentile of the daily normal tmax.
      - Heatwaves are detected using `heatwaveR::detect_event()`, with a minimum duration of 2 days,
        and the thresholds calculated in the previous step.
    2. Climatologies are summarized across areas of interest (selected LHAs), 
        and heatwaves at the AOI level are detected where:
      - the mean daily maximum of pixels in the AOI is greater than the mean 90% threshold of pixels in the LHA
      - The 75th percentile of tmax of the pixels in the AOI is >= 30C
     
#### Outputs:

- Daily temperature summaries at the LHA level, as well as heatwave summaries are generated and stored in csv files `out/heatwave_summaries`
- daily gridded temperature surfaces are saved in `out/data/daily_temps` as `.tif` files


### Air Quality (currently on hold)

The air quality data are sourced from the [B.C. Data Catalogue](https://catalogue.data.gov.bc.ca/dataset/air-quality-monitoring-verified-hourly-data), provided under the Open Government Licence&mdash;British Columbia (this file: ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/2009-LatestVerified/PM25.csv)

Using the method implemented in `rcaaqs::pm_24h_caaqs()`, data were aggregated to a daily average. 


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

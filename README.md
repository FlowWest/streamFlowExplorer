# streamFlowExplorer<a href='https://flowwest.github.io/streamFlowExplorer'></a><img src="man/figures/streamFlowExplorer%20conflict.png" align="right" height="250" style="float:right; height:250px;"/>

## What is streamFlowExplorer?

StreamFlowExplorer is a data package developed by FlowWest and Metropolitan Water District to explore existing California empirical and modeled flow datasets. This package provides summaries describing the following California flow datasets and highlighting limitations for various use cases. This package explores the utility of these flow data for use in Salmonid population modeling.

**Empirical:**

-   [United States Geological Survey (USGS)](https://dashboard.waterdata.usgs.gov/app/nwd/en/?region=lower48&aoi=default)
-   [California Data Exchange Center (CDEC)](https://cdec.water.ca.gov/webgis/?appid=cdecstation)

**Modeled Datasets:**

-   [CalSim](https://water.ca.gov/Library/Modeling-and-Analysis/Central-Valley-models-and-tools/CalSim-3)
-   [SacWam](https://www.waterboards.ca.gov/waterrights/water_issues/programs/bay_delta/sacwam/)
-   [Natural Flow Database](https://rivers.codefornature.org/#/map)

## Install

To install the `streamFlowExplorer` data package, please use the remotes package to download from GitHub.

``` r
#install.packages("remotes")
remotes::install_github("flowwest/streamFlowExplorer")
```

## Flow Data Use Cases

The below tree displays a series of decision points and associated pathways to provide a guidance on what data type best fits your intended use case.

![](man/figures/mwd-flows%20-%20clean%20version.png)

If you find that for your flow data use, you ended in a brown square where additional modeling is needed, please refer to the article on Flow Modeling Approaches.

The following table provides a brief overview of the benefits and limitations of each data type for some potential use cases. Since this review is focused on flow data for ecological analysis this table primary focuses on that, but it also includes a few use cases for non-ecological analysis since some of these datasets are primarily developed for non ecological purposes.

|                           |                                    |                                                                                                                   |                                    |                                                                                                                            |
|-----------|------------|----------------|--------------|---------------------|
| **Use Category**          | **Scale**                          | **Use Case**                                                                                                      | **Data Type**                      | **Limitations**                                                                                                            |
| Ecological Modeling       | Single system/ regional analysis   | Studying the effects of streamflow changes on aquatic ecosystems                                                  | Empirical Flow Data (USGS or CDEC) | Availability and coverage vary by system, there are data gaps on some systems making regional analysis challenging         |
| Ecological Modeling       | Single system/ regional analysis   | Analyzing long term trends on in streamflow data to understand the effect of climate change on aquatic ecosystems | Empirical Flow Data (USGS or CDEC) | Availability and coverage vary by system, there are data gaps on some systems making regional analysis challenging         |
| Ecological Modeling       | Regional analysis                  | Analyzing long term trends on in streamflow data to understand the effect of climate change on aquatic ecosystems | CalSim and SacWam                  | Monthly timestep is limiting, summarized flows can miss critical flow events that may have ecological effects              |
| Ecological Modeling       | Single system                      | Hydraulic modeling for planning a restoration project                                                             | Empirical Flow Data (USGS or CDEC) | Availability and coverage vary by system, there are data gaps on some systems                                              |
| Ecological Modeling       | Single system                      | Hydraulic modeling for planning a restoration project                                                             | CalSim and SacWam                  | Monthly timestep is limiting, summarized flows can miss critical flow events that may have ecological effects              |
| Water Resource Management | Regional analysis                  | Assessing water availability for agriculture, urban use, and ecosystems.                                          | Empirical Flow Data (USGS or CDEC) | Does not allow for testing alternatives, only provides empirical data on the system                                        |
| Water Resource Management | Regional analysis                  | Assessing water availability for agriculture, urban use, and ecosystems.                                          | CalSim and SacWam                  | Only available for certain scenarios, high level of effort and experience needed to model additional operations scenarios  |

## Modeling Methods to Fill Data Gaps

The table and diagram above recommend and describe the benefits and limitations of using different flow data sources for different use cases. However, every project is unique and sometimes additional flow modeling will be necessary to get the granularity and completeness you need. Please see the Flow Modeling Approaches article for an overview of approaches and examples of each one.

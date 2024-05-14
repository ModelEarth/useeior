---
title: "Compare USEEIOv2.0.1-411 and USEEIOv2.1-422 Model"
date: "2021-12-30"
output:
  html_document:
    keep_md: yes
editor_options: 
  chunk_output_type: console
---

This document presents comparison results of USEEIOv2.0.1-411 and USEEIOv2.1-422 model.

#### Compare flow totals between two models

```r
mA <- buildModel(modelname_pair[1])
#> 2021-12-30 09:37:19 INFO::Begin model initialization...
#> 2021-12-30 09:37:20 INFO::Initializing IO tables...
#> 2021-12-30 09:37:20 INFO::Initializing Gross Output tables...
#> 2021-12-30 09:37:21 INFO::Initializing Chain Price Index tables...
#> 2021-12-30 09:37:22 INFO::Loading disaggregation specification file for WasteDisaggregationDetail...
#> 2021-12-30 09:37:22 INFO::Initializing Disaggregation of IO tables...
#> 2021-12-30 09:37:23 INFO::Initializing model satellite tables...
#> 2021-12-30 09:37:23 INFO::Loading Water withdrawals flows from DataCommons...
#> 2021-12-30 09:37:23 INFO::Loading Criteria and Hazardous Air Emissions flows from DataCommons...
#> 2021-12-30 09:37:34 INFO::Loading Point source industrial releases to ground flows from DataCommons...
#> 2021-12-30 09:37:35 INFO::Loading Point source releases to water flows from DataCommons...
#> 2021-12-30 09:37:36 INFO::Loading Greenhouse Gases flows from DataCommons...
#> 2021-12-30 09:37:36 INFO::Loading Land use flows from DataCommons...
#> 2021-12-30 09:37:37 INFO::Loading Mineral extraction flows from DataCommons...
#> 2021-12-30 09:37:37 INFO::Loading Energy extraction flows from DataCommons...
#> 2021-12-30 09:37:37 WARNING::No data found for disaggregation of ENERGY for 562000/US - applying default allocation
#> 2021-12-30 09:37:37 INFO::Loading Nitrogen and Phosphorus Releases from Agriculture flows from DataCommons...
#> 2021-12-30 09:37:37 INFO::Loading Pesticide releases flows from DataCommons...
#> 2021-12-30 09:37:37 INFO::Loading Commercial non-hazardous waste excluding construction activities flows from DataCommons...
#> 2021-12-30 09:37:37 WARNING::No data found for disaggregation of CNHW for 562000/US - applying default allocation
#> 2021-12-30 09:37:38 INFO::Loading Commercial non-hazardous waste from construction activities flows from DataCommons...
#> 2021-12-30 09:37:38 INFO::Loading Commercial RCRA-defined hazardous waste flows from DataCommons...
#> 2021-12-30 09:37:41 INFO::Loading Employment flows from DataCommons...
#> 2021-12-30 09:37:41 INFO::Generating Value Added flows...
#> 2021-12-30 09:37:42 INFO::Initializing model indicators...
#> 2021-12-30 09:37:42 INFO::Getting Greenhouse Gases indicator from DataCommons...
#> 2021-12-30 09:37:42 INFO::Getting Acidification Potential indicator from DataCommons...
#> 2021-12-30 09:37:42 INFO::Getting Eutrophication Potential indicator from DataCommons...
#> 2021-12-30 09:37:43 INFO::Getting Freshwater Ecotoxicity Potential indicator from DataCommons...
#> 2021-12-30 09:37:50 INFO::Getting Human Health - Cancer indicator from DataCommons...
#> 2021-12-30 09:37:51 INFO::Getting Human Health - Noncancer indicator from DataCommons...
#> 2021-12-30 09:37:52 INFO::Getting Human Health Toxicity indicator from DataCommons...
#> 2021-12-30 09:37:55 INFO::Getting Human Health - Respiratory Effects indicator from DataCommons...
#> 2021-12-30 09:37:55 INFO::Getting Ozone Depletion indicator from DataCommons...
#> 2021-12-30 09:37:55 INFO::Getting Smog Formation Potential indicator from DataCommons...
#> 2021-12-30 09:37:56 INFO::Getting Freshwater withdrawals indicator from DataCommons...
#> 2021-12-30 09:37:56 INFO::Getting Land use indicator from DataCommons...
#> 2021-12-30 09:37:56 INFO::Getting Hazardous Air Pollutants indicator from DataCommons...
#> 2021-12-30 09:37:56 INFO::Getting Pesticides indicator from DataCommons...
#> 2021-12-30 09:37:56 INFO::Getting Nonrenewable Energy Use indicator from DataCommons...
#> 2021-12-30 09:37:57 INFO::Getting Renewable Energy Use indicator from DataCommons...
#> 2021-12-30 09:37:57 INFO::Getting Energy Use indicator from DataCommons...
#> 2021-12-30 09:37:57 INFO::Getting Minerals and Metals Use indicator from DataCommons...
#> 2021-12-30 09:37:57 INFO::Getting Value Added indicator from useeior...
#> 2021-12-30 09:37:57 INFO::Getting Jobs Supported indicator from useeior...
#> 2021-12-30 09:37:57 INFO::Getting Commercial RCRA Hazardous Waste indicator from useeior...
#> 2021-12-30 09:37:57 INFO::Getting Commercial Municipal Solid Waste indicator from useeior...
#> 2021-12-30 09:37:57 INFO::Getting Commercial Construction and Demolition Debris indicator from useeior...
#> 2021-12-30 09:37:58 INFO::Loading demand vectors ...
#> 2021-12-30 09:37:58 INFO::Loading CompleteProduction demand vector...
#> 2021-12-30 09:37:58 INFO::Loading DomesticProduction demand vector...
#> 2021-12-30 09:37:58 INFO::Loading CompleteConsumption demand vector...
#> 2021-12-30 09:37:58 INFO::Loading DomesticConsumption demand vector...
#> 2021-12-30 09:38:01 INFO::Building commodity-by-commodity A matrix (direct requirements)...
#> 2021-12-30 09:38:01 INFO::Building commodity-by-commodity A_d matrix (domestic direct requirements)...
#> 2021-12-30 09:38:01 INFO::Calculating L matrix (total requirements)...
#> 2021-12-30 09:38:01 INFO::Calculating L_d matrix (domestic total requirements)...
#> 2021-12-30 09:38:01 INFO::Building B matrix (direct emissions and resource use per dollar)...
#> 2021-12-30 09:38:04 INFO::Building C matrix (characterization factors for model indicators)...
#> 2021-12-30 09:38:09 INFO::Calculating D matrix (direct environmental impacts per dollar)...
#> 2021-12-30 09:38:09 INFO::Calculating M matrix (total emissions and resource use per dollar)...
#> 2021-12-30 09:38:09 INFO::Calculating M_d matrix (total emissions and resource use per dollar from domestic activity)...
#> 2021-12-30 09:38:10 INFO::Calculating N matrix (total environmental impacts per dollar)...
#> 2021-12-30 09:38:10 INFO::Calculating N_d matrix (total environmental impacts per dollar from domestic activity)...
#> 2021-12-30 09:38:10 INFO::Calculating Rho matrix (price year ratio)...
#> 2021-12-30 09:38:10 INFO::Calculating Phi matrix (producer over purchaser price ratio)...
#> 2021-12-30 09:38:10 INFO::Model build complete.
mB <- buildModel(modelname_pair[2])
#> 2021-12-30 09:38:10 INFO::Begin model initialization...
#> 2021-12-30 09:38:10 INFO::Initializing IO tables...
#> 2021-12-30 09:38:10 INFO::Initializing Gross Output tables...
#> 2021-12-30 09:38:12 INFO::Initializing Chain Price Index tables...
#> 2021-12-30 09:38:13 INFO::Loading aggregation specification file for ElectricityAggregationDetail...
#> 2021-12-30 09:38:13 INFO::Initializing Aggregation of IO tables...
#> 2021-12-30 09:38:13 INFO::Loading disaggregation specification file for ElectricityDisaggregationDetail...
#> 2021-12-30 09:38:13 INFO::Loading disaggregation specification file for WasteDisaggregationDetail...
#> 2021-12-30 09:38:13 INFO::Initializing Disaggregation of IO tables...
#> 2021-12-30 09:38:14 INFO::Initializing model satellite tables...
#> 2021-12-30 09:38:14 INFO::Loading Water withdrawals flows from DataCommons...
#> 2021-12-30 09:38:15 INFO::Loading Criteria and Hazardous Air Emissions flows from DataCommons...
#> 2021-12-30 09:38:30 INFO::Loading Point source industrial releases to ground flows from DataCommons...
#> 2021-12-30 09:38:30 INFO::Loading Point source releases to water flows from DataCommons...
#> 2021-12-30 09:38:32 INFO::Loading Greenhouse Gases flows from DataCommons...
#> 2021-12-30 09:38:32 INFO::Loading Land use flows from DataCommons...
#> 2021-12-30 09:38:32 INFO::Loading Mineral extraction flows from DataCommons...
#> 2021-12-30 09:38:32 INFO::Loading Energy extraction flows from DataCommons...
#> 2021-12-30 09:38:32 WARNING::No data found for disaggregation of ENERGY for 562000/US - applying default allocation
#> 2021-12-30 09:38:32 INFO::Loading Nitrogen and Phosphorus Releases from Agriculture flows from DataCommons...
#> 2021-12-30 09:38:32 INFO::Loading Pesticide releases flows from DataCommons...
#> 2021-12-30 09:38:33 INFO::Loading Commercial non-hazardous waste excluding construction activities flows from DataCommons...
#> 2021-12-30 09:38:33 WARNING::No data found for disaggregation of CNHW for 562000/US - applying default allocation
#> 2021-12-30 09:38:34 INFO::Loading Commercial non-hazardous waste from construction activities flows from DataCommons...
#> 2021-12-30 09:38:34 INFO::Loading Commercial RCRA-defined hazardous waste flows from DataCommons...
#> 2021-12-30 09:38:36 INFO::Loading Employment flows from DataCommons...
#> 2021-12-30 09:38:37 INFO::Generating Value Added flows...
#> 2021-12-30 09:38:37 INFO::Initializing model indicators...
#> 2021-12-30 09:38:37 INFO::Getting Greenhouse Gases indicator from DataCommons...
#> 2021-12-30 09:38:38 INFO::Getting Acidification Potential indicator from DataCommons...
#> 2021-12-30 09:38:38 INFO::Getting Eutrophication Potential indicator from DataCommons...
#> 2021-12-30 09:38:38 INFO::Getting Freshwater Ecotoxicity Potential indicator from DataCommons...
#> 2021-12-30 09:38:43 INFO::Getting Human Health - Cancer indicator from DataCommons...
#> 2021-12-30 09:38:45 INFO::Getting Human Health - Noncancer indicator from DataCommons...
#> 2021-12-30 09:38:46 INFO::Getting Human Health Toxicity indicator from DataCommons...
#> 2021-12-30 09:38:48 INFO::Getting Human Health - Respiratory Effects indicator from DataCommons...
#> 2021-12-30 09:38:49 INFO::Getting Ozone Depletion indicator from DataCommons...
#> 2021-12-30 09:38:49 INFO::Getting Smog Formation Potential indicator from DataCommons...
#> 2021-12-30 09:38:50 INFO::Getting Freshwater withdrawals indicator from DataCommons...
#> 2021-12-30 09:38:50 INFO::Getting Land use indicator from DataCommons...
#> 2021-12-30 09:38:50 INFO::Getting Hazardous Air Pollutants indicator from DataCommons...
#> 2021-12-30 09:38:50 INFO::Getting Pesticides indicator from DataCommons...
#> 2021-12-30 09:38:50 INFO::Getting Nonrenewable Energy Use indicator from DataCommons...
#> 2021-12-30 09:38:50 INFO::Getting Renewable Energy Use indicator from DataCommons...
#> 2021-12-30 09:38:50 INFO::Getting Energy Use indicator from DataCommons...
#> 2021-12-30 09:38:51 INFO::Getting Minerals and Metals Use indicator from DataCommons...
#> 2021-12-30 09:38:51 INFO::Getting Value Added indicator from useeior...
#> 2021-12-30 09:38:51 INFO::Getting Jobs Supported indicator from useeior...
#> 2021-12-30 09:38:51 INFO::Getting Commercial RCRA Hazardous Waste indicator from useeior...
#> 2021-12-30 09:38:51 INFO::Getting Commercial Municipal Solid Waste indicator from useeior...
#> 2021-12-30 09:38:51 INFO::Getting Commercial Construction and Demolition Debris indicator from useeior...
#> 2021-12-30 09:38:51 INFO::Loading demand vectors ...
#> 2021-12-30 09:38:51 INFO::Loading CompleteProduction demand vector...
#> 2021-12-30 09:38:51 INFO::Loading DomesticProduction demand vector...
#> 2021-12-30 09:38:51 INFO::Loading CompleteConsumption demand vector...
#> 2021-12-30 09:38:51 INFO::Loading DomesticConsumption demand vector...
#> 2021-12-30 09:38:51 INFO::Loading HouseholdConsumption demand vector...
#> 2021-12-30 09:38:55 INFO::Building commodity-by-commodity A matrix (direct requirements)...
#> 2021-12-30 09:38:55 INFO::Building commodity-by-commodity A_d matrix (domestic direct requirements)...
#> 2021-12-30 09:38:55 INFO::Calculating L matrix (total requirements)...
#> 2021-12-30 09:38:55 INFO::Calculating L_d matrix (domestic total requirements)...
#> 2021-12-30 09:38:55 INFO::Building B matrix (direct emissions and resource use per dollar)...
#> 2021-12-30 09:38:58 INFO::Building C matrix (characterization factors for model indicators)...
#> 2021-12-30 09:39:03 INFO::Calculating D matrix (direct environmental impacts per dollar)...
#> 2021-12-30 09:39:03 INFO::Calculating M matrix (total emissions and resource use per dollar)...
#> 2021-12-30 09:39:04 INFO::Calculating M_d matrix (total emissions and resource use per dollar from domestic activity)...
#> 2021-12-30 09:39:04 INFO::Calculating N matrix (total environmental impacts per dollar)...
#> 2021-12-30 09:39:04 INFO::Calculating N_d matrix (total environmental impacts per dollar from domestic activity)...
#> 2021-12-30 09:39:04 INFO::Calculating Rho matrix (price year ratio)...
#> 2021-12-30 09:39:04 INFO::Calculating Phi matrix (producer over purchaser price ratio)...
#> 2021-12-30 09:39:04 INFO::Model build complete.
```


```r
# Compare flow totals
model_com <- compareFlowTotals(mA, mB)
cat(paste("Number of flow totals by commodity passing:",model_com$N_Pass))
```

Number of flow totals by commodity passing: 2722

```r
cat(paste("Number of flow totals by commodity failing:",model_com$N_Fail))
```

Number of flow totals by commodity failing: 0

```r
#cat(paste("Sectors with flow totals failing:", paste(model_com$Failure$rownames, collapse = ", ")))
```

There are flow differences between USEEIOv2.0.1-411 and USEEIOv2.1-422 

Flows in USEEIOv2.0.1-411 not in USEEIOv2.1-422 are

character(0)

 Flows in USEEIOv2.1-422 not in USEEIOv2.0.1-411 are

character(0)

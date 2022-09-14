# Ukraine analyses
Analyses relating to how the Ukraine conflict has and could impact the UK.

## Loading visa data

- [`R/load Ukraine visa data - Local Authorities.R`](R/load%20Ukraine%20visa%20data%20-%20Local%20Authorities.R): Download and wrangle data on visa applications, visa issuance, and arrivals for the [Homes for Ukraine scheme in Local Authorities](https://www.gov.uk/guidance/ukraine-sponsorship-scheme-visa-data-by-country-upper-and-lower-tier-local-authority) across the UK. This code automatically downloads updated datasets when they are released by DLUHC.
- [`R/load Ukraine visa data - scraped.R`](R/load%20Ukraine%20visa%20data%20-%20scraped.R): We have manually scraped [UK summary data](https://www.gov.uk/government/publications/ukraine-family-scheme-application-data/ukraine-family-scheme-and-ukraine-sponsorship-scheme-homes-for-ukraine-visa-data) on the three visa schemes from DLUHC - this file loads it.

## Predicting arrivals in the UK
We developed a sophisticated and novel approach that simulates the entire visa process, from the initial application through to arriving in the UK - see [the simulation's README](simulating-arrivals.md) for details.

The outputs from the simulation are stored in `output-data/simulations`.

- [`R/arrivals from Ukraine - trends and forecasts.R`](R/arrivals%20from%20Ukraine%20-%20trends%20and%20forecasts.R): The main R script that initialises and runs the simulation to forecast arrivals under a baseline scenario and a scenario in which arrivals would surge.
- [`R/arrivals from Ukraine - regional forecasts.R`](R/arrivals%20from%20Ukraine%20-%20regional%20forecasts.R): Forecast arrivals by British Red Cross region.
- [`R/arrivals from Ukraine - check simulated arrivals against newly observed data.R`](R/check%20simulated%20arrivals%20against%20newly%20observed%20data.R): Check our predictions against the [latest DLUHC data](https://www.gov.uk/government/publications/ukraine-family-scheme-application-data).
- [`R/arrivals from Ukraine - projection from current visa backlog.R`](R/projection%20from%20current%20visa%20backlog.R): Models a scenario in which there are no new visa applications: how many arrivals would we expect purely based on the current backlog of visas issued?

## Anticipating homelessness among Ukrainian arrivals
Given the lack of data on Ukrainian homelessness in the devolved nations, we built an Index of Housing Insecurity that can be used to anticipate in which Local Authorities Ukrainian households might be at risk of homelessness. 

The Index of Housing Insecurity data are stored in `output-data/index-of-housing-insecurity`.

- [`R/load Ukraine homelessness data.R`](R/load%20Ukraine%20homelessness%20data.R): Download Ukrainian homelessness data for England, calculate trends, and save a local copy. You will only need to run this once.
- [`R/housing and homelessness - wrangle data.R`](R/housing%20and%20homelessness%20-%20wrangle%20data.R): Downloads and processes the underlying indicators of housing insecurity for each UK nation.
- [`R/housing and homelessness - build composite index.R`](R/housing%20and%20homelessness%20-%20build%20composite%20index.R): Builds and saves the composite Index of Housing Insecurity - one file per nation.
- [`R/housing and homelessness - check index.R`](R/housing%20and%20homelessness%20-%20check%20index.R): Explores how well the Index of Housing Insecurity predicts Ukrainian homelessness in England, showing that we could feasibly generalise the relationship to other nations. It also compares our Index to the Index of Multiple Deprivation.
- [`R/housing and homelessness - map.R`](R/housing%20and%20homelessness%20-%20map.R): Make a map showing the 10% of Local Authorities with the highest risks of housing insecurity, shaded by the number of Ukrainian people living in those areas.
- [`R/housing and homelessness - build statistical index.R`](R/housing%20and%20homelessness%20-%20build%20statistical%20index.R): An experiment at using machine learning techniques to predict Ukrainian homelessness in England then use that model to predict homelessness in devolved nations. The housing insecurity indicators are not comparable, so this wans't possible - but saving this code for posterity.
- [`R/ukraine homelessness.R`](R/ukraine%20homelessness.R): Analyse patterns and trends in risk of homelessness and temporary accommodation for Ukrainians in England.

## Census 2021 data on Ukrainians in England and Wales

- [`R/load-census-2021-ukraine.R`](R/load-census-2021-ukraine.R): Load pre-released Census 2021 data from ONS on counts of people from Ukraine living in Local Authorities in England and Wales.
- [`R/map-ukrainians-census-2021.R`](R/map-ukrainians-census-2021.R): Make a map based on the Census data.

## Under-doctored areas
We calculated the number of patients per GP in each Local Authority of the UK. Places with the highest numbers of patients per GP are known as ['under-doctored areas'](https://www.gponline.com/map-englands-underdoctored-areas/article/1739735). We then looked where people from Ukraine are living in under-doctored areas.

The data on under-doctored areas is saved in `output-data/under-doctored-areas`.

- [`R/under-doctored areas and Ukrainian families.R`](R/under-doctored%20areas%20and%20Ukrainian%20families.R): Generate the list of under-doctored areas and make a map highlighting where more Ukrainian people are living.

## British Red Cross capacity planning

- [`R/forecasts-cba.R`](R/forecasts-cba.R): Forecast cash-based assistance from the simulation of visa arrivals.
- [`R/capacity-analyses.R`](R/capacity-analyses.R): Forecast British Red Cross Crisis Response and Refugee Support capacity potentially needed to support Ukrainians, based on the simulation of visa arrivals.

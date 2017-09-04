Downloading and cleaning Monitoring Trends in Burn Severity (MTBS) fire data
================

Requirements
------------

Downloading and processing the data requires that you have [R](https://www.r-project.org/) installed, and the following R packages:

-   dplyr
-   rgdal
-   rgeos
-   tidyr

If you do not have these packages, you can try to install them with `install.packages()`, e.g.,

``` r
install.packages('tidyr')
```

The script `get-mtbs-data.R` pulls data from the internet, so you'll need an internet connection.

Running the script
------------------

To run the R script, you can use `Rscript` from the terminal:

``` bash
Rscript get-mtbs-data.R
```

Or, if you prefer, you can open the file in R or Rstudio and run it interactively.

What it produces
----------------

The script will produce a directory with the following structure:

    data/
      - processed/
        - fire-events.csv
        - fire-summary.csv
      - raw/
        - mtbs_fod_pts_data/
          - ...
        - us_eco_l3/
          - ...

### Event data

The file `fire-events.csv` has the raw data from MTBS where each row is a fire event:

| fire\_id              |  year|  month|  day|     lat|     long|  acres\_burned| na\_l3name             | na\_l2name                                            | na\_l1name                | yearmonth |  mean\_potential\_et|  total\_precip|  max\_air\_temp|  mean\_wind\_speed|
|:----------------------|-----:|------:|----:|-------:|--------:|--------------:|:-----------------------|:------------------------------------------------------|:--------------------------|:----------|--------------------:|--------------:|---------------:|------------------:|
| AL3038308812219980404 |  1998|      4|    4|  30.383|  -88.122|            705| Southern Coastal Plain | MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS | EASTERN TEMPERATE FORESTS | 1998-04   |             4.757972|       50.62457|        299.6400|           4.281175|
| AL3041008830120050916 |  2005|      9|   16|  30.410|  -88.301|           2914| Southern Coastal Plain | MISSISSIPPI ALLUVIAL AND SOUTHEAST USA COASTAL PLAINS | EASTERN TEMPERATE FORESTS | 2005-09   |             4.952767|       90.42460|        305.1668|           3.975596|
| AL3067208833720050207 |  2005|      2|    7|  30.672|  -88.337|            696| Southeastern Plains    | SOUTHEASTERN USA PLAINS                               | EASTERN TEMPERATE FORESTS | 2005-02   |             2.275852|      110.09175|        289.2758|           4.016748|
| AL3105208777220050211 |  2005|      2|   11|  31.052|  -87.772|           7334| Southeastern Plains    | SOUTHEASTERN USA PLAINS                               | EASTERN TEMPERATE FORESTS | 2005-02   |             2.275852|      110.09175|        289.2758|           4.016748|
| AL3110108837320050218 |  2005|      2|   18|  31.101|  -88.373|            757| Southeastern Plains    | SOUTHEASTERN USA PLAINS                               | EASTERN TEMPERATE FORESTS | 2005-02   |             2.275852|      110.09175|        289.2758|           4.016748|

The columns are as follows:

-   `fire_id`: unique event identifier
-   `year`, `month`, `day`: year, month, day of ignition
-   `lat`, `long`: lat and long of ignition
-   `acres_burned`: number of acres burned
-   `na_l3name`, `na_l2name`, `na_l1name`: level 3, 2, and 1 ecoregion names
-   `yearmonth`: year and month concatenated (e.g., '2010-01')

Both of these files have preprocessed [MACA](http://maca.northwestknowledge.net/) climate data summarized at the level of month and ecoregion, with four variables: mean daily potential evapotranspiration (`mean_potential_et`), total precipitation (`total_precip`), mean daily maximum air temperature (`max_air_temp`), and mean daily wind speed (`mean_wind_speed`).

### Summary data

The file `fire-summary.csv` has the number of fires and total burn area summarized for each of the 85 EPA level 3 ecoregions and 382 months over the range of dates in the MTBS data (for a total of 382 X 85 = 32470 rows), where each row is an ecoregion by month combination.

| yearmonth | na\_l3name                               |  n\_fire|  total\_acres\_burned|  year|  month|  mean\_potential\_et|  total\_precip|  max\_air\_temp|  mean\_wind\_speed|
|:----------|:-----------------------------------------|--------:|---------------------:|-----:|------:|--------------------:|--------------:|---------------:|------------------:|
| 1984-02   | Acadian Plains and Hills                 |        0|                     0|  1984|      2|            1.1805210|     116.813903|        275.4773|           4.962357|
| 1984-02   | Arizona/New Mexico Mountains             |        0|                     0|  1984|      2|            2.7397637|       3.008436|        285.1002|           3.876059|
| 1984-02   | Arizona/New Mexico Plateau               |        0|                     0|  1984|      2|            2.2535006|       3.420493|        283.5001|           3.200126|
| 1984-02   | Arkansas Valley                          |        0|                     0|  1984|      2|            2.5848122|      94.998374|        288.3537|           4.530387|
| 1984-02   | Aspen Parkland/Northern Glaciated Plains |        0|                     0|  1984|      2|            0.8037277|      15.325724|        274.3547|           3.577555|

### Computing polygon centroids and distance matrices

``` r
library(rgdal)
library(rgeos)

# load ecoregion shapefile
ecoregion_file <- list.files(pattern = 'us_eco_l3.shp$',
                             recursive = TRUE,
                             full.names = TRUE)
ecoregions <- readOGR(ecoregion_file)
```

#### Computing centroids

Next, we'll use the `gCentroid` function from the `rgeos` package to compute centroids for each polygon in the ecoregion shapefile:

``` r
centroids <- gCentroid(ecoregions, byid = TRUE)
```

Last, because some ecoregions are discontinuous, a subset of our ecoregions now have multiple centroids. To get just one centroid per ecoregion, we can average across all of the centroids for those ecoregions with multiple centroids:

``` r
library(dplyr)

ecoregion_centroids <- as.data.frame(centroids) %>%
  tbl_df %>%
  mutate(na_l3name = ecoregions$NA_L3NAME) %>%
  group_by(na_l3name) %>%
  summarize(x = mean(x), 
            y = mean(y))
```

This will return a data frame with the centroids for each ecoregion. As a final step, you may need to compute a distance matrix, which provides pairwise distances among all centroids.

#### Computing a distance matrix

``` r
D <- ecoregion_centroids %>%
  select(x, y) %>%
  dist %>%
  as.matrix

# associate ecoregions with rows and columns
rownames(D) <- ecoregion_centroids$na_l3name
colnames(D) <- ecoregion_centroids$na_l3name

# preview part of D
D[1:3, 1:3]
```

|                              |  Acadian Plains and Hills|  Arizona/New Mexico Mountains|  Arizona/New Mexico Plateau|
|------------------------------|-------------------------:|-----------------------------:|---------------------------:|
| Acadian Plains and Hills     |                         0|                     3443179.1|                   3514975.6|
| Arizona/New Mexico Mountains |                   3443179|                           0.0|                    200954.1|
| Arizona/New Mexico Plateau   |                   3514976|                      200954.1|                         0.0|

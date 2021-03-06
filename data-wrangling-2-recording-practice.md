data-wrangling 2 practice
================
Seonyoung Park (sp3804)
11/1/2020

## Scrap a table

I want the first table from
\[here\]<http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm>\]

read in the
html

``` r
url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"
drug_use_html = read_html(url)
```

1.  extract the table(s); from the website, I’ve extracted all the
    tables
2.  I’m gonna focus on the first one
3.  get html table
4.  cut the first row (the “note” at the bottom of the table appears in
    every column in the first row.)
5.  change it to table

<!-- end list -->

``` r
table_mar = 
  drug_use_html %>%
  html_nodes(css = "table") %>%
  first() %>%
  html_table %>%
  slice(-1) %>%
  as_tibble()

table_mar
```

    ## # A tibble: 56 x 16
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12+(P Value)` `12-17(2013-201…
    ##    <chr> <chr>            <chr>            <chr>          <chr>           
    ##  1 Tota… 12.90a           13.36            0.002          13.28b          
    ##  2 Nort… 13.88a           14.66            0.005          13.98           
    ##  3 Midw… 12.40b           12.76            0.082          12.45           
    ##  4 South 11.24a           11.64            0.029          12.02           
    ##  5 West  15.27            15.62            0.262          15.53a          
    ##  6 Alab… 9.98             9.60             0.426          9.90            
    ##  7 Alas… 19.60a           21.92            0.010          17.30           
    ##  8 Ariz… 13.69            13.12            0.364          15.12           
    ##  9 Arka… 11.37            11.59            0.678          12.79           
    ## 10 Cali… 14.49            15.25            0.103          15.03           
    ## # … with 46 more rows, and 11 more variables: `12-17(2014-2015)` <chr>,
    ## #   `12-17(P Value)` <chr>, `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `18-25(P Value)` <chr>, `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>,
    ## #   `26+(P Value)` <chr>, `18+(2013-2014)` <chr>, `18+(2014-2015)` <chr>,
    ## #   `18+(P Value)` <chr>

## Star Wars Movie Info

I want the data from [here](https://www.imdb.com/list/ls070150896/).

``` r
url="https://www.imdb.com/list/ls070150896/"

swm_html = read_html(url)
```

Grab elements that I want- using SelectorGadget 1. Extract each vector
2. Make a table using all the vectors.

``` r
title_vec = 
  swm_html %>%
  html_nodes(css = ".lister-item-header a") %>%
  html_text()

gross_rev_vec = 
  swm_html %>%
  html_nodes(css = ".text-muted .ghost~ .text-muted+ span") %>%
  html_text()

runtime_vec = 
  swm_html %>%
  html_nodes(css = ".runtime") %>%
  html_text()

swm_df = 
  tibble(
    title = title_vec,
    gross_rev = gross_rev_vec,
    runtime = runtime_vec
  )
```

## Get some water data

1.  This is coming from an API; if we can use csv, always use csv first.
    If not, use Jason, but more work to do.

2.  content(“parsed”) make a tibble.

<!-- end list -->

``` r
nyc_water = 
  GET("https://data.cityofnewyork.us/resource/ia2d-e54m.csv") %>%
  content("parsed")
```

    ## Parsed with column specification:
    ## cols(
    ##   year = col_double(),
    ##   new_york_city_population = col_double(),
    ##   nyc_consumption_million_gallons_per_day = col_double(),
    ##   per_capita_gallons_per_person_per_day = col_double()
    ## )

``` r
nyc_water = 
  GET("https://data.cityofnewyork.us/resource/ia2d-e54m.json") %>% 
  content("text") %>%
  jsonlite::fromJSON() %>%
  as_tibble()
```

## BRFSS

Same process, different data

``` r
brfss_2010 = 
  GET("https://chronicdata.cdc.gov/resource/acme-vg9e.csv") %>%
  content("parsed")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   year = col_double(),
    ##   sample_size = col_double(),
    ##   data_value = col_double(),
    ##   confidence_limit_low = col_double(),
    ##   confidence_limit_high = col_double(),
    ##   display_order = col_double(),
    ##   locationid = col_logical()
    ## )

    ## See spec(...) for full column specifications.

By default, the CDC API limits data to the first 1000 rows. Here I’ve
increased that by changing an element of the API query. I looked around
the website describing the API to find the name of the argument, and
then used the appropriate syntax for GET. To get the full data, I could
increase this so that I get all the data at once or I could try
iterating over chunks of a few thousand rows.

  - The
    “\(limit" parameter chooses how many records to return per page, and "\)offset”
    tells the API on what record to start returning data.
  - GET(“<http://google.com/>”, path = “search”, query = list(q =
    “ham”))

<!-- end list -->

``` r
brfss_2010 = 
  GET("https://chronicdata.cdc.gov/resource/acme-vg9e.csv", query = list("$limit"=5000)) %>%
  content("parsed")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_character(),
    ##   year = col_double(),
    ##   sample_size = col_double(),
    ##   data_value = col_double(),
    ##   confidence_limit_low = col_double(),
    ##   confidence_limit_high = col_double(),
    ##   display_order = col_double(),
    ##   locationid = col_logical()
    ## )

    ## See spec(...) for full column specifications.

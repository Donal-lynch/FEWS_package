<!-- README.md is generated from README.Rmd. Please edit that file -->
FEWS
====

The FEWS method was introduced by Frances Krsinich in [The FEWS index: Fixed effects with a window splice](https://www.researchgate.net/publication/303888203_The_FEWS_index_Fixed_effects_with_a_window_splice). This FEWS package aims to provide a flexible implementation of that method, with some additional splicing options available for users. As well as producing an index, the package provides users with some diagnostic results on the FEWS calculation which may be of use in data exploration, or in monitoring of production systems. In addition the package provides some utilities commonly used by Pricing teams from Statistical Agencies such as converting between indexes and movements.

Installation
============

FEWS is still in development, with the intention that it will exist on CRAN. For now it can be installed from GitHub using the following code

``` r

devtools::install_github("Donal-lynch/FEWS_package")

# Once installed, the package can be loaded as usual
library(FEWS)
```

Usage
=====

The primary function provided by the FEWS package is the `FEWS()` function. Running `?FEWS()` should give all the required information on how using the function. An example of running the `FEWS()` the function is shown below.

Example
-------

As part of the package, a couple of datasets are provided, including the Turvery dataset as found in the [Consumer Price Index Manual](https://www.ilo.org/wcmsp5/groups/public/---dgreports/---stat/documents/presentation/wcms_331153.pdf).

``` r
ggplot(turvey, aes(x = month, y = price)) + 
  geom_line(aes(color = commodity)) + 
  geom_point(aes(color = commodity))+
  theme_bw() +
  ggtitle("Artificial prices of seasonal products Data created by R. Turvey")
```

![](README-data_viz-1.png)

The FEWS is calculated below with a mean splice and a window length of 13 months.

``` r
turvey_FEWS <- FEWS(times = turvey$month,
                    logprice = log(turvey$price),
                    id = turvey$commodity,
                    window_length = 13,
                    weight = turvey$price * turvey$quantity,
                    splice_pos = "mean",
                    num_cores = NULL)
```

The resulting index is displayed below

``` r

ggplot(turvey_FEWS$fews, aes(x = price_date, y = fe_indexes)) + 
  geom_line() + 
  theme_bw() +
  ggtitle("FEWS with mean splice for Turvey data")+
  ylab("Price Index") + 
  xlab("Date")
```

![](README-fews_result-1.png)

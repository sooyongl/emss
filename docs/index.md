# emss

## Overview

This package aims to support Embedded Standard Setting, calculating
counts and weights.

## Install

``` r
devtools::install_github("sooyongl/emss", dependencies = T)
```

## Usage

``` r
library(emss)
```

``` r
fake_data <- genFakeDataSet(ngca = 3, 
               cor_val = 0.2,
               n = 30,
               nlevel = 3,
               ntable = 5,
               npanelist = 5,
               sdinp = 1,
               ecinp = 0,
               100,300)



fake_data
#> $setup
#> # A tibble: 3 × 7
#>   GCA   `Content Area` `Grade Group` `Number of Tables per Room` `Level Options`
#>   <chr> <chr>          <chr>         <chr>                       <chr>          
#> 1 M1    Math           1             5                           Level1, Level2…
#> 2 M2    Math           2             5                           Level1, Level2…
#> 3 M3    Math           3             5                           Level1, Level2…
#> # ℹ 2 more variables: SD <dbl>, EC <dbl>
#> 
#> $panelist
#> # A tibble: 75 × 3
#>   GCA   Panelist Table
#>   <chr> <chr>    <chr>
#> 1 M1    M1.T1.1  T1   
#> 2 M1    M1.T1.2  T1   
#> 3 M1    M1.T1.3  T1   
#> 4 M1    M1.T1.4  T1   
#> 5 M1    M1.T1.5  T1   
#> # ℹ 70 more rows
#> 
#> $rating
#> # A tibble: 2,250 × 8
#>   GCA   Subject Grade Round Table Panelist Item_ID ALD   
#>   <chr> <chr>   <chr> <dbl> <chr> <chr>    <chr>   <chr> 
#> 1 M1    Math    1         1 T1    M1.T1.1  M1_1    Level1
#> 2 M1    Math    1         1 T1    M1.T1.1  M1_2    Level2
#> 3 M1    Math    1         1 T1    M1.T1.1  M1_3    Level2
#> 4 M1    Math    1         1 T1    M1.T1.1  M1_4    Level1
#> 5 M1    Math    1         1 T1    M1.T1.1  M1_5    Level2
#> # ℹ 2,245 more rows
#> 
#> $item_data
#> # A tibble: 90 × 7
#>   GCA   Domain Item_ID Total_points Point Order_of_Difficulty Loc_RP50
#>   <chr> <chr>  <chr>          <dbl> <dbl>               <int>    <dbl>
#> 1 M1    Alg    M1_1               1     1                   1     100.
#> 2 M1    Geom   M1_2               1     1                   2     100.
#> 3 M1    Alg    M1_3               1     1                   3     100.
#> 4 M1    Calc   M1_4               1     1                   4     102.
#> 5 M1    Alg    M1_5               1     1                   5     102.
#> # ℹ 85 more rows
#> 
#> $examineedata_data
#> # A tibble: 680 × 3
#>   score  freq Grade
#>   <int> <int> <chr>
#> 1    84     3 M1   
#> 2    85     4 M1   
#> 3    86    12 M1   
#> 4    87     4 M1   
#> 5    88     4 M1   
#> # ℹ 675 more rows

# Or you can use your own data structured by the following table.
```

``` r
res <- emstans(data = fake_data,
               grades = c(1, 2),
               location = "Loc_RP50",
               method = "weight",
               smooth = FALSE)
```

## Summary

``` r
summary(res)
#>     unit cut_level modal median average agreement weighted_kappa
#> 1 M1-All    Level2 103.1  118.2  126.22     0.633          0.337
#> 2 M1-All    Level3 283.4  203.7  222.21     0.633          0.337
#> 3 M2-All    Level2 155.7  180.9  194.66     0.633          0.321
#> 4 M2-All    Level3 313.5  300.9  299.17     0.633          0.321
```

## Report

With `what = "all"`, the combined report is provided.

### Individual

``` r
report(res, what = "individual")
```

### Detailed

``` r
report(res, what = "detailed")
```

### Summary

``` r
report(res, what = "summary")
```

### Item Review

``` r
report(res, what = "review")
```

## Extract

[`extract()`](https://sooyongl.github.io/emss/reference/extract.ESS.md)
provides the primary results of ESS as a form of
[`list()`](https://rdrr.io/r/base/list.html).

``` r
extract(res)
```

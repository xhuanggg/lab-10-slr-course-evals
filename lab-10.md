Lab 10 - Grading the professor
================
Thomas Huang
2026-03-26

Here is a link to the [lab
instructions](https://datascience4psych.github.io/DataScience4Psych/lab10.html).

## Load Packages and Data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)

data(evals)
?evals
```

# Part 1

## Exercise 1

By looking at the histogram and the descriptives, the distirbution is
left skewed. In general, the students are generous.

``` r
psych::describe(evals$score)
```

    ##    vars   n mean   sd median trimmed  mad min max range skew kurtosis   se
    ## X1    1 463 4.17 0.54    4.3    4.22 0.59 2.3   5   2.7 -0.7     0.04 0.03

``` r
ggplot(evals, aes(x = score)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

![](lab-10_files/figure-gfm/exercise1_code-1.png)<!-- -->

## Exercise 2

It seems that there is a positive relationship between the two
variables. But there is also a lot of noise.

``` r
ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_point()
```

![](lab-10_files/figure-gfm/exercise2_code-1.png)<!-- -->

## Exercise 3

This function fixes stacked or overlapped observations, which gives a
better visualization of number of observations, density of observations,
and noise. One might be mislead by the last plot because it fails to
visualize lots of noise in the data.

``` r
ggplot(evals, aes(x = bty_avg, y = score)) +
  geom_jitter()
```

![](lab-10_files/figure-gfm/exercise3_code-1.png)<!-- -->

# Part 2

## Exercise 1

## Hint

For Exercise 12, the `relevel()` function can be helpful!

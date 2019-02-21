Fake Data Generation
================
Jeremy Albright
21 February, 2019

# Tutorials Data

The purpose of this repo is to document the creation of fake data used
in the Methods Consultants tutorials. This will provide full
replicability as well as public access to the data files for users
interested in playing around with other software. The files can be
accessed from [here](tree/master/data).

## t-tests

The t-test tutorials rely on fake IQ scores generated as follows:

``` r
set.seed(12345)
gender <- rbinom(100, size = 1, p = .5)

iq    <- tibble(id = rep(1:100, 2),
                 gender = rep(gender, 2), 
                 time = c(rep(0, 100), rep(1, 100))) %>%
  mutate(iq     = rnorm(n(), mean = 100 + 2*gender + 3*time + 3*gender*time, sd = 15)) %>%
  mutate(iq     = round(iq, 2)) %>%
  mutate(gender = factor(gender, levels = 0:1, labels = c("Male", "Female")),
         time   = factor(time,   levels = 0:1, labels = c("Time 1", "Time 2")))


head(iq)
```

<div class="kable-table">

| id | gender | time   |     iq |
| -: | :----- | :----- | -----: |
|  1 | Female | Time 1 |  93.89 |
|  2 | Female | Time 1 | 131.22 |
|  3 | Female | Time 1 | 102.80 |
|  4 | Female | Time 1 | 107.27 |
|  5 | Male   | Time 1 |  89.94 |
|  6 | Male   | Time 1 | 104.17 |

</div>

The one-sample and independent samples t-test assume that the 200
observations are independent. Create this file and save as an SPSS file.

``` r
iq %>%
  mutate(id = row_number()) %>% 
  select(id, gender, iq) %>%
  expss::apply_labels(id     = "Subject ID",
                      gender = "Gender",
                      iq     = "IQ") %>%
  write_sav("data/iq_long.sav")
```

The paired samples t-test assumes the data come from 100 paired
observations. Create this file in wide format in SPSS.

``` r
iq %>%
  select(id, time, iq) %>%
  spread(time, iq) %>%
  rename(Time_1 = `Time 1`, Time_2 = `Time 2`) %>%
  expss::apply_labels(id     = "Subject ID",
                      Time_1 = "IQ at Time 1",
                      Time_2 = "IQ at Time 2") %>%
  write_sav("data/iq_wide.sav")
```

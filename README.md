Fake Data Generation
================
Jeremy Albright
14 June, 2019

# Tutorials Data

The purpose of this repo is to document the creation of fake data used
in the Methods Consultants tutorials. This will provide full
replicability as well as public access to the data files for users
interested in playing around with other software. The files can be
accessed from
[here](https://github.com/jeralbri/tutorial-data/tree/master/data).

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

The one-sample and independent samples t-test examples assume that the
200 observations are independent. Create this file and save as an SPSS
file.

``` r
iq %>%
  mutate(id = row_number()) %>% 
  select(id, gender, iq) %>%
  expss::apply_labels(id     = "Subject ID",
                      gender = "Gender",
                      iq     = "IQ") %>%
  write_sav("data/iq_long.sav")
```

The paired samples t-test example assumes the data come from 100 paired
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

## ANOVA

The ANOVA data are fake data generated years ago for a class I taught.
The code (and random seed) are long gone, but the file remains. It’s
saved in this repo as an SPSS file, `anova.sav`.

## Cross-Sectional Mixed (Multilevel) Models

Replicating the results from Raudenbush and Bryk’s (2002) textbok
requires the High School and Beyond data. These data are available in
the `merTools` package from Jared Knowles and Carl Frederick. We provide
examples in both SPSS and Stata.

``` r
library(merTools)

write_sav(hsb, "data/hsb.sav")

write_dta(hsb, "data/hsb.dta")
```

## 2016 ANES

The 2016 American National Election Studies data are used in multiple
blog posts and tutorials. Accessing the original data requires
[registering](https://electionstudies.org/data-center/) to agree to the
terms of use (no fee, no annoying emails). Assuming you have registered,
the following syntax will prepare the data for the analysis we present.

``` r
anes <- read_sav("data/anes_timeseries_2016.sav") 

anes <- anes %>%
  select(pre_weight     = V160101,
         post_weight    = V160102,
         strat          = V160201,
         psu            = V160202,
         voted_2012     = V161005,
         get_news       = V161008,
         reg_vote       = V161011,
         right_track    = V161081,
         pres_approv    = V161082x,
         therm_dem      = V161086,
         therm_rep      = V161087,
         pers_finance   = V161110,
         has_hlth_care  = V161112,
         ideology_1     = V161126,
         ideology_2     = V161127,
         econ_now       = V161139,
         econ_change    = V161140x,
         vote_duty      = V161151x,
         party_id       = V161158x,
         govt_services  = V161178,
         guns           = V161187,
         immigrants     = V161192,
         build_wall     = V161196,
         help_blacks    = V161198,
         environment    = V161201,
         soc_sec_fund   = V161205,
         school_fund    = V161206,
         science_fund   = V161207,
         crime_fund     = V161208,
         childcare_fund = V161210,
         welfare_fund   = V161209,
         aid_poor_fund  = V161211,
         environ_fund   = V161212,
         trust_govt     = V161215,
         govt_corrupt   = V161218,
         trust_people   = V161219,
         warming_real   = V161221,
         warming_cause  = V161222, 
         relig_exempt   = V161227x,
         trans_bathroom = V161228x,
         protect_gays   = V161229x,
         abortion       = V161232,
         death_penalty  = V161233x,
         retro_econ     = V161235x,
         relig_import   = V161241,
         attend_church  = V161245,
         born_again     = V161263,
         age            = V161267,
         married        = V161268,
         occupation     = V161276x,
         work_now       = V161277,
         educ           = V161270, 
         union          = V161302,
         latino         = V161309,
         race           = V161310x,
         own_home       = V161334,
         gender         = V161342,
         own_stock      = V161350,
         income         = V161361x,
         numb_guns      = V161496,
         sex_orient     = V161511,
         vote           = V162034a 
         ) %>%
  filter(vote %in% c(1,2) & gender %in% c(1,2)) %>%
  mutate(vote = factor(vote, levels = 1:2, labels = c("Clinton", "Trump")),
         educ = case_when(
           educ %in% 1:8 ~ 1,
           educ %in% 9 ~ 2,
           educ %in% 10:12 ~ 3,
           educ %in% 13 ~ 4,
           educ %in% 14:16 ~ 5,
           TRUE ~ -999),
         gender = factor(gender, levels = 1:2, labels = c("Male", "Female"))) %>%
  mutate(educ = factor(educ, level = 1:5, labels = c("HS Not Completed",
                                                     "Completed HS",
                                                     "College < 4 Years",
                                                     "College 4 Year Degree",
                                                     "Advanced Degree"))) %>% 
  mutate_at(vars(-starts_with("therm"), age, vote, educ, gender), ~as_factor(.)) %>% 
  mutate(income = fct_recode(income, NULL = "-9. Refused",
                             NULL = "-5. Interview breakoff (sufficient partial IW)")) %>% 
  mutate(income_cont = substr(as.character(income), 1, 2)) %>% 
  mutate(educ_cont = as.numeric(educ))
```

Save the cleaned data file.

``` r
write_sav(anes, "data/cleaned-anes.sav")
```

## Latent Variable Models

The CFA and SEM examples in the tutorials use the illustrations from
Bollen’s (1989) classic textbook, *Structural Equations with Latent
Variables*, which, after thirty years, remains among the best resources
for understanding SEM estimators. The CFA example is based on eight
indicators of political democracy. Page 239 provides the necessary
summary statistics. SEM can of course be conducted using just means and
covariances, but in most applied settings researchers use the full
microdata. Assuming that the variables are mean centered, and hence all
means are zero, we can generate a fake data file consistent with the
summary
statistics.

#### Bollen’s CFA Example

``` r
cfa_corrs <- matrix(c(1.00, 0.604, 0.679, 0.693, 0.739, 0.650, 0.674, 0.666,
                      0.604, 1.00, 0.451, 0.719, 0.543, 0.705, 0.581, 0.606,
                      0.679, 0.451, 1.00, 0.609, 0.576, 0.427, 0.650, 0.530,
                      0.693, 0.719, 0.609, 1.00, 0.652, 0.659, 0.680, 0.737,
                      0.739, 0.543, 0.576, 0.652, 1.00, 0.565, 0.678, 0.630,
                      0.650, 0.705, 0.427, 0.659, 0.565, 1.00, 0.609, 0.753,
                      0.674, 0.581, 0.650, 0.680, 0.678, 0.609, 1.00, 0.712,
                      0.666, 0.606, 0.530, 0.737, 0.630, 0.753, 0.712, 1.00), 
                    byrow = T, ncol = 8)

cfa_sds <- c(2.623, 3.947, 3.281, 3.349, 2.613, 3.373, 3.286, 3.246)

cfa_covs <- diag(cfa_sds)%*%cfa_corrs%*%diag(cfa_sds)

cfa_mns <- rep(0, 8)

cfa_tbl <- MASS::mvrnorm(n = 75, mu = cfa_mns, Sigma = cfa_covs, 
                         empirical = T) %>% 
  as.data.frame() %>% 
  rename_all(~str_replace(.x, "V", "x"))
```

Check that the descriptives are close to the original:

``` r
#------ Should all be near zero
map_dbl(cfa_tbl, mean)
```

    ##            x1            x2            x3            x4            x5 
    ## -1.171336e-17  3.716645e-18 -2.065232e-16  1.783007e-17  1.795439e-17 
    ##            x6            x7            x8 
    ## -3.225140e-17 -5.194919e-17  1.780983e-18

``` r
#------ Should be near correlation in prior chunk
cor(cfa_tbl)
```

    ##       x1    x2    x3    x4    x5    x6    x7    x8
    ## x1 1.000 0.604 0.679 0.693 0.739 0.650 0.674 0.666
    ## x2 0.604 1.000 0.451 0.719 0.543 0.705 0.581 0.606
    ## x3 0.679 0.451 1.000 0.609 0.576 0.427 0.650 0.530
    ## x4 0.693 0.719 0.609 1.000 0.652 0.659 0.680 0.737
    ## x5 0.739 0.543 0.576 0.652 1.000 0.565 0.678 0.630
    ## x6 0.650 0.705 0.427 0.659 0.565 1.000 0.609 0.753
    ## x7 0.674 0.581 0.650 0.680 0.678 0.609 1.000 0.712
    ## x8 0.666 0.606 0.530 0.737 0.630 0.753 0.712 1.000

Save data.

``` r
write_sav(cfa_tbl, "data/bollen_cfa.sav")
```

#### Bollen’s SEM Example

Page 334 of Bollen (1989) contains means and the covariance matrix for
the example used to illustrate a full SEM with latent variables. We’ll
again create a microdata file consistent with these sufficient
statistics.

``` r
sem_covs <- matrix(c(6.89, 6.25, 5.84, 6.09, 5.06, 5.75, 5.81, 5.67, 0.73, 1.27, 0.91,
                     6.25, 15.58, 5.84, 9.51, 5.60, 9.39, 7.54, 7.76, 0.62, 1.49, 1.17,
                     5.84, 5.84, 10.76, 6.69, 4.94, 4.73, 7.01, 5.64, 0.79, 1.55, 1.04,
                     6.09, 9.51, 6.69, 11.22, 5.70, 7.44, 7.49, 8.01, 1.15, 2.24, 1.84,
                     5.06, 5.60, 4.94, 5.70, 6.83, 4.98, 5.82, 5.34, 1.08, 2.06, 1.58,
                     5.75, 9.39, 4.73, 7.44, 4.98, 11.38, 6.75, 8.25, 0.85, 1.81, 1.57,
                     5.81, 7.54, 7.01, 7.49, 5.82, 6.75, 10.80, 7.59, 0.94, 2.00, 1.63,
                     5.67, 7.76, 5.64, 8.01, 5.34, 8.25, 7.59, 10.53, 1.10, 2.23, 1.69,
                     0.73, 0.62, 0.79, 1.15, 1.08, 0.85, 0.94, 1.10, 0.54, 0.99, 0.82,
                     1.27, 1.49, 1.55, 2.24, 2.06, 1.81, 2.00, 2.23, 0.99, 2.28, 1.81,
                     0.91, 1.17, 1.04, 1.84, 1.58, 1.57, 1.63, 1.69, 0.82, 1.81, 1.98),
                   byrow = T, ncol = 11)

sem_mns <- c(5.46, 4.26, 6.56, 4.45, 5.14, 2.98, 6.20, 4.04, 5.05, 4.79, 3.56)

sem_tbl <- MASS::mvrnorm(n = 75, mu = sem_mns, Sigma = sem_covs, 
                         empirical = T) %>% 
  as.data.frame() %>% 
  rename_all(~str_replace(.x, "V", "y")) %>% 
  rename(x1 = y9, x2 = y10, x3 = y11)
```

Check the descriptives are close to the original:

``` r
#------ Should match means from prior chunk
map_dbl(sem_tbl, mean)
```

    ##   y1   y2   y3   y4   y5   y6   y7   y8   x1   x2   x3 
    ## 5.46 4.26 6.56 4.45 5.14 2.98 6.20 4.04 5.05 4.79 3.56

``` r
#------ Should be near covariance matrix in prior chunk
cov(sem_tbl)
```

    ##      y1    y2    y3    y4   y5    y6    y7    y8   x1   x2   x3
    ## y1 6.89  6.25  5.84  6.09 5.06  5.75  5.81  5.67 0.73 1.27 0.91
    ## y2 6.25 15.58  5.84  9.51 5.60  9.39  7.54  7.76 0.62 1.49 1.17
    ## y3 5.84  5.84 10.76  6.69 4.94  4.73  7.01  5.64 0.79 1.55 1.04
    ## y4 6.09  9.51  6.69 11.22 5.70  7.44  7.49  8.01 1.15 2.24 1.84
    ## y5 5.06  5.60  4.94  5.70 6.83  4.98  5.82  5.34 1.08 2.06 1.58
    ## y6 5.75  9.39  4.73  7.44 4.98 11.38  6.75  8.25 0.85 1.81 1.57
    ## y7 5.81  7.54  7.01  7.49 5.82  6.75 10.80  7.59 0.94 2.00 1.63
    ## y8 5.67  7.76  5.64  8.01 5.34  8.25  7.59 10.53 1.10 2.23 1.69
    ## x1 0.73  0.62  0.79  1.15 1.08  0.85  0.94  1.10 0.54 0.99 0.82
    ## x2 1.27  1.49  1.55  2.24 2.06  1.81  2.00  2.23 0.99 2.28 1.81
    ## x3 0.91  1.17  1.04  1.84 1.58  1.57  1.63  1.69 0.82 1.81 1.98

Save data.

``` r
write_sav(sem_tbl, "data/bollen_sem.sav")
```

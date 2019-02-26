---
title: "Montagu API"
author: "VIMC Technical Team"
date: "2019-02-19"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Montagu API}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

Montagu has an HTTP API, and this package wraps it to allow
programmatic access to the data in montagu.  The API is evolving and
this package does not yet implement the full set of API features.

The package currently contains two somewhat unrelated tasks -
authentication and working with the data.  As things evolve then this
will become less painful to use.

For external users, this is particularly awkward - internally we have
several copies of the montagu server for different purposes, but
externally there is only one.

### Authentication


```r
montagu::montagu_server_global_default_set(
  montagu::montagu_server("production", "montagu.vaccineimpact.org"))
```

On the first request you will be prompted for a password


```r
montagu::montagu_touchstones_list()
```

```
## Authorising with server 'production' (https://montagu.vaccineimpact.org)
```

```
##                         id                  name version   status
## 1             201710gavi-6            201710gavi       6     open
## 2             201710gavi-5            201710gavi       5     open
## 3             201710gavi-4            201710gavi       4 finished
## 4             201710gavi-3            201710gavi       3 finished
## 5             201710gavi-2            201710gavi       2 finished
## 6             201710gavi-1            201710gavi       1 finished
## 7        201810synthetic-3       201810synthetic       3     open
## 8        201810synthetic-2       201810synthetic       2     open
## 9        201810synthetic-1       201810synthetic       1     open
## 10           201310gavi-42            201310gavi      42 finished
## 11            201310gavi-1            201310gavi       1 finished
## 12           201510gavi-42            201510gavi      42 finished
## 13            201510gavi-9            201510gavi       9 finished
## 14            201510gavi-7            201510gavi       7 finished
## 15            201510gavi-5            201510gavi       5 finished
## 16            201510gavi-4            201510gavi       4 finished
## 17  201210gavi-201607wue-1  201210gavi-201607wue       1 finished
## 18 201210gavi-201303gavi-1 201210gavi-201303gavi       1 finished
## 19             201707wue-3             201707wue       3 finished
## 20             201707wue-2             201707wue       2 finished
## 21             201707wue-1             201707wue       1 finished
## 22  201310gavi-201707wue-1  201310gavi-201707wue       1 finished
## 23  201510gavi-201707wue-1  201510gavi-201707wue       1 finished
## 24  201210gavi-201707wue-1  201210gavi-201707wue       1 finished
## 25             201607wue-1             201607wue       1 finished
## 26            201708test-2            201708test       2 finished
## 27            201708test-1            201708test       1 finished
## 28             201802uxe-1             201802uxe       1 finished
## 29            201403gavi-1            201403gavi       1 finished
## 30             201807wue-3             201807wue       3 finished
## 31             201807wue-2             201807wue       2 finished
## 32             201807wue-1             201807wue       1 finished
## 33             201801rfp-1             201801rfp       1 finished
## 34             201804rfp-1             201804rfp       1 finished
## 35             201809hpv-1             201809hpv       1 finished
## 36             201810rfp-1             201810rfp       1 finished
```

### Demographic data

List available demographic data (this is cached within a session)


```r
touchstone_id <- "201710gavi-5"
montagu::montagu_demographics_list(touchstone_id)
```

```
##              id                                             name gendered
## 1       as_fert                Fertility: Age-specific fertility    FALSE
## 2        births                    Fertility: Births (number of)    FALSE
## 3     qq_births     Fertility: Births (number of) - quinquennial    FALSE
## 4     as_births               Fertility: Births by age of mother    FALSE
## 5           cbr                Fertility: Crude birth rate (CBR)    FALSE
## 6      birth_mf                    Fertility: Sex-ratio at birth    FALSE
## 7      fert_tot                       Fertility: Total Fertility    FALSE
## 8  net_mig_rate                    Migration: Net migration rate    FALSE
## 9     mort_rate             Mortality: Central Death Rate (ASMR)     TRUE
## 10          cdr                Mortality: Crude death rate (CDR)    FALSE
## 11     mort_age                Mortality: Deaths (number) by age     TRUE
## 12     mort_tot                 Mortality: Deaths (total number)     TRUE
## 13      life_ex      Mortality: Expected remaining years of life     TRUE
## 14          lx0              Mortality: Life expectancy at birth     TRUE
## 15 unwpp_cm_nmr  Mortality: Neonatal Mortality Rate (28-day NMR)    FALSE
## 16      p_dying           Mortality: Probability of dying by age     TRUE
## 17  n_survivors Mortality: Survivors from a birth-cohort of 100k     TRUE
## 18    unwpp_imr          Mortality: Under 1 Mortality Rate (IMR)    FALSE
## 19   unwpp_u5mr         Mortality: Under 5 Mortality Rate (U5MR)    FALSE
## 20          pgr                          Population: Growth Rate    FALSE
## 21      int_pop   Population: Interpolated (1-year time and age)     TRUE
## 22       qq_pop   Population: Quinquennial (5-year time and age)     TRUE
## 23      tot_pop                                Population: Total     TRUE
##        source
## 1  dds-201710
## 2  dds-201710
## 3  dds-201710
## 4  dds-201710
## 5  dds-201710
## 6  dds-201710
## 7  dds-201710
## 8  dds-201710
## 9  dds-201710
## 10 dds-201710
## 11 dds-201710
## 12 dds-201710
## 13 dds-201710
## 14 dds-201710
## 15 dds-201710
## 16 dds-201710
## 17 dds-201710
## 18 dds-201710
## 19 dds-201710
## 20 dds-201710
## 21 dds-201710
## 22 dds-201710
## 23 dds-201710
```

From this list, download a data set


```r
dat <- montagu::montagu_demographic_data(
    "cbr", "201710gavi-5", source_code = "dds-201710")
head(dat)
```

```
##   country_code_numeric country_code     country age_from age_to year
## 1                    4          AFG Afghanistan        0      0 1950
## 2                    4          AFG Afghanistan        0      0 1951
## 3                    4          AFG Afghanistan        0      0 1952
## 4                    4          AFG Afghanistan        0      0 1953
## 5                    4          AFG Afghanistan        0      0 1954
## 6                    4          AFG Afghanistan        0      0 1955
##   gender value
## 1   both  0.05
## 2   both  0.05
## 3   both  0.05
## 4   both  0.05
## 5   both  0.05
## 6   both  0.05
```
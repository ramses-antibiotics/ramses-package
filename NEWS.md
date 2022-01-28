
# Ramses 0.4.2

*1 February 2022*

## Improvements

* upgrade dependency on `AMR` package version >= 1.8.0
* better error messages in `clinical_feature_*()` functions #80
* `spell_id` is no longer required by `validate_investigations()` #77
* 

# Ramses 0.4.1

*5 August 2021*

## Improvements

* clarifications on `vignette("load-data")`
* corrections to `antibiotic_icd_indications`


# Ramses 0.4.0

*7 July 2021*

## Improvements

* faster implementation of prescription linkage using `igraph` dependency
* `TherapyEpisode` class now supporting multiple therapy episodes
* support for integer `*_id` variables
* updated schema of drug prescriptions and administrations: `drug_id` refactored into `drug_code`; `ATC_group` retired in favour of `drug_group`
* updated schema of microbiology isolates and susceptibilities: isolates are now identified by `isolate_id` rather than `organism_id`
* updated schema of microbiology susceptibilities: `drug_id`, `drug_name` and `drug_display_name` refactored into `agent_code`, `agent_name` and `agent_display_name`
* faster execution of `clinical_feature_*()` functions on PostgreSQL
* uniform handling of `prescription_status`: therapy episodes and bridge table records will not be generated for prescriptions with status `"draft"`, `"entered-in-error"`,  `"cancelled"`, or `"unknown"`.
* update `reference_loinc` to version 2.70 and expand to 10 new concepts

## Bug fixes

* fix bug in `validate_prescriptions()` checks on `daily_frequency`
* fix bug in `load_*()` handling of `data.table` objects

# Ramses 0.3.1

*17 March 2021*

## Bug fix

* fix bug in handling `data.table` objects in `arrange_variables()` and `validate_*()`

# Ramses 0.3.0

*26 February 2021*

## Features

* introduce S4 classes `Patient`, `MedicationRequest`, `TherapyEpisode`
* introduce therapy methods `therapy_table()`, `parenteral_changes()`, `clinical_feature_last()`, `clinical_feature_ols_trend()`, `clinical_feature_interval()`, `clinical_feature_mean()`

## Improvements

* remove dependencies
* improve handling of time data types by SQLite
* upgrade to `units` package version >=0.7-0

## Documentation

* new `ramses-objects` vignette
* new `therapy-episodes` vignette

# Ramses 0.2.1

*8 February 2021*

## Bug fixes

* Corrected incorrect transitive closure settings
* Corrected prescription edge classification

## Improvements

* Complete verification of prescription edge classification
* `therapy_id` now always set to the id of the first prescription in the episode

# Ramses 0.2.0

*25 January 2021*

* Support PostgreSQL
* Introduce standalone `patients` table and dataset

# Ramses 0.1.0

*8 January 2021*

* First minor version
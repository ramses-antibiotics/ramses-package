

# Ramses 0.3.1

## Bugfix

* fix bug in handling `data.table` objects in `arrange_variables()` and `validate_*()`

# Ramses 0.3.0

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

## Bug fixes

* Corrected incorrect transitive closure settings
* Corrected prescription edge classification

## Improvements

* Complete verification of prescription edge classification
* `therapy_id` now always set to the id of the first prescription in the episode

# Ramses 0.2.0

* Support PostgreSQL
* Introduce standalone `patients` table and dataset

# Ramses 0.1.0

* First minor version
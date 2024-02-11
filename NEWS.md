
# Ramses 0.7.2

*11 February 2024* 


## Changed

- Now requires minimum `AMR` v2.0.0.
- Updated function calls and documentation following deprecation of `AMR::as.rsi()`.


# Ramses 0.7.1

*19 November 2023* 


## Changed

- Now requires minimum `dbplyr` v2.4.0.
- Unit test maintenance required by `dbplyr` v2.4.0.


# Ramses 0.7.0

*11 April 2023* 

## Changed

- Now requires minimum R v4.0.0, `dplyr` v1.1.0, `dbplyr` v2.3.2 and `tidyselect` v1.2.0.
- Internal code maintenance in line with new tidy programming practice.
- Tolerance built into the timeline for handling of no growth results. Ramses does not 
currently prescribe whether no growth should be loaded into `microbiology_isolates` or not ([#108](https://github.com/ramses-antibiotics/ramses-package/issues/108)).
The timeline will now show specimens without an isolate record at the time of collection.


# Ramses 0.6.0

*23 October 2022* 

## Improvements

- Changed `validate_inpatient_diagnoses()` to accept two optional fields `diagnosis_start`
and `diagnosis_end` and take advantage of clinical systems such as problem lists
- Changed `therapy_timeline()` to take advantage of `diagnosis_start` and `diagnosis_end`
variables if present
- Changed the `inpatient_diagnoses` mock dataset to removed episode start and end fields [#103](https://github.com/ramses-antibiotics/ramses-package/issues/103) which 
duplicated information in `inpatient_episodes`
- Changed `bridge_*()` functions to add a field `antiinfective_type` to bridge tables [#76](https://github.com/ramses-antibiotics/ramses-package/issues/76) and prevent 
unrelated generation issues [#104](https://github.com/ramses-antibiotics/ramses-package/issues/104)
- General code maintenance and adaptation to ongoing changes to the `dplyr` data masking
approach [#105](https://github.com/ramses-antibiotics/ramses-package/issues/105).


# Ramses 0.5.2

*13 September 2022*

## Improvements

- `validate_investigations()` now expects `observation_code` values to be associated
with unique `observation_name` and `observation_display` values
- improvements to namespace handling in `validate_microbiology()`.


# Ramses 0.5.1

*30 August 2022*

## Bug fix

* Fix bug affecting `TherapyEpisode` instances created for more than 3 therapy
episodes (`id` with length > 3) [#100](https://github.com/ramses-antibiotics/ramses-package/issues/100) 


# Ramses 0.5.0

*27 August 2022*

## Improvements

* Local databases (`connect_local_database()` and `create_mock_database()`) 
are now powered by [DuckDB](https://duckdb.org/) rather than SQLite, bringing support for 
datetime variable types and increased performance. Ramses now depends on 
the [`duckdb`](https://CRAN.R-project.org/package=duckdb) library
* Added new S4 class `Encounter` [#92](https://github.com/ramses-antibiotics/ramses-package/issues/92) 
to manipulate hospitalisations (admissions)
with associated methods for `show()`, `compute()`, `collect()`, `Patient()`, 
`longitudinal_table()`, `clinical_feature_*()`, `therapy_timeline()`
* `TherapyEpisode()` and `Encounter()` have a new optional `extend_table_start` input
controlling their longitudinal table's start. This allows the creation of longitudinal
tables starting before the `therapy_start` or `admission_date`, respectively. More
detail is available from [`vignette("therapy-episodes")`](https://ramses-antibiotics.web.app/articles/therapy-episodes.html)
* Added `Patient()` function to create a `Patient` object from any other object
of class `RamsesObject`
* Improved documentation and export of S4 methods `compute()` and `collect()`.

## Breaking changes

* `therapy_table()` is now deprecated: use `longitudinal_table()` instead  [#86](https://github.com/ramses-antibiotics/ramses-package/issues/86)
* `bridge_spell_therapy_overlap()` is now deprecated : use 
`bridge_encounter_therapy_overlap()` instead  [#86](https://github.com/ramses-antibiotics/ramses-package/issues/86)
* Database table `inpatient_episodes` and documentation now refer to 'encounters' 
rather than 'spells'. Variable `spell_id` is now known as `encounter_id` [#86](https://github.com/ramses-antibiotics/ramses-package/issues/86).
Databases built with previous versions of Ramses will not work with Ramses 0.5.0
* Function signatures for all methods are standardised: function arguments 
`object` are now changed to `x`, except for `show(object)`.
* Support for SQLite databases is withdrawn.

# Ramses 0.4.4

*22 August 2022*

## Improvements

* `clinical_feature_*()` functions trigger a warning rather than an error if
no value matching `observation_code` is found in the`inpatient_investigations` table.


# Ramses 0.4.3

*2 August 2022*

## Improvements

* upgrade dependency on packages `dplyr` version >= 1.0.1 and `dbplyr` version >= 2.1.0
* improve `validate_inpatient_episodes()` [#85](https://github.com/ramses-antibiotics/ramses-package/issues/85)

## Bug fixes

* fix bug in `map_charlson_comorbidities()` with `tbl_df` objects  [#84](https://github.com/ramses-antibiotics/ramses-package/issues/84)
* fix bug in `compute()` with `TherapyEpisode` objects  [#83](https://github.com/ramses-antibiotics/ramses-package/issues/83)
* fix uncommon bug in `clinical_feature_*()` functions with PostgreSQL [#88](https://github.com/ramses-antibiotics/ramses-package/issues/88)


# Ramses 0.4.2

*28 January 2022*

## Improvements

* upgrade dependency on `AMR` package version >= 1.8.0
* better error messages in `clinical_feature_*()` functions [#80](https://github.com/ramses-antibiotics/ramses-package/issues/80)
* `spell_id` is no longer required by `validate_investigations()` [#77](https://github.com/ramses-antibiotics/ramses-package/issues/77)
* bridge tables will be generated by `bridge_tables()` even in the absence of defined daily doses (DDDs) in drug prescription and administration records
* improve `vignette("Ramses")`

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

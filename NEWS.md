
# Ramses 0.3.0

## Features

**ADD HYPERLINKS BEFORE SUBMISSION**

* introduce S4 classes `Patient`, `MedicationRequest`, `TherapyEpisode`
* introduce therapy methods `get_therapy_table()`, `parenteral_changes_get()`

## Improvements

* remove dependencies
* improve handling of time data types by SQLite

## Documentation

* new ramses-objects vignette

# Ramses 0.2.1

## Bug fixes

* Corrected incorrect transitive closure settings
* Corrected prescription edge classification

## Improvement

* Complete verification of prescription edge classification
* therapy_id now always set to the id of the first prescription in the episode

# Ramses 0.2.0

* Support PostgreSQL
* Introduce standalone `patients` table and dataset

# Ramses 0.1.0

* First minor version
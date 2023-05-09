-- Tied to the Ramses utily function .compute_date_dimensions()
CREATE TABLE dimension_date (
    date DATE NOT NULL,
    date_string_iso VARCHAR,
    date_string_dd_mm_yyyy VARCHAR,
    date_string_dd_mm_yy VARCHAR,
    date_string_full VARCHAR,
    calendar_year SMALLINT,
    calendar_quarter VARCHAR,
    calendar_month SMALLINT,
    calendar_month_name VARCHAR,
    calendar_month_short VARCHAR,
    day SMALLINT,
    day_name VARCHAR,
    day_name_short VARCHAR,
    week_day_numeric SMALLINT,
    week_starting VARCHAR,
    week_ending VARCHAR,
    financial_year_uk VARCHAR,
    financial_quarter_uk VARCHAR,
    financial_year_quarter_uk VARCHAR,
    PRIMARY KEY(date)
);

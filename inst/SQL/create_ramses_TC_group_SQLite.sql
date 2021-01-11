CREATE TABLE ramses_tc_group
(
    id INTEGER PRIMARY KEY,
    grp INTEGER NOT NULL,
    lvl INTEGER NOT NULL
);

CREATE INDEX ramses_tc_group_idx on ramses_tc_group(lvl, id);



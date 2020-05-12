CREATE TABLE ramses_TC_group
(
    id INTEGER PRIMARY KEY,
    grp INTEGER NOT NULL,
    lvl INTEGER NOT NULL
);

CREATE INDEX ramses_TC_group_idx on ramses_TC_group(lvl, id);



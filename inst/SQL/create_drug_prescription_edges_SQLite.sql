CREATE TABLE [drug_prescriptions_edges]
(
    [from_id]          TEXT NOT NULL,
    [to_id]            TEXT NOT NULL,
    [edge_type]        TEXT,
    [therapy_rank]     INTEGER,
    [combination_rank] INTEGER,
    [relation_type]    TEXT,
    [switch_oral]      INTEGER
    PRIMARY KEY([from_id], [to_id]])
);

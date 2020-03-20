CREATE TABLE [drug_prescriptions_edges]
(
    [from_id]          TEXT,
    [to_id]            TEXT,
    [edge_type]        TEXT,
    [therapy_rank]     INTEGER,
    [combination_rank] INTEGER,
    [relation_type]    TEXT,
    [switch_oral]      INTEGER
);

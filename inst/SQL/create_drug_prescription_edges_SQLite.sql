CREATE TABLE drug_prescriptions_edges
(
    patient_id TEXT NOT NULL,
    from_id          INTEGER NOT NULL,
    to_id            INTEGER NOT NULL,
    edge_type        TEXT,
    therapy_rank     INTEGER,
    combination_rank INTEGER,
    relation_type    TEXT,
    switch_oral      INTEGER,
    PRIMARY KEY(from_id, to_id)
);

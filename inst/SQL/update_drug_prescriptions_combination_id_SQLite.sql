UPDATE drug_prescriptions
SET combination_id = NULL;

UPDATE drug_prescriptions
SET combination_id = (SELECT combination_id
                    FROM @@@ramses_tc_table
                    WHERE id = drug_prescriptions.id )
WHERE EXISTS(SELECT combination_id
             FROM @@@ramses_tc_table
             WHERE id = drug_prescriptions.id );


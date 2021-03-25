UPDATE drug_prescriptions
SET combination_id = NULL;

UPDATE drug_prescriptions
SET combination_id = (SELECT combination_id
                    FROM @@@ramses_tc_table
                    WHERE prescription_id = drug_prescriptions.prescription_id AND patient_id = drug_prescriptions.patient_id)
WHERE EXISTS(SELECT combination_id
             FROM @@@ramses_tc_table
             WHERE prescription_id = drug_prescriptions.prescription_id AND patient_id = drug_prescriptions.patient_id);


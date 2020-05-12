UPDATE [drug_prescriptions]
SET [combination_id] = (SELECT [combination_id]
                    FROM @@@ramses_TC_table
                    WHERE [prescription_id] = [drug_prescriptions].[prescription_id])
WHERE EXISTS(SELECT [combination_id]
             FROM @@@ramses_TC_table
             WHERE [prescription_id] = [drug_prescriptions].[prescription_id]);

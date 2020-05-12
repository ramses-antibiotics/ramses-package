UPDATE [drug_prescriptions]
SET [therapy_id] = (SELECT [therapy_id]
                    FROM @@@ramses_TC_table
                    WHERE [prescription_id] = [drug_prescriptions].[prescription_id])
WHERE EXISTS(SELECT [therapy_id]
             FROM @@@ramses_TC_table
             WHERE [prescription_id] = [drug_prescriptions].[prescription_id]);
UPDATE [drug_prescriptions]
SET [therapy_id] = [prescription_id]
WHERE [therapy_id] IS NULL
  AND [prescription_status] NOT IN ('cancelled', 'draft', 'entered-in-error');

CREATE TABLE [ramses_temp_rank] AS
SELECT prescription_id,
       ROW_NUMBER() OVER (
           PARTITION BY [therapy_id]
           ORDER BY [prescription_start]
           ) AS [therapy_rank]
FROM [drug_prescriptions]
WHERE therapy_id IS NOT NULL;

UPDATE [drug_prescriptions]
SET [therapy_rank] = (SELECT [therapy_rank]
                    FROM [ramses_temp_rank]
                    WHERE [prescription_id] = [drug_prescriptions].[prescription_id])
WHERE EXISTS(SELECT [therapy_rank]
             FROM [ramses_temp_rank]
             WHERE [prescription_id] = [drug_prescriptions].[prescription_id]);

DROP TABLE [ramses_temp_rank];

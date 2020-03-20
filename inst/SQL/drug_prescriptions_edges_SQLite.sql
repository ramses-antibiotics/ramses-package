INSERT INTO [drug_prescriptions_edges] ([from_id], [to_id], [edge_type], [therapy_rank], [combination_rank],
                                        [relation_type], [switch_oral])
SELECT a.[prescription_id]                                                                                       AS [from_id]
     , b.[prescription_id]                                                                                       AS [to_id]
     , 'combination'                                                                                             AS [edge_type]
     , NULL                                                                                                     AS [therapy_rank]
    -- , ROW_NUMBER() OVER(PARTITION BY a.[patient_id], JULIANDAY(a.[authoring_date]) ORDER BY a.[drug_name] ) AS [combination_rank]
     , NULL AS [combination_rank]
     , CASE
           WHEN (a.[daily_frequency] = -1) and (b.[daily_frequency] = -1) THEN '1'
           WHEN (a.[daily_frequency] = -1) and (b.[daily_frequency] <> -1) THEN '2'
           WHEN (a.[daily_frequency] <> -1) and (b.[daily_frequency] = -1) and
                (DATETIME(a.[prescription_end]) < DATETIME(b.[prescription_start])) THEN '3'
           WHEN (a.[daily_frequency] <> -1) and (b.[daily_frequency] = -1) and
                (DATETIME(a.[prescription_end]) >= DATETIME(b.[prescription_start])) THEN '4'
           WHEN (a.[daily_frequency] <> -1) and (b.[daily_frequency] <> -1) and
                (DATETIME(a.[prescription_end]) >= DATETIME(b.[prescription_start])) and
                (DATETIME(a.[prescription_end]) >= DATETIME(b.[prescription_end]))
               THEN '5'
           WHEN (a.[daily_frequency] <> -1) and (b.[daily_frequency] <> -1) and
                (DATETIME(a.[prescription_end]) >= DATETIME(b.[prescription_start])) and
                (DATETIME(a.[prescription_end]) < DATETIME(b.[prescription_end]))
               THEN '6'
           WHEN (a.[daily_frequency] <> -1) and (b.[daily_frequency] <> -1) and
                (DATETIME(a.[prescription_end]) < DATETIME(b.[prescription_start])) THEN '7'
    END                                                                                                          AS [relation_type]
     , CASE WHEN a.[ATC_route] = 'P' and b.[ATC_route] <> 'P' THEN 1 ELSE 0 END                                                   AS switch_oral
FROM [temp_prescriptions] a
         INNER JOIN
     [temp_prescriptions] b
     ON a.[patient_id] = b.[patient_id]
         and a.[prescription_id] <> b.[prescription_id]
         and a.[prescription_status] not in('cancelled', 'draft', 'entered-in-error')
         and b.[prescription_status] not in('cancelled', 'draft', 'entered-in-error')
         and a.[prescription_context] = b.[prescription_context]
         -- -- Edit 19 April 2019: added combinations
         -- and a.[drug_id] <> b.[drug_id] ---- CONDITION FOR COMBINATION this excludes the doxi and gent combos
         and (JULIANDAY(DATETIME(b.[prescription_start])) - JULIANDAY(DATETIME(a.[prescription_start])) * 24 ) between 0 and @max_combination_start_gap --- CONDITION FOR COMBINATION
         and ABS(JULIANDAY(DATETIME(b.[authoring_date])) - JULIANDAY(DATETIME(a.[authoring_date])) * 24) <
             @max_combination_authoring_gap --- CONDITION FOR COMBINATION
WHERE (
              ((a.[daily_frequency] = -1) and (b.[daily_frequency] = -1)) -- 1
              or
              ((a.[daily_frequency] <> -1) and (b.[daily_frequency] <> -1) and
               (DATETIME(a.[prescription_end]) >= DATETIME(b.[prescription_start])) and (DATETIME(a.[prescription_end]) >= DATETIME(b.[prescription_end]))) -- 5
              or
              ((a.[daily_frequency] <> -1) and (b.[daily_frequency] <> -1) and
               (DATETIME(a.[prescription_end]) >= DATETIME(b.[prescription_start])) and (DATETIME(a.[prescription_end]) < DATETIME(b.[prescription_end]))) --6
          -- Edit 19 April 2019: added combinations for OOF then REG, like doxycycline or daptomycin
              or
              ((a.[daily_frequency] = -1) and (b.[daily_frequency] <> -1) and (a.[drug_id] = b.[drug_id])) --2
          )
ORDER BY a.[patient_id], a.[prescription_id], a.[authoring_date], b.[prescription_start];
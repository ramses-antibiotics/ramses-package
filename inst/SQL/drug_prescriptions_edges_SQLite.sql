WITH rx_edges AS (
    SELECT a.[patient_id]                                                           AS [patient_id]
         , a.[prescription_id]                                                      AS [from_id]
         , b.[prescription_id]                                                      AS [to_id]
         , a.[prescription_status]                                                  AS [from_status]
         , b.[prescription_status]                                                  AS [to_status]
         , a.[prescription_start]                                                   AS [from_start]
         , a.[prescription_end]                                                     AS [from_end]
         , a.[authoring_date]                                                       AS [from_authoring]
         , b.[prescription_start]                                                   AS [to_start]
         , b.[prescription_end]                                                     AS [to_end]
         , b.[authoring_date]                                                       AS [to_authoring]
         , a.[drug_id]                                                              AS [from_drug]
         , b.[drug_id]                                                              AS [to_drug]
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
               ELSE NULL
        END                                                                         AS [relation_type]
         , CASE WHEN a.[ATC_route] = 'P' and b.[ATC_route] <> 'P' THEN 1 ELSE 0 END AS switch_oral
    FROM [ramses_temp_prescriptions] a
             INNER JOIN
         [ramses_temp_prescriptions] b
         ON a.[patient_id] = b.[patient_id]
             and a.[prescription_id] <> b.[prescription_id]
             and a.[prescription_context] = b.[prescription_context]
)

INSERT
INTO [drug_prescriptions_edges] ([from_id], [to_id], [edge_type], [therapy_rank], [combination_rank],
                                 [relation_type], [switch_oral])

SELECT [from_id]
     , [to_id]
     , [edge_type]
     , NULL AS [therapy_rank]
     , NULL AS [combination_rank] --     ROW_NUMBER() OVER(PARTITION BY a.[patient_id], JULIANDAY(a.[authoring_date]) ORDER BY a.[drug_name] ) AS [combination_rank]
     , [relation_type]
     , [switch_oral]
FROM (SELECT *,
             CASE
                 WHEN
                         ([relation_type] IN ('1', '5', '6')
                             AND (STRFTIME('%s', [to_start]) - STRFTIME('%s', [from_start])) / 3600 BETWEEN 0 AND @max_combination_start_gap
                             AND ABS(STRFTIME('%s', [to_authoring]) - STRFTIME('%s', [from_authoring])) / 3600 <
                                 @max_combination_authoring_gap)
                         OR ([relation_type] = '2'
                         AND (STRFTIME('%s', [to_start]) - STRFTIME('%s', [from_start])) / 3600 BETWEEN 0 AND @max_combination_start_gap
                         AND ABS(STRFTIME('%s', [to_authoring]) - STRFTIME('%s', [from_authoring])) / 3600 < @max_combination_authoring_gap
                         AND [from_drug] = [to_drug])
                     THEN 'combination'
                 WHEN
                         ([relation_type] IN ('4', '5', '6'))
                         OR
                         ([relation_type] IN ('1', '2', '3', '7')
                             AND
                          ABS(STRFTIME('%s', [to_start]) - STRFTIME('%s', [from_start])) / 3600 < @max_continuation_gap)
                     THEN 'continuation'
                 ELSE NULL
                 END AS [edge_type]
      FROM rx_edges) final

WHERE final.[edge_type] IS NOT NULL
  AND final.[from_status] NOT IN ('cancelled', 'draft', 'entered-in-error')
  AND final.[to_status] NOT IN ('cancelled', 'draft', 'entered-in-error')

ORDER BY [patient_id], [from_id], [from_authoring], [to_start];

-- LEGACY
--
-- INSERT INTO [drug_prescriptions_edges] ([from_id], [to_id], [edge_type], [therapy_rank], [combination_rank],
--                                         [relation_type], [switch_oral])
-- SELECT a.[prescription_id]                                                                                       AS [from_id]
--      , b.[prescription_id]                                                                                       AS [to_id]
--      , 'combination'                                                                                             AS [edge_type]
--      , NULL                                                                                                     AS [therapy_rank]
--     -- , ROW_NUMBER() OVER(PARTITION BY a.[patient_id], JULIANDAY(a.[authoring_date]) ORDER BY a.[drug_name] ) AS [combination_rank]
--      , NULL AS [combination_rank]
--      , CASE
--            WHEN (a.[daily_frequency] = -1) and (b.[daily_frequency] = -1) THEN '1'
--            WHEN (a.[daily_frequency] = -1) and (b.[daily_frequency] <> -1) THEN '2'
--            WHEN (a.[daily_frequency] <> -1) and (b.[daily_frequency] = -1) and
--                 (DATETIME(a.[prescription_end]) < DATETIME(b.[prescription_start])) THEN '3'
--            WHEN (a.[daily_frequency] <> -1) and (b.[daily_frequency] = -1) and
--                 (DATETIME(a.[prescription_end]) >= DATETIME(b.[prescription_start])) THEN '4'
--            WHEN (a.[daily_frequency] <> -1) and (b.[daily_frequency] <> -1) and
--                 (DATETIME(a.[prescription_end]) >= DATETIME(b.[prescription_start])) and
--                 (DATETIME(a.[prescription_end]) >= DATETIME(b.[prescription_end]))
--                THEN '5'
--            WHEN (a.[daily_frequency] <> -1) and (b.[daily_frequency] <> -1) and
--                 (DATETIME(a.[prescription_end]) >= DATETIME(b.[prescription_start])) and
--                 (DATETIME(a.[prescription_end]) < DATETIME(b.[prescription_end]))
--                THEN '6'
--            WHEN (a.[daily_frequency] <> -1) and (b.[daily_frequency] <> -1) and
--                 (DATETIME(a.[prescription_end]) < DATETIME(b.[prescription_start])) THEN '7'
--     END                                                                                                          AS [relation_type]
--      , CASE WHEN a.[ATC_route] = 'P' and b.[ATC_route] <> 'P' THEN 1 ELSE 0 END                                                   AS switch_oral
-- FROM [ramses_temp_prescriptions] a
--          INNER JOIN
--      [ramses_temp_prescriptions] b
--      ON a.[patient_id] = b.[patient_id]
--          and a.[prescription_id] <> b.[prescription_id]
--          and a.[prescription_status] not in('cancelled', 'draft', 'entered-in-error')
--          and b.[prescription_status] not in('cancelled', 'draft', 'entered-in-error')
--          and a.[prescription_context] = b.[prescription_context]
--          -- -- Edit 19 April 2019: added combinations
--          -- and a.[drug_id] <> b.[drug_id] ---- CONDITION FOR COMBINATION this excludes the doxi and gent combos
--          and (JULIANDAY(DATETIME(b.[prescription_start])) - JULIANDAY(DATETIME(a.[prescription_start])) * 24 ) between 0 and @max_combination_start_gap --- CONDITION FOR COMBINATION
--          and ABS(JULIANDAY(DATETIME(b.[authoring_date])) - JULIANDAY(DATETIME(a.[authoring_date])) * 24) <
--              @max_combination_authoring_gap --- CONDITION FOR COMBINATION
-- WHERE (
--               ((a.[daily_frequency] = -1) and (b.[daily_frequency] = -1)) -- 1
--               or
--               ((a.[daily_frequency] <> -1) and (b.[daily_frequency] <> -1) and
--                (DATETIME(a.[prescription_end]) >= DATETIME(b.[prescription_start])) and (DATETIME(a.[prescription_end]) >= DATETIME(b.[prescription_end]))) -- 5
--               or
--               ((a.[daily_frequency] <> -1) and (b.[daily_frequency] <> -1) and
--                (DATETIME(a.[prescription_end]) >= DATETIME(b.[prescription_start])) and (DATETIME(a.[prescription_end]) < DATETIME(b.[prescription_end]))) --6
--           -- Edit 19 April 2019: added combinations for OOF then REG, like doxycycline or daptomycin
--               or
--               ((a.[daily_frequency] = -1) and (b.[daily_frequency] <> -1) and (a.[drug_id] = b.[drug_id])) --2
--           )
-- ORDER BY a.[patient_id], a.[prescription_id], a.[authoring_date], b.[prescription_start];
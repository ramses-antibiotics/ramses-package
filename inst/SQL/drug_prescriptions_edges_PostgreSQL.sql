WITH rx_edges AS (
    SELECT a.patient_id                                                           AS patient_id
         , a.id                                                      AS from_id
         , b.id                                                      AS to_id
         , a.prescription_status                                                  AS from_status
         , b.prescription_status                                                  AS to_status
         , a.prescription_start                                                   AS from_start
         , a.prescription_end                                                     AS from_end
         , a.authoring_date                                                       AS from_authoring
         , b.prescription_start                                                   AS to_start
         , b.prescription_end                                                     AS to_end
         , b.authoring_date                                                       AS to_authoring
         , a.drug_id                                                              AS from_drug
         , b.drug_id                                                              AS to_drug
         , CASE
               WHEN (a.daily_frequency = -1) and (b.daily_frequency = -1) THEN '1'
               WHEN (a.daily_frequency = -1) and (b.daily_frequency <> -1) THEN '2'
               WHEN (a.daily_frequency <> -1) and (b.daily_frequency = -1) and
                    (a.prescription_end < b.prescription_start) THEN '3'
               WHEN (a.daily_frequency <> -1) and (b.daily_frequency = -1) and
                    (a.prescription_end >= b.prescription_start) THEN '4'
               WHEN (a.daily_frequency <> -1) and (b.daily_frequency <> -1) and
                    (a.prescription_start <= b.prescription_start) and
                    (a.prescription_end >= b.prescription_end) THEN '5'
               WHEN (a.daily_frequency <> -1) and (b.daily_frequency <> -1) and
                    (a.prescription_end >= b.prescription_start) and
                    (a.prescription_end < b.prescription_end)
                   THEN '6'
               WHEN (a.daily_frequency <> -1) and (b.daily_frequency <> -1) and
                    (a.prescription_end < b.prescription_start) THEN '7'
               ELSE NULL
        END                                                                         AS relation_type
         , CASE WHEN a."ATC_route" = 'P' and b."ATC_route" <> 'P' THEN 1 ELSE 0 END AS switch_oral
    FROM drug_prescriptions a
             INNER JOIN
         drug_prescriptions b
         ON a.patient_id = b.patient_id
             and a.prescription_id <> b.prescription_id
             and a.prescription_context = b.prescription_context
             and a.antiinfective_type = b.antiinfective_type
)

INSERT
INTO drug_prescriptions_edges (patient_id, from_id, to_id, edge_type, therapy_rank, combination_rank,
                                 relation_type, switch_oral)

SELECT
       patient_id
     , from_id
     , to_id
     , edge_type
     , NULL AS therapy_rank
     , NULL AS combination_rank --     ROW_NUMBER() OVER(PARTITION BY a.patient_id, JULIANDAY(a.authoring_date) ORDER BY a.drug_name ) AS combination_rank
     , relation_type
     , switch_oral
FROM (SELECT *,
             CASE
                 WHEN
                         (relation_type IN ('1', '5', '6')
                             AND ABS(EXTRACT(EPOCH FROM ( to_start::TIMESTAMP - from_start::TIMESTAMP ))) / 3600 BETWEEN 0 AND @max_combination_start_gap
                             AND ABS(EXTRACT(EPOCH FROM ( to_authoring::TIMESTAMP - from_authoring::TIMESTAMP ))) / 3600 <
                                 @max_combination_authoring_gap)
                         OR (relation_type = '2'
                         AND (EXTRACT(EPOCH FROM ( to_authoring::TIMESTAMP -  from_authoring::TIMESTAMP ))) / 3600 BETWEEN 0 AND @max_combination_start_gap
                         AND ABS(EXTRACT(EPOCH FROM ( to_authoring::TIMESTAMP -  from_authoring::TIMESTAMP ))) / 3600 < @max_combination_authoring_gap
                         AND from_drug = to_drug)
                     THEN 'combination'
                 WHEN
                         (relation_type IN ('4', '5', '6'))
                         OR
                         (relation_type IN ('1', '2', '3', '7')
                             AND
                          ABS(EXTRACT(EPOCH FROM ( to_start::TIMESTAMP - from_start::TIMESTAMP ))) / 3600 < @max_continuation_gap)
                     THEN 'continuation'
                 ELSE NULL
                 END AS edge_type
      FROM rx_edges) final

WHERE
      (final.edge_type = 'combination')
      OR
      (final.edge_type = 'continuation'
           AND (final.from_status NOT IN ('cancelled', 'draft', 'entered-in-error')
        AND final.to_status NOT IN ('cancelled', 'draft', 'entered-in-error')))

ORDER BY patient_id, from_id, from_authoring, to_start;

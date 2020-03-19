INSERT INTO [drug_prescriptions_edges]  (from_id, to_id, [type_code], [type_label], [TherapyRank], [CombiRank],[relation_type], switch_oral)
SELECT
	  a.prescription_id as from_id
	, b.prescription_id as to_id
	, 1 as type_code
	, 'isCombinedwith' as type_label
	, [TherapyRank] = NULL
	, CAST(ROW_NUMBER() OVER(PARTITION BY a.patient_id, DATEPART(DAY,  a.authoring_date) order by a.Trade) as integer)  as [CombiRank]
	, CASE 
		WHEN (a.[prescription_status] = 'OOF') and (b.[prescription_status] = 'OOF')   THEN '1'
		WHEN (a.[prescription_status] = 'OOF') and (b.[prescription_status] <> 'OOF')   THEN '2'
		WHEN (a.[prescription_status] <> 'OOF') and (b.[prescription_status] = 'OOF') and (a.[est_end_admin] < b.[first_admin_time]) THEN '3'
		WHEN (a.[prescription_status] <> 'OOF') and (b.[prescription_status] = 'OOF') and (a.[est_end_admin] >= b.[first_admin_time]) THEN '4'
		WHEN (a.[prescription_status] <> 'OOF') and (b.[prescription_status] <> 'OOF') and (a.[est_end_admin] >= b.[first_admin_time]) and (a.[est_end_admin] >= b.[est_end_admin]) THEN '5'
		WHEN (a.[prescription_status] <> 'OOF') and (b.[prescription_status] <> 'OOF') and (a.[est_end_admin] >= b.[first_admin_time]) and (a.[est_end_admin] < b.[est_end_admin]) THEN '6'
		WHEN (a.[prescription_status] <> 'OOF') and (b.[prescription_status] <> 'OOF') and (a.[est_end_admin] < b.[first_admin_time]) THEN '7'
	END AS relation_type
	, CASE
		WHEN (LEFT(a.[route], 1) = 'I') and (LEFT(b.[route], 1) <> 'I') THEN 1
		ELSE 0
	END AS switch_oral
FROM
	[Abx_Prescriptions_DDD] a
inner JOIN
	[Abx_Prescriptions_DDD] b
ON	a.patient_id = b.patient_id 
	-- Edit 16 Apr 2019: remove the exclusion of REG_T, replaced with condition that both are TTA or neither 
	--and a.[prescription_status] <> 'REG_T' AND b.[prescription_status] <> 'REG_T' 
	and ( (a.[prescription_status] <> 'REG_T' AND b.[prescription_status] <> 'REG_T') OR (a.[prescription_status] = 'REG_T' AND b.[prescription_status] = 'REG_T') )
	and a.prescription_id <> b.prescription_id
 	and a.Trade <> b.Trade ---- CONDITION FOR COMBINATION
	and datediff(hh, a.first_admin_time, b.first_admin_time) between 0 and 24 --- CONDITION FOR COMBINATION
	and abs(DATEDIFF(hh, a.authoring_date, b.authoring_date)) <6 --- CONDITION FOR COMBINATION
WHERE  
	(
	((a.[prescription_status] = 'OOF') and (b.[prescription_status] = 'OOF') ) -- 1
	or
	((a.[prescription_status] <> 'OOF') and (b.[prescription_status] <> 'OOF') and (a.[est_end_admin] >= b.[first_admin_time]) and (a.[est_end_admin] >= b.[est_end_admin])) -- 5
	or
	((a.[prescription_status] <> 'OOF') and (b.[prescription_status] <> 'OOF') and (a.[est_end_admin] >= b.[first_admin_time]) and (a.[est_end_admin] < b.[est_end_admin])) --6
	-- Edit 19 April 2019: added combinations for OOF then REG, like doxycycline or daptomycin
	or
	((a.[prescription_status] = 'OOF') and (b.[prescription_status] <> 'OOF') and (a.vtm_nm = b.vtm_nm)) --2
	)
order by a.patient_id, a.prescription_id, a.[first_admin_time], b.[first_admin_time];
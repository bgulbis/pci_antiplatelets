WITH ICD AS (
	SELECT DISTINCT
		NOMENCLATURE.NOMENCLATURE_ID
	FROM
		NOMENCLATURE
	WHERE
		REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '^027[0-3]') > 0 
		AND NOMENCLATURE.SOURCE_VOCABULARY_CD = 641836527 -- ICD-10-CM
		AND NOMENCLATURE.PRINCIPLE_TYPE_CD = 761 -- Procedure
), PATIENTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		ENCOUNTER.DISCH_DT_TM
	FROM
		ENCOUNTER,
		ICD,
		PROCEDURE
	WHERE
		ENCOUNTER.ORGANIZATION_ID = 1 -- Memorial Hermann Hospital
		AND ENCOUNTER.REG_DT_TM BETWEEN 
			pi_to_gmt(
				TO_DATE(
					@Prompt('Enter begin date', 'D', , mono, free, persistent, {'01/01/1800 00:00:00'}, User:80),
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				),
				pi_time_zone(1, @Variable('BOUSER'))
			)
			AND pi_to_gmt(
				TO_DATE(
					@Prompt('Enter end date', 'D', , mono, free, persistent, {'01/01/1800 23:59:59'}, User:81),
					pi_get_dm_info_char_gen('Date Format Mask|FT','PI EXP|Systems Configuration|Date Format Mask')
				),
				pi_time_zone(1, @Variable('BOUSER'))
			)
		AND ENCOUNTER.ENCNTR_TYPE_CLASS_CD = 42631 -- Inpatient
		AND ENCOUNTER.LOC_FACILITY_CD = 3310 -- HH HERMANN
		AND ENCOUNTER.ENCNTR_ID = PROCEDURE.ENCNTR_ID
		AND ICD.NOMENCLATURE_ID = PROCEDURE.NOMENCLATURE_ID
), CANGRELOR_PTS AS (
	SELECT DISTINCT
		ENCOUNTER.ENCNTR_ID,
		ENCOUNTER.PERSON_ID,
		ENCOUNTER.DISCH_DT_TM,
		'CANGRELOR' AS PT_GROUP
	FROM
		ENCNTR_ALIAS,
		ENCOUNTER
	WHERE
	    ENCNTR_ALIAS.ALIAS IN @prompt('Enter value(s) for Alias','A',,Multi,Free,Persistent,,User:0)
		AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
		AND ENCNTR_ALIAS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
), ALL_PTS AS (
	SELECT 
		CANGRELOR_PTS.ENCNTR_ID,
		CANGRELOR_PTS.PERSON_ID,
		CANGRELOR_PTS.DISCH_DT_TM
	FROM 
		CANGRELOR_PTS
	
	UNION
	
	SELECT 
		PATIENTS.ENCNTR_ID,
		PATIENTS.PERSON_ID,
		PATIENTS.DISCH_DT_TM
	FROM 
		PATIENTS	
)

SELECT DISTINCT
	ENCNTR_ALIAS.ALIAS AS FIN,
	pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))) AS DOSE_DATETIME,
	pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS MEDICATION,
	CE_MED_RESULT.ADMIN_DOSAGE AS DOSE,
	pi_get_cv_display(CE_MED_RESULT.DOSAGE_UNIT_CD) AS DOSE_UNIT,
	CE_MED_RESULT.INFUSION_RATE AS RATE,
	pi_get_cv_display(CE_MED_RESULT.INFUSION_UNIT_CD) AS RATE_UNIT,
	pi_get_cv_display(CE_MED_RESULT.ADMIN_ROUTE_CD) AS ROUTE		
FROM 
	ALL_PTS,
	CE_MED_RESULT,
	CLINICAL_EVENT,
	ENCNTR_ALIAS
WHERE
	ALL_PTS.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
	AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
	AND ALL_PTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
	AND CLINICAL_EVENT.EVENT_CLASS_CD = 158 -- MED
	AND ALL_PTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
	AND CLINICAL_EVENT.EVENT_CD IN (
		37556189, -- aspirin
		37556579, -- clopidogrel
		405732890, -- prasugrel
		652582238, -- ticagrelor
		1693675401, -- cangrelor
		37556001, -- abciximab
		37556867, -- eptifibatide
		37558212, -- tirofiban
		37556313, -- bivalirudin
		37556844, -- enoxaparin
		37557146, -- heparin
		37558355, -- warfarin
		535736194, -- dabigatran
		642177890, -- rivaroxaban
		894197564, -- apixaban
		1466817862, -- edoxaban
		37556077, -- alteplase
		37557978, -- reteplase
		37558157 -- tenecteplase
	)
	AND CLINICAL_EVENT.EVENT_END_DT_TM < ALL_PTS.DISCH_DT_TM
	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31' 
	AND CLINICAL_EVENT.EVENT_ID = CE_MED_RESULT.EVENT_ID
	AND (
		CE_MED_RESULT.ADMIN_DOSAGE > 0
		OR CE_MED_RESULT.IV_EVENT_CD IN (
			688706, -- Begin Bag
			688709 -- Rate Change
		)
	)

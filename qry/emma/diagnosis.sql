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
	pi_get_cv_display(NOMENCLATURE.SOURCE_VOCABULARY_CD) AS ICD_TYPE,
	NOMENCLATURE.SOURCE_IDENTIFIER AS ICD_CODE,
	NOMENCLATURE.SOURCE_STRING AS ICD_DESCRIPTION,
	DIAGNOSIS.DIAG_PRIORITY AS CODE_PRIORITY
FROM 
	ALL_PTS,
	DIAGNOSIS,
	ENCNTR_ALIAS,
	NOMENCLATURE
WHERE
	ALL_PTS.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
	AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
    AND ALL_PTS.ENCNTR_ID = DIAGNOSIS.ENCNTR_ID
	AND DIAGNOSIS.DIAG_TYPE_CD = 26244 -- Final
	AND DIAGNOSIS.NOMENCLATURE_ID = NOMENCLATURE.NOMENCLATURE_ID
	AND NOMENCLATURE.SOURCE_VOCABULARY_CD IN (
		739, -- ICD-9-CM
		641836527 -- ICD-10-CM
	)
	AND NOMENCLATURE.PRINCIPLE_TYPE_CD = 751 -- Disease or Syndrome

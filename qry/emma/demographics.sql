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
), WEIGHTS AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID,
		MAX(CLINICAL_EVENT.RESUlT_VAL) KEEP (DENSE_RANK FIRST ORDER BY CLINICAL_EVENT.EVENT_END_DT_TM) AS WEIGHT
	FROM
		ALL_PTS,
		CLINICAL_EVENT
	WHERE
		CLINICAL_EVENT.EVENT_CD = 30107 -- Weight
		AND ALL_PTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_END_DT_TM <= ALL_PTS.DISCH_DT_TM
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'	
		AND ALL_PTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 159 -- NUM
		AND CLINICAL_EVENT.RESULT_UNITS_CD = 170 -- kg
	GROUP BY
		CLINICAL_EVENT.ENCNTR_ID
), HEIGHTS AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID,
		MAX(CLINICAL_EVENT.RESUlT_VAL) KEEP (DENSE_RANK FIRST ORDER BY CLINICAL_EVENT.EVENT_END_DT_TM) AS HEIGHT
	FROM
		ALL_PTS,
		CLINICAL_EVENT
	WHERE
		CLINICAL_EVENT.EVENT_CD = 30066 -- Height
		AND ALL_PTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_END_DT_TM <= ALL_PTS.DISCH_DT_TM
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'	
		AND ALL_PTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 159 -- NUM
		AND CLINICAL_EVENT.RESULT_UNITS_CD = 164 -- cm
	GROUP BY
		CLINICAL_EVENT.ENCNTR_ID
), BMI AS (
	SELECT DISTINCT
		CLINICAL_EVENT.ENCNTR_ID,
		MAX(CLINICAL_EVENT.RESUlT_VAL) KEEP (DENSE_RANK FIRST ORDER BY CLINICAL_EVENT.EVENT_END_DT_TM) AS BMI
	FROM
		ALL_PTS,
		CLINICAL_EVENT
	WHERE
		CLINICAL_EVENT.EVENT_CD = 119838802 -- Body Mass Index
		AND ALL_PTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
		AND CLINICAL_EVENT.EVENT_END_DT_TM <= ALL_PTS.DISCH_DT_TM
		AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31'	
		AND ALL_PTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
		AND CLINICAL_EVENT.EVENT_CLASS_CD = 159 -- NUM
	GROUP BY
		CLINICAL_EVENT.ENCNTR_ID
)

SELECT DISTINCT
	ENCNTR_ALIAS.ALIAS AS FIN,
	CANGRELOR_PTS.PT_GROUP,
	pi_get_cv_display(ENCOUNTER.ENCNTR_TYPE_CLASS_CD) AS ENCNTR_TYPE_CLASS,
    TRUNC(((pi_from_gmt(ENCOUNTER.REG_DT_TM, (pi_time_zone(1, @Variable('BOUSER'))))) - PERSON.BIRTH_DT_TM) / 365.25, 0) AS AGE,
	pi_get_cv_display(PERSON.RACE_CD) AS RACE,
	pi_get_cv_display(PERSON.SEX_CD) AS SEX,
	HEIGHTS.HEIGHT AS HEIGHT_CM,
	WEIGHTS.WEIGHT AS WEIGHT_KG,
	BMI.BMI AS BMI
FROM 
	ALL_PTS,
	BMI,
	CANGRELOR_PTS,
	ENCNTR_ALIAS,
	ENCOUNTER,
	HEIGHTS,
	PERSON,
	WEIGHTS
WHERE
	ALL_PTS.ENCNTR_ID = CANGRELOR_PTS.ENCNTR_ID(+)
	AND ALL_PTS.ENCNTR_ID = ENCOUNTER.ENCNTR_ID
	AND ALL_PTS.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
	AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
	AND ALL_PTS.PERSON_ID = PERSON.PERSON_ID
	AND ALL_PTS.ENCNTR_ID = HEIGHTS.ENCNTR_ID(+)
	AND ALL_PTS.ENCNTR_ID = WEIGHTS.ENCNTR_ID(+)
	AND ALL_PTS.ENCNTR_ID = BMI.ENCNTR_ID(+)
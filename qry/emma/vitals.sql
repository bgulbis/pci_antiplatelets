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
), CATHS AS (
	SELECT DISTINCT
		ALL_PTS.ENCNTR_ID,
		PROCEDURE.PROC_DT_TM
	FROM 
		ALL_PTS,
		NOMENCLATURE,
		PROCEDURE
	WHERE
		ALL_PTS.ENCNTR_ID = PROCEDURE.ENCNTR_ID
		AND PROCEDURE.NOMENCLATURE_ID = NOMENCLATURE.NOMENCLATURE_ID
		AND NOMENCLATURE.SOURCE_VOCABULARY_CD = 641836527 -- ICD-10-CM
		AND REGEXP_INSTR(NOMENCLATURE.SOURCE_IDENTIFIER, '^027[0-3]') > 0 
)

SELECT DISTINCT
	ENCNTR_ALIAS.ALIAS AS FIN,
	pi_from_gmt(CLINICAL_EVENT.EVENT_END_DT_TM, (pi_time_zone(1, @Variable('BOUSER')))) AS VITAL_DATETIME,
	pi_get_cv_display(CLINICAL_EVENT.EVENT_CD) AS VITAL,
	CLINICAL_EVENT.RESULT_VAL AS RESULT_VAL,
	pi_get_cv_display(CLINICAL_EVENT.RESULT_UNITS_CD) AS RESULT_UNITS
FROM 
	CANGRELOR_PTS,
	CATHS,
	CLINICAL_EVENT,
	ENCNTR_ALIAS
WHERE
	CANGRELOR_PTS.ENCNTR_ID = ENCNTR_ALIAS.ENCNTR_ID
	AND ENCNTR_ALIAS.ENCNTR_ALIAS_TYPE_CD = 619 -- FIN NBR
	AND CANGRELOR_PTS.ENCNTR_ID = CATHS.ENCNTR_ID
	AND CANGRELOR_PTS.ENCNTR_ID = CLINICAL_EVENT.ENCNTR_ID
	AND CLINICAL_EVENT.EVENT_CLASS_CD = 159 -- NUM
	AND CANGRELOR_PTS.PERSON_ID = CLINICAL_EVENT.PERSON_ID
	AND CLINICAL_EVENT.EVENT_CD IN (
		31806, -- Hct
		31854, -- Hgb
		30098, -- Systolic Blood Pressure
		134401648, -- Arterial Systolic BP 1
		30051, -- Diastolic Blood Pressure
		134401703, -- Arterial Diastolic BP 1
		119822453, -- Mean Arterial Pressure
		173814326, -- Mean Arterial Pressure (Invasive)
		30065, -- Peripheral Pulse Rate
		119822527, -- Apical Heart Rate
		30094, -- Respiratory Rate
		10739818, -- SpO2 percent
		30100, -- Temperature
		119822492, -- Temperature Tympanic
		119822505, -- Temperature Rectal
		119822517, -- Temperature Axillary
		119822523, -- Temperature Intravascular
		119822536, -- Temperature Oral
		172563303, -- Temperature Skin
		172563306, -- Temperature Esophageal
		172563327, -- Temperature Brain
		263779626, -- Temperature Bladder		
		10679282 -- Temperature Sensor
	)
	AND CLINICAL_EVENT.EVENT_END_DT_TM BETWEEN
		CATHS.PROC_DT_TM - 0.5 
		AND CATHS.PROC_DT_TM + 1
	AND CLINICAL_EVENT.VALID_UNTIL_DT_TM > DATE '2099-12-31' 
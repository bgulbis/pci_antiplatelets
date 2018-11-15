library(tidyverse)
library(edwr)
library(openxlsx)
library(icd)

# MBO project folder:
# /Resident Projects/pci_antiplatelets

dir_raw <- "data/raw"
tz <- "US/Central"

# 01_patients ------------------------------------------

pts <- read_data(dir_raw, "patients", FALSE) %>%
    as.patients()

mbo_id <- concat_encounters(pts$millennium.id)
print(mbo_id)

# 02_measures -----------------------------------------

mbo_id_split <- concat_encounters(pts$millennium.id, 40)
print(mbo_id_split)

measures <- read_data(dir_raw, "measures", FALSE) %>%
    as.events(order_var = FALSE) %>%
    mutate_at("event.result", as.numeric)

weights <- measures %>%
    filter(
        event == "weight",
        event.result.units == "kg"
    ) %>%
    arrange(millennium.id, event.datetime) %>%
    distinct(millennium.id, .keep_all = TRUE) %>%
    select(millennium.id, weight = event.result)

heights <- measures %>%
    filter(
        event == "height",
        event.result.units == "cm"
    ) %>%
    arrange(millennium.id, event.datetime) %>%
    distinct(millennium.id, .keep_all = TRUE) %>%
    select(millennium.id, height = event.result)

bmi <- measures %>%
    filter(event == "body mass index") %>%
    arrange(millennium.id, event.datetime) %>%
    distinct(millennium.id, .keep_all = TRUE) %>%
    select(millennium.id, bmi = event.result)

# 03_demographics --------------------------------------

demog <- read_data(dir_raw, "demographics", FALSE, TRUE) %>%
    as.demographics(extras = list("fin" = "Financial Number"))

# 04_diagnosis -----------------------------------------

diag <- read_data(dir_raw, "diagnosis", FALSE, TRUE) %>%
    as.diagnosis(extras = list("diag.description" = "Diagnosis Description")) %>%
    filter(diag.type == "FINAL")

data_primary <- diag %>%
    filter(diag.seq == "Primary") %>%
    select(millennium.id, diag.code, diag.description) %>%
    inner_join(demog[c("millennium.id", "fin")], by = "millennium.id") %>%
    select(fin, everything(), -millennium.id)

comorb <- icd10_comorbid_ahrq(diag, return_df = TRUE) %>%
    select(millennium.id, HTN, DM, DMcx) %>%
    mutate(diabetes = DM | DMcx) %>%
    select(millennium.id, htn = HTN, diabetes)

af <- diag %>%
    filter(str_detect(diag.code, "I48")) %>%
    distinct(millennium.id) %>%
    mutate(a.fib = TRUE)

cva <- diag %>%
    filter(str_detect(diag.code, "I63")) %>%
    distinct(millennium.id) %>%
    mutate(cva = TRUE)

pe <- diag %>%
    filter(str_detect(diag.code, "I26|I2782")) %>%
    distinct(millennium.id) %>%
    mutate(pe = TRUE)

dvt <- diag %>%
    filter(str_detect(diag.code, "I82")) %>%
    distinct(millennium.id) %>%
    mutate(dvt = TRUE)

data_pmh <- demog %>%
    select(fin, millennium.id) %>%
    full_join(comorb, by = "millennium.id") %>%
    full_join(af, by = "millennium.id") %>%
    full_join(cva, by = "millennium.id") %>%
    full_join(pe, by = "millennium.id") %>%
    full_join(dvt, by = "millennium.id") %>%
    mutate_if(is.logical, funs(coalesce(., FALSE))) %>%
    select(-millennium.id)

# 05_procedures ----------------------------------------

data_proc <- read_data(dir_raw, "procedures", FALSE, TRUE) %>%
    as.procedures(extras = list("proc.description" = "Procedure Description")) %>%
    filter(str_detect(proc.code, "027")) %>%
    inner_join(demog[c("millennium.id", "fin")], by = "millennium.id") %>%
    select(fin, everything(), -millennium.id)

# 06_allergies -----------------------------------------

data_allergies <- read_data(dir_raw, "allergies", FALSE, TRUE) %>%
    rename(
        millennium.id = `Encounter Identifier`,
        allergy = Allergies,
        allergy.status = `Allergy Active Status`
    ) %>%
    mutate_at("allergy", str_to_lower) %>%
    filter(
        !is.na(allergy),
        allergy.status == "Active",
        !str_detect(allergy, "nkf")
    ) %>%
    distinct(millennium.id, allergy) %>%
    inner_join(demog, by = "millennium.id") %>%
    select(fin, allergy)

# 07_outpatient meds -----------------------------------

outpt_meds <- read_data(dir_raw, "outpatient", FALSE, TRUE) %>%
    as.meds_home()

data_meds_home <- outpt_meds %>%
    filter(
        med.type == "Recorded / Home Meds",
        med %in% c("clopidogrel", "prasugrel", "ticagrelor")
    ) %>%
    distinct(millennium.id, med) %>%
    mutate(val = TRUE) %>%
    spread(med, val) %>%
    full_join(demog[c("millennium.id", "fin")], by = "millennium.id") %>%
    mutate_if(is.logical, funs(coalesce(., FALSE))) %>%
    select(fin, everything(), -millennium.id)

data_meds_dc <- outpt_meds %>%
    filter(
        med.type == "Prescription/Discharge Order",
        med %in% c(
            "clopidogrel", 
            "prasugrel",
            "ticagrelor",
            "warfarin",
            "apixaban",
            "dabigatran",
            "rivaroxaban",
            "enoxaparin",
            "aspirin"
        )
    ) %>%
    distinct(millennium.id, med) %>%
    mutate(val = TRUE) %>%
    spread(med, val) %>%
    full_join(demog[c("millennium.id", "fin")], by = "millennium.id") %>%
    mutate_if(is.logical, funs(coalesce(., FALSE))) %>%
    select(fin, everything(), -millennium.id)

# 08_insurance -----------------------------------------

data_insur <- read_data(dir_raw, "insurance") %>%
    rename(
        millennium.id = `Millennium Encounter ID`,
        plan.name = `Primary Health Plan Name`,
        plan.type = `Primary Health Plan Type`,
        plan.class = `Primary Health Plan Fin Class`
    ) %>%
    distinct(millennium.id, plan.type, .keep_all = TRUE) %>%
    inner_join(demog, by = "millennium.id") %>%
    select(fin, plan.name, plan.type, plan.class)

# tidy data --------------------------------------------

data_patients <- demog %>%
    left_join(weights, by = "millennium.id") %>%
    left_join(heights, by = "millennium.id") %>%
    left_join(bmi, by = "millennium.id") %>%
    select(
        fin,
        age,
        gender,
        race,
        weight,
        height,
        bmi
    )

# export data ------------------------------------------

data_list <- list(
    "patients" = data_patients,
    "allergies" = data_allergies,
    "pmh" = data_pmh,
    "primary diagnosis" = data_primary,
    "procedure" = data_proc,
    "insurance" = data_insur,
    "home meds" = data_meds_home,
    "dc meds" = data_meds_dc
)

write.xlsx(data_list, "data/external/pci_antiplatelet_data.xlsx")

dirr::gzip_files(recursive = TRUE)

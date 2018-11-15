library(tidyverse)
library(edwr)
library(openxlsx)

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

measures <- read_data(dir_raw, "measures", FALSE) %>%
    as.events() %>%
    mutate_at("event.result", as.numeric)

# 03_demographics --------------------------------------

demog <- read_data(dir_raw, "demographics", FALSE, TRUE) %>%
    as.demographics(extras = list("fin" = "Financial Number"))

# 04_diagnosis -----------------------------------------

diag <- read_data(dir_raw, "diagnosis", FALSE, TRUE) %>%
    as.diagnosis(extras = list("diag.description" = "Diagnosis Description"))

# 05_procedures ----------------------------------------

proc <- read_data(dir_raw, "procedures", FALSE, TRUE) %>%
    as.procedures(extras = list("proc.description" = "Procedure Description"))

# 06_allergies -----------------------------------------

allergies <- read_data(dir_raw, "allergies", FALSE, TRUE) %>%
    rename(
        millennium.id = `Encounter Identifier`,
        allergy = Allergies,
        allergy.status = `Allergy Active Status`
    ) %>%
    filter(
        !is.na(allergy),
        allergy.status == "Active"
    )

# 07_insurance -----------------------------------------

insur <- read_data(dir_raw, "insurance") %>%
    distinct() %>%
    rename(
        millennium.id = `Millennium Encounter ID`,
        plan.name = `Primary Health Plan Name`,
        plan.type = `Primary Health Plan Type`,
        plan.class = `Primary Health Plan Fin Class`
    ) 

# tidy data --------------------------------------------



# export data ------------------------------------------

dirr::gzip_files(recursive = TRUE)

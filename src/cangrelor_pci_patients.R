library(tidyverse)
library(readxl)

pts <- read_excel(
    "data/external/cangrelor_pci_patients.xlsx",
    skip = 1, 
    col_names = c("fin", "indication")
) %>%
    mutate_at("fin", str_replace_all, pattern = "^C0", replacement = "")

mbo_fin <- edwr::concat_encounters(pts$fin)
mbo_fin

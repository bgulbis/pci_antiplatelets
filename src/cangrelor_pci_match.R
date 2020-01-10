library(tidyverse)
library(readxl)
library(MatchIt)
library(openxlsx)

pts_cang <- read_excel(
    "data/external/cangrelor_pci_match.xlsx", 
    sheet = "Cangrelor",
    col_names = c("fin", "sex", "age", "chf", "dm", "indication", "approach"),
    skip = 1
) %>%
    mutate(cangrelor = TRUE)

pts_other <- read_excel(
    "data/external/cangrelor_pci_match.xlsx", 
    sheet = "Other",
    col_names = c("fin", "sex", "age", "chf", "dm", "indication", "approach"),
    skip = 1
) %>%
    filter(!is.na(age)) %>%
    mutate(cangrelor = FALSE)

df <- bind_rows(pts_cang, pts_other) %>%
    mutate_at(c("sex", "indication", "approach"), factor) %>%
    mutate_at(c("chf", "dm"), ~(. == 1))

set.seed(77123)
m_data <- matchit(
    cangrelor ~ sex + age + chf + dm + indication + approach,
    data = df, 
    ratio = 2
)

df_match <- match.data(m_data)

write.xlsx(df_match, "data/external/cangrelor_pci_match_results.xlsx")

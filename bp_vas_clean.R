bp1 <- read_excel("BP_and_VAS.xlsx")
bp2 <- read_excel("BP_and_VAS (1).xlsx")

# Zusammenfügen
bp_vas <- bind_rows(bp1, bp2)

bp_vas[bp_vas == 999] <- NA


names(bp_vas)

new_names <- c(
  "ID",
  "Baseline_Sys_1", "Baseline_Dia_1", "Baseline_Pulse_1",
  "Baseline_Sys_2", "Baseline_Dia_2", "Baseline_Pulse_2",
  "PostTSM_Sys_1", "PostTSM_Dia_1", "PostTSM_Pulse_1",
  "PostTSM_Sys_2", "PostTSM_Dia_2", "PostTSM_Pulse_2",
  
  "Interview_Sys_1", "Interview_Dia_1", "Interview_Pulse_1",
  "Interview_Sys_2", "Interview_Dia_2", "Interview_Pulse_2",
  
  "Enc1_Sys", "Enc1_Dia", "Enc1_Pulse",
  "Empty_23", "Empty_24", "Empty_25",  # diese musst du evtl. löschen
  
  "Arithm_Sys_1", "Arithm_Dia_1", "Arithm_Pulse_1",
  "Arithm_Sys_2", "Arithm_Dia_2", "Arithm_Pulse_2",
  
  "Enc2_Sys_1", "Enc2_Dia_1", "Enc2_Pulse_1",
  "Enc2_Sys_2", "Enc2_Dia_2", "Enc2_Pulse_2",
  
  "PostTSST_Sys_1", "PostTSST_Dia_1", "PostTSST_Pulse_1",
  "PostTSST_Sys_2", "PostTSST_Dia_2", "PostTSST_Pulse_2",
  
  "VAS_1", "VAS_2", "VAS_3",
  
  "PostDMS_Sys_1", "PostDMS_Dia_1", "PostDMS_Pulse_1",
  "PostDMS_Sys_2", "PostDMS_Dia_2", "PostDMS_Pulse_2"
)

names(bp_vas) <- new_names

bp_vas_merged <- bp_vas %>%
  group_by(ID) %>%
  summarise(across(everything(), ~ first(na.omit(.))), .groups = "drop")


bp_vas_clean <- bp_vas_merged %>%
  mutate(across(where(is.numeric), ~ ifelse(. == 999, NA, .)))

#erste zeile entfernen

bp_vas_clean <- bp_vas_clean[-1, ]

#ID und 3 raus, da nur NA 
bp_vas_clean <- bp_vas_clean %>%
  filter(if_any(-ID, ~ !is.na(.)))

bp_vas_clean <- bp_vas_clean %>%
  filter(if_any(starts_with("Baseline_") | starts_with("PostTSST_"), ~ !is.na(.)))


bp_vas_clean <- bp_vas_clean %>%
  mutate(across(-ID, ~ as.numeric(.)))
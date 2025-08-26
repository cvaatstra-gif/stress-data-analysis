library(readxl)
library(dplyr)
library(purrr)


new_names <- c(
  "ID",
  "Baseline_Sys_1", "Baseline_Dia_1", "Baseline_Pulse_1",
  "Baseline_Sys_2", "Baseline_Dia_2", "Baseline_Pulse_2",
  "PostTSM_Sys_1", "PostTSM_Dia_1", "PostTSM_Pulse_1",
  "PostTSM_Sys_2", "PostTSM_Dia_2", "PostTSM_Pulse_2",
  
  
  "Interview_Sys_1", "Interview_Dia_1", "Interview_Pulse_1",
  "Interview_Sys_2", "Interview_Dia_2", "Interview_Pulse_2",
  
  
  "Enc1_Sys_1", "Enc1_Dia_1", "Enc1_Pulse_1",
  "Enc1_Sys_2", "Enc1_Dia_2", "Enc1_Pulse_2",  
  
  
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

bp_vas_clean <- map_dfr(
  c("BP_and_VAS.xlsx", "BP_and_VAS (1).xlsx"), read_excel
) %>%
  mutate(across(where(is.numeric), ~ na_if(., 999))) %>%
  set_names(new_names) %>%
  group_by(ID) %>%
  summarise(across(everything(), ~ first(na.omit(.))), .groups = "drop") %>%
  slice(-1) %>%
  filter(
    if_any(-ID, ~ !is.na(.)),
    if_any(starts_with("Baseline_") | starts_with("PostTSST_"), ~ !is.na(.))
  ) %>%
  mutate(across(-ID, as.numeric)) %>%
  # drop columns that are entirely 999 (now NA)
  select(where(~ !all(is.na(.))))



bp_vas_clean <- bp_vas_clean %>%
  mutate(across(-ID, ~ na_if(., 999)))

# ✅ 1. Zeitpunkte definieren
zeitpunkte <- c("Baseline", "PostTSM", "PostTSST", "Interview", "Enc1", "Arithm", "Enc2", "PostDMS")

# ✅ 2. Für jeden Zeitpunkt: Systole/Diastole-Paare überprüfen und ggf. tauschen
for (zeit in zeitpunkte) {
  for (i in 1:2) {
    sys_name <- paste0(zeit, "_Sys_", i)
    dia_name <- paste0(zeit, "_Dia_", i)
    
    if (all(c(sys_name, dia_name) %in% names(bp_vas_clean))) {
      swap_name <- paste0("Swap_", zeit, "_", i)
      
      bp_vas_clean <- bp_vas_clean %>%
        mutate(
          !!swap_name := .data[[dia_name]] > .data[[sys_name]],
          !!sys_name := ifelse(.data[[swap_name]], .data[[dia_name]], .data[[sys_name]]),
          !!dia_name := ifelse(.data[[swap_name]], .data[[sys_name]], .data[[dia_name]])
        ) %>%
        select(-all_of(swap_name))  # Swap-Spalte entfernen
    }
  }
}


# Mittelwerte für Pulse, Systole und Diastole berechnen
messungen <- c("Pulse", "Sys", "Dia")
for (zeit in zeitpunkte) {
  for (mess in messungen) {
    cols <- paste0(zeit, "_", mess, "_", 1:2)
    existing <- intersect(names(bp_vas_clean), cols)
    if (length(existing) > 0) {
      bp_vas_clean[[paste(mess, zeit, sep = "_")]] <-
        rowMeans(bp_vas_clean[existing], na.rm = TRUE)
    }
  }
}

# Basline zusammen mittelwert und Post TSST, wenn nur ein wert vorliegt so lassen, wenn zwei vorliegen mittelwert


names(bp_vas_clean)

# Kombinationen aus Messarten und Zeitpunkten erzeugen
mean_cols <- as.vector(outer(messungen, zeitpunkte, paste, sep = "_"))

# Datenrahmen mit den Mittelwerten aufbauen
mittelwerte <- bp_vas_clean %>%
  select(ID, VAS_1, VAS_2, VAS_3, all_of(mean_cols))




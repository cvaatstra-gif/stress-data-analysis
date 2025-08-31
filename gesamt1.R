# Einlesen
library(readxl)
library(ggplot2)
library(dplyr)
library(effectsize)
library(purrr)

limesurvey <- read_excel("limesurvey_all_final.xlsx")

# Relevante Spalten anhand der aktuellen Spaltennamen bestimmen
id_col <- names(limesurvey)[8]
geschlecht_col <- names(limesurvey)[9]
alter_col <- names(limesurvey)[10]
rrs_cols <- names(limesurvey)[131:152]

# Spalten sinnvoll umbenennen
limesurvey <- limesurvey %>%
  rename(
    ID = all_of(id_col),
    Geschlecht = all_of(geschlecht_col),
    Alter = all_of(alter_col)
  ) %>%
  rename_with(~ paste0("RRS_", seq_along(.)), all_of(rrs_cols))

# ID, Geschlecht, Alter, RRS-Items
fragebogen_clean <- limesurvey %>%
  select(ID, Geschlecht, Alter, starts_with("RRS_"))

#Erste Spalte weg weil nur angaben von der Umfrage
fragebogen_clean <- fragebogen_clean[-1, ]

# Neue Codierung für Antwortoptionen
antwort_coding <- c("fast nie" = 1, "manchmal" = 2, "häufig" = 3, "fast immer" = 4)

# Alle RRS-Spalten numerisch umcodieren
fragebogen_clean <- fragebogen_clean %>%
  mutate(across(starts_with("RRS_"), ~ as.numeric(recode(.x, !!!antwort_coding))))

# Gesamt-RRS-Score berechnen
fragebogen_clean <- fragebogen_clean %>%
  mutate(RRS_Score = rowSums(across(starts_with("RRS_")), na.rm = TRUE))

# Check: erste Item-Spalte
table(fragebogen_clean$RRS_1)  

# Check: Scores insgesamt
summary(fragebogen_clean$RRS_Score)
table(fragebogen_clean$Geschlecht)

#Mittelwert, N, SD, Min, Max nach Geschlecht 
fragebogen_clean %>%
  group_by(Geschlecht) %>%
  summarise(
    n = n(),
    Mittelwert = mean(RRS_Score, na.rm = TRUE),
    SD = sd(RRS_Score, na.rm = TRUE),
    Min = min(RRS_Score, na.rm = TRUE),
    Max = max(RRS_Score, na.rm = TRUE)
  )
# t-Test: Unterschiede in der RRS zwischen Männern und Frauen
t.test(RRS_Score ~ Geschlecht, data = fragebogen_clean)
#nicht signifikant



#____________________________
#bp_vas_clean erstellen und bereinigen

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

#Alle 999 rausfiltern
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



# Kombinationen aus Messarten und Zeitpunkten erzeugen
mean_cols <- as.vector(outer(messungen, zeitpunkte, paste, sep = "_"))

# Datenrahmen mit den Mittelwerten aufbauen
mittelwerte <- bp_vas_clean %>%
  select(ID, VAS_1, VAS_2, VAS_3, all_of(mean_cols))


#### alles neue zusammen

gesamt1 <- left_join(fragebogen_clean, mittelwerte, by = "ID")


gesamt1 <- gesamt1 %>%
  select(-starts_with("RRS..."))


summary(gesamt1$RRS_Score)

#Conditions 
con <- read_excel("cond_overview.xlsx")


# Erstmal sicherstellen, dass beide IDs als character vorliegen
con$ID <- as.character(con$ID)
gesamt1$ID <- as.character(gesamt1$ID)

# Alle Zeilen entfernen, bei denen die ID fehlt
con <- con %>% filter(!is.na(ID))
con <- con %>% filter(rowSums(is.na(.)) < ncol(.))
con <- con %>%
  filter(grepl("^ID", ID))

gesamt1 <- gesamt1 %>%
  select(-matches("^Condition"))


gesamt1 <- left_join(gesamt1, con[, c("ID", "Condition")], by = "ID")
gesamt1 <- gesamt1 %>%
  select(-matches("^Condition"))


# Dann wie geplant aufteilen
gesamt1$Treatment <- substr(gesamt1$Condition, 1, 1)
gesamt1$TMS <- substr(gesamt1$Condition, 2, 2)

# Zuerst sicherstellen, dass Treatment numerisch ist
gesamt1$Treatment <- as.numeric(gesamt1$Treatment)


library(ggplot2)


#Wurde RSS gut aufgeteilt? Unterscheiden sich die Mittelwerte signifikant?
t.test(RRS_Score ~ Treatment, data = gesamt1)

# VAS_1: z.B. "Wie gestresst fühlst du dich?"
t.test(VAS_1 ~ Treatment, data = gesamt1)

# VAS_2: z.B. "Wie angespannt fühlst du dich?"
t.test(VAS_2 ~ Treatment, data = gesamt1)

# VAS_3: z.B. "Wie nervös fühlst du dich?"
t.test(VAS_3 ~ Treatment, data = gesamt1)

library(tidyr)
library(dplyr)
library(afex)
library(ez)
library(tidyverse)



names(gesamt1)


# Anova 

pulse_long %>%
  filter(!is.na(Wert)) %>%
  group_by(Zeitpunkt, Treatment) %>%
  summarise(n = n_distinct(ID), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Treatment, values_from = n,
                     names_prefix = "Treatment_")


pulse_long <- gesamt1 %>%
  select(ID, Treatment, Pulse_Baseline, Pulse_Interview) %>%
  pivot_longer(cols = starts_with("Pulse_"),
               names_to = "Zeitpunkt",
               values_to = "Puls") %>%
  mutate(Zeitpunkt = recode(Zeitpunkt,
                            "Pulse_Baseline" = "Baseline",
                            "Pulse_Interview" = "Interview"),
         Zeitpunkt = as.factor(Zeitpunkt),
         Treatment = as.factor(Treatment))


print(anova_puls)

table(pulse_long$Treatment)

table(pulse_long$Treatment, pulse_long$Zeitpunkt)

pulse_long <- pulse_long %>%
  mutate(ID = as.factor(ID),
         Treatment = as.factor(Treatment),
         Zeitpunkt = as.factor(Zeitpunkt))

pulse_long %>%
  group_by(ID, Zeitpunkt) %>%
  summarise(NAs = sum(is.na(Puls)), .groups = "drop") %>%
  filter(NAs > 0)

pulse_long %>%
  filter(is.na(Puls))

pulse_long_clean <- pulse_long %>%
  group_by(ID) %>%
  filter(!any(is.na(Puls))) %>%
  ungroup()

pulse_long_clean <- pulse_long_clean %>%
  mutate(ID = as.factor(ID),
         Treatment = as.factor(Treatment),
         Zeitpunkt = as.factor(Zeitpunkt))

anova_puls <- ezANOVA(
  data = pulse_long_clean,
  dv = Puls,
  wid = ID,
  within = .(Zeitpunkt),
  between = .(Treatment),
  type = 3,
  detailed = TRUE
)

library(tidyverse)
library(ez)

# Systole
sys_long <- gesamt1 %>%
  select(ID, Treatment, Sys_Baseline, Sys_Interview) %>%
  pivot_longer(cols = c(Sys_Baseline, Sys_Interview),
               names_to = "Zeitpunkt",
               values_to = "Systole") %>%
  mutate(Zeitpunkt = factor(Zeitpunkt, levels = c("Sys_Baseline", "Sys_Interview")),
         ID = as.factor(ID),
         Treatment = as.factor(Treatment))

# Diastole
dia_long <- gesamt1 %>%
  select(ID, Treatment, Dia_Baseline, Dia_Interview) %>%
  pivot_longer(cols = c(Dia_Baseline, Dia_Interview),
               names_to = "Zeitpunkt",
               values_to = "Diastole") %>%
  mutate(Zeitpunkt = factor(Zeitpunkt, levels = c("Dia_Baseline", "Dia_Interview")),
         ID = as.factor(ID),
         Treatment = as.factor(Treatment))

sys_long <- sys_long %>%
  filter(!is.na(Systole), !is.na(Treatment), !is.na(ID))

dia_long <- dia_long %>%
  filter(!is.na(Diastole), !is.na(Treatment), !is.na(ID))

# Für deine ANOVA-Datenframes (z.B. sys_long oder dia_long)

sys_long <- sys_long %>%
  mutate(
    ID = as.factor(ID),
    Treatment = as.factor(Treatment),
    Zeitpunkt = as.factor(Zeitpunkt)
  )

dia_long <- dia_long %>%
  mutate(
    ID = as.factor(ID),
    Treatment = as.factor(Treatment),
    Zeitpunkt = as.factor(Zeitpunkt)
  )

# Systole
anova_sys <- ezANOVA(
  data = sys_long,
  dv = Systole,
  wid = ID,
  within = .(Zeitpunkt),
  between = .(Treatment),
  type = 3,
  detailed = TRUE
)


# Diastole
anova_dia <- ezANOVA(
  data = dia_long,
  dv = Diastole,
  wid = ID,
  within = .(Zeitpunkt),
  between = .(Treatment),
  type = 3,
  detailed = TRUE
)
print(anova_sys)
print(anova_dia)


zeitpunkte <- c("Baseline", "PostTSM", "PostTSST", "Interview", "Enc1", "Arithm", "Enc2", "PostDMS")

library(dplyr)

# Anwesenheitsmatrix
library(dplyr)

# Definiere die Zeitpunkte
zeitpunkte <- c("Baseline", "PostTSM", "PostTSST", "Interview", "Enc1", "Arithm", "Enc2", "PostDMS")

# Neue Übersichtstabelle: 1 = Daten vorhanden (mind. einer von Puls, Sys, Dia), 0 = alle NA
overview <- gesamt1 %>%
  mutate(across(everything(), ~ ifelse(is.nan(.x), NA, .x))) %>% # falls NaN statt NA vorkommt
  transmute(
    ID,
    Treatment,
    !!!setNames(
      lapply(zeitpunkte, function(tp) {
        rowSums(select(., starts_with(paste0("Pulse_", tp)),
                       starts_with(paste0("Sys_", tp)),
                       starts_with(paste0("Dia_", tp))), na.rm = TRUE) > 0
      }),
      paste0("has_", zeitpunkte)
    )
  )
colSums(select(overview, starts_with("has_")))

overview %>%
  group_by(Treatment) %>%
  summarise(across(starts_with("has_"), sum), .groups = "drop")

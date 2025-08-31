# Einlesen
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(effectsize)
library(purrr)
library(ez)
library(afex)
library(stringr)

library(lmtest); library(sandwich)
library(emmeans)


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

# Alle RRS-Spalten numerisch umcodieren  und Gesamt-Score berechnen
fragebogen_clean <- fragebogen_clean %>%
  mutate(across(starts_with("RRS_"), ~ as.numeric(recode(.x, !!!antwort_coding)))) %>%
  mutate(RRS_Score = rowSums(across(starts_with("RRS_")), na.rm = TRUE)) %>%
  select(ID, Geschlecht, Alter, RRS_Score)

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
    if_any(starts_with("Baseline_"), ~ !is.na(.)) |
      if_any(starts_with("PostTSST_"), ~ !is.na(.))
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

#Conditions 
con <- read_excel("cond_overview.xlsx")

# IDs normalisieren und vorbereiten
normalize_id <- function(x) {
  x %>% str_trim() %>% str_to_upper()
}

gesamt1 <- gesamt1 %>%
  mutate(ID = as.character(ID)) %>%
  mutate(ID = normalize_id(ID))

con <- con %>%
  mutate(ID = as.character(ID)) %>%
  filter(!is.na(ID)) %>%
  filter(rowSums(is.na(.)) < ncol(.)) %>%
  filter(grepl("^ID", ID)) %>%
  mutate(ID = normalize_id(ID))

# Auf nicht gematchte IDs prüfen
unmatched_ids <- anti_join(gesamt1, con, by = "ID")
if (nrow(unmatched_ids) > 0) {
  message("Nicht gematchte IDs: ", paste(unmatched_ids$ID, collapse = ", "))
}

gesamt1 <- left_join(gesamt1, select(con, ID, Condition), by = "ID") %>%
  mutate(
    Treatment = as.numeric(substr(Condition, 1, 1)),
    TMS = substr(Condition, 2, 2)
  ) %>%
  select(-Condition)



#Wurde RSS gut aufgeteilt? Unterscheiden sich die Mittelwerte signifikant?
t.test(RRS_Score ~ Treatment, data = gesamt1)

# VAS_1: z.B. "Wie gestresst fühlst du dich?"
t.test(VAS_1 ~ Treatment, data = gesamt1)

# VAS_2: z.B. "Wie angespannt fühlst du dich?"
t.test(VAS_2 ~ Treatment, data = gesamt1)

# VAS_3: z.B. "Wie nervös fühlst du dich?"
t.test(VAS_3 ~ Treatment, data = gesamt1)


# Anova  Manipulation check Puls 


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
pulse_long %>%
  filter(!is.na(Puls)) %>%
  group_by(Zeitpunkt, Treatment) %>%
  summarise(n = n_distinct(ID), .groups = "drop") %>%
  pivot_wider(names_from = Treatment, values_from = n,
              names_prefix = "Treatment_")


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
print(anova_puls)



names(gesamt1)



# 1) Stress-Mittelwert pro VP bilden (über die gewünschten Stress-Phasen)
stress_vars <- c("Pulse_PostTSST", "Pulse_Interview", 
                 "Pulse_Enc1", "Pulse_Arithm", "Pulse_Enc2")

dat_pulse_agg <- gesamt1 %>%
  mutate(
    # Anzahl vorhandener Stress-Zeitpunkte (optional für QC)
    n_stress_available = rowSums(!is.na(across(all_of(stress_vars)))),
    # Mittelwert über Stress-Zeitpunkte (mind. 1 Wert nötig)
    Pulse_StressMean = rowMeans(across(all_of(stress_vars)), na.rm = TRUE)
  )

# 2) Long-Format: Baseline vs. StressMean
pulse_agg_long <- dat_pulse_agg %>%
  select(ID, Treatment, Pulse_Baseline, Pulse_StressMean, n_stress_available) %>%
  pivot_longer(
    cols = c(Pulse_Baseline, Pulse_StressMean),
    names_to = "Zeitpunkt",
    values_to = "Puls"
  ) %>%
  mutate(
    Zeitpunkt = recode(Zeitpunkt,
                       "Pulse_Baseline"   = "Baseline",
                       "Pulse_StressMean" = "StressMean"),
    ID        = as.factor(ID),
    Treatment = as.factor(Treatment),
    Zeitpunkt = factor(Zeitpunkt, levels = c("Baseline", "StressMean"))
  )

# 3) (Optional) Übersicht: Stichprobenzahlen je ZP x Treatment (ohne NAs in Puls)
pulse_agg_long %>%
  filter(!is.na(Puls)) %>%
  group_by(Zeitpunkt, Treatment) %>%
  summarise(n = n_distinct(ID), .groups = "drop") %>%
  pivot_wider(names_from = Treatment, values_from = n, names_prefix = "Treatment_")

# 4) Nur VP behalten, die für beide Zeitpunkte (Baseline & StressMean) Werte haben
pulse_agg_long_clean <- pulse_agg_long %>%
  group_by(ID) %>%
  filter(!any(is.na(Puls))) %>%
  ungroup()

# 5) Mixed ANOVA: within = Zeitpunkt (Baseline vs. StressMean), between = Treatment
anova_pulse_agg <- ezANOVA(
  data = pulse_agg_long_clean,
  dv   = Puls,
  wid  = ID,
  within  = .(Zeitpunkt),
  between = .(Treatment),
  type = 3,
  detailed = TRUE
)

print(anova_pulse_agg)


#Zeitpunkt   1  41 1.406694e+02  596.3087 9.671911e+00 3.397994e-03     * 0.0148621183
#Treatment:Zeitpunkt   1  41 1.069578e+02  596.3087 7.354023e+00 9.732437e-03     * 0.0113407824


########################### Systole

# 1) Stress-Mittelwert pro VP bilden (über die gewünschten Stress-Phasen)
systole_vars <- c("Sys_PostTSST", "Sys_Interview", 
                 "Sys_Enc1", "Sys_Arithm", "Sys_Enc2")

dat_sys_agg <- gesamt1 %>%
  mutate(
    # Anzahl vorhandener Stress-Zeitpunkte (optional für QC)
    n_stress_available = rowSums(!is.na(across(all_of(systole_vars)))),
    # Mittelwert über Stress-Zeitpunkte (mind. 1 Wert nötig)
    Sys_StressMean = rowMeans(across(all_of(systole_vars)), na.rm = TRUE)
  )


# 2) Long-Format: Baseline vs. StressMean
systole_agg_long <- dat_sys_agg %>%
  select(ID, Treatment, Sys_Baseline, Sys_StressMean, n_stress_available) %>%
  pivot_longer(
    cols = c(Sys_Baseline, Sys_StressMean),
    names_to = "Zeitpunkt",
    values_to = "Systole"
  ) %>%
  mutate(
    Zeitpunkt = recode(Zeitpunkt,
                       "Sys_Baseline"   = "Baseline",
                       "Sys_StressMean" = "StressMean"),
    ID        = as.factor(ID),
    Treatment = as.factor(Treatment),
    Zeitpunkt = factor(Zeitpunkt, levels = c("Baseline", "StressMean"))
  )

# Nur VP behalten, die für beide Zeitpunkte (Baseline & StressMean) Werte haben
systole_agg_long_clean <- systole_agg_long %>%
  group_by(ID) %>%
  filter(!any(is.na(Systole))) %>%
  ungroup()

# Mixed ANOVA
anova_systole_agg <- ezANOVA(
  data = systole_agg_long_clean,
  dv   = Systole,
  wid  = ID,
  within  = .(Zeitpunkt),
  between = .(Treatment),
  type = 3,
  detailed = TRUE
)

print(anova_systole_agg)

###############################  Dystole


# Stress-Phasen definieren (Diastole)
dia_vars <- c("Dia_PostTSST", "Dia_Interview", 
              "Dia_Enc1", "Dia_Arithm", "Dia_Enc2")

dat_dia_agg <- gesamt1 %>%
  mutate(
    n_stress_available = rowSums(!is.na(across(all_of(dia_vars)))),
    Dia_StressMean = rowMeans(across(all_of(dia_vars)), na.rm = TRUE)
  )

# Long-Format: Baseline vs. StressMean
dia_agg_long <- dat_dia_agg %>%
  select(ID, Treatment, Dia_Baseline, Dia_StressMean, n_stress_available) %>%
  pivot_longer(
    cols = c(Dia_Baseline, Dia_StressMean),
    names_to = "Zeitpunkt",
    values_to = "Diastole"
  ) %>%
  mutate(
    Zeitpunkt = recode(Zeitpunkt,
                       "Dia_Baseline"   = "Baseline",
                       "Dia_StressMean" = "StressMean"),
    ID        = as.factor(ID),
    Treatment = as.factor(Treatment),
    Zeitpunkt = factor(Zeitpunkt, levels = c("Baseline", "StressMean"))
  )

# Nur VP mit vollständigen Daten behalten
dia_agg_long_clean <- dia_agg_long %>%
  group_by(ID) %>%
  filter(!any(is.na(Diastole))) %>%
  ungroup()

# Mixed ANOVA: within = Zeitpunkt, between = Treatment
anova_dia_agg <- ezANOVA(
  data = dia_agg_long_clean,
  dv   = Diastole,
  wid  = ID,
  within  = .(Zeitpunkt),
  between = .(Treatment),
  type = 3,
  detailed = TRUE
)

print(anova_dia_agg)

library(dplyr)
library(tidyr)

# Optional: Welche IDs haben fehlendes Treatment?
pulse_agg_long_clean %>%
  filter(is.na(Treatment)) %>%
  distinct(ID) %>% arrange(ID)

# Helper-Funktion: fasst M ± SD (n) pro Treatment x Zeitpunkt zusammen und pivotiert wide
make_summary_table <- function(df, value_col) {
  df %>%
    filter(!is.na(Treatment)) %>%                                 # NA-Treatment raus
    mutate(Treatment = dplyr::recode_factor(as.character(Treatment),
                                            `0` = "Kontrolle",
                                            `1` = "Stress")) %>%   # Labels
    group_by(Treatment, Zeitpunkt) %>%
    summarise(
      M  = mean(.data[[value_col]], na.rm = TRUE),
      SD = sd(  .data[[value_col]], na.rm = TRUE),
      n  = dplyr::n_distinct(ID),
      .groups = "drop"
    ) %>%
    mutate(stat = sprintf("%.1f ± %.1f (n=%d)", M, SD, n)) %>%
    select(Treatment, Zeitpunkt, stat) %>%
    pivot_wider(names_from = Zeitpunkt, values_from = stat) %>%
    arrange(Treatment)
}

# Tabellen bauen
pulse_table    <- make_summary_table(pulse_agg_long_clean,   "Puls")
systole_table  <- make_summary_table(systole_agg_long_clean, "Systole")
dia_table      <- make_summary_table(dia_agg_long_clean,     "Diastole")

# Anzeigen
pulse_table
systole_table
dia_table


######################################## H1 
#Trait-Rumination sagt das subjektive Stresserleben in der Stressbedingung stärker vorher als in der Kontrollbedingung. 



# 1) Subjektiven Stress bilden (Mittel aus VAS_1..VAS_3)
dat_h1 <- gesamt1 %>%
  mutate(
    SubjStress   = rowMeans(across(c(VAS_1, VAS_2, VAS_3)), na.rm = TRUE),
    Treatment01  = ifelse(Treatment == 1, 1, 0),
    Treatment_f  = factor(Treatment01, levels = c(0,1), labels = c("Kontrolle","Stress")),
    RRS_z        = as.numeric(scale(RRS_Score, center = TRUE, scale = TRUE)),
    SubjStress_z = as.numeric(scale(SubjStress,   center = TRUE, scale = TRUE))
  ) %>%
  filter(!is.na(Treatment_f), !is.na(RRS_z), !is.na(SubjStress_z))

# 2) Modell: Interaktion Rumination × Treatment
m_h1 <- lm(SubjStress_z ~ RRS_z * Treatment_f, data = dat_h1)

# 3) Robuste Standardfehler (HC3)
lmtest::coeftest(m_h1, vcov = sandwich::vcovHC(m_h1, type = "HC3"))

# 4) Simple Slopes (Steigung von RRS in jeder Gruppe + Unterschied)
emtrends(m_h1, ~ Treatment_f, var = "RRS_z") %>% summary(infer = TRUE)
pairs(emtrends(m_h1, ~ Treatment_f, var = "RRS_z"))

# 5) Plot zur Veranschaulichung
ggplot(dat_h1, aes(RRS_z, SubjStress_z, color = Treatment_f)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Trait-Rumination (z)", y = "Subjektiver Stress (z)", color = "Treatment") +
  theme_minimal()

# H1 mit RRS und Puls

library(dplyr); library(lmtest); library(sandwich); library(emmeans); library(ggplot2)

Pulse_StressMean <- c("Pulse_PostTSST","Pulse_Interview","Pulse_Enc1","Pulse_Arithm","Pulse_Enc2")

dat_pulse <- gesamt1 %>%
  mutate(
    Pulse_StressMean = rowMeans(across(all_of(stress_vars)), na.rm = TRUE),
    DeltaPulse       = Pulse_StressMean - Pulse_Baseline,
    Treatment_f      = factor(ifelse(Treatment==1,1,0), levels=c(0,1), labels=c("Kontrolle","Stress")),
    RRS_z            = as.numeric(scale(RRS_Score)),
    DeltaPulse_z     = as.numeric(scale(DeltaPulse))
  ) %>%
  filter(!is.na(Treatment_f), !is.na(RRS_z), !is.na(DeltaPulse_z))

m_pulse_delta <- lm(DeltaPulse_z ~ RRS_z * Treatment_f, data = dat_pulse)
coeftest(m_pulse_delta, vcov = vcovHC(m_pulse_delta, type = "HC3"))

# Simple slopes (Steigung von RRS in jeder Gruppe + Unterschied)
emtrends(m_pulse_delta, ~ Treatment_f, var = "RRS_z") %>% summary(infer = TRUE)
pairs(emtrends(m_pulse_delta, ~ Treatment_f, var = "RRS_z"))

# Plot
ggplot(dat_pulse, aes(RRS_z, DeltaPulse_z, color = Treatment_f)) +
  geom_point(alpha=.5) + geom_smooth(method="lm", se=TRUE) +
  labs(x="Trait-Rumination (z)", y="ΔPuls (z)", color="Treatment")



# Pulse_StressMean ~ Pulse_Baseline + RRS_z * Treatment_f  (robuste SEs)
m_pulse_ancova <- lm(scale(Pulse_StressMean) ~ scale(Pulse_Baseline) + 
                       scale(RRS_Score) * Treatment_f, data = gesamt1)
lmtest::coeftest(m_pulse_ancova, vcov = sandwich::vcovHC(m_pulse_ancova, type="HC3"))
emmeans::pairs(emmeans::emtrends(m_pulse_ancova, ~ Treatment_f, var="scale(RRS_Score)"))

psych::alpha(gesamt1[, c("VAS_1","VAS_2","VAS_3")])

car::vif(m_h1)            # Multikollinearität
bptest(m_h1)              # Breusch-Pagan (wir nutzen ohnehin HC3)
plot(m_h1, which=1:2)     # Residuen / QQ


car::vif(m_h1, type = "predictor")

dat_h1_ec <- gesamt1 %>%
  mutate(
    SubjStress   = rowMeans(across(c(VAS_1, VAS_2, VAS_3)), na.rm = TRUE),
    RRS_z        = as.numeric(scale(RRS_Score)),
    Treatment_c  = ifelse(Treatment == 1,  0.5, -0.5),   # -0.5 = Kontrolle, +0.5 = Stress
    SubjStress_z = as.numeric(scale(SubjStress))
  ) %>%
  filter(!is.na(Treatment_c), !is.na(RRS_z), !is.na(SubjStress_z))

m_h1_ec <- lm(SubjStress_z ~ RRS_z * Treatment_c, data = dat_h1_ec)

car::vif(m_h1_ec, type = "predictor")              # VIF neu checken
lmtest::coeftest(m_h1_ec, vcov = sandwich::vcovHC(m_h1_ec, type="HC3"))

plot(m_h1, which = 4)                      # Cook's distance
which.max(cooks.distance(m_h1))            # auffälligster Fall
car::outlierTest(m_h1)                     # Bonferroni Outlier Test (vorsichtig interpretieren)

MASS::rlm(SubjStress_z ~ RRS_z * Treatment_f, data = dat_h1) %>% summary()



#H2: 
#Trait-Rumination ist mit einem abgeschwächten stimulus priority Effekt verbunden,
#d. h. einer geringeren Differenz in der Erinnerungsleistung zwischen beachteten und unbeachteten Reizen im Wiedererkennungstest.


limesurvey <- read_excel("limesurvey_all_final.xlsx")

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

names(gesamt1)

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


#### Puls_Arithm


# Long-Format für Puls (Baseline vs. Arithm)
pulse_arithm_long <- gesamt1 %>%
  select(ID, Treatment, Pulse_Baseline, Pulse_Arithm) %>%
  pivot_longer(
    cols = c(Pulse_Baseline, Pulse_Arithm),
    names_to = "Zeitpunkt",
    values_to = "Puls"
  ) %>%
  mutate(
    Zeitpunkt = recode(Zeitpunkt,
                       "Pulse_Baseline" = "Baseline",
                       "Pulse_Arithm"   = "Arithm"),
    ID        = as.factor(ID),
    Treatment = as.factor(Treatment),
    Zeitpunkt = factor(Zeitpunkt, levels = c("Baseline", "Arithm"))
  )

# Stichprobenzahlen je Zeitpunkt x Treatment (ohne NAs)
pulse_arithm_long %>%
  filter(!is.na(Puls)) %>%
  group_by(Zeitpunkt, Treatment) %>%
  summarise(n = n_distinct(ID), .groups = "drop") %>%
  pivot_wider(names_from = Treatment, values_from = n, names_prefix = "Treatment_")

# Nur VP mit vollständigen Daten behalten
pulse_arithm_long_clean <- pulse_arithm_long %>%
  group_by(ID) %>%
  filter(!any(is.na(Puls))) %>%
  ungroup()

# Mixed ANOVA (within: Zeitpunkt; between: Treatment)
anova_pulse_arithm <- ezANOVA(
  data    = pulse_arithm_long_clean,
  dv      = Puls,
  wid     = ID,
  within  = .(Zeitpunkt),
  between = .(Treatment),
  type    = 3,
  detailed = TRUE
)

print(anova_pulse_arithm)

#Despreptive Sttatistik
pulse_long_clean %>%
  group_by(Treatment, Zeitpunkt) %>%
  summarise(M = round(mean(Puls, na.rm=TRUE), 1),
            SD = round(sd(Puls, na.rm=TRUE), 1),
            n = dplyr::n_distinct(ID), .groups="drop")
pulse_arithm_long_clean %>%
  group_by(Treatment, Zeitpunkt) %>%
  summarise(M = round(mean(Puls, na.rm=TRUE), 1),
            SD = round(sd(Puls, na.rm=TRUE), 1),
            n = dplyr::n_distinct(ID), .groups="drop")



########################### Systole

# === SYSTOLE: Baseline vs. Interview ===

sys_int_long <- gesamt1 %>%
  select(ID, Treatment, Sys_Baseline, Sys_Interview) %>%
  pivot_longer(
    cols = c(Sys_Baseline, Sys_Interview),
    names_to = "Zeitpunkt",
    values_to = "Systole"
  ) %>%
  mutate(
    Zeitpunkt = recode(Zeitpunkt,
                       "Sys_Baseline"  = "Baseline",
                       "Sys_Interview" = "Interview"),
    ID        = as.factor(ID),
    Treatment = as.factor(Treatment),
    Zeitpunkt = factor(Zeitpunkt, levels = c("Baseline","Interview"))
  )

# (optional) Zelln
sys_int_long %>%
  filter(!is.na(Systole)) %>%
  group_by(Zeitpunkt, Treatment) %>%
  summarise(n = n_distinct(ID), .groups="drop")

# vollständige Fälle
sys_int_long_clean <- sys_int_long %>%
  group_by(ID) %>%
  filter(!any(is.na(Systole))) %>%
  ungroup()

anova_sys_int <- ezANOVA(
  data    = sys_int_long_clean,
  dv      = Systole,
  wid     = ID,
  within  = .(Zeitpunkt),
  between = .(Treatment),
  type    = 3,
  detailed = TRUE
)
print(anova_sys_int)

# Deskriptivwerte
sys_int_long_clean %>%
  group_by(Treatment, Zeitpunkt) %>%
  summarise(M = round(mean(Systole, na.rm=TRUE), 1),
            SD = round(sd(Systole, na.rm=TRUE), 1),
            n = dplyr::n_distinct(ID), .groups="drop")


# 
# === SYSTOLE: Baseline vs. Arithm ===

sys_ari_long <- gesamt1 %>%
  select(ID, Treatment, Sys_Baseline, Sys_Arithm) %>%
  pivot_longer(
    cols = c(Sys_Baseline, Sys_Arithm),
    names_to = "Zeitpunkt",
    values_to = "Systole"
  ) %>%
  mutate(
    Zeitpunkt = recode(Zeitpunkt,
                       "Sys_Baseline" = "Baseline",
                       "Sys_Arithm"   = "Arithm"),
    ID        = as.factor(ID),
    Treatment = as.factor(Treatment),
    Zeitpunkt = factor(Zeitpunkt, levels = c("Baseline","Arithm"))
  )

# (optional) Zelln
sys_ari_long %>%
  filter(!is.na(Systole)) %>%
  group_by(Zeitpunkt, Treatment) %>%
  summarise(n = n_distinct(ID), .groups="drop")

# vollständige Fälle
sys_ari_long_clean <- sys_ari_long %>%
  group_by(ID) %>%
  filter(!any(is.na(Systole))) %>%
  ungroup()

anova_sys_ari <- ezANOVA(
  data    = sys_ari_long_clean,
  dv      = Systole,
  wid     = ID,
  within  = .(Zeitpunkt),
  between = .(Treatment),
  type    = 3,
  detailed = TRUE
)
print(anova_sys_ari)

# Deskriptivwerte
sys_ari_long_clean %>%
  group_by(Treatment, Zeitpunkt) %>%
  summarise(M = round(mean(Systole, na.rm=TRUE), 1),
            SD = round(sd(Systole, na.rm=TRUE), 1),
            n = dplyr::n_distinct(ID), .groups="drop")
# Dystole 

# === DIASTOLE: Baseline vs. Interview ===


dia_int_long <- gesamt1 %>%
  select(ID, Treatment, Dia_Baseline, Dia_Interview) %>%
  pivot_longer(
    cols = c(Dia_Baseline, Dia_Interview),
    names_to = "Zeitpunkt",
    values_to = "Diastole"
  ) %>%
  mutate(
    Zeitpunkt = recode(Zeitpunkt,
                       "Dia_Baseline"  = "Baseline",
                       "Dia_Interview" = "Interview"),
    ID        = as.factor(ID),
    Treatment = as.factor(Treatment),
    Zeitpunkt = factor(Zeitpunkt, levels = c("Baseline","Interview"))
  )

# vollständige Fälle
dia_int_long_clean <- dia_int_long %>%
  group_by(ID) %>%
  filter(!any(is.na(Diastole))) %>%
  ungroup()

anova_dia_int <- ezANOVA(
  data    = dia_int_long_clean,
  dv      = Diastole,
  wid     = ID,
  within  = .(Zeitpunkt),
  between = .(Treatment),
  type    = 3,
  detailed = TRUE
)
print(anova_dia_int)

# Deskriptivwerte
dia_int_long_clean %>%
  group_by(Treatment, Zeitpunkt) %>%
  summarise(M = round(mean(Diastole, na.rm=TRUE), 1),
            SD = round(sd(Diastole, na.rm=TRUE), 1),
            n = dplyr::n_distinct(ID), .groups="drop")



# === DIASTOLE: Baseline vs. Arithm ===


dia_ari_long <- gesamt1 %>%
  select(ID, Treatment, Dia_Baseline, Dia_Arithm) %>%
  pivot_longer(
    cols = c(Dia_Baseline, Dia_Arithm),
    names_to = "Zeitpunkt",
    values_to = "Diastole"
  ) %>%
  mutate(
    Zeitpunkt = recode(Zeitpunkt,
                       "Dia_Baseline" = "Baseline",
                       "Dia_Arithm"   = "Arithm"),
    ID        = as.factor(ID),
    Treatment = as.factor(Treatment),
    Zeitpunkt = factor(Zeitpunkt, levels = c("Baseline","Arithm"))
  )

# vollständige Fälle
dia_ari_long_clean <- dia_ari_long %>%
  group_by(ID) %>%
  filter(!any(is.na(Diastole))) %>%
  ungroup()

anova_dia_ari <- ezANOVA(
  data    = dia_ari_long_clean,
  dv      = Diastole,
  wid     = ID,
  within  = .(Zeitpunkt),
  between = .(Treatment),
  type    = 3,
  detailed = TRUE
)
print(anova_dia_ari)

# Deskriptivwerte
dia_ari_long_clean %>%
  group_by(Treatment, Zeitpunkt) %>%
  summarise(M = round(mean(Diastole, na.rm=TRUE), 1),
            SD = round(sd(Diastole, na.rm=TRUE), 1),
            n = dplyr::n_distinct(ID), .groups="drop")




######################################## H1 
#h1: Trait-Rumination sagt das subjektive Stresserleben in der Stressbedingung stärker vorher als in der Kontrollbedingung. 


library(dplyr)
library(lmtest); library(sandwich)
library(emmeans)
library(broom)
library(purrr)
library(ggplot2)

items <- c("VAS_1","VAS_2","VAS_3")

fit_one <- function(item) {
  dat <- gesamt1 %>%
    transmute(
      ID,
      Treatment_f = factor(ifelse(Treatment==1,1,0), c(0,1), c("Kontrolle","Stress")),
      RRS_z       = as.numeric(scale(RRS_Score)),
      DV_z        = as.numeric(scale(.data[[item]]))
    ) %>%
    filter(!is.na(Treatment_f), !is.na(RRS_z), !is.na(DV_z))
  
  m <- lm(DV_z ~ RRS_z * Treatment_f, data = dat)
  V <- sandwich::vcovHC(m, type="HC3")
  
  # Koeffizienten (robust) tidy
  ct <- lmtest::coeftest(m, vcov = V)
  tid <- broom::tidy(ct) %>%
    mutate(item = item)
  
  # Simple slopes für RRS_z je Gruppe
  ss <- emmeans::emtrends(m, ~ Treatment_f, var = "RRS_z") %>%
    summary(infer = TRUE) %>%
    as.data.frame() %>%
    transmute(item = item,
              Treatment_f,
              slope = RRS_z.trend, SE = SE, df = df,
              t = t.ratio, p = p.value,
              lower = lower.CL, upper = upper.CL)
  
  list(model = m, coefs = tid, slopes = ss)
}

res <- map(items, fit_one)

# 1) Tabelle: Interaktion pro Item (HC3-robust)
tab_int <- map_dfr(res, ~ .x$coefs) %>%
  filter(term == "RRS_z:Treatment_fStress") %>%
  transmute(
    Item = item,
    b = estimate, SE = std.error, t = statistic, p = p.value
  ) %>%
  arrange(Item) %>%
  mutate(p_BH = p.adjust(p, method = "BH"))

tab_int

# 2) Tabelle: Simple Slopes je Item & Gruppe
tab_slopes <- map_dfr(res, ~ .x$slopes)
tab_slopes

# 3) (optional) hübscher Plot je Item
dat_plot <- gesamt1 %>%
  mutate(
    Treatment_f = factor(ifelse(Treatment==1,1,0), c(0,1), c("Kontrolle","Stress")),
    RRS_z       = as.numeric(scale(RRS_Score)),
    VAS_1_z = as.numeric(scale(VAS_1)),
    VAS_2_z = as.numeric(scale(VAS_2)),
    VAS_3_z = as.numeric(scale(VAS_3))
  ) %>%
  tidyr::pivot_longer(cols = c(VAS_1_z, VAS_2_z, VAS_3_z),
                      names_to = "Item", values_to = "DV_z")

ggplot(dat_plot, aes(RRS_z, DV_z, color = Treatment_f)) +
  geom_point(alpha=.4) +
  geom_smooth(method="lm", se=TRUE) +
  facet_wrap(~ Item, nrow = 1) +
  labs(x="Trait-Rumination (z)", y="VAS (z)", color="Treatment") +
  theme_minimal()








library(sandwich); library(lmtest)

dat_long <- gesamt1 %>%
  transmute(
    ID,
    Treatment_f = factor(ifelse(Treatment==1,1,0), c(0,1), c("Kontrolle","Stress")),
    RRS_z       = as.numeric(scale(RRS_Score)),
    VAS_1, VAS_2, VAS_3
  ) %>%
  tidyr::pivot_longer(cols = c(VAS_1, VAS_2, VAS_3),
                      names_to = "Item", values_to = "VAS") %>%
  filter(!is.na(VAS)) %>%
  mutate(VAS_z = as.numeric(scale(VAS)),
         Item  = factor(Item))

m_long <- lm(VAS_z ~ RRS_z * Treatment_f * Item, data = dat_long)

# Cluster-robuste Kovarianzmatrix (ID)
V_cl <- sandwich::vcovCL(m_long, cluster = ~ ID, type = "HC3")

# Gesamt-ANOVA artig (Waldtest) für alle Terme mit robusten SEs:
lmtest::coeftest(m_long, vcov = V_cl)

# Omnibus: unterscheidet sich die Moderation über Items?
# -> Teste alle 'RRS_z:Treatment_fStress:Item...' gemeinsam:
car::linearHypothesis(m_long,
                      c("RRS_z:Treatment_fStress:ItemVAS_2 = 0",
                        "RRS_z:Treatment_fStress:ItemVAS_3 = 0"),
                      vcov = V_cl
)


library(dplyr); library(broom); library(sandwich); library(lmtest); library(purrr)

items <- c("VAS_1","VAS_2","VAS_3")

fit_int <- function(item){
  dat <- gesamt1 %>%
    transmute(Treatment_f = factor(ifelse(Treatment==1,1,0), c(0,1), c("Kontrolle","Stress")),
              RRS_z = as.numeric(scale(RRS_Score)),
              DV_z  = as.numeric(scale(.data[[item]]))) %>%
    filter(!is.na(Treatment_f), !is.na(RRS_z), !is.na(DV_z))
  m <- lm(DV_z ~ RRS_z * Treatment_f, data = dat)
  ct <- lmtest::coeftest(m, vcov = sandwich::vcovHC(m, type="HC3")) |> broom::tidy()
  ct |> mutate(item = item) |> filter(term == "RRS_z:Treatment_fStress")
}

tab_int <- map_dfr(items, fit_int) %>%
  transmute(Item = item, b = estimate, SE = std.error, t = statistic, p = p.value) %>%
  mutate(
    p_bonf = p.adjust(p, method = "bonferroni"),
    p_holm = p.adjust(p, method = "holm"),
    p_bh   = p.adjust(p, method = "BH"),
    alpha_bonf = 0.05/3
  )
tab_int



ggplot(
  dat_plot %>% filter(!is.na(Treatment_f)), 
  aes(RRS_z, DV_z, color = Treatment_f)
) +
  geom_point(alpha = .4) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ Item, nrow = 1) +
  labs(x = "Trait-Rumination (z)", y = "VAS (z)", color = "Treatment") +
  theme_minimal()





#H2: 
#Trait-Rumination ist mit einem abgeschwächten stimulus priority Effekt verbunden,
#d. h. einer geringeren Differenz in der Erinnerungsleistung zwischen beachteten und unbeachteten Reizen im Wiedererkennungstest.


limesurvey <- read_excel("limesurvey_all_final.xlsx")

overview <- read_excel("overview_srt.xlsx")


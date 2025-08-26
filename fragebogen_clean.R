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

limesurvey <- read_excel("limesurvey_all_final.xlsx")

# ID, Geschlecht, Alter, RRS-Items
fragebogen_clean <- limesurvey %>%
  select(8, 9, 10, 131:152)  

# Spalten umbenennen
names(fragebogen_clean)[1:3] <- c("ID", "Geschlecht", "Alter")

#Erste Spalte weg
fragebogen_clean <- fragebogen_clean[-1, ]

# Neue Codierung für Antwortoptionen
antwort_coding <- c("fast nie" = 1, "manchmal" = 2, "häufig" = 3, "fast immer" = 4)

# Alle 22 RRS-Spalten numerisch umcodieren (Spalten 4 bis 25)
fragebogen_clean[, 4:25] <- lapply(fragebogen_clean[, 4:25], function(x) {
  as.numeric(recode(x, !!!antwort_coding))
})

# Gesamt-RRS-Score berechnen
fragebogen_clean <- fragebogen_clean %>%
  mutate(RRS_Score = rowSums(select(., 4:25), na.rm = TRUE))

# Check: erste Item-Spalte
table(fragebogen_clean[[4]])  
# Check: Scores insgesamt
summary(fragebogen_clean$RRS_Score)

fragebogen_clean %>% filter(RRS_Score == 0)
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
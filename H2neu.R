
#H2: 
#Trait-Rumination ist mit einem abgeschwächten stimulus priority Effekt verbunden,
#d. h. einer geringeren Differenz in der Erinnerungsleistung zwischen beachteten und unbeachteten Reizen im Wiedererkennungstest.


pfad <- "overview_srt.csv"
overview <- read_excel("overview_srt.xlsx")



#1. Einlesen & Spaltennamen säuber

library(readr); library(janitor)

srt <- readr::read_delim(
  file   = pfad,
  delim  = ",",
  na     = c("", "NA", "None"),   # <- hier!
  trim_ws = TRUE,
  guess_max = 10000,
  locale = readr::locale(encoding = "UTF-8", decimal_mark = "."),
  show_col_types = FALSE
) |> janitor::clean_names()


srt$rt  <- as.numeric(srt$rt)
srt$ans <- as.numeric(srt$ans)


# ---- 2) Trials aufräumen + Outcomes bauen ----
# Robust: nimm 'ID' falls vorhanden, sonst 'id'
library(dplyr); library(stringr)

# Helper einmal definieren (falls noch nicht geschehen):
normalize_id <- function(x) paste0("ID", sprintf("%03d", as.integer(readr::parse_number(as.character(x)))))

id_source <- if ("ID" %in% names(srt)) "ID" else if ("id" %in% names(srt)) "id" else NA_character_
if (is.na(id_source)) stop("Keine ID-Spalte gefunden (erwarte 'ID' oder 'id').")

srt_clean <- srt %>%
  mutate(
    ID   = normalize_id(.data[[id_source]]),
    type = tolower(trimws(type)),
    resp = tolower(trimws(ans_binarize)),
    # rt/ans sind jetzt numerisch (s. Fix oben); falls nicht:
    rt   = as.numeric(rt),
    ans  = as.numeric(ans)
  ) %>%
  filter(type %in% c("old","new"), resp %in% c("old","new")) %>%
  mutate(
    outcome = case_when(
      type=="old" & resp=="old" ~ "hit",
      type=="old" & resp=="new" ~ "miss",
      type=="new" & resp=="old" ~ "false_alarm",
      type=="new" & resp=="new" ~ "correct_rejection",
      TRUE ~ NA_character_
    ),
    outcome = factor(outcome, levels = c("hit","miss","false_alarm","correct_rejection")),
    is_hit = outcome == "hit",
    is_fa  = outcome == "false_alarm"
  )

sum(is.na(srt$rt))          # viele NAs? ok, „None“ wurde zu NA
readr::problems(srt)        # sollte jetzt leer sein
srt_clean %>% count(ID)     # Trials pro Person


#3.Quality-Check: Konfusionsmatrix pro Person

confusion_id <- srt_clean %>%
  count(id, outcome, name="n") %>%
  tidyr::complete(id, outcome, fill = list(n = 0)) %>%
  tidyr::pivot_wider(names_from = outcome, values_from = n)


#4.Signal-Detection: d′, c, RT pro ID

library(dplyr); library(stringr); library(tidyr)

# Welche Kategorien gibt es überhaupt?
srt_clean %>% mutate(category = tolower(trimws(category))) %>%
  count(category)

# Wie verteilen sich Kategorien pro ID?
srt_clean %>% mutate(category = tolower(trimws(category))) %>%
  count(ID, category) %>% arrange(ID, desc(n))


# vereinheitlichte Kategorie (z.B. beide Scene-Varianten zu "scene")
canonize_cat <- function(x){
  x <- tolower(trimws(x))
  x <- str_replace_all(x, "^scene.*", "scene")      # "scenein"/"sceneout" -> "scene"
  x <- str_replace_all(x, "stationary", "stationery")
  x
}

srt_clean <- srt_clean %>%
  mutate(category_clean = canonize_cat(category))

# Beispiel: eingelesener Lookup
# att_lookup <- readr::read_csv("att_lookup_singlecol_filled.csv", show_col_types = FALSE)

att_lookup <- att_lookup %>%
  mutate(
    ID                 = as.character(ID),
    attended_category  = canonize_cat(attended_category)
  )

# Fehlende IDs im Lookup?
srt_clean %>% distinct(ID) %>%
  anti_join(att_lookup %>% distinct(ID), by = "ID")




srt_att <- srt_clean %>%
  left_join(att_lookup, by = "ID") %>%
  mutate(
    att = case_when(
      is.na(attended_category) ~ NA_character_,                              
      
      # keine Zuordnung im Lookup
      category_clean == attended_category ~ "att",
      TRUE ~ "unatt"
    )
  )

# IDs ohne Zuordnung (att = NA) – die fehlen dir später:
srt_att %>% filter(is.na(att)) %>% distinct(ID)

library(dplyr)
library(stringr)
library(readr)
library(tidyr)

# 0) Kategorien vereinheitlichen (wichtig!)
canonize_cat <- function(x){
  x <- tolower(trimws(x))
  x <- str_replace_all(x, "^scene.*", "scene")      # scenein/sceneout -> scene
  x <- str_replace_all(x, "stationary", "stationery")
  x
}
srt_clean <- srt_clean %>% mutate(category_clean = canonize_cat(category))

# 1) Überblick: Welche Kategorien gibt es?
unique(srt_clean$category_clean)
# -> diese Schreibweise später im Lookup verwenden

# 2) Template für *alle* IDs erzeugen (mit Hilfsspalten zur Orientierung)
cats_by_id <- srt_clean %>%
  count(ID, category_clean, name = "n") %>%
  arrange(ID, desc(n)) %>%
  group_by(ID) %>%
  summarise(
    cats_with_counts = paste0(category_clean, " (n=", n, ")", collapse = " | "),
    top1 = dplyr::first(category_clean),
    top2 = dplyr::nth(category_clean, 2),
    .groups = "drop"
  )

att_lookup_template <- srt_clean %>%
  distinct(ID) %>%
  left_join(cats_by_id, by = "ID") %>%
  mutate(
    # Hier trägst du später die(n) attended-Kategorie(n) ein:
    att_cat1 = NA_character_,
    att_cat2 = NA_character_
  ) %>%
  arrange(ID)

# 3) Template speichern – in Excel/CSV ausfüllen (att_cat1/att_cat2)
write_csv(att_lookup_template, "att_lookup_twocols_TEMPLATE.csv")


library(readxl)
library(dplyr)
library(janitor)
library(readr)
library(stringr)
library(tidyr)

library(dplyr)
library(readr)
library(stringr)
library(janitor)
library(tidyr)

library(dplyr)
library(readr)
library(stringr)
library(janitor)
library(tidyr)

# Helpers
normalize_id <- function(x) paste0("ID", sprintf("%03d", as.integer(readr::parse_number(as.character(x)))))
canonize_cat <- function(x){
  x <- tolower(trimws(x))
  x <- str_replace_all(x, "^scene.*", "scene")    # scenein/sceneout -> scene
  x <- str_replace_all(x, "^fruits?$", "fruit")   # fruit/fruits -> fruit
  x <- str_replace_all(x, "^tools?$",  "tools")   # tool/tools   -> tools
  x <- str_replace_all(x, "stationary", "stationery")
  x
}

# --- 1) con (cond_overview.xlsx) säubern & eindeutigen Lookup bauen ---
# con ist schon eingelesen: con <- readxl::read_excel("cond_overview.xlsx") |> janitor::clean_names()

# Spaltennamen festlegen (bei dir heißen sie wohl so):
id_col  <- "id"
att_col <- "target_category"

con_clean <- con %>%
  mutate(
    id_num = readr::parse_number(as.character(.data[[id_col]])),  # zieht Zahl; "Total" -> NA
    ID     = normalize_id(id_num),
    attended_category = canonize_cat(.data[[att_col]])
  ) %>%
  filter(!is.na(id_num), !is.na(attended_category), attended_category != "")

# Diagnose: gibt es IDs mehrfach? (=> hier steckt die many-to-many-Ursache)
dups <- con_clean %>% count(ID) %>% filter(n > 1)
dups  # sollte leer sein; falls nicht, entscheiden wir, welche Zeile gilt (meist sind sie identisch)

# Falls doch mehrfach vorkommen: nimm die häufigste Kategorie je ID (oder die erste nicht-NA)
att_lookup <- con_clean %>%
  count(ID, attended_category, name = "freq") %>%
  slice_max(freq, by = ID, n = 1, with_ties = FALSE) %>%  # eine Zeile pro ID
  select(ID, attended_category)

# Sicherheitscheck: jetzt wirklich 1 Zeile pro ID?
stopifnot(all(att_lookup %>% count(ID) %>% pull(n) == 1))

# --- 2) Recog-Trials mappen (many-to-one Join) ---
srt_att <- srt_clean %>%
  mutate(category_clean = canonize_cat(category)) %>%
  left_join(att_lookup, by = "ID", relationship = "many-to-one") %>%  # dplyr >= 1.1
  mutate(
    att = case_when(
      is.na(attended_category) ~ NA_character_,
      category_clean == attended_category ~ "att",
      TRUE ~ "unatt"
    )
  )

# IDs ohne Zuordnung (fehlen in con oder target_category leer):
srt_att %>% filter(is.na(att)) %>% distinct(ID)

sd_att <- srt_att %>%
  filter(!is.na(att)) %>%
  group_by(ID, att) %>%
  summarise(
    n_old = sum(type=="old"), n_new = sum(type=="new"),
    k_hit = sum(outcome=="hit"), k_fa = sum(outcome=="false_alarm"),
    p_hit = (k_hit + 0.5)/(n_old + 1),
    p_fa  = (k_fa  + 0.5)/(n_new + 1),
    dprime = qnorm(p_hit) - qnorm(p_fa),
    .groups = "drop"
  )

# Jede ID sollte jetzt GENAU 2 Zeilen haben (att & unatt):
sd_att %>% count(ID) %>% filter(n != 2)

library(dplyr); library(tidyr); library(lmtest); library(sandwich); library(ggplot2); library(readr)

# Falls noch nicht geschehen: sd_att existiert (d′ pro ID × att)
# -> SP_dprime pro Person
sp <- sd_att %>%
  select(ID, att, dprime) %>%
  pivot_wider(names_from = att, values_from = dprime) %>%
  filter(!is.na(att), !is.na(unatt)) %>%
  mutate(SP_dprime = att - unatt) %>%
  select(ID, SP_dprime)

# Helper, um IDs im Hauptdatensatz zu normalisieren
normalize_id <- function(x) paste0("ID", sprintf("%03d", as.integer(readr::parse_number(as.character(x)))))

# Merge mit gesamt1 + Prädiktoren vorbereiten
dat_sp <- gesamt1 %>%
  mutate(
    ID          = normalize_id(ID),
    RRS_z       = as.numeric(scale(RRS_Score)),
    Treatment_f = factor(ifelse(Treatment==1,1,0), c(0,1), c("Kontrolle","Stress"))
  ) %>%
  left_join(sp, by = "ID")

# Quick check: wie viele vollständige Fälle?
sum(complete.cases(dat_sp$SP_dprime, dat_sp$RRS_z))

# --- H2: Regression (attentuierter Priority-Effekt = negativer Koeffizient) ---
m_sp <- lm(SP_dprime ~ RRS_z, data = dat_sp)
lmtest::coeftest(m_sp, vcov = sandwich::vcovHC(m_sp, type = "HC3"))

# Optional: nur unter Stress stärker? (Interaktion)
m_sp_int <- lm(SP_dprime ~ RRS_z * Treatment_f, data = dat_sp)
lmtest::coeftest(m_sp_int, vcov = sandwich::vcovHC(m_sp_int, type = "HC3"))

# Optional: generelle Gedächtnisleistung kontrollieren
dprime_mean <- sd_att %>% group_by(ID) %>% summarise(dprime_mean = mean(dprime, na.rm=TRUE), .groups="drop")
dat_sp2 <- dat_sp %>% left_join(dprime_mean, by="ID") %>% mutate(dprime_mean_z = scale(dprime_mean))
m_sp_cov <- lm(SP_dprime ~ RRS_z + dprime_mean_z, data = dat_sp2)
lmtest::coeftest(m_sp_cov, vcov = sandwich::vcovHC(m_sp_cov, type = "HC3"))

# Deskriptiv: SP_d′ nach Treatment
dat_sp %>%
  group_by(Treatment_f) %>%
  summarise(M = mean(SP_dprime, na.rm=TRUE),
            SD = sd(SP_dprime, na.rm=TRUE),
            n = dplyr::n(), .groups="drop")

# Plot (zur Illustration)
ggplot(dat_sp, aes(RRS_z, SP_dprime, color = Treatment_f)) +
  geom_point(alpha=.5) +
  geom_smooth(method="lm", se=TRUE) +
  labs(x="Trait-Rumination (z)", y="Stimulus-Priority (SP d′ = d′att − d′unatt)", color="Treatment") +
  theme_minimal()

library(dplyr)

dat_cells <- dat_sp %>%
  mutate(Rum_grp = if_else(RRS_z <= median(RRS_z, na.rm=TRUE), "niedrig", "hoch")) %>%
  group_by(Treatment_f, Rum_grp) %>%
  summarise(M = mean(SP_dprime, na.rm=TRUE),
            SD = sd(SP_dprime, na.rm=TRUE),
            n = dplyr::n(),
            .groups="drop")
dat_cells

library(dplyr)
library(ggplot2)
library(readr)

# Falls noch nicht vorhanden: Helper wie zuvor
normalize_id <- function(x) paste0("ID", sprintf("%03d", as.integer(readr::parse_number(as.character(x)))))

# --- Fig. A: Within-Person d′ (att vs. unatt), nach Treatment ---
# Treatment an sd_att hängen:
sd_att_treat <- sd_att %>%
  # hole Treatment aus deinem Hauptdatensatz
  left_join(
    gesamt1 %>%
      mutate(ID = normalize_id(ID),
             Treatment_f = factor(ifelse(Treatment==1,1,0),
                                  c(0,1), c("Kontrolle","Stress"))) %>%
      select(ID, Treatment_f),
    by = "ID"
  ) %>%
  mutate(att = factor(att, levels = c("unatt","att")))  # Reihenfolge links→rechts

p1 <- ggplot(sd_att_treat, aes(x = att, y = dprime, color = Treatment_f, group = ID)) +
  # dünne Verbindungs-Linien pro Person (innerhalb-subjektlich)
  geom_line(alpha = 0.25) +
  # Punkte pro Person (blass)
  geom_point(alpha = 0.25, position = position_jitter(width = 0.02, height = 0)) +
  # Gruppenmittel ± SE nach Treatment
  stat_summary(aes(group = Treatment_f),
               fun = mean, geom = "point", size = 3, position = position_dodge(width = 0.3)) +
  stat_summary(aes(group = Treatment_f),
               fun.data = mean_se, geom = "errorbar", width = 0.1,
               position = position_dodge(width = 0.3)) +
  labs(x = "Aufmerksamkeit (Recognition-Bedingung)",
       y = "d′ (Sensitivität)",
       color = "Treatment",
       title = "Fig. A · Stimulus Priority (att > unatt) innerhalb Person") +
  theme_minimal()

# --- Fig. B: H2 – SP_d′ ~ Rumination (z), getrennt nach Treatment ---
p2 <- ggplot(dat_sp, aes(x = RRS_z, y = SP_dprime, color = Treatment_f)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Trait-Rumination (z)",
       y = "Stimulus-Priority-Index (SP d′ = d′att − d′unatt)",
       color = "Treatment",
       title = "Fig. B · H2: SP d′ in Abhängigkeit von Rumination") +
  theme_minimal()

# Anzeigen
p1
p2

# Optional speichern
# ggsave("FigA_Priority_within.png", p1, width = 7, height = 5, dpi = 300)
# ggsave("FigB_SPdprime_vs_RRS.png", p2, width = 7, height = 5, dpi = 300)
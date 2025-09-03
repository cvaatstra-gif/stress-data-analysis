

library(fs)
library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
library(tidyr)
library(readr)

path <- "C:/Users/Rabi/Desktop/R/stress-data-analysis"


#H2: 
#Trait-Rumination ist mit einem abgeschwächten stimulus priority Effekt verbunden,
#d. h. einer geringeren Differenz in der Erinnerungsleistung zwischen beachteten und unbeachteten Reizen im Wiedererkennungstest.


limesurvey <- read_excel("limesurvey_all_final.xlsx")

overview <- read_excel("overview_srt.xlsx")

library(readr)
library(dplyr)
library(janitor)

pfad <- "C:/Users/Rabi/Desktop/R/stress-data-analysis/overview_srt.csv"

library(readr)
library(dplyr)
library(janitor)



# richtig: Komma als Delimiter, Dezimalpunkt
srt <- read_delim(
  file   = pfad,
  delim  = ",",
  na     = c("", "NA"),
  trim_ws = TRUE,
  guess_max = 10000,
  locale = locale(encoding = "UTF-8", decimal_mark = "."),
  show_col_types = FALSE
) |> clean_names()

glimpse(srt)



# 1) Trials aufräumen
srt_clean <- srt %>%
  mutate(
    id  = sprintf("%03d", as.integer(id)),
    type = tolower(type),
    ans_binarize = tolower(ans_binarize),
    # falls ans/rt noch als Text vorliegen:
    ans = suppressWarnings(parse_double(as.character(ans))),
    rt  = suppressWarnings(parse_double(as.character(rt)))
  ) %>%
  filter(type %in% c("old","new"))

# 2) Trial-Level Klassifikation
srt_clean <- srt_clean %>%
  mutate(
    is_hit = type == "old" & ans_binarize == "old",
    is_fa  = type == "new" & ans_binarize == "old"
  )

# 3) Zusammenfassung pro ID
summ_id <- srt_clean %>%
  group_by(id) %>%
  summarise(
    n_old  = sum(type == "old"),
    n_new  = sum(type == "new"),
    k_hit  = sum(is_hit, na.rm = TRUE),
    k_fa   = sum(is_fa,  na.rm = TRUE),
    hit_rate = k_hit / n_old,
    fa_rate  = k_fa  / n_new,
    # log-lineare Korrektur (verhindert 0/1-Randfälle)
    p_hit = (k_hit + 0.5) / (n_old + 1),
    p_fa  = (k_fa  + 0.5) / (n_new + 1),
    dprime = qnorm(p_hit) - qnorm(p_fa),
    rt_mean = mean(rt, na.rm = TRUE),
    .groups = "drop"
  )

# optional: Zusammenfassung pro ID x Kategorie
summ_cat <- srt_clean %>%
  group_by(id, category) %>%
  summarise(
    n_old  = sum(type == "old"),
    n_new  = sum(type == "new"),
    k_hit  = sum(is_hit, na.rm = TRUE),
    k_fa   = sum(is_fa,  na.rm = TRUE),
    p_hit  = (k_hit + 0.5) / (n_old + 1),
    p_fa   = (k_fa  + 0.5) / (n_new + 1),
    dprime = qnorm(p_hit) - qnorm(p_fa),
    rt_mean = mean(rt, na.rm = TRUE),
    .groups = "drop"
  )
library(readr); library(dplyr); library(janitor)


srt <- read_delim(
  file   = pfad,
  delim  = ",",
  na = c("", "NA"),
  trim_ws = TRUE,
  guess_max = 10000,
  locale = locale(encoding = "UTF-8", decimal_mark = "."),
  show_col_types = FALSE
) |> clean_names()

glimpse(srt)   # sollte Spalten wie id, category, type, ans, rt, ans_binarize zeigen

srt_clean <- srt %>%
  mutate(
    id   = sprintf("%03d", as.integer(id)),
    type = tolower(type),
    resp = tolower(ans_binarize)   # bereits binär: "old"/"new"
    # Falls du stattdessen eine 1–5 Skala in 'ans' hast:
    # resp = ifelse(as.numeric(ans) <= 3, "old", "new")
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
    outcome = factor(outcome, levels = c("hit","miss","false_alarm","correct_rejection"))
  )

confusion_id <- srt_clean %>%
  count(id, outcome, name="n") %>%
  tidyr::complete(id, outcome, fill = list(n = 0)) %>%
  tidyr::pivot_wider(names_from = outcome, values_from = n)

sd_id <- srt_clean %>%
  group_by(id) %>%
  summarise(
    n_old  = sum(type=="old"),
    n_new  = sum(type=="new"),
    k_hit  = sum(outcome=="hit"),
    k_fa   = sum(outcome=="false_alarm"),
    # log-lineare Korrektur → verhindert unendliche z-Werte
    p_hit  = (k_hit + 0.5) / (n_old + 1),
    p_fa   = (k_fa  + 0.5) / (n_new + 1),
    dprime = qnorm(p_hit) - qnorm(p_fa),
    c      = -0.5 * (qnorm(p_hit) + qnorm(p_fa)),
    rt_mean = mean(as.numeric(rt), na.rm = TRUE),
    .groups = "drop"
  )

sd_cat <- srt_clean %>%
  group_by(id, category) %>%
  summarise(
    n_old=sum(type=="old"), n_new=sum(type=="new"),
    k_hit=sum(outcome=="hit"), k_fa=sum(outcome=="false_alarm"),
    p_hit=(k_hit+0.5)/(n_old+1), p_fa=(k_fa+0.5)/(n_new+1),
    dprime = qnorm(p_hit) - qnorm(p_fa),
    c      = -0.5 * (qnorm(p_hit) + qnorm(p_fa)),
    rt_mean = mean(as.numeric(rt), na.rm=TRUE),
    .groups="drop"
  )

srt_clean <- srt_clean %>%
  mutate(
    ID_num = as.integer(parse_number(as.character(id))),   # zieht die Ziffern heraus
    ID     = paste0("ID", sprintf("%03d", ID_num))
  ) %>%
  select(-id, -ID_num)   # alte Hilfsspalten weg

sum(is.na(srt_clean$ID))          # sollte 0 sein
srt_clean %>% filter(is.na(ID))   # zeigt problematische Zeilen


library(dplyr)
library(readr)   # parse_number

# 1) Helper: ID auf "ID###" bringen – egal ob "2", "002" oder "ID002"
normalize_id <- function(x) {
  paste0("ID", sprintf("%03d", as.integer(parse_number(as.character(x)))))
}

# 2) IDs in beiden Datensätzen normalisieren
gesamt1_fix <- gesamt1 %>%
  mutate(
    ID = normalize_id(ID),
    Treatment_f = factor(ifelse(Treatment==1,1,0), c(0,1), c("Kontrolle","Stress"))
  )

sd_id_fix <- sd_id %>%
  mutate(
    # nimmt 'ID' falls vorhanden, sonst 'id'
    ID = normalize_id(coalesce(ID, id))
  ) %>%
  select(-id) %>% distinct()

# 3) Join
dat <- gesamt1_fix %>%
  left_join(sd_id_fix, by = "ID")

# 4) Check: welche IDs matchen noch nicht?
anti_join(sd_id_fix %>% distinct(ID), gesamt1_fix %>% distinct(ID), by = "ID")



dat <- gesamt1 %>%
  mutate(ID = sprintf("%03d", as.integer(ID)),
         Treatment_f = factor(ifelse(Treatment==1,1,0), c(0,1), c("Kontrolle","Stress"))) %>%
  left_join(sd_id, by = c("ID" = "id"))

# einfacher Gruppenvergleich d′
t.test(dprime ~ Treatment_f, data = dat)

# oder lineares Modell (robuste SEs empfohlen)
library(lmtest); library(sandwich)
m <- lm(dprime ~ Treatment_f, data = dat)
lmtest::coeftest(m, vcov = sandwich::vcovHC(m, type="HC3"))

glimpse(srt_clean)
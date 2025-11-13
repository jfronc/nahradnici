library(tidyverse)
library(magrittr)

#  stáhneme a extrahujeme data do složky
temp <- tempfile()
download.file("https://www.psp.cz/eknih/cdrom/opendata/poslanci.zip", temp)
file.path("psp/data/poslanci") %>% unzip(temp, exdir = .)
file.remove(temp)

read_psp <- function (file) {
  df <- readr::read_delim(file, # utils::read.table nepřevedla správně některé řádky
                          delim = "|",
                          col_names = FALSE,
                          locale = locale(encoding = "windows-1250"),
                          show_col_types = FALSE) |>
    dplyr::select(!last_col()) # poslední sloupec je plonkový
  return(df)
}

pohlavi <- function (surname) {
  if (grepl("(ová|á$)", surname)) {
    return("Ž")
  } else {
    return("M")
  }  
}

poslanec <- read_psp("psp/data/poslanci/poslanec.unl") # parsing issue refers to phone no.
colnames(poslanec) <- c('mp_id', 'id', 'region_id', 'list_id', 'org_id', 'web', 'street', 'municipality', 'postcode', 'email','phone', 'fax', 'psp_phone', 'facebook', 'photo')

osoby <- read_psp("psp/data/poslanci/osoby.unl")
colnames(osoby) <- c('id', 'title_pre', 'PRIJMENI', 'JMENO', 'title_post', 'narozeni', 'pohlavi', 'zmena', 'umrti')
osoby$narozeni %<>% gsub("(\\d{2})\\.(\\d{2})\\.(\\d{4})", "\\3-\\2-\\1", .) %>% as.Date()

zarazeni <- read_psp("psp/data/poslanci/zarazeni.unl")
colnames(zarazeni) <- c('id', 'org_id', 'cl_funkce', 'since', 'until', 'since_f', 'until_f')
zarazeni %<>% filter(org_id == 174) %>% # PSP10
  left_join(osoby) %>%
  mutate(
    since = as.Date(since),
    until = as.Date(until),
    narozeni = na_if(narozeni, as.Date("1900-01-01")),
    VEK = year(as.period(interval(start = narozeni, end = as.Date("2025-10-04"))))
  )

ppl <- readRDS(here::here("psp/data/ppl.rds"))

symdiff(
  filter(zarazeni, since == "2025-10-04") %>% select(JMENO, PRIJMENI, VEK),
  filter(ppl, !is.na(PORADIMAND)) %>% select(JMENO, PRIJMENI, VEK)
)
  
ppl %<>%
  left_join(select(zarazeni, JMENO, PRIJMENI, VEK, since, until), by = join_by(JMENO, PRIJMENI, VEK)) %>%
  group_by(VOLKRAJ, NAZ_STR) %>%
  filter(any(!is.na(PORADIMAND))) %>%
  mutate(
    state = case_when(
      !is.na(PORADIMAND) & is.na(until) ~ 11, # poslanec od začátku
      since > as.Date("2025-10-04") & is.na(until) ~ 12, # poslanec–náhradník
      row_number() == which(is.na(since))[1] ~ 21, # první náhradník
      !is.na(since) & !is.na(until) ~ 3, # bývalý poslanec
      .default = 0
    ),
    weight = sum(!is.na(PORADIMAND))
  )

if (nrow(filter(ppl, state == 11)) + nrow(filter(ppl, state == 12)) != 200) {
  stop("Poslanců dle 'state' není 200!")
}
filter(ppl, state == 12)

saveRDS(ppl, here::here("psp/data/candidates.rds"))

pctg <- scales::label_percent(
  suffix = " %"
)

nahradnici <- filter(ppl, state == 21) %>%
  mutate(
    pohlavi = pohlavi(PRIJMENI)
  )

nahradnici %>%
  group_by(NAZ_STR) %>%
  summarise(
    total_weight = sum(weight),
    pct_female = pctg(sum(weight[pohlavi == "Ž"]) / total_weight),
    pct_male   = pctg(sum(weight[pohlavi == "M"]) / total_weight),
    .groups = "drop"
  )


library(tidyverse)
library(magrittr)

ppl <- readRDS(here::here("psp/data/ppl.rds"))

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
    dplyr::select(!last_col())
  return(df)
  }

poslanec <- read_psp("psp/data/poslanci/poslanec.unl")
colnames(poslanec) <- c('mp_id', 'id', 'region_id', 'list_id', 'org_id', 'web', 'street', 'municipality', 'postcode', 'email','phone', 'fax', 'psp_phone', 'facebook', 'photo')

osoby <- read_psp("psp/data/poslanci/osoby.unl")
colnames(osoby) <- c('id', 'title_pre', 'PRIJMENI', 'JMENO', 'title_post', 'narozeni', 'pohlavi', 'zmena', 'umrti')
osoby$narozeni %<>% gsub("(\\d{2})\\.(\\d{2})\\.(\\d{4})", "\\3-\\2-\\1", .) %>% as.Date()

zarazeni <- read_psp("psp/data/poslanci/zarazeni.unl")
colnames(zarazeni) <- c('id', 'org_id', 'cl_funkce', 'since', 'until', 'since_f', 'until_f')
zarazeni$since %<>% as.Date()
zarazeni$until %<>% as.Date()
zarazeni %<>% filter(org_id == 174) %>% # PSP10
  left_join(osoby) %>%
  mutate(
    VEK = year(as.period(interval(start = narozeni, end = as.Date("2025-10-04"))))
  )

symdiff(
  select(zarazeni, JMENO, PRIJMENI, VEK),
  filter(ppl, !is.na(PORADIMAND)) %>% select(JMENO, PRIJMENI, VEK)
)
  
ppl %<>% left_join(select(zarazeni, JMENO, PRIJMENI, VEK, since, until), by = join_by(JMENO, PRIJMENI, VEK))

ppl %<>% mutate(
  state = case_when(
    !is.na(PORADIMAND) ~ 1, # poslanec od začátku
    .default = 0 # neposlanec
    )
  )

saveRDS(ppl, here::here("psp/data/candidates.rds"))



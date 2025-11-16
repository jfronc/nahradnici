xfun::pkg_attach("tidyverse", "magrittr", "xml2")

# psrk <- read_xml("https://www.volby.cz/opendata/ps2025/xml/psrk.xml") |> xml_ns_strip()
# vysledky <- read_xml("https://www.volby.cz/appdata/ps2025/odata/vysledky.xml") |> xml_ns_strip()
# psvolkr <- read_xml("https://www.volby.cz/opendata/ps2025/xml/psvolkr.xml") |> xml_ns_strip()
# cpp <- read_xml("https://www.volby.cz/opendata/ps2025/xml/cpp.xml") |> xml_ns_strip()

# saveRDS(as.character(psrk), here::here("psp/data/psrk.rds"))
# saveRDS(as.character(vysledky), here::here("psp/data/vysledky.rds"))
# saveRDS(as.character(psvolkr), here::here("psp/data/psvolkr.rds"))
# saveRDS(as.character(cpp), here::here("psp/data/cpp.rds"))

psrk <- readRDS(here::here("psp/data/psrk.rds")) |> read_xml()
vysledky <- readRDS(here::here("psp/data/vysledky.rds")) |> read_xml()
psvolkr <- readRDS("psp/data/psvolkr.rds") |> read_xml()
cpp <- readRDS("psp/data/cpp.rds") |> read_xml()

# Helper to extract text of child elements as columns within namespace
extract_fields <- function(node, fields) {
  map_chr(fields, ~ xml_text(xml_find_first(node, .x)))
}

partylist <- function(party, kraj, doc) {
  rows <- doc |>
    xml_find_all(paste0("PS_REGKAND_ROW[KSTRANA='", party, "' and VOLKRAJ='", kraj, "']"))
  
  fields <- c("JMENO", "PRIJMENI", "VEK", "POVOLANI", "PSTRANA", "NSTRANA", "PORCISLO", "PORADIMAND", "PORADINAHR", "PLATNOST", "POCHLASU", "POCPROC")
  
  map_df(rows, ~ set_names(extract_fields(.x, fields), fields)) %>%
    mutate(
      VEK = as.integer(VEK), # věk ke druhému dni voleb
      PSTRANA = as.integer(PSTRANA),
      NSTRANA = as.integer(NSTRANA),
      POCHLASU = as.integer(POCHLASU),
      POCPROC = as.double(POCPROC),
      PORADINAHR = as.integer(PORADINAHR) %>% na_if(0),
      PORADIMAND = as.integer(PORADIMAND) %>% na_if(0),
      KSTRANA = party,
      VOLKRAJ = kraj
    ) |>
    arrange(PORADIMAND, PORADINAHR)
}

# partylist(6, 1, psrk)

get_votes <- function (doc) {
  if (!is.na(xml_attr(xml_find_all(doc, "HODNOTY_STRANA"), "MANDATY"))) {
    tibble(
      KSTRANA = xml_attr(doc, "KSTRANA"),
      NAZ_STR = xml_attr(doc, "NAZ_STR"),
      HLASY   = as.integer(xml_attr(xml_find_first(doc, ".//HODNOTY_STRANA"), "HLASY"))
    )
  } else {
    NULL
  }
}

lists <- xml_find_all(vysledky, ".//CR/STRANA") %>%
  map(get_votes) %>%
  list_rbind() %>%
  arrange(desc(HLASY))


fields_k <- c("VOLKRAJ", "NAZVOLKRAJ")
kraje <- xml_find_all(psvolkr, ".//PS_VOLKRAJ_ROW") %>%
  map_df( ~ set_names(extract_fields(.x, fields_k), fields_k)) %>% 
  mutate(
    VOLKRAJ = as.integer(VOLKRAJ)
  )
  
fields_cpp <- c("PSTRANA", "NAZEV_STRP", "ZKRATKAP8")
parties <- xml_find_all(cpp, ".//CPP_ROW") %>%
  map_df( ~ set_names(extract_fields(.x, fields_cpp), fields_cpp)) %>% 
  mutate(
    PSTRANA = as.integer(PSTRANA)
  )

grid <- expand_grid(
  KSTRANA = lists$KSTRANA,
  VOLKRAJ = 1:14
)

ppl <- map2(
  grid$KSTRANA,
  grid$VOLKRAJ,
  ~ partylist(.x, .y, psrk),
  .progress = TRUE
  ) %>% list_rbind()

ppl %<>%
  left_join(select(lists, KSTRANA, NAZ_STR)) %>%
  left_join(kraje)

if (nrow(filter(ppl, !is.na(PORADIMAND))) != 200) {
  stop("PORADIMAND != 200")
  }

saveRDS(ppl, here::here("psp/data/ppl.rds"))
saveRDS(parties, here::here("psp/data/parties.rds"))

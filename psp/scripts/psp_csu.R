xfun::pkg_attach("tidyverse", "magrittr", "xml2")

psrk <- read_xml("https://www.volby.cz/opendata/ps2025/xml/psrk.xml") |> xml_ns_strip()
vysledky <- read_xml("https://www.volby.cz/appdata/ps2025/odata/vysledky.xml") |> xml_ns_strip()

# Helper to extract text of child elements as columns within namespace
extract_fields <- function(node, fields) {
  map_chr(fields, ~ xml_text(xml_find_first(node, .x)))
}

partylist <- function(party, kraj, doc) {
  rows <- doc |>
    xml_find_all(paste0("PS_REGKAND_ROW[KSTRANA='", party, "' and VOLKRAJ='", kraj, "']"))
  
  fields <- c("JMENO", "PRIJMENI", "VEK", "PORADIMAND", "PORADINAHR")
  
  map_df(rows, ~ set_names(extract_fields(.x, fields), fields)) %>%
    mutate(
      VEK = as.integer(VEK), # věk ke druhému dni voleb
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


parties <- xml_find_all(vysledky, ".//CR/STRANA") %>%
  map(get_votes) %>%
  list_rbind() %>%
  arrange(desc(HLASY))


grid <- expand_grid(
  KSTRANA = parties$KSTRANA,
  VOLKRAJ = 1:14
)

ppl <- map2(
  grid$KSTRANA,
  grid$VOLKRAJ,
  ~ partylist(.x, .y, psrk),
  .progress = TRUE
  ) %>% list_rbind()

ppl %<>% left_join(select(parties, KSTRANA, NAZ_STR))

saveRDS(ppl, here::here("psp/data/ppl.rds"))

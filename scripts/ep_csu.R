xfun::pkg_attach("tidyverse", "magrittr", "xml2")

eprk <- read_xml("https://www.volby.cz/opendata/ep2024/xml/eprk.xml") |> xml_ns_strip()
vysledky <- read_xml("https://www.volby.cz/pls/ep2024/vysledky") |> xml_ns_strip()

# Helper to extract text of child elements as columns within namespace
extract_fields <- function(node, fields) {
  map_chr(fields, ~ xml_text(xml_find_first(node, .x)))
}

partylist <- function(party, doc) {
  rows <- doc |>
    xml_find_all(paste0(".//EP_REGKAND_ROW[ESTRANA='", party, "']"))
  
  fields <- c("JMENO", "PRIJMENI", "VEK", "PORADIMAND", "PORADINAHR")
  
  map_df(rows, ~ set_names(extract_fields(.x, fields), fields)) %>%
    mutate(
      VEK = as.integer(VEK),
      PORADINAHR = as.integer(PORADINAHR),
      PORADINAHR = na_if(PORADINAHR, 0),
      PORADIMAND = as.integer(PORADIMAND),
      PORADIMAND = na_if(PORADIMAND, 0)
    ) %>%
    arrange(PORADINAHR) %>%
    arrange(PORADIMAND) %>%
    return()
}

# partylist(17, eprk)

parties <- xml_find_all(eprk, ".//EP_REGKAND_ROW[MANDAT='A']") %>%
  xml_find_all(".//ESTRANA") %>%
  xml_text() %>%
  unique()

parties <- xml_find_all(vysledky, ".//STRANA") %>%
  map_df(~ {
    if (length(xml_find_all(.x, ".//POSLANEC")) > 0) {
      tibble(
        ESTRANA = xml_attr(.x, "ESTRANA"),
        NAZ_STR = xml_attr(.x, "NAZ_STR"),
        HLASY   = as.integer(xml_attr(xml_find_first(.x, ".//HLASY_STRANA"), "HLASY"))
      )
    } else {
      NULL
    }
  }) %>%
  arrange(desc(HLASY))

ppl <- map(set_names(parties$ESTRANA, parties$NAZ_STR),
                  ~partylist(.x, eprk))

ppl %<>% map(~mutate(.x,
  FROM = ifelse(!is.na(PORADIMAND), as.Date("2024-07-15"), NA),
  TO = NA
  ))



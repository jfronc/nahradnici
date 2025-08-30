xfun::pkg_attach("tidyverse", "magrittr", "xml2")

ns <- c(ns = "http://www.volby.cz/ep/")

xml <- "https://www.volby.cz/opendata/ep2024/xml/eprk.xml"
doc <- read_xml(xml) |> xml_ns_strip()

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

# partylist(17, doc)

parties <- xml_find_all(doc, ".//EP_REGKAND_ROW[MANDAT='A']") %>%
  xml_find_all(".//ESTRANA") %>%
  xml_text() %>%
  unique()

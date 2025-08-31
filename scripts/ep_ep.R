xfun::pkg_attach("httr2", "tidyverse", "magrittr")

# helper to fetch meps by status (incoming/outgoing)
fetch_meps <- function(status = c("incoming", "outgoing")) {
  status <- match.arg(status)
  
  url <- glue::glue(
    "https://data.europarl.europa.eu/api/v2/meps/show-{status}?country-of-representation=CZ&format=application%2Fld%2Bjson"
  )
  
  resp <- request(url) |> req_perform() |> resp_body_json()
  
  tibble(
    id         = map_chr(resp$data, "identifier"),
    givenName  = map_chr(resp$data, "givenName"),
    familyName = map_chr(resp$data, "familyName"),
    status     = status
  )
}

# get both incoming + outgoing
meps <- bind_rows(
  fetch_meps("incoming"),
  fetch_meps("outgoing")
)

mep_profile <- function(id) {
  attr <- paste0("https://data.europarl.europa.eu/api/v2/meps/", id,
                 "?format=application%2Frdf%2Bxml") |>
    request() |> req_perform() |> resp_body_string() |>
    read_xml() |>
    xml_find_all(".//org:hasMembership[
                 .//org:organization[@rdf:resource='https://data.europarl.europa.eu/org/ep-10'] and
                 .//org:role[@rdf:resource='https://data.europarl.europa.eu/def/ep-roles/MEMBER']
               ]//org:memberDuring") |>
    xml_attr("resource")
  
  profile <- tibble(
      id        = id,
      startDate = str_extract(attr, "\\d{8}(?=-)") %>% lubridate::as_date(),
      endDate   = str_extract(attr, "\\d{8}$") %>% lubridate::as_date()
    )
  
  return(profile)
}

mep_profile(197526)

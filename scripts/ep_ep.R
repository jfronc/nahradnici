xfun::pkg_attach("httr2", "jsonlite", "tidyverse", "magrittr")

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
  profile <- paste0("https://data.europarl.europa.eu/api/v2/meps/", id,
    "?format=application%2Fld%2Bjson") |>
    request() |> req_perform() |> resp_body_string() |>
    fromJSON(flatten = TRUE)
  
  profile %<>%
    tibble(
      id        = profile$identifier,
      name      = profile$label,
      startDate = map_chr(profile$hasMembership, ~ .x$memberDuring$startDate %||% NA),
      endDate   = map_chr(profile$hasMembership, ~ .x$memberDuring$endDate   %||% NA),
      organization      = map_chr(profile$hasMembership, ~ .x$organization %||% NA)
    ) %>%
    unnest(c(startDate, endDate, organization)) %>%
    filter(organization == "org/ep-10") %>%
    select(id, name, startDate, endDate)
  
  return(profile)
}

mep_profile(261038)

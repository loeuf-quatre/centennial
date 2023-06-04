###############################################################################
# 01-voting-districts-etc.R                                                   #
# Voting district shapefiles, demographics and election results               #
###############################################################################

# Housekeeping ----------------------------------------------------------------

library(readxl)
library(sf)
library(tidycensus)
library(tigris)

# Census authentication
pat_census <- keyring::key_get("census")
census_api_key(pat_census)

# Census variable codes
vars <- load_variables(dataset = "dhc", year = 2020)
vars <- unique(select(vars, variable = name, label, concept))
vars <- splitstackshape::cSplit(vars, "label", "!!")


# 2022 Voting Districts
vtd <- voting_districts(state = 08)


# Tidy Data -------------------------------------------------------------------

# Voting District Shapefiles ------------------------------


# 2022 Election Results -----------------------------------



# Demographics --------------------------------------------

# https://data.census.gov/table?g=050XX00US08003$7000000



# 2022 Results by Precinct

"
Precinct context:
https://redistrictingdatahub.org/data/about-our-data/election-results-and-precinct-boundaries/
"

# https://redistrictingdatahub.org/dataset/vest-2020-colorado-precinct-boundaries-and-election-results-shapefile/
e20 <- read_sf("/Users/edwardgivens/Downloads/co_2020/co_2020.shp")

e20 <- e20 %>%
  select(
    STATEFP:PRECINCT,
    G20USSDHIC,
    G20USSRGAR,
    geometry
  ) %>%
  mutate(
    dem_gop_votes = G20USSDHIC + G20USSRGAR,
    dem_pct = G20USSDHIC / dem_gop_votes
  ) %>%
  clean_names() %>%
  relocate(
    geometry,
    .after = last_col()
  )

e20 <- st_zm(e20, drop = T, what = "ZM")

pal <- colorNumeric(c("Red", "White", "Blue"), domain = c(0, 1))

# Levels: Population, turnout, %Dem, %White, Average income, Average age
sp <- get_acs(
  geography = "tract",
  table = "B01003",
  state = "CO",
  geometry = FALSE,
  year = 2020
)
tracts <- tracts(state = 08)

get_decennial(
  geography = "voting district",
  table = "PCT9_012N",
  state = "CO"
)

nice <- left_join(tracts, sp, by = "GEOID")

pal2 <- colorNumeric("Blues", domain = nice$estimate)

leaflet() %>%
  addTiles(
  ) %>%
  addPolygons(
    data = e20,
    color = ~pal(dem_pct),
    weight = 0,
    fillOpacity = .5,
    group = "Vote"
  ) %>%
  addPolygons(
    data = nice,
    color = ~pal2(estimate),
    fillOpacity = .8,
    group = "Pop"
  ) %>%
  addLayersControl(
    overlayGroups = c("Vote", "Pop"),
    options = layersControlOptions(collapsed = FALSE)
  )

# 22222

e22 <- read_xlsx("/Users/edwardgivens/Downloads/2022GeneralPrecinctLevelResultsPublic.xlsx")

e22 %>%
  filter(
    Office == "United States Senator"
  )



get_decennial(
  geography = "voting district",
  table = "PCT9_012N",
  state = "CO"
)


  
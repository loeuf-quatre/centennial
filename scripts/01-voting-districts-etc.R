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

# https://www.census.gov/newsroom/blogs/random-samplings/2014/07/understanding-geographic-relationships-counties-places-tracts-and-more.html
# 2022 Voting Districts
vtd <- voting_districts(state = 08, county = 041)
bg <- block_groups(state = 08, county = 041)
trx <- tracts(state = 08, county = 041)
counties <- counties(state = 08)
e20 <- read_sf("/Users/edwardgivens/Downloads/co_2020/co_2020.shp")
e20 <- st_zm(e20, drop = T, what = "ZM")

leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = bg,
    color = "green",
    fillOpacity = .5,
    group = "Block Groups"
  ) %>%
  addPolygons(
    data = trx,
    color = "purple",
    fillOpacity = .5,
    group = "Tracts"
  ) %>%
  addPolygons(
    data = vtd,
    color = "red",
    fillOpacity = .5,
    group = "Voting Districts"
  ) %>%
  addPolygons(
    data = counties,
    color = "blue",
    fillOpacity = .5,
    group = "Counties"
  ) %>%
  addLayersControl(
    overlayGroups = c("Block Groups", "Tracts", "Voting Districts", "Counties"),
    options = layersControlOptions(collapsed = FALSE)
  )

# VTDs nested within counties

# Tidy Data -------------------------------------------------------------------

# Shapefiles ----------------------------------------------

counties <- counties(state = 08)
vtd <- voting_districts(state = 08, county = 041) # El Paso County
trx <- tracts(state = 08, county = 041)
bg <- block_groups(state = 08, county = 041)

"
VTD <------ Counties -------> Tract ------> Block Group
"

leaflet() %>%
  addProviderTiles(
    providers$Stamen.Toner
  ) %>%
  addPolygons(
    data = bg,
    color = "#002868",
    fillOpacity = .5,
    group = "Block Groups"
  ) %>%
  addPolygons(
    data = trx,
    color = "#BF0A30",
    fillOpacity = .5,
    group = "Tracts"
  ) %>%
  addPolygons(
    data = vtd,
    color = "#FFD700",
    fillOpacity = .5,
    group = "Voting Districts"
  ) %>%
  addPolygons(
    data = counties,
    color = "grey",
    fillOpacity = .5,
    group = "Counties"
  ) %>%
  addLayersControl(
    overlayGroups = c("Block Groups", "Tracts", "Voting Districts", "Counties"),
    options = layersControlOptions(collapsed = FALSE)
  )

ah <- bg %>%
  group_by(
    GEOID
  ) %>%
  group_split() %>%
  map(
    ~ st_intersection(.x, vtd), # Intersected area between BG and VTD
    .progress = TRUE
  ) %>%
  map(
    ~ st_make_valid(.x)
  ) %>%
  map(
    ~ mutate(.x, area = st_area(.x))
  ) %>%
  bind_rows() %>%
  filter(
    as.numeric(area) > 0
  ) %>%
  mutate(
    geometry = st_collection_extract(geometry)
  ) %>%
  mutate(
    vtd_assigned = VTDST20[area == max(area)],
    .by = GEOID
  ) %>%
  filter(
    VTDST20 == vtd_assigned
  ) %>%
  distinct(
    GEOID,
    VTDST20
  )

bg <- left_join(bg, ah)

# Total population
pop <- get_acs(
  geography = "block group",
  table = "B02001", # Total population
  state = "CO",
  geometry = FALSE,
  year = 2020
)

# Race
race <- get_acs(
  geography = "block group",
  table = "B02001", # Total population
  state = "CO",
  geometry = FALSE,
  year = 2020
)

# Age
married <- get_acs(
  geography = "block group",
  table = "B12002A", # Total population
  state = "CO",
  geometry = FALSE,
  year = 2020
)

# Income
income <- get_acs(
  geography = "block group",
  table = "B19013", # Total population
  state = "CO",
  geometry = FALSE,
  year = 2020
)

# Education

# Marital Status


# Occupation
# Marital status
married <- get_acs(
  geography = "block group",
  table = "C24020", # Total population
  state = "CO",
  geometry = FALSE,
  year = 2020
)



bg <- inner_join(bg, income, by = "GEOID")
pal <- colorNumeric("Reds", domain = bg$estimate)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(
    data = bg,
    color = ~pal(estimate),
    popup = ~ VTDST20,
      popupOptions = popupOptions(
        noHide = TRUE, 
        direction = 'top', 
        textOnly = FALSE,
        opacity = .5,
        style = list(
          'color' = 'black',
          'font-family' = 'sans-serif',
          'font-size' = '12px',
          'background-color' = '#FBF9E6',
          'border-color' = 'rgba(0, 0, 0, .5)'
        )
      ),
    group = "Block Groups"
  ) %>%
  addPolygons(
    data = vtd,
    popup = ~ VTDST20,
      popupOptions = popupOptions(
        noHide = TRUE, 
        direction = 'top', 
        textOnly = FALSE,
        opacity = .5,
        style = list(
          'color' = 'black',
          'font-family' = 'sans-serif',
          'font-size' = '12px',
          'background-color' = '#FBF9E6',
          'border-color' = 'rgba(0, 0, 0, .5)'
        )
      ),
    color = "red",
    group = "VTD"
  ) %>%
  addLayersControl(
    overlayGroups = c("Block Groups", "VTD"),
    options = layersControlOptions(collapsed = FALSE)
  )


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
pop <- get_acs(
  geography = "block group",
  table = "B01003", # Total population
  state = "CO",
  geometry = FALSE,
  year = 2020
)

bg <- inner_join(bg, pop, by = "GEOID")

pal <- colorNumeric("Blues", domain = bg$estimate)



tracts <- tracts(state = 08)

get_decennial(
  geography = "block",
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

e22 <- e22 %>%
  filter(
    Office == "United States Senator"
  )



get_decennial(
  geography = "voting district",
  table = "PCT9_012N",
  state = "CO"
)


  
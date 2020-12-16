library(readxl)
library(tidyverse)

#save(fips, state_names, vote, state_vote, state_leg, rps, yale, ipeds_info, revenue, out_of_state, stars, act, file="Final Paper/data/preprocessing data.RData")
load("Final Paper/data/preprocessing data.RData")
load("Final Paper/data/project data.RData")

fips <- read_tsv("Final Paper/data/fips.tsv")

state_names <- vote %>%
  ungroup() %>%
  distinct(state, .keep_all = T) %>%
  select(state, state_po) %>%
  mutate(state_po = ifelse(state=="Rhode Island", "RI", state_po))

## STARS ----


ipeds <- left_join(
  read_csv("Final Paper/data/ipeds-instinfo.csv"),
  read_csv("Final Paper/data/IPEDS-fte.csv")) %>%
  janitor::clean_names()

stars <- read_csv("Final Paper/data/stars-benchmark.csv") %>%
  rename_all(funs(str_replace_all( ., "(%)", "" )
  )) %>%
  janitor::clean_names() %>%
  select(institution, overall_score) %>%
  filter(overall_score != "Reporter") %>%
  mutate(overall_score = as.numeric(overall_score),
         institution = str_replace(institution,
                                   "California State University, ",
                                   "California State University-"),
         institution = str_replace(institution,
                                   "University of California, ",
                                   "University of California-"),
         institution = str_replace(institution,
                                   "University of North Carolina, ",
                                   "University of North Carolina at "),
         institution = recode(institution,
                              "Washington University in St. Louis" = "Washington University in St Louis",
                              "Virginia Tech" = "Virginia Polytechnic Institute and State University",
                              "University of Washington, Seattle" = "University of Washington-Seattle Campus",
                              "University of Virginia" = "University of Virginia-Main Campus",
                              "University of Tennessee at Knoxville" = "University of Tennessee-Knoxville",
                              "University of South Florida St. Petersburg" = "University of South Florida-St Petersburg",
                              "University of South Florida" = "University of South Florida-Main Campus",
                              "University of South Carolina" = "University of South Carolina-Columbia",
                              "University of Pittsburgh" = "University of Pittsburgh-Pittsburgh Campus",
                              "University of New Hampshire" = "University of New Hampshire-Main Campus",
                              "University of Nevada Las Vegas" = "University of Nevada-Las Vegas",
                              "University of Nebraska - Lincoln" = "University of Nebraska-Lincoln",
                              "University of Missouri, Kansas City" = "University of Missouri-Kansas City",
                              "University of Missouri" = "University of Missouri-St Louis",
                              "University of Minnesota, Twin Cities" = "University of Minnesota-Twin Cities",
                              "University of Minnesota, Morris" = "University of Minnesota-Morris",
                              "University of Minnesota, Duluth" = "University of Minnesota-Duluth",
                              "University of Michigan" = "University of Michigan-Ann Arbor",
                              "University of Massachusetts Medical School" = "University of Massachusetts Medical School Worcester",
                              "University of Massachusetts Lowell" = "University of Massachusetts-Lowell",
                              "University of Massachusetts Amherst" = "University of Massachusetts-Amherst",
                              "University of Maryland, College Park" = "University of Maryland-College Park",
                              "University of Maryland, Baltimore County" = "University of Maryland-Baltimore County",
                              "University of Illinois, Urbana-Champaign" = "University of Illinois at Urbana-Champaign",
                              "University of Cincinnati" = "University of Cincinnati-Main Campus",
                              "University at Albany" = "SUNY at Albany",
                              "Tulane University" = "Tulane University of Louisiana",
                              "Trinity College (CT)" = "Trinity College",
                              "The University of Texas at Dallas" = "The University of Texas at Dallas",
                              "The Ohio State University at Newark" = "Ohio State University-Newark Campus",
                              "The Ohio State University at Marion" = "Ohio State University-Marion Campus",
                              "The Ohio State University at Mansfield" = "Ohio State University-Mansfield Campus",
                              "The Ohio State University at Lima" = "Ohio State University-Lima Campus",
                              "The Ohio State University" = "Ohio State University-Main Campus",
                              "Texas State University, San Marcos" = "Texas State University",
                              "Texas A&M University" = "Texas A & M University-College Station",
                              "State University of New York at Oneonta" = "SUNY Oneonta",
                              "State University of New York at Geneseo" = "SUNY College at Geneseo",
                              "State University of New York at Fredonia" = "SUNY at Fredonia",
                              "State University of New York at Cortland" = "SUNY Cortland",
                              "State University of New York at Brockport" = "SUNY College at Brockport",
                              "State University of New York Polytechnic Institute" = "SUNY Polytechnic Institute",
                              "State University of New York College of Environmental Science and Forestry" =
                                "SUNY College of Environmental Science and Forestry",
                              "St. Lawrence University" = "St Lawrence University",
                              "St. John's University, New York" = "St John's University-New York",
                              "Southern Illinois University Edwardsville" = "Southern Illinois University-Edwardsville",
                              "Southern Illinois University Carbondale" = "Southern Illinois University-Carbondale",
                              "Slippery Rock University" = "Slippery Rock University of Pennsylvania",
                              "Sewanee - The University of the South" = "The University of the South",
                              "Saint Joseph's College - ME" = "Saint Joseph's College of Maine",
                              "Saint John's University" = "Saint Johns University",
                              "Purchase College - State University of New York" = "SUNY at Purchase College",
                              "Pratt Institute" = "Pratt Institute-Main",
                              "Pennsylvania State University" = "Pennsylvania State University-Main Campus",
                              "Paul Smith's College of Arts & Sciences" = "Paul Smiths College of Arts and Science",
                              "Oklahoma State University" = "Oklahoma State University-Main Campus",
                              "Ohio University" = "Ohio University-Main Campus",
                              "North Carolina State University" = "North Carolina State University at Raleigh",
                              "New Mexico State University" = "New Mexico State University-Main Campus",
                              "Missouri State University" = "Missouri State University-Springfield",
                              "Miami University" = "Miami University-Oxford",
                              "Metropolitan Community College" = "Metropolitan Community College Area",
                              "Louisiana State University" = "Louisiana State University and Agricultural & Mechanical College",
                              "LIU Post" = "Long Island University",
                              "Indiana University-Purdue University Indianapolis (IUPUI)" = "Indiana University-Purdue University-Indianapolis",
                              "Indiana University Bloomington" = "Indiana University-Bloomington",
                              "Hobart and William Smith Colleges" = "Hobart William Smith Colleges",
                              "Franklin W. Olin College of Engineering" = "Franklin W Olin College of Engineering",
                              "Evergreen State College, The" = "The Evergreen State College",
                              "Concordia College - Moorhead" = "Concordia College at Moorhead",
                              "Columbia University" = "Columbia University in the City of New York",
                              "Colorado State University" = "Colorado State University-Fort Collins",
                              "College of William & Mary" = "William & Mary",
                              "Arizona State University" = "Arizona State University-Tempe",
                              "California State Polytechnic University, Pomona" = "California State Polytechnic University-Pomona",
                              "California State Polytechnic University" = "California Polytechnic State University-San Luis Obispo",
                              "University of North Carolina at Wilmington" = "University of North Carolina Wilmington"
         )) %>%
  left_join(ipeds %>%
              select(institution_name, unit_id),
            by = c("institution"="institution_name")) %>%
  add_row(tribble(~institution, ~overall_score, ~unit_id,
                  "Westminster College", 47.23, 230807,
                  "University of St Thomas", 49.23, 174914,
                  "The University of Texas at Dallas", 65.21, 228787,
                  "Sterling College", 78.28, 231095,
                  "The University of the South", 53.15	, 221519,
                  "California Polytechnic State University-San Luis Obispo", 70.16, 110422
  )) %>%
  filter(!is.na(unit_id)) %>%
  left_join(ipeds %>%
              select(institution_name, unit_id, fips_state_code_hd2019,
                     fips_county_code_hd2019, state, county, carnegie, control))

## County-level 2016 votes for the Democratic presidential candidate (Vote percentage) ----

load("Final Paper/data/countypres_2000-2016.RData")
vote <- x %>%
  filter(year == 2012 | year == 2016) %>%
  filter(party == "democrat") %>%
  mutate(vote_pct = candidatevotes / totalvotes) %>%
  group_by(state, state_po, county, FIPS, party) %>%
  summarize(vote_pct_dem = mean(vote_pct)) %>%
  left_join(x %>%
              filter(year == 2012 | year == 2016) %>%
              filter(party == "republican") %>%
              mutate(vote_pct = candidatevotes / totalvotes) %>%
              group_by(state, state_po, county, FIPS, party) %>%
              summarize(vote_pct_rep = mean(vote_pct)),
            by=c("state", "county", "FIPS", "state_po"))

state_vote <- x %>%
  filter(year == 2012 | year == 2016) %>%
  filter(party == "democrat") %>%
  mutate(vote_pct = candidatevotes / totalvotes) %>%
  group_by(state) %>%
  summarize(vote_pct_dem_state = mean(vote_pct, na.rm=T)) %>%
  left_join(x %>%
              filter(year == 2012 | year == 2016) %>%
              filter(party == "republican") %>%
              mutate(vote_pct = candidatevotes / totalvotes) %>%
              group_by(state) %>%
              summarize(vote_pct_rep_state = mean(vote_pct, na.rm=T)),
            by=c("state"))

######################################

## State Legislative Composition Data ----
# https://www.ncsl.org/research/about-state-legislatures/partisan-composition.aspx

state2010 <- read_excel("project ideas/climate change/data/ncsl state govt/2010.xlsx", skip = 2) %>% 
  slice(-51:-53, -55:-59) %>%
  janitor::clean_names() %>%
  mutate(dem = as.numeric(senate_dem) + as.numeric(house_dem),
         rep = as.numeric(senate_rep) + as.numeric(house_rep),
         pct_dem = dem/as.numeric(total_seats),
         pct_rep = rep/as.numeric(total_seats)) %>%
  mutate(state = str_replace(state,"\\*", ""),
         year  = 2010)

state2011 <- read_excel("project ideas/climate change/data/ncsl state govt/2011.xlsx", skip = 2) %>% 
  slice(-51:-53, -55:-59) %>%
  janitor::clean_names() %>%
  mutate(dem = as.numeric(senate_dem) + as.numeric(house_dem),
         rep = as.numeric(senate_rep) + as.numeric(house_rep),
         pct_dem = dem/as.numeric(total_seats),
         pct_rep = rep/as.numeric(total_seats)) %>%
  mutate(state = str_replace(state,"\\*", ""),
         year  = 2011)

state2012 <- read_excel("project ideas/climate change/data/ncsl state govt/2012.xlsx", skip = 1) %>% 
  slice(-51:-53, -55:-59) %>%
  janitor::clean_names() %>%
  mutate(dem = as.numeric(senate_dem) + as.numeric(house_dem),
         rep = as.numeric(senate_rep) + as.numeric(house_rep),
         pct_dem = dem/as.numeric(total_seats),
         pct_rep = rep/as.numeric(total_seats)) %>%
  mutate(state = str_replace(state,"\\*", ""),
         year  = 2012)

state2013 <- read_excel("project ideas/climate change/data/ncsl state govt/2013.xlsx", skip = 1) %>% 
  slice(-51:-53, -55:-59) %>%
  janitor::clean_names() %>%
  mutate(dem = as.numeric(senate_dem) + as.numeric(house_dem),
         rep = as.numeric(senate_rep) + as.numeric(house_rep),
         pct_dem = dem/as.numeric(total_seats),
         pct_rep = rep/as.numeric(total_seats)) %>%
  mutate(state = str_replace(state,"\\*", ""),
         year  = 2013)

state2014 <- read_excel("project ideas/climate change/data/ncsl state govt/2014.xlsx", skip = 1) %>% 
  slice(-51:-53, -55:-59) %>%
  janitor::clean_names() %>%
  mutate(dem = as.numeric(senate_dem) + as.numeric(house_dem),
         rep = as.numeric(senate_rep) + as.numeric(house_rep),
         pct_dem = dem/as.numeric(total_seats),
         pct_rep = rep/as.numeric(total_seats)) %>%
  slice(-52) %>%
  mutate(state = str_replace(state,"\\*", ""),
         year  = 2014)

state2015 <- read_excel("project ideas/climate change/data/ncsl state govt/2015.xlsx", skip = 1) %>% 
  slice(-51:-53, -55:-59) %>%
  janitor::clean_names() %>%
  mutate(dem = as.numeric(senate_dem) + as.numeric(house_dem),
         rep = as.numeric(senate_rep) + as.numeric(house_rep),
         pct_dem = dem/as.numeric(total_seats),
         pct_rep = rep/as.numeric(total_seats)) %>%
  slice(-52) %>%
  mutate(state = str_replace(state,"\\*", ""),
         year  = 2015)

state2016 <- read_excel("project ideas/climate change/data/ncsl state govt/2016.xlsx", skip = 1) %>% 
  slice(-51:-53, -55:-59) %>%
  janitor::clean_names() %>%
  mutate(dem = as.numeric(senate_dem) + as.numeric(house_dem),
         rep = as.numeric(senate_rep) + as.numeric(house_rep),
         pct_dem = dem/as.numeric(total_seats),
         pct_rep = rep/as.numeric(total_seats)) %>%
  slice(-52) %>%
  mutate(state = str_replace(state,"\\*", ""),
         year  = 2016)

state2017 <- read_excel("project ideas/climate change/data/ncsl state govt/2017.xlsx", skip = 1) %>% 
  slice(-51:-53, -55:-59) %>%
  janitor::clean_names() %>%
  mutate(dem = as.numeric(senate_dem) + as.numeric(house_dem),
         rep = as.numeric(senate_rep) + as.numeric(house_rep),
         pct_dem = dem/as.numeric(total_seats),
         pct_rep = rep/as.numeric(total_seats)) %>%
  slice(-52)  %>%
  mutate(state = str_replace(state,"\\*", ""),
         year  = 2017)

state2018 <- read_excel("project ideas/climate change/data/ncsl state govt/2018.xlsx", skip = 1) %>% 
  slice(-51:-53, -55:-59) %>%
  janitor::clean_names() %>%
  mutate(dem = as.numeric(senate_dem) + as.numeric(house_dem),
         rep = as.numeric(senate_rep) + as.numeric(house_rep),
         pct_dem = dem/as.numeric(total_seats),
         pct_rep = rep/as.numeric(total_seats)) %>%
  slice(-52) %>%
  mutate(state = str_replace(state,"\\*", ""),
         year  = 2018)

state2019 <- read_excel("project ideas/climate change/data/ncsl state govt/2019.xlsx", skip = 1) %>% 
  slice(-51:-53, -55:-59) %>%
  janitor::clean_names() %>%
  mutate(dem = as.numeric(senate_dem) + as.numeric(house_dem),
         rep = as.numeric(senate_rep) + as.numeric(house_rep),
         pct_dem = dem/as.numeric(total_seats),
         pct_rep = rep/as.numeric(total_seats)) %>%
  slice(-52) %>%
  mutate(state = str_replace(state,"\\*", ""),
         year  = 2019)

state2020 <- read_excel("project ideas/climate change/data/ncsl state govt/2020.xlsx", skip = 1) %>% 
  slice(-51:-53, -55:-59) %>%
  janitor::clean_names() %>%
  mutate(dem = as.numeric(senate_dem) + as.numeric(house_dem),
         rep = as.numeric(senate_rep) + as.numeric(house_rep),
         pct_dem = dem/as.numeric(total_seats),
         pct_rep = rep/as.numeric(total_seats)) %>%
  slice(-52) %>%
  mutate(state = str_replace(state,"\\*", ""),
         year  = 2020)

state_data <- rbind(state2010, state2011, state2012, state2013, state2014,state2015, state2016, state2017, state2018, state2019, state2020)


#nebraska https://ballotpedia.org/Nebraska_State_Senate_(Unicameral)#Partisan_affiliation

ne <- tribble(~state, ~year, ~total_seats, ~dem, ~rep,
              "Nebraska", 2014,49,12,35,
              "Nebraska", 2016,49,15,32,
              "Nebraska", 2018,49,18,29) %>%
  mutate(pct_dem = dem/total_seats,
         pct_rep = rep/total_seats)



# Percent of Democratic/Republican Mayors, 2010-2020
# https://en.wikipedia.org/wiki/List_of_members_of_the_Council_of_the_District_of_Columbia

dc <-  tribble(~state, ~pct_dem, ~pct_rep,
               "District of Columbia", .86, 0)

state_data <- state_data %>%
  plyr::rbind.fill(ne)


state_leg <- state_data %>%
  group_by(state) %>%
  summarize(pct_dem = mean(pct_dem, na.rm=T),
            pct_rep = mean(pct_rep, na.rm = T)) %>%
  rbind(dc) %>%
  drop_na(pct_dem) %>%
  left_join(state_names, by=c("state" = "state"))

save(state_data, state_leg, file="final paper/data/state_govt_data.Rdata")

################################

# RPS Targets ----
# https://www.ncsl.org/research/energy/renewable-portfolio-standards.aspx (October, 2020)

rps <- read_csv("Final Paper/data/rps.csv") %>%
  left_join(state_names, by=c("state"="state_po"))

# Yale Climate Communications ----

yale <- read_csv("Final Paper/data/YCOM_2018_Data.csv") %>%
  filter(GeoType == "County") %>%
  separate(GeoName, into=c("County", "State"), sep = ", ") %>%
  mutate(County = str_replace(County, " County", "")) %>%
  left_join(state_names, by=c("State" = "state")) %>%
  mutate(County = case_when(
    County == "DeKalb" ~ "De Kalb",
    County =="Prince George's" ~ "Prince Georges",
    County == "St. Lawrence" ~ "St Lawrence",
    County == "St. Louis city" ~ "St Louis City",
    County == "St. Louis" ~ "St Louis",
    County == "Radford city" ~ "Radford City",
    County == "Lexington city" ~ "Lexington City",
    County == "Richmond city" ~ "Richmond City",
    County == "St. Joseph" ~ "St Joseph",
    County == "Orleans Parish" ~ "Orleans",
    County == "Do�a Ana, New Mexico" ~ "Dona Ana",
    County == "East Baton Rouge Parish" ~ "East Baton Rouge",
    County == "Harrisonburg city" ~ "Harrisonburg City",
    County == "District of Columbia" ~ "Washington",
    County == "Williamsburg city" ~ "Williamsburg City",
    TRUE ~ County),
    State = ifelse(County == "Dona Ana", 
                   "New Mexico", State)) %>%
  left_join(fips, by=c("state_po" = "State", 
                       "County" = "Name")) %>%
  mutate(FIPS = as.numeric(FIPS)) %>%
  mutate(FIPS = case_when(
    County == "Dona Ana" ~ 35013,
    County == "Providence" ~ 44007,
    TRUE ~ FIPS
  ))

# Ipeds Data ----

# carnegie

ipeds_info <- read_csv("Final Paper/data/ipeds-instinfo.csv") %>%
  janitor::clean_names() %>%
  select(unit_id, institution_name, fips_state_code_hd2019, fips_county_code_hd2019, state, county, control, carnegie)

# revenue fte

# rev_gasb <- read_csv("Final Paper/data/ipeds-finances-fte-gasb.csv") %>%
#   janitor::clean_names() %>%
#   pivot_longer(3:46) %>%
#   separate(name, into=c("type", "year"), sep="drvf") %>%
#   group_by(unit_id, institution_name, year) %>%
#   summarize(revenue = sum(value, na.rm = T),
#             .groups = "drop") %>%
#   filter(revenue > 0) %>%
#   group_by(unit_id, institution_name) %>%
#   summarize(revenue = mean(revenue))
# 
# 
# rev_other <- read_csv("Final Paper/data/ipeds-finances-fte-fasb-profit.csv") %>%
#   janitor::clean_names() %>%
#   pivot_longer(3:44) %>%
#   separate(name, into=c("type", "year"), sep="drvf") %>%
#   group_by(unit_id, institution_name, year) %>%
#   summarize(revenue = sum(value, na.rm = T),
#             .groups="drop") %>%
#   filter(revenue > 0) %>%
#   group_by(unit_id, institution_name) %>%
#   summarize(revenue = mean(revenue))

# revenue total

rev_public <- read_csv("Final Paper/data/ipeds-finances-public.csv") %>%
  janitor::clean_names() %>%
  pivot_longer(3:42) %>%
  filter(str_detect(name, "tuition")) %>%
  mutate(name = str_replace(name, "fees", "")) %>%
  separate(name, into=c("type", "year"), sep="_f") %>%
  group_by(unit_id, institution_name, year) %>%
  summarize(revenue = sum(value, na.rm = T),
            .groups="drop") %>%
  filter(revenue > 0) %>%
  group_by(unit_id, institution_name) %>%
  summarize(revenue = mean(revenue)) %>%
  mutate(source = "public")

rev_private_public <- read_csv("Final Paper/data/ipeds-finances-private-or-public.csv") %>%
  janitor::clean_names() %>%
  pivot_longer(3:46) %>%
  filter(str_detect(name, "tuition")) %>%
  mutate(name = str_replace(name, "fees", "")) %>%
  separate(name, into=c("type", "year"), sep="_f") %>%
  group_by(unit_id, institution_name, year) %>%
  summarize(revenue = sum(value, na.rm = T),
            .groups="drop") %>%
  filter(revenue > 0) %>%
  group_by(unit_id, institution_name) %>%
  summarize(revenue = mean(revenue)) %>%
  mutate(source = "private or public")


rev_for_profit <- read_csv("Final Paper/data/ipeds-finances-private-for-profit.csv") %>%
  janitor::clean_names() %>%
  pivot_longer(3:55) %>%
  filter(str_detect(name, "tuition")) %>%
  mutate(name = str_replace(name, "fees", "")) %>%
  separate(name, into=c("type", "year"), sep="_f") %>%
  group_by(unit_id, institution_name, year) %>%
  summarize(revenue = sum(value, na.rm = T),
            .groups="drop") %>%
  filter(revenue > 0) %>%
  group_by(unit_id, institution_name) %>%
  summarize(revenue = mean(revenue)) %>%
  mutate(source = "profit")

revenue <- rbind(rev_public, rev_private_public, rev_for_profit) %>%
  group_by(unit_id, institution_name) %>%
  summarize(revenue = mean(revenue)) #averages duplicates
# 
# rev_dupes <- revenue %>%
#   group_by(unit_id) %>%
#   count() %>%
#   filter(n>1)
# 
# revenue %>%
#   filter(unit_id %in% rev_dupes$unit_id) %>% view()

# out of state students ----

out_of_state <- read_csv("Final Paper/data/ipeds-out-of-state.csv") %>%
  janitor::clean_names() %>%
  pivot_longer(3:9) %>%
  group_by(unit_id, institution_name) %>%
  summarize(out_of_state = mean(value, na.rm = T)) %>%
  mutate(out_of_state = ifelse(is.nan(out_of_state),
                               0, out_of_state))

# selectivity
# https://carnegieclassifications.iu.edu/methodology/ugrad_profile.php

sat_act_conc <- read_excel("Final Paper/data/sat_act_concordance_2018.xlsx") %>%
  mutate(SAT = str_replace(SAT, "\\*", ""),
         SAT = as.numeric(SAT))

act_only <- read_csv("Final Paper/data/ipeds-act.csv") %>%
  janitor::clean_names() %>%
  select(1,2, starts_with("act_")) %>%
  pivot_longer(3:11, names_to="year", values_to="act") %>%
  group_by(unit_id) %>%
  summarize(act = mean(act, na.rm = T),
            .groups="drop") %>%
  mutate(act = ifelse(is.nan(act), 0, act))

sat <- read_csv("Final Paper/data/ipeds-act.csv") %>%
  janitor::clean_names() %>%
  select(1,2, (starts_with("sat_") & contains("reading"))) %>%
  pivot_longer(3:11, names_to="year", values_to="sat_r") %>%
  mutate(year = str_replace(year, "sat_evidence_based_reading_and_writing_75th_percentile_score_adm", ""),
         year=str_replace(year, "sat_critical_reading_75th_percentile_score_adm", ""),
         year=str_replace(year, "sat_critical_reading_75th_percentile_score_ic", ""),
         year=str_replace(year, "_rv", "")) %>%
  left_join(read_csv("Final Paper/data/ipeds-act.csv") %>%
              janitor::clean_names() %>%
              select(1, (starts_with("sat_") & contains("math"))) %>%
              pivot_longer(2:10, names_to="year", values_to="sat_m") %>%
              mutate(year = str_replace(year, "sat_math_75th_percentile_score_adm", ""),
                     year = str_replace(year, "sat_math_75th_percentile_score_ic", ""),
                     year = str_replace(year, "_rv", "")),
            by=c("unit_id", "year")) %>%
  mutate(sat = sat_r + sat_m) %>%
group_by(unit_id) %>%
  summarize(sat = plyr::round_any(mean(sat, na.rm = T),10),
            .groups="drop") %>%
  mutate(sat = ifelse(is.nan(sat), 0, sat)) %>%
  left_join(sat_act_conc, by=c("sat" = "SAT"))

act_corrected <- act_only %>%
  filter(unit_id %in% stars$unit_id) %>%
  filter(act == 0) %>%
  left_join(sat, by="unit_id") %>%
  mutate(act = ACT,
         act = ifelse(is.na(ACT), 0, act)) %>%
  filter(act == 0) %>%
  select(unit_id, act)

act <- act_only %>%
  filter(unit_id %in% stars$unit_id) %>%
  filter(act != 0) %>%
  rbind(act_corrected) %>%
  mutate(act = ifelse(act == 0, NA, act))
  
# selectivity - pct admitted ----

selectivity <- read_csv("Final Paper/data/ipeds-selectivity.csv") %>%
  janitor::clean_names() %>%
  pivot_longer(3:11, names_to="year", values_to="pct") %>%
  group_by(unit_id) %>%
  summarize(selectivity = mean(pct, na.rm = T)/100) %>%
  mutate(selectivity = ifelse(is.nan(selectivity), NA, selectivity))

selectivity %>%
  filter(unit_id %in% data$unit_id)

# NA values - do not include


# data combined ----

data <- stars %>%
  left_join(vote, 
            by=c("fips_county_code_hd2019" = "FIPS")) %>%
  left_join(state_vote, by=c("state.x" = "state")) %>%
  left_join(yale %>% 
              select(County, State, human, state_po, FIPS), by=c("fips_county_code_hd2019" = "FIPS")) %>%
  left_join(ipeds_info %>%
              select(unit_id, control, carnegie),
            by="unit_id") %>%
  left_join(revenue, by="unit_id") %>%
  left_join(out_of_state, by="unit_id") %>%
  left_join(state_leg, by=c("state.x" = "state")) %>%
  left_join(rps, by=c("state.x" = "state.y")) %>%
  left_join(act, by="unit_id") %>%
  select(-state, -note, -state_po.y, -control.y, -carnegie.y, -state.y, -institution_name.x) %>%
  rename("stars_score" = overall_score,
          "state" = state.x,
         "county" = county.x,
         "climate_chg_agree" = human,
         "carnegie" = carnegie.x,
         "control" = control.x,
         "state_leg_pct_dem" = pct_dem,
         "state_leg_pct_rep" = pct_rep,
         "rps_target" = target) %>%
  mutate(revenue_mil = revenue/1000000,
         control = ifelse(str_detect(control, "Private"), 
                          "Private", "Public"),
         control = factor(control),
         control = relevel(control, ref="Public"),
         carnegie = case_when(
           carnegie == "Associates" ~ "Other",
           carnegie == "Associate's" ~ "Other",
           carnegie == "Special Focus" ~ "Other",
           TRUE ~ carnegie),
         carnegie = factor(carnegie),
         carnegie = relevel(carnegie, ref="Doctoral"),
         stars_score = stars_score/100,
         out_of_state = out_of_state/100,
         out_of_state_c = out_of_state - mean(out_of_state),
         climate_chg_agree = climate_chg_agree/100,
         revenue_mil_c = revenue_mil-mean(revenue_mil),
         state_leg_pct_dem_c = state_leg_pct_dem - mean(state_leg_pct_dem)) %>%
  group_by(state) %>%
  mutate(vote_pct_dem_c5 = vote_pct_dem - .50,
         vote_pct_dem_cwc = vote_pct_dem - vote_pct_dem_state) %>%
  select(unit_id, institution, state, county, stars_score, carnegie, control, vote_pct_dem, vote_pct_dem_c5, vote_pct_dem_cwc, vote_pct_dem_state, vote_pct_rep, climate_chg_agree, revenue, revenue_mil, revenue_mil_c, out_of_state, out_of_state_c, rps_target, state_leg_pct_dem, state_leg_pct_rep, state_leg_pct_dem_c, fips_county_code_hd2019) %>%
  distinct(institution, .keep_all = T)

p_missing <- unlist(lapply(data, function(x) sum(is.na(x))))/nrow(data)
sort(p_missing[p_missing > 0], decreasing = TRUE)



save(data, file="Final Paper/data/project data.RData")

# 
# state_support <- readxl::read_excel("Final Paper/data/SHEEO_SHEF_FY18_Nominal_Data.xlsx", skip=15) %>%
#   rename("state" = ...1,
#          "year" = ...2,
#          "support" = ...6) %>%
#   select(state, year, support) %>%
#   group_by(state) %>%
#   summarize(support = mean(support)) %>%
#   filter(state != "US") %>%
#   mutate(state = ifelse(state=="Washington DC", "District of Columbia", state))
# 
# state_leg_w_support <- state_leg %>%
#   left_join(state_support)
# 
# cor.test(state_leg_w_support$pct_dem, state_leg_w_support$support)

## ipeds size info

ipeds_size <- read.csv("Final Paper/data/ipeds_size.csv") %>%
  janitor::clean_names() %>%
  left_join(ipeds_info, by="unit_id") %>%
  filter(unit_id %in% stars$unit_id)

ipeds_size %>%
  group_by(carnegie) %>%
  summarize(n = n(),
    size = median(institution_size_category_hd2019),
            enrollment = mean(total_enrollment_drvef2018, na.rm=T)) 

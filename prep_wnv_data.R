library(dplyr)
library(readr)
library(usmap)
library(tidycensus)


#read and combine CDC ArboNET case data.
df <- list.files(path="/Users/quinnadams/Desktop/CU-PostDoc/Research/Ryan_analysis_rep/", 
                 pattern = "csv",full.names = TRUE) %>% 
  lapply(read.csv) %>% 
  bind_rows 

#handle location information
df$state <- substr(df$Location, start = 1, stop = 2)
df$county_name <- substr(df$Location, start = 5, stop = 20)

df <- df %>%
  rename(total_cases = Total.human.disease.cases,
         neuro_cases = Neuroinvasive.disease.cases,
         blood_donor = X..Presumptive.viremic.blood.donors)

# df$FIPS <- fips(state = df$state, county = df$county_name)
data(fips_codes)

df <- df %>%
  mutate(county_name = paste0(county_name, " County"))


df_recode <- df %>% mutate(recoded_county = case_when(county_name %in% c("Acadia Parish County") ~ "Acadia Parish",
                                         county_name %in% c("Alexandria City County") ~ "Alexandria city",
                                         county_name %in% c("Allen Parish County") ~ "Allen Parish",
                                         county_name %in% c("Anchorage Muny County") ~ "Anchorage Municipality",
                                         county_name %in% c("Ascension Parish County") ~ "Ascension Parish",
                                         county_name %in% c("Assumption Paris County") ~ "Assumption Parish",
                                         county_name %in% c("Avoyelles Parish County") ~ "Avoyelles Parish",
                                         county_name %in% c("Baltimore City County") ~ "Baltimore city",
                                         county_name %in% c("Beauregard Paris County") ~ "Beauregard Parish",
                                         county_name %in% c("Bossier Parish County") ~ "Bossier Parish",
                                         county_name %in% c("Buena Vista City County") ~ "Buena Vista city",
                                         county_name %in% c("Caddo Parish County") ~ "Caddo Parish",
                                         county_name %in% c("Calcasieu Parish County") ~ "Calcasieu Parish",
                                         county_name %in% c("Caldwell Parish County") ~ "Caldwell Parish",
                                         county_name %in% c("Cameron Parish County") ~ "Cameron Parish",
                                         county_name %in% c("Capitol County") ~ "Capitol",
                                         county_name %in% c("Carson City County") ~ "Carson City",
                                         county_name %in% c("Catahoula Parish County") ~ "Catahoula Parish",
                                         county_name %in% c("Chesapeake City County") ~ "Chesapeake city",
                                         county_name %in% c("Claiborne Parish County") ~ "Claiborne Parish",
                                         county_name %in% c("Colonial Heights County") ~ "Colonial Heights city",
                                         county_name %in% c("Concordia Parish County") ~ "Concordia Parish",
                                         county_name %in% c("Danville City County") ~ "Danville city",
                                         county_name %in% c("	De Soto Parish County") ~ "	De Soto Parish",
                                         county_name %in% c("DuPage County") ~ "DuPage Parish",
                                         county_name %in% c("East Baton Rouge County") ~ "East Baton Rouge Parish",
                                         county_name %in% c("East Carroll Par County") ~ "East Carroll Parish",
                                         county_name %in% c("East Feliciana P County") ~ "East Feliciana Parish",
                                         county_name %in% c("Evangeline Paris County") ~ "Evangeline Parish",
                                         county_name %in% c("Fairfax City County") ~ "Fairfax city",
                                         county_name %in% c("Falls Church City County") ~ "Falls Church city",
                                         county_name %in% c("Fond Du Lac County County") ~ "Fond du Lac County",
                                         county_name %in% c("Fredericksburg C County") ~ "Fredericksburg city",
                                         county_name %in% c("Galax City County") ~ "Galax city",
                                         county_name %in% c("Grant Parish County") ~ "Grant Parish",
                                         county_name %in% c("Greater Bridgepo County") ~ "Greater Bridgeport",
                                         county_name %in% c("Hampton City  County") ~ "Hampton city",
                                         county_name %in% c("Harrisonburg Cit County") ~ "Harrisonburg city",
                                         county_name %in% c("Hopewell City  County") ~ "Hopewell city",
                                         county_name %in% c("Iberia Parish  County") ~ "Iberia Parish",
                                         county_name %in% c("Iberville Parish  County") ~ "Iberville Parish",
                                         county_name %in% c("Jackson Parish  County") ~ "Jackson Parish",
                                         county_name %in% c("Jefferson Davis  County") ~ "Jefferson Davis Parish",
                                         county_name %in% c("Jefferson Parish  County") ~ "Jefferson Parish",
                                         county_name %in% c("King And Queen County") ~ "King and Queen County",
                                         county_name %in% c("La Grange County") ~ "LaGrange County",
                                         county_name %in% c("La Moure County") ~ "LaMoure County",
                                         county_name %in% c("La Porte County") ~ "LaPorte County",
                                         county_name %in% c("La Salle Parish County") ~ "La Salle Parish",
                                         county_name %in% c("Lac Qui Parle County") ~ "Lac qui Parle County",
                                         county_name %in% c("Lafayette Parish County") ~ "Lafayette Parish",
                                         county_name %in% c("Lafourche Parish County") ~ "Lafourche Parish",
                                         county_name %in% c("Lake of The Wood County") ~ "Lake of the Woods County",
                                         county_name %in% c("Lewis And Clark County") ~ "Lewis and Clark County",
                                         county_name %in% c("Lincoln Parish County") ~ "Lincoln Parish",
                                         county_name %in% c("St. Johns County") ~ "St. Johns County",
                                         county_name %in% c("St Joseph County") ~ "St. Joseph County",
                                         county_name %in% c("De Kalb County") ~ "DeKalb County",
                                         # county_name %in% c("De Witt County") ~ "DeWitt County",
                                         county_name %in% c("Franklin Parish County") ~ "Franklin Parish",
                                         county_name %in% c("Livingston Paris County") ~ "Livingston Parish",
                                         county_name %in% c("Lower Connecticu County") ~ "Lower Connecticut River Valley",
                                         county_name %in% c("Lynchburg City County") ~ "Lynchburg city",
                                         county_name %in% c("Madison Parish County") ~ "Madison Parish",
                                         county_name %in% c("Manassas Park Ci County") ~ "Manassas Park city",
                                         county_name %in% c("Manassas City County") ~ "Manassas city",
                                         county_name %in% c("Martinsville Cit County") ~ "Martinsville city",
                                         county_name %in% c("Mcclain County") ~ "McClain County",
                                         county_name %in% c("Mccone County") ~ "McCone County",
                                         county_name %in% c("Mccook County") ~ "McCook County",
                                         county_name %in% c("Mccracken County") ~ "McCracken County",
                                         county_name %in% c("Mccreary County") ~ "McCreary County",
                                         county_name %in% c("Mcculloch County") ~ "McCulloch County",
                                         county_name %in% c("Mccurtain County") ~ "McCurtain County",
                                         county_name %in% c("Mcdonald County") ~ "McDonald County",
                                         county_name %in% c("Mcdonough County") ~ "McDonough County",
                                         county_name %in% c("Mcdowell County") ~ "McDowell County",
                                         county_name %in% c("Mcduffie County") ~ "McDuffie County",
                                         county_name %in% c("Mchenry County") ~ "McHenry County",
                                         county_name %in% c("Mcintosh County") ~ "McIntosh County",
                                         county_name %in% c("Mckean County") ~ "McKean County",
                                         county_name %in% c("McKenzie County") ~ "McKenzie County",
                                         county_name %in% c("Mckinley County") ~ "McKinley County",
                                         county_name %in% c("Mclean County") ~ "McLean County",
                                         county_name %in% c("Mclennan County") ~ "McLennan County",
                                         county_name %in% c("Mcleod County") ~ "McLeod County",
                                         county_name %in% c("Mcminn County") ~ "McMinn County",
                                         county_name %in% c("Mcmullen County") ~ "McMullen County",
                                         county_name %in% c("Mcnairy County") ~ "McNairy County",
                                         county_name %in% c("Mcpherson County") ~ "McPherson County",
                                         county_name %in% c("Morehouse Parish County") ~ "Morehouse Parish",
                                         county_name %in% c("Natchitoches Par County") ~ "Natchitoches Parish",
                                         county_name %in% c("Naugatuck Valley County") ~ "Naugatuck Valley",
                                         county_name %in% c("Newport News Cit County") ~ "Newport News city",
                                         county_name %in% c("Norfolk City County") ~ "Norfolk city",
                                         county_name %in% c("Northeastern Con County") ~ "Northeastern Connecticut",
                                         county_name %in% c("Northwest Hills County") ~ "Northwest Hills",
                                         county_name %in% c("O Brien County") ~ "O'Brien County",
                                         county_name %in% c("Oglala Lakota Co County") ~ "Oglala Lakota County",
                                         county_name %in% c("Orleans Parish County") ~ "Orleans Parish",
                                         county_name %in% c("Ouachita Parish County") ~ "Ouachita Parish",
                                         county_name %in% c("Petersburg City County") ~ "Petersburg city",
                                         county_name %in% c("Plaquemines Pari County") ~ "Plaquemines Parish",
                                         county_name %in% c("Pointe Coupee Pa County") ~ "Pointe Coupee Parish",
                                         county_name %in% c("Poquoson City County") ~ "Poquoson city",
                                         county_name %in% c("Portsmouth City County") ~ "Portsmouth city",
                                         county_name %in% c("Prince Georges County") ~ "Prince George's County",
                                         county_name %in% c("Queen Annes County") ~ "Queen Anne's County",
                                         county_name %in% c("Radford City County") ~ "Radford city",
                                         county_name %in% c("Rapides Parish County") ~ "Rapides Parish",
                                         county_name %in% c("Red River Parish County") ~ "Red River Parish",
                                         county_name %in% c("Richland Parish County") ~ "Richland Parish",
                                         county_name %in% c("Richmond City County") ~ "Richmond city",
                                         county_name %in% c("Roanoke City County") ~ "Roanoke city",
                                         county_name %in% c("Sabine Parish County") ~ "Sabine Parish",
                                         county_name %in% c("Salem City County") ~ "Salem city",
                                         county_name %in% c("South Central Co County") ~ "South Central Connecticut",
                                         county_name %in% c("Southeastern Con County") ~ "Southeastern Connecticut",
                                         county_name %in% c("St Bernard Paris County") ~ "St. Bernard Parish",
                                         county_name %in% c("St Charles County") ~ "St. Charles County",
                                         county_name %in% c("St Charles Paris County") ~ "St. Charles Parish",
                                         county_name %in% c("St Clair County") ~ "St. Clair County",
                                         county_name %in% c("St Croix County") ~ "St. Croix County",
                                         county_name %in% c("St Francois County") ~ "St. Francois County",
                                         county_name %in% c("St Helena Parish County") ~ "St. Helena Parish",
                                         county_name %in% c("St James Parish County") ~ "St. James Parish",
                                         county_name %in% c("St John The Bapt County") ~ "St. John the Baptist Parish",
                                         county_name %in% c("St Johns County") ~ "St. Johns County",
                                         county_name %in% c("St Joseph County") ~ "St. Joseph County",
                                         county_name %in% c("St Landry Parish County") ~ "St. Landry Parish",
                                         county_name %in% c("St Lawrence County") ~ "St. Lawrence County",
                                         county_name %in% c("St Louis City County") ~ "St. Louis city",
                                         county_name %in% c("St Louis County") ~ "St. Louis County",
                                         county_name %in% c("St Lucie County") ~ "St. Lucie County",
                                         county_name %in% c("St Martin Parish County") ~ "St. Martin Parish",
                                         county_name %in% c("St Mary Parish County") ~ "St. Mary Parish",
                                         county_name %in% c("St Marys County") ~ "St. Mary's County",
                                         county_name %in% c("St Tammany Paris County") ~ "St. Tammany Parish",
                                         county_name %in% c("Suffolk City County") ~ "Suffolk city",
                                         county_name %in% c("Tangipahoa Paris County") ~ "Tangipahoa Parish",
                                         county_name %in% c("Tensas Parish County") ~ "Tensas Parish",
                                         county_name %in% c("Terrebonne Paris County") ~ "Terrebonne Parish",
                                         county_name %in% c("Union Parish County") ~ "Union Parish",
                                         county_name %in% c("Vermilion Parish County") ~ "Vermilion Parish",
                                         county_name %in% c("Vernon Parish County") ~ "Vernon Parish",
                                         county_name %in% c("Virginia Beach C County") ~ "Virginia Beach city",
                                         # county_name %in% c("Washington County") ~ "District of Columbia",
                                         county_name %in% c("Washington Paris County") ~ "Washington Parish",
                                         county_name %in% c("Waynesboro City County") ~ "Waynesboro city",
                                         county_name %in% c("Webster Parish County") ~ "Webster Parish",
                                         county_name %in% c("West Baton Rouge County") ~ "West Baton Rouge Parish",
                                         county_name %in% c("West Carroll Par County") ~ "West Carroll Parish",
                                         county_name %in% c("West Feliciana P County") ~ "West Feliciana Parish",
                                         county_name %in% c("Western Connecti County") ~ "Western Connecticut",
                                         county_name %in% c("Winchester City County") ~ "Winchester city",
                                         county_name %in% c("Winn Parish County") ~ "Winn Parish",
                                         county_name %in% c("Du Page County") ~ "DuPage County",
                                         
                                         county_name %in% c("Iberia Parish County") ~ "Iberia Parish",
                                         county_name %in% c("Iberville Parish County") ~ "Iberville Parish",
                                         county_name %in% c("Jefferson Parish County") ~ "Jefferson Parish",
                                         county_name %in% c("Mckenzie County") ~ "McKenzie County",
                                         county_name %in% c("Hampton City County") ~ "Hampton city",
                                         county_name %in% c("St Francis County") ~ "St. Francis County",
                                         county_name %in% c("Fond Du Lac County") ~ "Fond du Lac County",
                                         county_name %in% c("Jackson Parish County") ~ "Jackson Parish",
                                         county_name %in% c("Bienville Parish County") ~ "Bienville Parish",
                                         county_name %in% c("De Soto Parish County") ~ "De Soto Parish",
                                         county_name %in% c("Franklin City County") ~ "Franklin city",
                                         county_name %in% c("Bristol City County") ~ "Bristol city",
                                         county_name %in% c("Hopewell City County") ~ "Hopewell city",
                                         county_name %in% c("Falls Church Cit County") ~ "Falls Church city",
                                         county_name %in% c("De Soto County") ~ "DeSoto County",
                                         county_name == "De Witt County" & state == "TX" ~ "DeWitt County",
                                         county_name == "Washington County" & state == "DC" ~ "District of Columbia",
                                         county_name == "La Salle County" & state == "IL" ~ "LaSalle County"
                                         
                                         ) )

df_recode$county <- ifelse(is.na(df_recode$recoded_county), df_recode$county_name, df_recode$recoded_county)



#merge with FIPS data
df_with_fips <- left_join(df_recode, fips_codes,
                          by = c("state" = "state",
                                 "county" = "county"))
df_with_fips <- df_with_fips %>%
  filter(state != "PR") %>%
  select(Year, Location, total_cases, neuro_cases, blood_donor, state, county, state_code, county_code)

write.csv(df_with_fips, "clean_wnv_cases.csv")








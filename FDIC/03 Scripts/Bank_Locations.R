
getwd()

library(tidyverse)
library(ggthemes)
library(sf)
library(ggmap)
library(tidygeocoder)
library(tidycensus)
library(mapview)
library(tmap)

census_api_key("658d2b9a22ad0ae4e3f06c3753013994a3d339bc",install = TRUE)

#read in FDIC insured bank locations within the State of Maryland
# use when you have an internet connection
data <- "https://raw.githubusercontent.com/crobbs05/FDIC_Insured_Banks_Maryland/main/FDIC/02%20Data/md_fdic_bank_locations.csv"
md_locations <- read.csv(data)

#read in banks in moco and pg county
moco_pg_banks <- read.csv("02 Data/Tabular/moco_pg_banks.csv_v02")

#write to local folder
write.csv(moco_pg_banks,"02 Data/Tabular/moco_pg_banks.csv_v02",row.names = FALSE)


#column names to a single address column for gecoding
md_locations <- unite(md_locations, "BANK_ADDRESS",ADDRESS,CITY,STALP, ZIP,sep = " ", remove = FALSE)


# relocate full address to begin of dataset
md_locations <- relocate(md_locations,c(NAME,OFFNAME), .before = BANK_ADDRESS) %>%
relocate(c(CITY,COUNTY,ZIP,STALP,BKCLASS,SERVTYPE,CBSA,CBSA_DIV,CBSA_METRO_NAME), .after = BANK_ADDRESS)


#change the name to Prince George's from Prince George'S
md_locations$COUNTY <- recode(md_locations$COUNTY,"Prince George'S" = "Prince George's")

# selecting the first 12 columns of the dataset
md_locations <- select(md_locations,c(1:12))


#get long and lat coordinates with mutate_geocode function from ggmap package
moco_pg_banks <- md_locations %>% filter(COUNTY  %in% c("Montgomery","Prince George's")) %>% mutate_geocode(BANK_ADDRESS)




#change the name to Prince George's from Prince George'S
#moco_pg_banks$COUNTY[moco_pg_banks$COUNTY=="Prince George'S"] <- "Prince George's"


#indexing md_locations dataset to get banks in montgomery county only
#moco_banks<- moco_pg_banks[moco_pg_banks$COUNTY == "Montgomery",]


#indexing md_locations dataset to get banks in montgomery county only
#pgco_banks<- moco_pg_banks[moco_pg_banks$COUNTY == "Prince George's",]

#pgbanks <- moco_pg_banks %>% filter(COUNTY == "Prince George's")


#shows values without lat and long coordinates in dataset
missing_coordinates <- moco_pg_banks[!complete.cases(moco_pg_banks$lat),]


#removed rows with missing variables.a total of three rows where removed
moco_pg_banks <- na.omit(moco_pg_banks)

#map locations in prince george's and montgomery county
moco_pg_bank_locations <- st_as_sf(moco_pg_banks,coords = c("lon","lat"), crs = 4269)

# was not working. I used moco_pg_banks_v02 in QGIS to create a geopackage
st_write(moco_pg_bank_locations,dsn = "02 Data/Spatial/moco_pg_bank_locations_v05.gpkg",append = FALSE)

#create final dataset with geopackage
final_bank_locations <- st_read(dsn = "02 Data/Spatial/moco_pg_bank_locations_v05.gpkg")

#use tidycensus to get census tracts and median income data for montgomery and prince george's county 

variables <- load_variables(year = 2018,dataset = "acs5/subject",cache = TRUE)

counties <- c("Montgomery","Prince George's")


moco_pgco_cty_boundaries_data <- get_acs(geography = "tract",
variables = c(median_income = "S1901_C01_012", population = "S0101_C01_001"),state = "MD",county = counties, geometry = TRUE)

#quick map of bank locations
mapview(final_bank_locations)
#quick map of montgomery and prince george's county
mapview(moco_pgco_cty_boundaries_data)

median_income_variable <- moco_pgco_cty_boundaries_data %>%  filter(variable == "median_income")

population_variable <- moco_pgco_cty_boundaries_data %>%  filter(variable == "population")

moco_pgco_cty_boundaries_data[moco_pgco_cty_boundaries_data$variable == "population",]

median_income_variable %>% ggplot(mapping =aes(fill = estimate)) + geom_sf(color = NA)+ coord_sf(crs = 4269) + scale_fill_viridis_c() + theme_void()


population_variable %>% ggplot(mapping =aes(fill = estimate)) + geom_sf(color = NA)+ coord_sf(crs = 4269) + scale_fill_viridis_c() + theme_void()


#visualizing the location of the banks with median income
tmap_mode("view")
tm_shape(median_income_variable) + tm_polygons("estimate") + 
  tm_shape(final_bank_locations)+ tm_symbols(col = "red",size =.015,alpha =.25)
  

class(population_variable)

moco_pg_banks %>% count(ZIP, sort = TRUE) %>% head(15) %>% 
mutate(zip_code = reorder(ZIP,n)) %>% 
ggplot(mapping = aes(x = zip_code,y = n))+
geom_col(alpha = .50,  fill= "blue")+
geom_text(mapping = aes(label = n), nudge_y = -.75, color  ="white",fontface = "bold")+
coord_flip()+
theme_tufte()+
theme(plot.title = element_text(hjust = .075),plot.subtitle =element_text(hjust = .069),axis.title.y = element_blank())+
labs(title = "Number of Banks by Zipcode", subtitle = "Top 15 Zipcodes Statewide",  y = "Number of Banks by Zipcode")

table(moco_pg_banks$BKCLASS)

which(names(moco_pg_banks) == "STNAME")

#number of FDIC Insured Banks in Maryland
moco_pg_banks %>% count(COUNTY,sort = TRUE) %>% 
head(10) %>% mutate(county_name = reorder(COUNTY,n)) %>% 
ggplot(mapping = aes(x = county_name,y = n, fill = county_name)) +
geom_col(fill = "darkgreen",color ="black")+
geom_text(aes(label = n),color = "white", nudge_y = -3, size =3)+
coord_flip()+
theme_minimal()+
theme(text = element_text(color = "navy"))+
labs(title =  "Number of FDIC Insured Banks in Maryland",x = "County Names", y = "Number of Banks")



#FDIC Insured  Banks by Metropolitan 
moco_pg_banks %>% count(CBSA, sort = TRUE) %>%
mutate(csba_reorder = reorder(CBSA,n)) %>% 
ggplot(mapping = aes(x = csba_reorder, y = n))+
geom_col(fill = "navy") + 
geom_text(aes(label = n), color = "white",nudge_y =-.10)+
coord_flip()+ 
scale_y_log10()+ 
theme_tufte()+
labs(title = " FDIC Insured Banks by Metropolitan Region", y  = "Number of Banks")+
theme(axis.title.y =element_blank(), plot.title = element_text(hjust = .075))
  

#Number of Banks Branches by Association
moco_pg_banks %>% count(NAME,sort = TRUE) %>% 
head(10) %>%  
mutate(bank_names = reorder(NAME,n)) %>% 
ggplot(mapping = aes(bank_names, n)) + 
geom_col(fill = "navy", color = "black", alpha = 0.5) + 
geom_text(aes(label = n),nudge_y = 5)+
coord_flip()+
theme_minimal()+
labs(title = "Number of Banks Branches by Association",  y = "Number of Banks")+
theme(axis.title.y =element_blank(), plot.title = element_text(hjust = .075))





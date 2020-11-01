
getwd()

library(tidyverse)
library(ggthemes)
library(sf)
library(ggmap)
library(tidygeocoder)
#read in FDIC insured bank locations within the State of Maryland
# use when you have an internet conection
data <- "https://raw.githubusercontent.com/crobbs05/FDIC_Insured_Banks_Maryland/main/FDIC/02%20Data/md_fdic_bank_locations.csv"
md_locations <- read.csv(data)


#use when you do not have access to internet
md_locations <- read.csv("FDIC/02 Data/md_fdic_bank_locations.csv")


edit(md_locations)
#concatenate columns to create address
#Will use to get lat and long coordinates for bank locations
md_locations <- md_locations %>% mutate(bank_address = paste(ADDRESS,CITY,STALP, ZIP,sep = " "))


#getting long and lat coordinates with mutate_geocode function from ggmap package
moco_pg_banks <-md_locations %>% filter(COUNTY  %in% c("Montgomery","Prince George'S")) %>% mutate_geocode(bank_address)

#changing the name to Prince George's from Prince George'S
md_locations$COUNTY[md_locations$COUNTY=="Prince George'S"] <- "Prince George's"

#indexing md_locations dataset to get banks in montgomery county only
moco<- md_locations[md_locations$COUNTY == "Montgomery",]

#indexing md_locations dataset to get banks in montgomery county only
pgco<- md_locations[md_locations$COUNTY == "Prince George's",]

#shows values without lat and long coordinates in dataset
missing_coordinates <- moco_pg_banks[!complete.cases(moco_pg_banks$lat),]




#write to local folder
write.csv(Moco_PG_Banks,"moco_pg_banks.csv")

#read in banks in moco and pg county
moco_pg_banks <- read.csv("moco_pg_banks.csv")


md_locations %>% count(ZIP, sort = TRUE) %>% head(15) %>% 
mutate(zip_code = reorder(ZIP,n)) %>% 
ggplot(mapping = aes(x = zip_code,y = n))+
geom_col(alpha = .50,  fill= "blue")+
geom_text(mapping = aes(label = n), nudge_y = -.75, color  ="white",fontface = "bold")+
coord_flip()+
theme_tufte()+
theme(plot.title = element_text(hjust = .075),plot.subtitle =element_text(hjust = .069),axis.title.y = element_blank())+
labs(title = "Number of Banks by Zipcode", subtitle = "Top 15 Zipcodes Statewide",  y = "Number of Banks by Zipcode")



which(names(md_locations) == "STNAME")

#number of FDIC Insured Banks in Maryland
md_locations %>% count(COUNTY,sort = TRUE) %>% 
head(10) %>% mutate(county_name = reorder(COUNTY,n)) %>% 
ggplot(mapping = aes(x = county_name,y = n, fill = county_name)) +
geom_col(fill = "darkgreen",color ="black")+
geom_text(aes(label = n),color = "white", nudge_y = -3, size =3)+
coord_flip()+
theme_minimal()+
theme(text = element_text(color = "navy"))+
labs(title =  "Number of FDIC Insured Banks in Maryland",x = "County Names", y = "Number of Banks")



#FDIC Insured  Banks by Metropolitan 
md_locations %>% count(CBSA, sort = TRUE) %>%
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
md_locations %>% count(NAME,sort = TRUE) %>% 
head(10) %>%  
mutate(bank_names = reorder(NAME,n)) %>% 
ggplot(mapping = aes(bank_names, n)) + 
geom_col(fill = "navy", color = "black", alpha = 0.5) + 
geom_text(aes(label = n),nudge_y = 5)+
coord_flip()+
theme_minimal()+
labs(title = "Number of Banks Branches by Association",  y = "Number of Banks")+
theme(axis.title.y =element_blank(), plot.title = element_text(hjust = .075))





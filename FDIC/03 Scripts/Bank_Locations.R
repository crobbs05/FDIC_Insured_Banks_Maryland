
setwd("R/FDIC/")
library(tidyverse)
library(ggthemes)
#read in FDIC insured bank locations within the State of Maryland
data <- "https://raw.githubusercontent.com/crobbs05/FDIC_Insured_Banks_Maryland/main/md_fdic_bank_locations.csv"

md_locations <- read.csv(data)

view(md_locations)

which(names(md_locations) == "STNAME")

#number of FDIC Insured Banks in Maryland
md_locations %>% count(COUNTY,sort = TRUE) %>% 
head(10) %>% mutate(county_name = reorder(COUNTY,n)) %>% 
ggplot(mapping = aes(x = county_name,y = n, fill = county_name)) +
geom_col(fill = "darkgreen",color ="black")+
geom_text(aes(label = n),color = "white", nudge_y = -3)+
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
  
md_locations <- md_locations %>% mutate(bank_address = paste(ADDRESS,CITY,STALP, ZIP,sep = " "))

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





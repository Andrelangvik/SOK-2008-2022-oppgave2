
library(readr) # fileformat of the dataset
library(ggplot2)     # the ggplot package
library(tidyverse)  # the tidyverse package

# To carry out the assignment, you will need to combine the union_unempl data with map data. 

union<- read_csv("union_unempl.csv") #This loads the data with information about the variables of interest
View(union) #Displays the data
#To combine the unemployment and union data with the map data, we merge on country name. 
#We face two problems here: 1) United Kingdom is called "UK" in the map data, 2) the variable "country" is called "region" in the map data. We need to change this.

#Changing the name of a single observation. The below code changes all observations called "United Kingdom" to "UK" in the union data. 
union$country <- gsub("United Kingdom", "UK", union$country)

View(union) 

# Renaming a variable. The below code renames the variable "Country" to "Region".
names(union)[names(union) == "country"] <- "region"
View(union) 

# The "Coord" variable takes 5 discrete levels. I
#t may therefore be better to use a discrete scale for the coloring. 
# To do this, simply replace 
#"scale_fill_gradient(name="name", low="color1", high="color2", na.value="grey50")" with 
#"scale_fill_brewer(name="name", palette = "Set1")" (or another set you prefer)



#Utfordring 2.3.1:

mapdata <- map_data("world") #provid World data with longitudes og lattitudes (cordrinates).
union <- left_join(mapdata, union, by = "region") #integrat mapdata with union data by region
union1 <- union %>% filter(!is.na(union$unempl)) #remove the countrys with "NA", it gives the variables from the orginal data sett.
union1


map1 <- ggplot(union1, aes(x = long, y = lat, group=group))+ 
  geom_polygon(aes(fill = unempl), color = "black") + 
  ggtitle("Map of unemploymentrate in Europa") + 
  scale_fill_gradient(name = "% unemploymentrate") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
map1

#Utfordring 2.3.2:

#fagforreningtetthet:
map2 <- ggplot(union1, aes(x = long, y = lat, group=group))+ 
  geom_polygon(aes(fill = density), color = "black") + 
  ggtitle("Map of union density in Europa") + 
  scale_fill_gradient(name = "% density", low = "yellow", high = "red", na.value = "grey") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
map2


union2 <- union1 %>% 
  mutate(excess_coverage = union1$coverage-union1$density)

#Excess coverage
map3 <- ggplot(union2, aes(x = long, y = lat, group=group))+ 
  geom_polygon(aes(fill = excess_coverage), color = "black") + 
  ggtitle("Map of Excess coverage in Europa") + 
  scale_fill_gradient(name = "% coverage", low = "yellow", high = "green", na.value = "grey") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
map3

#Koordinering av lønnsfastsettelse

map4 <- ggplot(union1, aes(x = long, y = lat, group=group))+ 
  geom_polygon(aes(fill = coord), color = "black") + 
  ggtitle("Map of Coordination of wage determination in Europa") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        rect = element_blank())
map4

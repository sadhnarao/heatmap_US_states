# load ggplot2 and requisites for map creation

install.packages("maps")

library(ggplot2)
library(viridis)
library(dplyr)
require(maps)
require(viridis)
theme_set(
  theme_void()
)


# Prepare the LS companies data
library(dplyr)
company_data<-read.csv("all_progs_companies.csv")
co_data <- data.frame(company_data)
co_data$region <- tolower(company_data$state)
head(co_data)

# change factor class to character

co_data %>% mutate(region= trimws(as.character(region))); 
#states_map <- region %>% mutate(region = trimws(as.character(region)))

#trim

co_data$region <- stringr::str_trim(co_data$region)


# Retrieve the states map data | code: https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
states_map <- data.frame(map_data("state"))
companies_map <- left_join(states_map, co_data, by=c("region"= "region"))

head(companies_map)


# # Get a data frame with longitude, latitude, and size of bubbles (a bubble = a city)
# data <- world.cities("capital") %>% filter(country.etc=="US")


# # Create the map
# ggplot(companies_map, aes(long, lat, group = group))+
#   geom_polygon(aes(fill = companies), color = "white")+
#   scale_fill_viridis_c(option = "C")
# 


# Create the map: Rescale Viridis https://stackoverflow.com/questions/48424682/how-do-i-limit-the-range-of-the-viridis-colour-scale
ggplot(companies_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = companies), color = "white")+
  scale_fill_viridis_c(direction =  -1, limits = c(0, 42),oob = scales::squish, rescaler = function(x, to = c(0, 1), from = NULL) {
    ifelse(x<42, 
           scales::rescale(x,
                           to = to,
                           from = c(min(x, na.rm = TRUE), 42)),
           1)}) + guides(fill = guide_colourbar(barwidth = 0.5, barheight = 20)) 


# using scale fill to try and create a non linear scale

ggplot(companies_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = companies), color = "white")+
  
  scale_fill_gradientn( colours = viridis(11, option = "B", direction = -1), #https://www.rdocumentation.org/packages/viridisLite/versions/0.3.0/topics/viridis
                       breaks=c(0, 5,10,15,20, 25,30,40,50),
                       limits=c(0,45), 
                       guide = guide_colorbar(barwidth = 0.8, barheight = 18),
                       na.value = "#39568CFF", 
                       trans = "identity", rescaler = function(x, to = c(0, 1), from = NULL) {
                         ifelse(x<42, 
                                scales::rescale(x,
                                                to = to,
                                                from = c(min(x, na.rm = TRUE), 42)),
                                1)}) + guides(fill = guide_colourbar(barwidth = 0.5, barheight = 20))



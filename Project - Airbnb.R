library(ggplot2)
library(tidyverse)
airbnb1 <-
  read.csv('train.csv')

#An overall look of the dataset
head(airbnb)
summary(airbnb)
names(airbnb)


library(Hmisc)
library(tidyverse)






####1.Cleaning Data
#a.Drop meaningless columns
airbnb2 <- airbnb1[, !names(airbnb1) %in% c('thumbnail_url','reviews','description',
                                            'amenities','name','id')]
#b.Check null value
sapply(airbnb2, function(x) sum(is.na(x)))

#c.Review score rating impute by mean
airbnb2$review_scores_rating <-impute(airbnb2$review_scores_rating, mean)

#d.Delete records that has blank value in host information
airbnb2 <- airbnb2 %>% filter(host_has_profile_pic  != '' | host_identity_verified  != "" | host_since != "") 

#e.Delete records that has null value in any one column
airbnb3 <-na.omit(airbnb2)

#f.Deal with many value in property types
airbnb3 <-  airbnb3 %>%
  mutate(prop_type = case_when(property_type == "Apartment" ~ "Apartment", 
                                         property_type == "House" ~ "House", 
                                         property_type == "Condominium" ~ "Condominium", 
                                         property_type == "Townhouse" ~ "Townhouse",
                                         property_type == " Loft" ~ "Loft",
                                         TRUE ~ "Other")) %>% 
  mutate(host_year = as.numeric(substr(host_since,0,4))) %>% 
  mutate(host_years = 2019 - host_year)





####2. Data Visualization
#a.Correlation Map
install.packages("corrplot")
library(corrplot)
correlation <- airbnb3[,!names(airbnb3) %in% c('latitude','longitude')]
nums <- unlist(lapply(correlation, is.numeric))
mydata.cor = cor(correlation[,nums])
corrplot(mydata.cor, method = "number")
#b. other data visualization parts
# accommodates vs price correlation
ggplot(data = airbnb3 , aes(x=accommodates, y= log_price)) +
  geom_jitter(alpha = 1, aes(color = city)) +
  facet_wrap(~city) +
  geom_smooth(method = "lm") +
  theme(legend.position="right",axis.text.x=element_text(angle=90, hjust=1)) +
  labs (title="Accommodates/Price Correlation", x="Accommodates", y="Price (in natural log)") +
  theme (plot.title=element_text(hjust=0.5))

# price per city
ggplot(airbnb3, aes(x=as.factor(city),y=log_price,color=city)) + geom_boxplot() +
  labs (title="Price per each city", x="City", y="Log price") +
  theme (plot.title=element_text(hjust=0.5))

airbnb3 %>% group_by (city) %>% summarise(numbers = n()) %>%
  ggplot(aes(x=city, y=numbers)) +
  labs (x="City", y="Numbers of observations") +
  ggtitle("Numbers of observations for each city") +
  geom_bar(stat='identity',aes(fill=numbers)) +
  geom_text(aes(label=numbers), position=position_dodge(width=0.9), vjust = -0.25) +
  theme (plot.title=element_text(hjust=0.5))

# price per roomtypes
ggplot(airbnb3, aes(x=as.factor(room_type),y=log_price,color=room_type)) + geom_boxplot() +
  labs (title="Price per each room types", x="room types", y="Log price") +
  theme (plot.title=element_text(hjust=0.5))

# price per property types
ggplot(airbnb3, aes(x=as.factor(prop_type),y=log_price,color=prop_type)) + geom_boxplot() +
  labs (title="Price per each property types", x="Property types", y="Log price") +
  theme (plot.title=element_text(hjust=0.5))

# Numbers of reviews/ review score rating and host years vs price
# Review score ratings
airbnb3%>% mutate(accommodates_group = case_when(
  accommodates >= 13 ~ "Above 13",
  accommodates >= 10 ~ "10-12",
  accommodates >= 7 ~ "7-9",
  accommodates >= 4 ~ "4-6",
  accommodates >= 1 ~ "1-3"
)) %>%
  ggplot(aes(x=review_scores_rating, y= log_price)) +
  geom_jitter(alpha = 1, aes(color = accommodates_group)) +
  facet_wrap(~city) +
  geom_smooth(method = "lm") +
  theme(legend.position="right",axis.text.x=element_text(angle=90, hjust=1)) +
  labs (title="Review_Score_Rating/Price Correlation", x="Review_Score_Rating", y="Price (in natural log)") +
  theme (plot.title=element_text(hjust=0.5))
# Number of reviews
airbnb3%>% mutate(accommodates_group = case_when(
  accommodates >= 13 ~ "Above 13",
  accommodates >= 10 ~ "10-12",
  accommodates >= 7 ~ "7-9",
  accommodates >= 4 ~ "4-6",
  accommodates >= 1 ~ "1-3"
)) %>%
  ggplot(aes(x=number_of_reviews, y= log_price)) +
  geom_jitter(alpha = 1,  aes(color = accommodates_group)) +
  facet_wrap(~city) +
  geom_smooth(method = "lm") +
  theme(legend.position="right",axis.text.x=element_text(angle=90, hjust=1)) +
  labs (title="Numbers of review/Price Correlation", x="Review_Score_Rating", y="Price (in natural log)") +
  theme (plot.title=element_text(hjust=0.5))

# Host years
airbnb3%>% mutate(accommodates_group = case_when(
  accommodates >= 13 ~ "Above 13",
  accommodates >= 10 ~ "10-12",
  accommodates >= 7 ~ "7-9",
  accommodates >= 4 ~ "4-6",
  accommodates >= 1 ~ "1-3"
)) %>%
  ggplot(aes(x=host_years, y= log_price)) +
  geom_jitter(alpha = 1, aes(color = accommodates_group)) +
  facet_wrap(~city) +
  geom_smooth(method = "lm") +
  theme(legend.position="right",axis.text.x=element_text(angle=90, hjust=1)) +
  labs (title="Host years/Price Correlation", x="Host years", y="Price (in natural log)") +
  theme (plot.title=element_text(hjust=0.5))


#c.heat map for LA and NYC
# Summarizing price by zipcode for LA
airbnb3_la<- airbnb3 %>% filter(city == 'LA') %>% group_by(zipcode) %>%
  summarise(
    number = n(),
    avg = mean(log_price))
airbnb3_la

# Avg price by zip for NYC
airbnb3_NYC<- airbnb3 %>% filter(city == 'NYC') %>% group_by(zipcode) %>%
  summarise(
    number = n(),
    avg = mean(log_price))
airbnb3_NYC


# Prepare the zip poly data for US
mydata <- readOGR(dsn = ".", layer = "cb_2016_us_zcta510_500k")

# Texas zip code data
zip <- read_csv("zip_code_database.csv")
la <- filter(zip, primary_city == "Los Angeles")


# Get polygon data for la only
mypoly <- subset(mydata, ZCTA5CE10 %in% la$zip)

# Create a new group with the first 5 digit.
# Drop unnecessary factor levels.
# Add a fake numeric variable, which is used for coloring polygons later.

mypoly$group <- substr(mypoly$ZCTA5CE10, 1,5)
mypoly$ZCTA5CE10 <- droplevels(mypoly$ZCTA5CE10)


set.seed(111)
mypoly$value <- sample.int(n = 10000, size = nrow(mypoly), replace = TRUE)
mypoly$value <- airbnb3_la$avg

# Merge polygons using the group variable
# Create a data frame for ggplot.
mypoly.union <- unionSpatialPolygons(mypoly, mypoly$group)

mymap <- fortify(mypoly.union)

# Check how polygons are like

plot(mypoly)
plot(mypoly.union, add = T, border = "red", lwd = 1)

# Convert SpatialPolygons to data frame and aggregate the fake values
mypoly.df <- as(mypoly, "data.frame") %>%
  group_by(group) %>%
  summarise(value = sum(value))

# Find a center point for each zip code area
centers <- data.frame(gCentroid(spgeom = mypoly.union, byid = TRUE))
centers$zip <- rownames(centers)

# Finally, drawing a graphic
ggplot() +
  geom_cartogram(data = mymap, aes(x = long, y = lat, map_id = id), map = mymap) +
  geom_cartogram(data = airbnb3_la, aes(fill = avg, map_id = zipcode), map = mymap) +
  geom_text_repel(data = centers, aes(label = zip, x = x, y = y), size = 3) +
  scale_fill_gradientn(colours = rev(brewer.pal(10, "Spectral"))) +
  coord_map() +
  theme_map()



##d.Another Version of heat map
#Airbnb price, Los Angeles
library(ggmap)
ggmap::register_google(key = "AIzaSyCJZ2XRR76SqYPIzMP6s-1mXn3eKXKgDTo")
library(data.table)
suppressMessages(library(gdata))
suppressMessages(library(dplyr))
library(rbokeh)
library(RColorBrewer)

apt_s <- airbnb3 %>% select(longitude,latitude,log_price,neighbourhood,zipcode)
p <- ggmap(get_googlemap(center = c(lon = -118.243683, lat = 34.052235),
                         zoom = 9, scale = 2,
                         maptype ='roadmap',
                         color = "bw"))
p + geom_point(aes(x = longitude, y = latitude,  colour = col1), data = apt_s,alpha=0.25, size = 0.5) + 
  theme(legend.position="bottom") + labs(x = "Longitude", y = "Latitude", title = "Airbnb price, Los Angeles")

#How many posts in LA
library(ggmap)
library(ggplot2) 
library(tidyverse)
apt_la <- airbnb3 %>% filter(city =='LA')
LA = c(min(apt_la$longitude), min(apt_la$latitude), max(apt_la$longitude), max(apt_la$latitude)) 
LA = c(--118.542587, 33.745571, -117.9889525, 34.16945)
get_map(location = LA, source = "google", maptype = "roadmap", crop = FALSE, color = "bw") -> LA_map 
ggmap(LA_map) +
  stat_density2d(data=apt_la, aes(x = longitude, y = latitude,
                                  fill = ..level..,
                                  alpha = ..level..), geom = "polygon") +  
  
  scale_fill_gradient(low = "#3BE819", high = "#B5170B") +  
  
  theme(axis.text=element_blank(), panel.grid=element_blank(), axis.title=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), legend.position="none",
        panel.background=element_blank(), plot.title = element_text(face="bold", colour="black")) +  
  
  labs(title = "Listing density plot by zip code, LA") +  
  
  scale_alpha_continuous(range=c(0.1,0.4)) 


#How many posts in NY
apt_ny <- airbnb3 %>% filter(city =='NYC')
NY = c(min(apt_ny$longitude), min(apt_ny$latitude), max(apt_ny$longitude), max(apt_ny$latitude)) 
get_map(location = NY, source = "google", maptype = "roadmap", crop = FALSE, color = "bw",zoom=13) -> NY_map 
ggmap(NY_map) +
  stat_density2d(data=apt_ny, aes(x = longitude, y = latitude,
                                  fill = ..level..,
                                  alpha = ..level..), geom = "polygon") +  
  
  scale_fill_gradient(name = "Listing Number",low = "#3BE819", high = "#B5170B") +  
  
  theme(axis.text=element_blank(), panel.grid=element_blank(), axis.title=element_blank(),
        axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), legend.position="none",
        panel.background=element_blank(), plot.title = element_text(face="bold", colour="black")) +  
  
  labs(title = "Listing density plot by zip code,NYC") +  
  
  scale_alpha_continuous(range=c(0.1,0.4))
#https://stackoverflow.com/questions/45319970/generating-spatial-heat-map-via-ggmap-in-r-based-on-a-value
#https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/ggmap/ggmapCheatsheet.pdf
#https://www.littlemissdata.com/blog/maps



####3.Model
#a.Intuitive Model
lm_intuitive <- lm(exp(log_price) ~ accommodates+ beds+  bedrooms + bathrooms + 
                      number_of_reviews + review_scores_rating, data= airbnb3)
summary(lm_intuitive)
extractAIC(lm_intuitive)
plot(lm_intuitive)

#b.Log_transformed Model

lm_intuitive_log<- lm(log_price ~ accommodates+ beds+  bedrooms + bathrooms + 
                     number_of_reviews + review_scores_rating, data= airbnb3)
summary(lm_intuitive_log)
extractAIC(lm_intuitive_log)

#c.Final Model

lm_final <-lm(log_price ~  accommodates+ beds+  bedrooms + bathrooms + 
            as.factor(room_type) + as.factor(city) +
            as.factor(prop_type)+ review_scores_rating, data= airbnb3)
summary(lm_final) # 0.5505
plot(lm_final)
extractAIC(lm_final) #101242.5




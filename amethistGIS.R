
library(sf)
library(tidyverse)
library(ggmap)
library(ggpubr) #Combining plots
theme_set(theme_pubr())

# import data and convert to spatial format
BangweBoundary <- read_csv("data/BangwePoly.csv") %>%
  st_as_sf(wkt = "WKT", crs = 4326)

NaperiBoundary <- read_csv("data/NaperiPoly.csv") %>%
  st_as_sf(wkt = "WKT" , crs = 4326)

ChirimbaBoundary <- read_csv("data/ChirimbaPoly.csv") %>%
  st_as_sf(wkt = "WKT" , crs = 4326)

#Read Balntyre Boundary
BtBoundary <- read_csv("data/blantyre_boundary.csv") %>%
  st_as_sf(wkt = "WKT" , crs = 4326)



#Import sex worker data points
SexWorkVenues <- read_csv("data/waypoints_Combined.csv") %>% st_as_sf (coords = c("Longitude", "Latitude"), crs = 4326)

#Define the sex work venues that fall inside a polygon
Bangwe_SexV <- st_intersection(SexWorkVenues, BangweBoundary)

Chirimba_SexV <- st_intersection(SexWorkVenues, ChirimbaBoundary)

Naperi_SexV <- st_intersection(SexWorkVenues, NaperiBoundary)

#Append the three data frames created
AllSexVenues <- bind_rows(Bangwe_SexV,Chirimba_SexV,Naperi_SexV)

AllPolygons <- bind_rows(BangweBoundary, ChirimbaBoundary, NaperiBoundary)
head(AllSexVenues)

AllPolygons <- rename(AllPolygons,Cluster=Name)


#Define colors to be used
myColors <- c("#440154", "#DBC505", "#4699dd")

plot1 <- ggplot()+
  geom_sf(data = BtBoundary, color="black", size=0.8)+
  geom_sf(data = AllPolygons, aes(fill = Cluster))+
  labs(title = "Sex Work Venue Distribution") +
  annotation_scale(location="br")+
  annotation_north_arrow(location="tl")+
  scale_fill_manual(values = c("#969696", "#72c4f7","#56ddc5"), name= "Cluster")+
  geom_sf(data = AllSexVenues, aes(col=Institution, shape=Institution), size=4)+
  scale_color_manual(values=myColors)+
  theme(legend.position = "right")


#Bargraph showing sex work variation between MLW and Pakachere
plot2 <- ggplot(AllSexVenues) +
  aes(x = Label, fill = Institution) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c(MLW = "#440154",
                               Pakachere = "#DBC505")) +
  labs(x = "Sex Work Venue", y = "Count", title = "Sex Work Venue Variations") +
  theme_gray() +
  theme(plot.title = element_text(size = 13L, hjust = 0.5), axis.title.y = element_text(size = 11L),
        axis.title.x = element_text(size = 11L))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


PlotBangwe <- ggplot()+
  geom_sf(data = BangweBoundary)+
  labs(title = "Sex Work Venue Distribution in Bangwe Cluster") +
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr")+
  scale_fill_manual(values = c("#969696", "#72c4f7","#56ddc5"), name= "Cluster")+
  geom_sf(data = Bangwe_SexV, aes(col=Institution, shape=Institution), size=4)+
  scale_color_manual(values=myColors)+
  theme_gray()

PlotNaperi <- ggplot()+
  geom_sf(data = NaperiBoundary)+
  labs(title = "Sex Work Venue Distribution in Naperi Cluster") +
  annotation_scale(location="br")+
  annotation_north_arrow(location="tl")+
  scale_fill_manual(values = c("#969696", "#72c4f7","#56ddc5"), name= "Cluster")+
  geom_sf(data = Naperi_SexV, aes(col=Institution, shape=Institution), size=4)+
  scale_color_manual(values=myColors)+
  theme_gray()


PlotChirimba <- ggplot()+
  geom_sf(data = ChirimbaBoundary)+
  labs(title = "Sex Work Venue Distribution in Chirimba Cluster") +
  annotation_scale(location="br")+
  annotation_north_arrow(location="tr")+
  scale_fill_manual(values = c("#969696", "#72c4f7","#56ddc5"), name= "Cluster")+
  geom_sf(data = Chirimba_SexV, aes(col=Institution, shape=Institution), size=4)+
  scale_color_manual(values=myColors)+
  theme_gray()

#Sex Work Variation Per Cluster
plot3 <- ggplot(AllSexVenues) +
  aes(x = Name, fill = Institution) +
  labs(title = "Sex Work Venue Variations in Clusters") +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c(MLW = "#440154",
                               Pakachere = "#DBC505")) +
  labs(x = "Cluster Name", y = "Count") +
  theme_gray() +
  theme(plot.title = element_text(hjust = 0.5))


figure1 <- ggarrange(PlotBangwe, PlotNaperi, PlotChirimba, plot3,
                     labels = c("A", "B", "C", "D"),
                     ncol = 2, nrow = 2)
figure1



figure2 <- ggarrange(plot1, plot2,
                     labels = c("A", "B"),
                     ncol = 2, nrow = 1)
figure2

EnumerationMain <- st_read("data/Enumeration areas 2018/2018_Malawi_Enumeration_Areas.shp")


BTUrbanEnumberation <- filter(EnumerationMain, DIST_NAME== "Blantyre City")

BTUrbanEnumberation <- BTUrbanEnumberation %>%
  mutate(combi_code = case_when(TA_NAME == 'Bangwe Mthandizi Ward' ~ 1,
                                TA_NAME == 'Bangwe Ward' ~ 2,
                                TA_NAME == 'Blantyre City Centre Ward' ~ 3,
                                TA_NAME == 'Blantyre South Ward'  ~ 4,
                                TA_NAME == 'Chigumula Ward' ~ 5,
                                TA_NAME == 'Chilomoni Ward' ~ 6,
                                TA_NAME == 'Green Corner Ward' ~ 7,
                                TA_NAME == 'Limbe Central Ward' ~ 8,
                                TA_NAME == 'Mapanga Ward' ~ 9,
                                TA_NAME == 'Mbayani Ward' ~ 10,
                                TA_NAME == 'Michiru Ward' ~ 11,
                                TA_NAME == 'Misesa Ward' ~ 12,
                                TA_NAME == 'Mzedi Ward' ~ 13,
                                TA_NAME == 'Namalimwe Ward' ~ 14,
                                TA_NAME == 'Namiyango Ward' ~ 15,
                                TA_NAME == 'Ndirande Gamulani' ~ 16,
                                TA_NAME == 'Ndirande Makata Ward' ~ 17,
                                TA_NAME == 'Ndirande Matope Ward' ~ 18,
                                TA_NAME == 'Nkolokoti Ward' ~ 19,
                                TA_NAME == 'Nyambadwe Ward' ~ 20,
                                TA_NAME == 'Soche East Ward' ~ 21,
                                TA_NAME == 'Soche West Ward' ~ 22,
                                TA_NAME == 'South Lunzu Ward' ~ 23,
                                TA_NAME == 'Ndirande Gamulani Ward' ~ 24))



#write.csv(BTUrbanEnumberation,"data\\BTEnumeration.csv", row.names = FALSE)

BTUrbanEnumerationGrouped <- BTUrbanEnumberation %>%
  group_by(combi_code) %>%
  st_union()


# Draw the newly grouped polygons
BT_ea <- unique(BTUrbanEnumberation$combi_code)
combine_BTea <- list()
for(i in 1:length(BT_ea)){
  combine_BTea[[i]] <- filter(BTUrbanEnumberation,combi_code==BT_ea[i])
}

BTEAMerged <- lapply(combine_BTea,FUN = st_union)
plot(BTUrbanEnumerationGrouped)
for(i in 1:length(BT_ea)){
  plot(BTEAMerged[[i]],add=TRUE)
}


BTEAMerged <- lapply(combine_BTea,FUN = st_union)

plot(BTUrbanEnumerationGrouped)

for(i in 1:length(BT_ea)){plot(BTEAMerged[[i]],add=TRUE)}


for(i in 1:length(BT_ea)){
  merged <- write.csv(BTEAMerged[[i]],add=TRUE)
  write.csv(merged, file="merged1.csv")}

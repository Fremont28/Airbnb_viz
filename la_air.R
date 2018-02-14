la_air<-read.csv(file.choose(),header=TRUE) 

#import libraries 
library(rworldmap) 
library(plyr)
library(ggmap)

#count neighborhoods
barrios_la<-count(la_air,la_air$neighbourhood)
barrios_la

#neighborhood mean lat/long
barrios_loc<-aggregate(la_air[,7:8], list(la_air$neighbourhood), mean)
barrios_loc
#average price in neighborhoods
barrios_price<-aggregate(la_air[,10], list(la_air$neighbourhood), mean)
barrios_price 

#combine the two dataframes
la_barrios_final<-cbind(barrios_la,barrios_loc,barrios_price)
names(la_barrios_final) 
colnames(la_barrios_final)[1]<-"neighbourhood"
colnames(la_barrios_final)[7]<-"price"

#greater than 450 airbnb listings
la_barrios_final1<-subset(la_barrios_final,n>450)
write.csv(la_barrios_final1,file="barrios.csv")
colnames(la_barrios_final1)[1]<-"neighbourhood"
colnames(la_barrios_final1)[7]<-"price"

map_la <- get_map(location = c(lon = mean(la_air$longitude), lat = mean(la_air$latitude)), zoom = 10,
                      maptype = "satellite", scale = 2)

# plotting the map with some points on it source: https://stackoverflow.com/questions/14288001/geom-text-not-working-when-ggmap-and-geom-point-used
ggmap(map_la) +
  geom_point(data = la_barrios_final1, aes(x = longitude, y = latitude,size=n,colour=neighbourhood))+
  guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  geom_text(data = la_barrios_final1, aes(x = longitude, y = latitude, label = neighbourhood), 
            size = 3, vjust = 0, hjust = -0.5,color="green")+xlab("")+ylab("")+
  ggtitle("Venice Beach Draws in the Crowds")+theme(plot.title = element_text(hjust = 0.5))

#price point neighborhoods
ggmap(map_la) +
  geom_point(data = la_barrios_final1, aes(x = longitude, y = latitude,size=n,colour=price))+
  guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  geom_text(data = la_barrios_final1, aes(x = longitude, y = latitude, label = neighbourhood), 
            size = 3, vjust = 0, hjust = -0.5,color="green")













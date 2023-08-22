require(pacman)
p_load(rnaturalearth,rnaturalearthdata,rgdal,sf,ggplot2,plotKML,dplyr,galacticPubs)
asia<-ne_countries(continent="Asia",returnclass="sf")
plot(asia)

world<-ne_countries(returnclass="sf")



#Get data from online database
f <- rnaturalearth::ne_download(scale=50,type="geography_regions_polys",category="physical", returnclass = "sf") 

#extract borneo
borneo<-subset(f,LABEL=="BORNEO")
borneo<-cbind(borneo,st_coordinates(st_centroid(borneo)))
borneo$Name<-"Borneo"
#Zoomed out orienting view
ggplot()+geom_sf(data=world)+xlim(90,151)+ylim(-41,27)+theme_void()+
  geom_sf(data=borneo,fill=gpColors("flare"),alpha=.4,color="transparent")

gpsave("Borneo_map.png")

#Zoom in for detail of 3 countries
ggplot()+geom_abline(intercept=0,slope=0,linetype="dashed",size=1.1,color="#363636")+
  annotate("text",x=145,hjust=0.95,y=2,label="Equator",family="Montserrat",size=7,color="#363636")+
  geom_sf(data=world)+xlim(95,145)+ylim(-20,20)+theme_void()+
  geom_sf(data=borneo,fill=gpColors("flare"),alpha=.4,color="transparent")

gpsave("Borneo_map_zoomed.png")



 ## Make political map showing the 3 countries that claim borneo
countries<-rnaturalearth::ne_download(scale=50,type="countries", returnclass = "sf")
Malaysia<-subset(countries,NAME=="Malaysia")
Indonesia<-subset(countries,NAME=="Indonesia")
Brunei<-subset(countries,NAME=="Brunei")

island<-geom_sf(data=borneo,color=gpColors("galactic black"),alpha=0,size=1)
#no countries
(b1<-ggplot()+geom_abline(intercept=0,slope=0,linetype="dashed",size=1.1,color="gray80")+geom_sf(data=world)+xlim(95,145)+ylim(-20,20)+theme_void()+island
)
gpsave("Borneo_map_1-no-countries.png")

#  Malaysia
(b2<-b1+geom_sf(data=Malaysia,inherit.aes = F,fill=gpColors("lightning purple"),color=gpColors("lightning purple"),alpha=.7))+island
gpsave("Borneo_map_2-malaysia.png")

#Indonesia
(b3<-b2+geom_sf(data=Indonesia,inherit.aes = F,fill=gpColors("hy"),color=gpColors("hy"),alpha=.7))+island
gpsave("Borneo_map_3-malay+indo.png")

##Brunei
(b4 <- b3+ geom_sf(data=Brunei,inherit.aes = F,fill="#FFC300",color="#FFC300",alpha=1))+island
  gpsave("Borneo_map_4-malay+indo+brunei.png")

# +
#   geom_sf(data=borneo,col=gpColors("flare"),alpha=1,fill="transparent")
  #+
  #geom_text(data=borneo,aes(X,Y,label=LABEL),size=7,fontface="bold")+

#export Borneo map to kml
st_write(borneo %>% dplyr::select(Name,geometry),"assets/R/borneo.kml",driver="kml",append=FALSE)
# maptools::kmlPoints(obj=as.SpatialPoints(st_coordinates(st_centroid(borneo))),"assets/R/borneo.kml",kmlname="label",name="BORNEO")

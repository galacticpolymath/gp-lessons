require(pacman)
p_load(rnaturalearth,rnaturalearthdata,rgdal,sf,sp,ggplot2,plotKML,dplyr,galacticPubs,
       maptools,ggmap)
na<-ne_countries(continent="North America",returnclass="sf")
states<-ne_download(scale=50,type="states")

plot(na)
plot(states)

#Get just Tennessee and Kentucky state boundaries
ky_tn<-subset(states,name%in%c("Tennessee","Kentucky"))
plot(ky_tn)

world<-ne_countries(returnclass="sf")

plot(ky_tn+na)

#Get data from online database
all_rivers <- rnaturalearth::ne_download(scale=10,type="rivers_north_america",category="physical",returnclass="sp")
barren <- subset(all_rivers,label=="Barren")
#, returnclass = "sf" (needed for ggplot2)
#export Barren River info to KML
# st_write(barren %>% dplyr::select(name,geometry),"assets/_r_outputs/barren.kml",driver="kml",append=FALSE)

#Export river KML
maptools::kmlLines(barren,kmlfile = "assets/_r_outputs/barren.kml",col="cyan",lwd = 3
                    )

#Export state outlines
maptools::kmlPolygons(ky_tn,
                      kmlfile = "assets/_r_outputs/ky_tn.kml",
                      col = "#4530384C",
                      border = "gray80")


#Export study locations
#geocode requires setting up account with Google Cloud API
site_names<-c("East Fork of the Barren River in Monroe County, Kentucky","line creek, clay county, tn")
study_locations<-ggmap::geocode(location=site_names)

maptools::kmlPoints(sp::SpatialPointsDataFrame(study_locations,
                                               data=data.frame(name=site_names)),
                      kmlfile = "assets/_r_outputs/study_sites.kml",
                    name=c('Barren River "East Fork"',"Line Creek")

                    )

#extract borneo
# borneo<-cbind(borneo,st_coordinates(st_centroid(borneo)))
#
#
# #Zoom in for detail of 3 countries
# ggplot()+
#   # annotate("text",x=145,hjust=0.95,y=2,label="Equator",family="Montserrat",size=7,color="#363636")+
#   geom_sf(data=ky_tn,fill=gpColors("flare"),alpha=.4,color="gray50")+
#   geom_sf(data=barren)+theme_void()
#
# gpsave("Borneo_map_zoomed.png")
#
#
#
#  ## Make political map showing the 3 countries that claim borneo
# countries<-rnaturalearth::ne_download(scale=50,type="countries", returnclass = "sf")
# Malaysia<-subset(countries,NAME=="Malaysia")
# Indonesia<-subset(countries,NAME=="Indonesia")
# Brunei<-subset(countries,NAME=="Brunei")
#
# island<-geom_sf(data=borneo,color=gpColors("galactic black"),alpha=0,size=1)
# #no countries
# (b1<-ggplot()+geom_abline(intercept=0,slope=0,linetype="dashed",size=1.1,color="gray80")+geom_sf(data=world)+xlim(95,145)+ylim(-20,20)+theme_void()+island
# )
# gpsave("Borneo_map_1-no-countries.png")
#
# #  Malaysia
# (b2<-b1+geom_sf(data=Malaysia,inherit.aes = F,fill=gpColors("lightning purple"),color=gpColors("lightning purple"),alpha=.7))+island
# gpsave("Borneo_map_2-malaysia.png")
#
# #Indonesia
# (b3<-b2+geom_sf(data=Indonesia,inherit.aes = F,fill=gpColors("hy"),color=gpColors("hy"),alpha=.7))+island
# gpsave("Borneo_map_3-malay+indo.png")
#
# ##Brunei
# (b4 <- b3+ geom_sf(data=Brunei,inherit.aes = F,fill="#FFC300",color="#FFC300",alpha=1))+island
#   gpsave("Borneo_map_4-malay+indo+brunei.png")
#
# # +
# #   geom_sf(data=borneo,col=gpColors("flare"),alpha=1,fill="transparent")
#   #+
#   #geom_text(data=borneo,aes(X,Y,label=LABEL),size=7,fontface="bold")+
#
# #export Borneo map to kml
# st_write(borneo %>% dplyr::select(Name,geometry),"assets/R/borneo.kml",driver="kml",append=FALSE)
# # maptools::kmlPoints(obj=as.SpatialPoints(st_coordinates(st_centroid(borneo))),"assets/R/borneo.kml",kmlname="label",name="BORNEO")

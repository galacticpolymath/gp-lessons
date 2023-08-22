# make figures
# 
require(pacman)
p_load(galacticEdTools,tidyverse,galacticPubs)

calling<-read_csv("data/orig-client-data_NoEdit/L.pa_Calling rate data.csv")
calling$hr<-gsub("00h","",calling$Time_Block)

#Male calling rate
g0 <- calling  %>% 
      ggplot(aes(y = Calls_per_hour, x = hr,col=Sex, shape=Sex)) + 
      geom_point(size=1.5) + 
      theme_galactic(text.cex=1.5,font.face = c(1,4,4)) + theme(legend.text = element_text(face=4,colour=gpColors("hydr")),
                                                                axis.title=element_text(colour=gpColors("hydr"))) +
      xlab("Time (hour of day)") + 
      ylab("Calls per Hour")+
      ggtitle("Figure 1. Guardian Frog Call Rates by Sex")

(g<-g0+scale_shape_manual(values=c(21,23))+
      scale_color_manual(values=c(gpPal[[2]]$hex[1],gpPal[[2]]$hex[13])) )
gpsave("frog-call-rates_LABELED.png")

g0+blank_labs(title_txt=NA,font.face=1)+scale_color_manual(values=c(gpPal[[2]]$hex[1],gpPal[[2]]$hex[13]),labels=c("____________","____________"))+scale_shape_manual(values=c(21,23),labels=c("____________","____________"))+theme(legend.key.size = unit(2, 'lines'),
                                                                                                                                                                                                                         legend.key.width=unit(1,"line"))

gpsave("frog-call-rates_BLANK.png") 
                                                  
#output data table for activity

tbl<-rbind(head(calling,4),tail(calling,4,keepnums=T))[,-3]
names(tbl)[3]<-"hour_of_day"
tbl
write.csv(tbl,"data/part2_data_table.csv")

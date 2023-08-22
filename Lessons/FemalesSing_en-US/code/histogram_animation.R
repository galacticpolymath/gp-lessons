#Install/load pacman
if(!require(pacman)){install.packages("pacman");require(pacman)}
#Install/load tons of packages
p_load(ggplot2,tidyverse,lubridate,ggpubr,gganimate,grid,viridis,gifski,transformr)

# #bring in GP styling
# source("https://raw.githubusercontent.com/galacticpolymath/ggGalactic/master/ggGalactic.R")
show.gpPal(2)
data(trees)
range(trees$Height)
breaksNlabs <- seq(60,90,5)




h<-ggplot(trees,aes(x=Height))+geom_histogram(breaks=breaksNlabs,col=gpPal[[2]]$hex[16],fill=gpPal[[2]]$hex[11])+ggGalactic()+labs(x="Tree Height (ft)",y="Count",title="Histogram of Cherry Tree Heights")+scale_x_continuous(labels=breaksNlabs, breaks = breaksNlabs)+scale_y_continuous(breaks=seq(0,10,1),labels=c(0:10))#,"",2,"",4,"",6,"",8,"",
h
ggsave("assets/cherry-trees_hist.png",width=10,height=6)


#Reverse engineer a dotplot from this, cuz dotplot is soooo stupid!
brks<-h[["scales"]][["scales"]][[1]][["breaks"]]
mids<-sapply(1:(length(brks)-1),function(i){median(c(brks[i],brks[i+1]))})

broken<-cut(trees$Height,brks,labels=F)
key<-sort(unique(broken))
names(key)<-paste0("`",mids,"`")
dotX<-sort(recode(as.character(broken),"1"="62.5","2"="67.5","3"="72.5","4"="77.5","5"="82.5","6"="87.5" ))
dotY<-unlist(sapply(table(dotX),function(x) 1:x))
x.orig=sort(trees$Height)
y.orig=unlist(sapply(table(x.orig),function(x) 1:x))

dot<-data.frame(x=c(x.orig,as.numeric(dotX),x.orig),y=c(y.orig,as.numeric(dotY),y.orig),state=c(sapply(1:3,function(x) rep(x,length(x.orig)))))
dot$col=factor(rep(sort(broken),3))
#Unbinned dot plot
binless<-ggplot(dot %>% subset(state==1),aes(x=x,y=y))+geom_point(size=5)+ggGalactic()+labs(x="Tree Height (ft)",y="Count",title="Dot Plot of Tree Heights")+scale_y_continuous(breaks=seq(0,10,2),limits=c(0,10),labels=seq(0,10,2))+scale_x_continuous(labels=breaksNlabs, breaks = breaksNlabs,limits=c(60,90))
binless
ggsave("assets/cherry-trees_Dotplot-unbinned.png",width=10,height=6)

#Add bins
#arrows: paste0('\u2190',binlabs,'\u2192')
binlabs<-sapply(1:(length(brks)-1),function(i) {paste0(" >",brks[i]," : ",brks[i+1]," ")})

fills<-viridis::viridis(6)
cols<-c(rep("white",3),rep(1,3))#grey.colors(6,start=0, end=1,rev=T)[c(1,1,1,1,)]


binful<-ggplot(dot %>% subset(state==1),aes(x=x,y=y))+ggGalactic()+labs(x="Tree Height (ft)",y="Count",title="Dot Plot of Tree Heights")+scale_y_continuous(breaks=seq(0,10,2),limits=c(0,10),labels=seq(0,10,2))+scale_fill_manual(values=fills)+scale_x_continuous(labels=breaksNlabs, breaks = breaksNlabs,limits=c(60,90))+geom_vline(xintercept=seq(60,90,5))+geom_point(size=5,aes(fill=col),col=1,shape=21,stroke=1,show.legend=F)+geom_label(data=data.frame(mids),aes(x=mids,y=.25),label=binlabs,hjust=0.5,size=6,fill=fills,col=cols)+ ggtitle("Dot Plot with 5 ft. Bins")
binful
ggsave("assets/cherry-trees_dotplot-labeled-bins.png",width=10,height=6)



#Binning Dot Plot Animation
(binz0<-ggplot(dot,aes(x=x,y=y-.5))+ggGalactic()+labs(x="Tree Height (ft)",y="Count",title="Combining Data Points into Bins")+scale_y_continuous(breaks=seq(0,10,2),limits=c(-1,10),labels=seq(0,10,2))+scale_fill_manual(values=fills)+scale_x_continuous(labels=breaksNlabs, breaks = breaksNlabs,limits=c(60,90))+geom_vline(xintercept=seq(60,90,5))+
    geom_label(data=data.frame(mids),aes(x=mids,y=-.75),label=binlabs,hjust=0.5,size=6,vjust=0.5,fill=fills,col=cols) +geom_point(size=6,aes(fill=col),col=1,shape=21,stroke=1,show.legend=F))


#Save animation as GIF
binz<-gpLogo(binz0,which="horiz_logoWords_whiteBlack",xNPC=.845,yNPC=.95,size=.1,cloudinaryString = "h_300,w_1800,c_lpad,b_rgb:090816,o_100")
animate(binz+transition_states(state),duration=6,renderer=gifski_renderer(),width = 833, height = 500,units="px")
anim_save("assets/cherry-trees_dotplot-labeledBins.gif")


#Save as MP4 video
animate(binz+transition_states(state),duration=12,renderer=ffmpeg_renderer(format="mp4",options=list(filter_complex="loop=3:start=0")),width=1000,height=600,units="px")
anim_save("assets/cherry-trees_dotplot-labeledBins.mp4")

#Can't get it to transition to the histogram, so I'll have to do in manually in Premiere
      # gpLogo(binz0,which="horiz_logoWords_whiteBlack",xNPC=.845,yNPC=.95,size=.1,cloudinaryString = "h_300,w_1800,c_lpad,b_rgb:090816,o_100")+
      # geom_histogram(data=trees,aes(x=Height),breaks=breaksNlabs,col=gpPal2[21],fill=gpPal2[13],inherit.aes = F)+
      #   transition_layers(layer_length=1,keep_layers=c(Inf,Inf,Inf,0,0),from_blank=F)+
      #   enter_fade()





 #Get rid of bin marks n labels
preHist<-ggplot(dot %>% subset(state==2),aes(x=x,y=y))+geom_point(size=5)+ggGalactic()+labs(x="Tree Height (ft)",y="Count",title="This is Basically a Histogram")+scale_x_continuous(labels=breaksNlabs, breaks = breaksNlabs,limits=c(60,90))+scale_y_continuous(breaks=seq(0,10,2),limits=c(0,10),labels=seq(0,10,2))
preHist
ggsave("assets/cherry-trees_prehistogram.png",width=10,height=6)

#Finally, a histogram
# histogram<-ggplot(dot,aes(x=x,y=y-.5))+geom_histogram(data=trees,aes(x=Height),breaks=breaksNlabs,col=gpPal2[21],fill=gpPal2[13],inherit.aes = F)+ggGalactic()+labs(x="Tree Height (ft)",y="Count",title="Histogram of Tree Heights")+scale_x_continuous(labels=breaksNlabs, breaks = breaksNlabs,limits=c(60,90))+scale_y_continuous(breaks=seq(0,10,2),limits=c(0,10),labels=seq(0,10,2))
# histogram
# ggsave("cherry-trees_histogram.png",width=10,height=6)

#already made above


ggplot(trees,aes(x=Height))+geom_histogram(breaks=breaksNlabs,col=gpPal[[2]]$hex[16],fill=gpPal[[2]]$hex[11])+
  ggtitle(" ")+xlab(" ")+ylab(" ")+
  scale_y_continuous(breaks=seq(0,10,2),limits=c(-1,10),labels=seq(0,10,2))+
  scale_x_continuous(labels=breaksNlabs, breaks = breaksNlabs,limits=c(60,90))+
  theme(
  plot.background =element_rect(fill="transparent",colour="transparent"),
  panel.background =element_rect(fill="transparent",colour="transparent"),
  panel.grid=element_line(colour="transparent"),
  axis.text=element_text(colour="transparent"),
  axis.ticks=element_line(colour="transparent"),
  panel.border=element_blank(),
  axis.title=element_text(colour="transparent"),
  plot.title=element_text(colour="transparent")
)
ggsave("cherry-trees_justHist_transBG.png",width=10,height=6,dpi=100,bg="transparent")

require(dplyr);require(ggplot2);require(galacticEdTools);require(galacticPubs)

get_base_10_breaks<-function(min_val,max_val,n_breaks){
  log_min<-log10(min_val)
  log_max<-log10(max_val)
  incr<-(log_max-log_min)/n_breaks
  c(min_val,sapply(1:n_breaks,function(i){10^(log_min+i*incr)}))
}

rev_s_curve <- function(x,shape,height,shift,minval) {minval+height*exp(shape*(x-shift))/(minval+exp(shape*(x-shift)))}

x<-get_base_10_breaks(min_val=1,max_val=1.6e6,n_breaks=100)

#Mauveine
d_synth <- tibble(units=x,price=rev_s_curve(x=x,shape=-.0005,height=3e6,shift=1e3,minval=1.50),source_material="Fossil Fuels")
d_tyrian <- tibble(units=x,price=rev_s_curve(x,shape=-.1,height=5e2,shift=20,minval=40),source_material="Murex snails")
d<-rbind(d_synth)#,d_tyrian)
d %>% ggplot(aes(units,price,color=source_material)) +
 #geom_smooth(formula="y~x",method="loess",se = FALSE,span=.5)+
  geom_point()+
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x,n=6),minor_breaks=NULL,
                labels = scales::label_comma(accuracy = 1),
                limits=c(1,NA),expand=expansion(c(0.05,0.3)))+
  scale_y_log10(n.breaks=6,labels=scales::label_dollar(),limits=c(0.1,NA),expand=expansion(c(0.02,0.2)))

#############
#Just graph...abandoning trying to actually figure out the formulas :/
(base_graf<-ggplot(d) +
   geom_point(aes(x = units, y = price), color = "transparent") +
   scale_x_log10(
     breaks = scales::trans_breaks("log10", function(x)
       10 ^ x, n = 6),
     minor_breaks = NULL,
     labels = scales::label_comma(accuracy = 1),
     limits = c(1, NA),
     expand = expansion(c(0.05, 0.08))
   ) +
   scale_y_log10(
     n.breaks = 7,
     minor_breaks=NULL,
     labels = scales::label_dollar(accuracy = 1),
     limits = c(0.1, NA),
     expand = expansion(c(0.02, 0.09))
   ) +
   #annotation_logticks(short = unit(1, "npc"), mid = unit(1, "npc"), long = unit(1,"npc"),colour="white") +
   theme_galactic(text.cex = 1.5, font.face = 2) +
   ylab("Price Per Dyed Item") + xlab("Number of Dyed Items Produced"))
gpsave("0-price~units_grid (no x-padding).png")

###############
#Add first points

# a first item price points
base_graf+annotate("point",inherit.aes=F,x=c(1),y=c(1.57e6),color=c("#2c83c3"),size=3)
gpsave("1a-price~units_1st points.png")

# c 2 first item price points
base_graf+annotate("point",inherit.aes=F,x=c(1,1),y=c(1.57e6,200),color=c("#2c83c3","#cb1f8e"),size=3)
gpsave("1b-price~units_1st points.png")



# c all 3 first item price points
base_graf+annotate("point",inherit.aes=F,x=c(1,1,1),y=c(1.57e6,200,1.6e3),color=c("#2c83c3","#cb1f8e","#6c2d82"),size=3)
gpsave("1c-price~units_1st points.png")

#######
### Plot graph for presentation 3 side note demonstrating need for log scale

# c all 3 first item price points (but more space on y-axis)
base_graf+theme(axis.text=element_text(size=36) )+annotate("point",inherit.aes=F,x=c(1,1,1),y=c(1.57e6,200,1.6e3),color=c("#2c83c3","#cb1f8e","#6c2d82"),size=3)
gpsave("1c-price~units_1st points_(no extra space, but wide).png",width=8)

# c all 3 first item price points (but more space on y-axis)
base_graf+theme(axis.ticks.length.y = unit(36,"pt"),axis.text=element_text(size=36) )+annotate("point",inherit.aes=F,x=c(1,1,1),y=c(1.57e6,200,1.6e3),color=c("#2c83c3","#cb1f8e","#6c2d82"),size=3)
gpsave("1c-price~units_1st points_(more spacey y-axis).png",width=8)

d %>% ggplot(aes(units,price,color=source_material)) +
  geom_point(aes(x = units, y = price), color = "transparent")+
  annotate("point",inherit.aes=F,x=c(1,1,1),y=c(1.57e6,200,1.6e3),color=c("#2c83c3","#cb1f8e","#6c2d82"),size=3)+
  theme_galactic(text.cex = 1.5, font.face = 2) +
   ylab("Price Per Dyed Item") + xlab("Number of Dyed Items Produced") +
  scale_x_continuous(
    n.breaks=6 ,
    minor_breaks = NULL,
     labels = scales::label_comma(accuracy = 1),
     limits = c(1, 1e6),
     expand = expansion(c(0.05, 0.08))
   ) +
   scale_y_continuous(
     n.breaks = 7,
     minor_breaks=NULL,
     labels = scales::label_dollar(accuracy = 1),
     limits = c(0.1, NA),
     expand = expansion(c(0.02, 0.09))
   )
gpsave("1b-price~units_1st points_(Normal_axes)_overlapping.png",width=8)

### jitter point 1
d %>% ggplot(aes(units,price,color=source_material)) +
  geom_point(aes(x = units, y = price), color = "transparent")+
  annotate("point",inherit.aes=F,x=c(1,7e3,1),y=c(1.57e6,200,1.6e3),color=c("#2c83c3","#cb1f8e","#6c2d82"),size=3)+
  theme_galactic(text.cex = 1.5, font.face = 2) +
   ylab("Price Per Dyed Item") + xlab("Number of Dyed Items Produced") +
  scale_x_continuous(
    n.breaks=6 ,
    minor_breaks = NULL,
     labels = scales::label_comma(accuracy = 1),
     limits = c(1, 1e6),
     expand = expansion(c(0.05, 0.08))
   ) +
   scale_y_continuous(
     n.breaks = 7,
     minor_breaks=NULL,
     labels = scales::label_dollar(accuracy = 1),
     limits = c(0.1, NA),
     expand = expansion(c(0.02, 0.09))
   )
gpsave("1b-price~units_1st points_(Normal_axes)_nonoverlapping.png",width=8)



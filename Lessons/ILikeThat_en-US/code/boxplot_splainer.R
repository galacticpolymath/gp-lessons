require(pacman)
p_load(galacticPubs,galacticEdTools,ggplot2,gganimate,dplyr,readr,skimr)

####
# SOURCE 4: 2012 Army Anthropometric Survey
# web http://mreed.umtri.umich.edu/mreed/downloads.html
# direct link http://mreed.umtri.umich.edu/mreed/downloads/anthro/ANSUR2Distribution.zip

ansur_f0<-read_csv("data/ANSUR2 anthropometrics/ANSUR II FEMALE Public.csv")
ansur_m0<-read_csv("data/ANSUR2 anthropometrics/ANSUR II MALE Public.csv")
#traits of interest to extract
set.seed(63)
fems<-ansur_f0 %>% mutate(height_cm=Heightin*2.5)%>% select(height_cm,Gender) %>% slice_sample(n=25)
set.seed(63)
males<-ansur_m0 %>% mutate(height_cm=Heightin*2.5)%>% select(height_cm,Gender) %>% slice_sample(n=25)

heights<-bind_rows(fems,males) %>% mutate(id=sample(1:50))

#Looks good (no outliers for height)
#
mytheme<- theme_galactic(base.theme = "linedraw",text.cex = c(0.8,1,1,1),grid.col = "gray80")

(g0 <- heights %>% ggplot(aes(x=1,y=height_cm,group=id))+
  scale_x_discrete()+
  mytheme+
  scale_y_continuous(limits=c(0,231),expand=expansion(0,0))+
  labs(x="X-Axis is Not Meaningful",y="Height (cm)",title="50 Human Heights",subtitle="Data Points"))

#show stacked height points
(g_stack<-g0+
    geom_point(pch=21,stroke=.6,col=gpColors("burst"),size=3)+
    geom_point(pch=21,stroke=0,col=gpColors("burst"),fill=gpColors("burst"),size=3,alpha=0.3)+
    transition_reveal(along=id)+
    ease_aes(y="quadratic"))

animate(
      g_stack,
      renderer = gifski_renderer(file = "assets/_r_outputs/boxplot_explainer_1.gif"),
      res = 150,
      width =
        5,
      height = 5,
      units = "in",

    )


#Function for getting bee swarm points modified from:
#https://www.r-statistics.com/2011/03/beeswarm-boxplot-and-plotting-it-with-r/
get_swarm_coords <- function(x,id){
#check if ids are valid:
if(sum(duplicated(id))>1){
  warning("Duplicated IDs found! This might cause problems for you.")
}
    unique.vals <- length(unique(x))

    N <- length(x)
    # browser()
    N.val <-min(N/10,unique.vals)

    # determine some correction to make the V shape,
    # based on the range
    # y.corr <- diff(range(x))*0.05


    d<-dplyr::tibble(x_orig=x,
                       x_new=ave(x,cut(x,N.val),FUN=min) %>% signif(digits=4),
                       id=id
                       )


    # get spread out x-values after they've been grouped into "cuts"
    x_spread <- table(d$x_new)
    x_spread <- x_spread/max(x_spread)

    # add the x_units to the data frame
    d$x_units <- x_spread[match(d$x_new,names(x_spread))]


    # Get the unique values of x
    unique_new_vals <- sort(unique(d$x_new))

    df<-lapply(1:length(unique_new_vals),function(i){
      d_i<-d %>% dplyr::filter(x_new==unique_new_vals[i]) %>% arrange(x_orig)
      d_i %>%
        mutate(X=seq(-x_units[1],x_units[1],length=nrow(d_i))) %>%
        mutate(Y=x_orig)


    })


    out<-dplyr::bind_rows(df)
    #order output by input id
    out[match(id,out$id),]

}


#transition to illustrate jitter
swarm<-get_swarm_coords(heights$height_cm,id = heights$id)
heights2<-bind_rows(heights,heights)
heights2$state=c(rep(1,nrow(heights)),rep(2,nrow(heights)))
heights2$new_x=c(rep(0,nrow(heights)),swarm$X)
heights2$new_y=c(heights$height_cm,swarm$Y)
heights2$id=c(heights$id,swarm$id)




(g_jit_anim <- heights2 %>% ggplot(aes(x=new_x,y=new_y,group=id))+
  scale_x_discrete(expand=expansion(0.1))+
  scale_y_continuous(limits=c(0,235),expand=expansion(0,0))+
  mytheme+
  labs(x="X-Axis is Not Meaningful",y="Height (cm)",title="50 Human Heights",subtitle="Data Points")+
    geom_point(pch=21,stroke=0,col=gpColors("burst"),fill=gpColors("burst"),size=3,alpha=0.3)+
  geom_point(pch=21,stroke=.6,col=gpColors("burst"),size=3)+
  transition_states(state,transition_length=1,state_length=1))

animate(
  g_jit_anim,
  renderer = gifski_renderer(file = "assets/_r_outputs/boxplot_explainer_2.gif"),
  res = 150,
  width = 5,
  height = 5,
  units = "in"
)

#Jittered static plot
(g_jit_static<- heights2 %>%  dplyr::filter(state==2) %>% ggplot(aes(x=new_x,y=new_y,group=id))+
  scale_x_discrete(expand=expansion(0.1))+
  scale_y_continuous(limits=c(0,225),expand=expansion(0,0))+
  mytheme+theme(plot.margin=margin(0,0,0,0))+
  labs(x="X-Axis is Not Meaningful",y="Height (cm)",title="",subtitle="Data Points")+
  geom_point(pch=21,stroke=.6,col=gpColors("burst"),fill=colorspace::lighten(gpColors("burst"),.7),size=2))
gpsave("boxplot_explainer_1 (static).png")


#Make a histogram
(g_hist<-heights %>% ggplot(aes(y=height_cm))+
  # geom_point(pch=21,stroke=.6,col="gray30",size=3)+
  geom_histogram(col=gpColors("burst"),fill=colorspace::lighten(gpColors("burst"),amount = .7),bins = 20)+
    # geom_histogram(col="transparent",fill=gpColors("burst"),bins = 10,alpha=0.3)+
    theme_galactic(base.theme = "linedraw",text.cex = c(0.8,1,1,1),grid.col = "gray80")+
    theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),plot.margin = margin(5,10,0,-30))+
  scale_x_continuous(n.breaks = 10,minor_breaks = NULL,breaks=seq(0,18,6))+
  scale_y_continuous(limits=c(0,225),breaks=seq(0,250,50),expand=expansion(0,0))+
  labs(x="Counts",y="",subtitle="Histogram")
)


#Make a box plot

(g_box<-heights %>% ggplot(aes(x=1,y=height_cm))+
  # geom_point(pch=21,stroke=.6,col="gray30",size=3)+
  geom_boxplot(col=gpColors("burst"),fill=colorspace::lighten(gpColors("burst"),amount=0.7),size=0.6)+
  theme_galactic(base.theme = "linedraw",text.cex = c(0.8,1,1,1),grid.col = "gray80")+
    theme(axis.text=element_blank(),axis.ticks=element_blank(),plot.margin = margin(5,10,0,-30))+
  scale_y_continuous(limits=c(0,225),breaks=seq(0,250,50),expand=expansion(0,0))+
  scale_x_discrete()+
  labs(x="",y="",subtitle="Box Plot"))


#Combine plots
(g_jit_static+xlab("")+
    g_hist+
    g_box+
    patchwork::plot_layout(widths=c(1,1,1),nrow=1,heights = 1)
)
    # patchwork::plot_annotation(title="150 Human Heights")
gpsave("boxplot_explainer_all 3 types.png")


#Splitting into M and F
heights3<-bind_rows(heights2 %>% dplyr::filter(state==2)%>% mutate(state=recode(state,`2`=1)),
                    heights2 %>% dplyr::filter(state==2) %>% mutate(lab=Gender))

#Add 1 to male rows to shift them over
male_state2_rows<-which(heights3$Gender=="Male"&heights3$state==2)
heights3$new_x[male_state2_rows]<-heights3$new_x[male_state2_rows]+1

#Subtract 1 from female rows to shift them over
fem_state2_rows<-which(heights3$Gender=="Female"&heights3$state==2)
heights3$new_x[fem_state2_rows]<-heights3$new_x[fem_state2_rows]-1


#add labels to just 1st entry of M & F in each state
heights3$lab<-NA
heights3$lab_x<-NA
firstF1<-min(which(heights3$Gender=="Female"&heights3$state==1))
firstF2<-min(which(heights3$Gender=="Female"&heights3$state==2))
firstM1<-min(which(heights3$Gender=="Male"&heights3$state==1))
firstM2<-min(which(heights3$Gender=="Male"&heights3$state==2))
heights3$lab[c(firstF2)]<-"Females"
heights3$lab[c(firstM2)]<-"Males"
heights3$lab_x[firstF1] <- -.35
heights3$lab_x[firstF2] <- -1.1
heights3$lab_x[firstM1] <- .35
heights3$lab_x[firstM2] <- 1.1

(g_jit_anim_sexes <- heights3 %>% ggplot(aes(x=new_x,y=new_y,group=id))+
  scale_x_discrete(expand=expansion(0.1))+
  scale_y_continuous(limits=c(0,231),expand=expansion(0,0))+
  mytheme+
  labs(x="X-Axis is Not Meaningful",y="Height (cm)",title="50 Human Heights",subtitle="Data Points")+
  geom_point(pch=21,stroke=0.6,col=gpColors("burst"),fill=colorspace::lighten(gpColors("burst"),.8),size=3)+
  geom_text(y=130,aes(x=lab_x,label=lab,state=state),size=10,color=gpColors("bur"))+
  transition_states(states = state,transition_length=1,state_length=1)
  )

animate(
  g_jit_anim_sexes,
  renderer = gifski_renderer(file = "assets/_r_outputs/boxplot_explainer_3.gif"),
  res = 150,
  width = 5,
  height = 5,
  units = "in",
  nframes=120
)

#Take a histogram ggplot object and use it to create a binned or unbinned dot plot
#Reverse engineer a dotplot from this, cuz dotplot is soooo stupid!
require(rlang)
dotplot<-function(h,binned=FALSE,size=5){
  xName=h$mapping$x %>% rlang::quo_name()
  h_data<-as.data.frame(subset(h$data,complete.cases(h$data[,xName])))
  h_aes <- as.data.frame(layer_data(h,1))
  #extract break points from bin min max values
  brks<-c(h_aes$xmin,max(h_aes$xmax))
  #brks<-h[["scales"]][["scales"]][[1]][["breaks"]] #(This way was not generalizable)
  mids<-h_aes$x #sapply(1:(length(brks)-1),function(i){median(c(brks[i],brks[i+1]))})
  broken<-cut(unlist(h_data[,xName]),brks,labels=mids,include.lowest = TRUE)
 
  #original 
  x.orig=sort(h_data[,xName])
  #expand the number of points up to the total count
  y.orig=as.vector(unlist(sapply(table(x.orig),function(x) 1:x)))
  
  x.binned=sort(as.numeric(as.vector(broken)))
  y.binned=as.vector(unlist(sapply(table(x.binned),function(x) 1:x)))
  dot<-data.frame(x=c(x.orig,x.binned,x.orig),y=c(y.orig,y.binned,y.orig),state=c(sapply(1:3,function(x) rep(x,length(x.orig)))))
  dot$col=factor(rep(sort(broken),3))
  binlabs<-sapply(1:(length(brks)-1),function(i) {paste0(" >",brks[i]," : ",brks[i+1]," ")})
  fills<-viridis::viridis(6)
  cols<-c(rep("white",3),rep(1,3))
  # if user wants binned dot plot, this is state 2, otherwise 1. 
  # Data's prepped to do a gganimate between states 1, 2, 3 
  # (where 1 is unbinned, 2 is binned, and 3 is unbinned again)
  binState<-ifelse(binned,2,1)
  ggplot(dot %>% subset(state==binState),aes(x=x,y=y))+geom_point(size=size,pch=21,color=h_aes$colour[1],fill=h_aes$fill[1])+ggGalactic()+scale_x_continuous(limits=layer_scales(h)$x$get_limits())+scale_y_continuous(limits=layer_scales(h)$y$get_limits())+xlab(h$labels$x)+ylab(h$labels$y)
}


require(pacman)
p_load(galacticPubs,galacticEdTools,dplyr,readr,readxl,ggplot2)

# Define function to convert wavelength to RGB color
# From https://gist.github.com/friendly/67a7df339aa999e2bcfcfec88311abfc
# Wavelength to RGB
#
# This function converts a given wavelength of light to an
# approximate RGB color value.
#
wavelength_to_rgb <- function(wavelength, gamma=0.8){

#
#    Based on code by Dan Bruton
#    http://www.physics.sfasu.edu/astro/color/spectra.html
#    '''

    if (wavelength >= 380 & wavelength <= 440) {
        attenuation = 0.3 + 0.7 * (wavelength - 380) / (440 - 380)
        R = ((-(wavelength - 440) / (440 - 380)) * attenuation) ^ gamma
        G = 0.0
        B = (1.0 * attenuation) ^ gamma
        }
    else if (wavelength >= 440 & wavelength <= 490) {
        R = 0.0
        G = ((wavelength - 440) / (490 - 440)) ^ gamma
        B = 1.0
        }
    else if (wavelength >= 490 & wavelength <= 510) {
        R = 0.0
        G = 1.0
        B = (-(wavelength - 510) / (510 - 490)) ^ gamma
        }
    else if (wavelength >= 510 & wavelength <= 580) {
        R = ((wavelength - 510) / (580 - 510)) ^ gamma
        G = 1.0
        B = 0.0
        }
    else if (wavelength >= 580 & wavelength <= 645) {
        R = 1.0
        G = (-(wavelength - 645) / (645 - 580)) ^ gamma
        B = 0.0
        }
    else if (wavelength >= 645 & wavelength <= 750) {
        attenuation = 0.3 + 0.7 * (750 - wavelength) / (750 - 645)
        R = (1.0 * attenuation) ^ gamma
        G = 0.0
        B = 0.0
        }
    else {
        R = 1
        G = 1
        B = 1
        }
    R = R * 255
    G = G * 255
    B = B * 255
    return (rgb(floor(R), floor(G), floor(B), max=255))
}

spec_data <- read_excel("data/orig-client-data_noEdit/barrenense_MSP_forNatalie.xlsx",sheet="Graph")

# Darter sensitivity curve
names(spec_data)[1]<-"wavelength"
manual_colors<-sapply(spec_data$wavelength,function(x) wavelength_to_rgb(x))
names(manual_colors)<-as.character(spec_data$wavelength)
#Make negative numbers 0
spec_data$LWS<-ifelse(spec_data$LWS<0,0,spec_data$LWS)


(g1 <- ggplot(spec_data) +
  geom_smooth(formula="y~x",aes(x = wavelength, y = LWS), method="loess",se = FALSE,size=2,colour=gpColors("galactic black"),span=0.1) +
  geom_smooth(formula="y~x",aes(x = wavelength, y = MWS), method="loess",se = FALSE,size=2,linetype="dashed",colour=gpColors("galactic black"),span=0.1) +
  theme_galactic(pad.outer = rep(20,4)) +
  ylab("Darter Visual Excitation") + xlab("Wavelength of Light (nm)")+
  # ggplot2::geom_vline(xintercept=570,linetype="dashed",color="orange")+
  # ggplot2::geom_vline(xintercept=584,color="red")+
  scale_y_continuous(labels = scales::percent))
gpsave(plot=g1,"darter_vis_excitation_curve.png",width=10,height=10*9/16)


#Make Medium+Long wave length curve
ggplot(spec_data) +
  geom_smooth(formula="y~x",aes(x = wavelength, y = LWS), method="loess",se = FALSE,size=2,colour=gpColors("atomic blue"),span=0.1) +
  theme_galactic(pad.outer = rep(20,4)) +
    geom_smooth(formula="y~x",aes(x = wavelength, y = MWS), method="loess",se = FALSE,size=2,linetype="dashed",colour=gpColors("galactic black"),span=0.1) +
  ylab("Darter Visual Excitation") + xlab("Wavelength of Light (nm)")+
  scale_y_continuous(labels = c("0%","25%","50%","75%","100%"),breaks=seq(0,1,.25),limits=c(0,1))
gpsave("darter_vis_excitation_curve_L-only.png",width=10,height=10*9/16)

#Make just a medium wave length curve
ggplot(spec_data) +
  geom_smooth(formula="y~x",aes(x = wavelength, y = MWS), method="loess",se = FALSE,size=2,linetype="dashed",colour=gpColors("atomic blue"),span=0.1) +
  theme_galactic(pad.outer = rep(20,4)) +
  ylab("Darter Visual Excitation") + xlab("Wavelength of Light (nm)")+
  scale_y_continuous(labels = scales::percent)
gpsave("darter_vis_excitation_curve_M-only.png",width=10,height=10*9/16)




#Make blank graph for human spec (to be made in Illustrator)
ggplot(spec_data) +
  geom_smooth(formula="y~x",aes(x = wavelength, y = LWS), method="loess",se = FALSE,size=2,colour="transparent",span=0.1) +
  geom_smooth(formula="y~x",aes(x = wavelength, y = MWS), method="loess",se = FALSE,size=2,linetype="dashed",colour="transparent",span=0.1) +
  theme_galactic(pad.outer = rep(20,4)) +
  ylab("Human Visual Excitation") +
  xlab("Wavelength of Light (nm)")+
  scale_y_continuous(labels =c("0%","25%", "50%","75%", "100%"),breaks=seq(0,1,.25),limits=c(-.2,1.05),expand=expansion(0,0))+
#Add spectrum below graph
  geom_segment(aes(x=wavelength,xend=wavelength,y=-0.175,yend=-0.01,col=as.character(wavelength)),show.legend = F)+
  scale_color_manual(values=manual_colors)+
  geom_rect(xmin=300,xmax=380,ymin=-0.175,ymax=-0.01,fill="gray35")+
  annotate("text",x=340,y=-0.1,label="UV",vjust=0.5,size=10,col="gray80")
gpsave("blank_human_vis_excitation_curve.png",width=10,height=10*9/16)


#Try another dataset from Jenny
(spec_data2<-read_excel("data/orig-client-data_NoEdit/GovardovskiiA1A2.xls","Sheet1"))
ggplot(spec_data2) + geom_smooth(formula="y~x",aes(x = wavelength...4, y = `spectrum (600.65 nm)`), method="loess",se = FALSE,color=gpPal[[1]]$hex[1],size=2) +
  theme_galactic(text.cex = 1.5) +
  ylim(0,1)+
  ylab("Darter Visual Excitation") + xlab("Wavelength (nm)")+
  ggplot2::geom_vline(xintercept=570,linetype="dashed",color="orange")+
  ggplot2::geom_vline(xintercept=584,color="red")

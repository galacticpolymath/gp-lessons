require(remotes);require(viridis)
setWavPlayer('/usr/bin/afplay')

# From github
devtools::install_github("maRce10/dynaSpec")
devtools::install_github("maRce10/warbleR")


#load package
library(dynaSpec);library(warbleR)

frogs<-read_wave("assets/videos/video_project-files/Borneo_situating-vid/frogs.wav")


scrolling_spectro(wave = frogs, wl = 300, 
    ovlp = 90, pal = magma, 
    grid = FALSE, flim = c(1, 10), width = 1920, 
    height = 400, res = 120, collevels = seq(-50, 0, 5), 
    file.name = "assets/videos/video_project-files/Borneo_situating-vid/frogs_spec.mp4", colbg = "black", 
    speed = 1, axis.type = "none", loop = 1, fastdisp=T,t.display = 4,lower.spectro = F)

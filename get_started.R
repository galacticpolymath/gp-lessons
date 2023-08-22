#Install/update galacticPubs and galacticEdTools
library(devtools)
remotes::install_github("galacticpolymath/galacticPubs")
remotes::install_github("galacticpolymath/galacticEdTools")
library(galacticPubs)
googledrive::drive_auth() #need to authorize your account first time you use this

#Easiest way to use the galacticPubs workflow is with the editor
editor()

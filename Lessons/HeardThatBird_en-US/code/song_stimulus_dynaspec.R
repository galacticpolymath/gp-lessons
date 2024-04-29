pacman::p_load(dynaSpec,dplyr)
pacman::p_load_gh("galacticpolymath/galacticPubs",update=TRUE)
require(galacticPubs)
proj <- "HeardThatBird"
WD <- "/Users/mattwilkins/Library/CloudStorage/GoogleDrive-matt@galacticpolymath.com/Shared drives/GP-Studio/Edu/Lessons/HeardThatBird_en-US"
stimuli_path <- fs::path(WD=WD,"assets","birdsong stimuli","audio","essential_5_WAVs") %>% paste0(.,"/")
dest_path <-  fs::path(WD=WD,"assets","birdsong stimuli","video_dynamic spectrograms") %>% paste0(.,"/")
f <- list.files(stimuli_path,full.names = T,pattern="\\.wav")

# stim1_orig <- fs::path(stimuli_path,"exile_HD_clip_orig.wav")
# stim1_orig_spec <- prep_static_ggspectro(stim1_orig,specWidth=12,onlyPlotSpec = F,bg = "black",bgFlood = T,xLim=Inf,min_dB = -40,ampTrans=3,colPal = c("black","white") )
# paged_spectro(stim1_orig_spec,vidName = "Spec--exile_HD_clip_orig")
#
#
# stim1_edit <- fs::path(stimuli_path,"exile_HD_clip_enhanced.wav")
# stim1_edit_spec <- prep_static_ggspectro(stim1_edit,specheight = 2,specWidth=12,onlyPlotSpec = F,bg = "black",bgFlood = T,xLim=Inf)
# paged_spectro(stim1_edit_spec,vidName = "Spec--exile_HD_clip_(enhanced)")


# Modify params for specs -------------------------------------------------

prep_spec <- \(...,onlyPlotSpec=F){prep_static_ggspectro(...,bg="black",onlyPlotSpec=onlyPlotSpec,bgFlood=T,yLim = 8,xLim=Inf)}
mk_mov <- \(...){paged_spectro(...,cursorCol="white",highlightAlpha = 0,destFolder = dest_path)}


# 1. Look at all these chickens ------------------------------------
# STIMULUS

stim1 <- prep_spec(fs::path(stimuli_path,"5 chickens.wav"),ampTrans=1.5)
mk_mov(stim1,vidName="1.Chickens-stimulus")
# HINTS

#canada goose
CAGO_url <- "https://xeno-canto.org/792016/download"
CAGO_spec <- prep_spec(CAGO_url,crop=c(1,3),min_db=-35)
mk_mov(CAGO_spec,vidName="1.Chickens-hint_CAGO")

#chicken
chkn_url <- "https://xeno-canto.org/669954/"
chkn_spec <- prep_spec(chkn_url,ampTrans=2,crop=c(3,6))
mk_mov(chkn_spec,"1.Chickens-hint_Actual-Chicken")

#snow goose
SNGO_url <- "https://xeno-canto.org/509699"
SNGO_spec <- prep_spec(SNGO_url,crop=c(3,6),ampTrans=2,min_dB=-25)
mk_mov(SNGO_spec,vidName="1.Chickens-hint_SnowGoose(Correct)")



# 4. Downton ------------------------------------
# STIMULUS
stimulus_name <- "downton"
stimulus_num <- 2

stim2 <- prep_spec(fs::path(stimuli_path,paste0("5 ",stimulus_name),ext = "wav"),ampTrans=2)
mk_mov(stim2,vidName=paste0(stimulus_num,".",stimulus_name,"_stimulus"))
# HINTS

#jackdaw
jackdaw_url <- "https://xeno-canto.org/878107"
jackdaw_spec <- prep_spec(jackdaw_url,crop=c(2,5),ampTrans=1)
mk_mov(jackdaw_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_jackdaw"))

#raven
raven_url <- "https://xeno-canto.org/833029"
raven_spec <- prep_spec(raven_url,ampTrans=3,crop=c(0.5,3.5))
mk_mov(raven_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_raven"))

#Eurasian magpie (Correct)
magpie_url <- "https://xeno-canto.org/872510"
magpie_spec <- prep_spec(magpie_url,crop=c(12,14),ampTrans=2)
mk_mov(magpie_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_magpie"," (Correct)"))

# 2. Colbert ------------------------------------
# STIMULUS
stimulus_name <- "colbert"
stimulus_num <- 3

stim3 <- prep_spec(fs::path(stimuli_path,paste0("5 ",stimulus_name),ext = "wav"),ampTrans=1.5)
mk_mov(stim3,vidName=paste0(stimulus_num,".",stimulus_name,"_stimulus"))
# HINTS

#bald eagle
eagle_url <- "https://xeno-canto.org/451490"
eagle_spec <- prep_spec(eagle_url,crop=c(11,14),ampTrans=1)
mk_mov(eagle_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_eagle"))

#Peregrine Falcon
falcon_url <- "https://xeno-canto.org/748303"
falcon_spec <- prep_spec(falcon_url,ampTrans=3,crop=c(2.52,6.5))
mk_mov(falcon_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_falcon"))

#red tailed hawk (Correct)
hawk_url <- "https://xeno-canto.org/603736"
hawk_spec <- prep_spec(hawk_url,crop=c(1,4),ampTrans=2)
mk_mov(hawk_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_hawk"," (Correct)"))

# 3. Jumanji ------------------------------------
# STIMULUS
stimulus_name <- "jumanji"
stimulus_num <- 4

stim4 <- prep_spec(fs::path(stimuli_path,"5 jumanji_cleaned",ext = "wav"),ampTrans=1.5, xLim=5)
mk_mov(stim4,vidName=paste0(stimulus_num,".",stimulus_name,"_stimulus"))
# HINTS

#Indian Peafowl (Peacock)
pea_url <- "https://xeno-canto.org/812476"
pea_spec <- prep_spec(pea_url,crop=c(11,14),ampTrans=2,min_dB=-35)
mk_mov(pea_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_pea"))

#belted kingfisher
BEKI_url <- "https://xeno-canto.org/448752"
BEKI_spec <- prep_spec(BEKI_url,ampTrans=2,crop=c(1.75,4))
mk_mov(BEKI_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_BEKI"))

#kookaburra (Correct)
kook_url <- "https://xeno-canto.org/587002"
kook_spec <- prep_spec(kook_url,crop=c(6,11),ampTrans=2)
mk_mov(kook_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_kook"," (Correct)"))

# 5. Horizon Zero Dawn ------------------------------------
# STIMULUS
stimulus_name <- "HZD"
stimulus_num <- 5

stim3 <- prep_spec(fs::path(stimuli_path,paste0("5 ",stimulus_name),ext = "wav"),ampTrans=1.5)
mk_mov(stim3,vidName=paste0(stimulus_num,".",stimulus_name,"_stimulus"))
# HINTS

#wood thrush
WOTH_url <- "https://xeno-canto.org/818024"
WOTH_spec <- prep_spec(WOTH_url,crop=c(10,15),ampTrans=2,min_dB=-35)
mk_mov(WOTH_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_WOTH"))

#American robin
AMRO_url <- "https://xeno-canto.org/744275"
AMRO_spec <- prep_spec(AMRO_url,ampTrans=3,crop=c(1,7.5))
mk_mov(AMRO_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_AMRO"))

#hermit thrush (Correct)
HETU_url <- "https://xeno-canto.org/750449"
HETU_spec <- prep_spec(HETU_url,crop=c(3,6),ampTrans=2,min_dB=-35)
mk_mov(HETU_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_HETU"," (Correct)"))

# 6. Blackbird ------------------------------------
# STIMULUS
stimulus_name <- "beatles"
stimulus_num <- 6

stim3 <- prep_spec(fs::path(stimuli_path,paste0("5 ",stimulus_name),ext = "wav"),ampTrans=1.5)
mk_mov(stim3,vidName=paste0(stimulus_num,".",stimulus_name,"_stimulus"))
# HINTS

#American Robin
AMRO_url <- "https://xeno-canto.org/744275"
AMRO_spec <- prep_spec(AMRO_url,crop=c(17.5,21),ampTrans=2)
mk_mov(AMRO_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_AMRO"))

# #Common Starling
# COST_url <- "https://xeno-canto.org/796160"
# COST_spec <- prep_spec(COST_url,ampTrans=2,crop=c(5,11),min_dB=-35)
# mk_mov(COST_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_COST"))
#
#Red-winged Blackbird
RWBL_url <- "https://xeno-canto.org/759001"
RWBL_spec <- prep_spec(RWBL_url,crop=c(37.5,40),ampTrans=2.5)
mk_mov(RWBL_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_RWBL"))


#common blackbird (Correct)
COBL_url <- "https://xeno-canto.org/877992"
COBL_spec <- prep_spec(COBL_url,crop=c(1,8),ampTrans=2,xLim=Inf)
mk_mov(COBL_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_COBL"," (Correct)"))

# 7. Doja Cat ------------------------------------
# STIMULUS
stimulus_name <- "doja"
stimulus_num <- 7

stim7 <- prep_spec(fs::path(stimuli_path,paste0("5 ",stimulus_name),ext = "wav"),ampTrans=1)
mk_mov(stim7,vidName=paste0(stimulus_num,".",stimulus_name,"_stimulus"))
# HINTS

#barred owl
BAOW_url <- "https://xeno-canto.org/861603"
BAOW_spec <- prep_spec(BAOW_url,crop=c(2,7),ampTrans=2)
mk_mov(BAOW_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_BAOW"))

#Mourning dove
MODO_url <- "https://xeno-canto.org/621378"
MODO_spec <- prep_spec(MODO_url,ampTrans=3,crop=c(1,6))
mk_mov(MODO_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_MODO"))

#common loon (Correct)
# loon_url <- "https://xeno-canto.org/824369" #slightly different variant from song
# loon_spec <- prep_spec(loon_url,crop=c(1,4),ampTrans=2)
# mk_mov(loon_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_loon"," (Correct)"))
#common loon (Correct)
loon_url <- "https://xeno-canto.org/839985" #more similar to song
loon_spec <- prep_spec(loon_url,crop=c(0,4),ampTrans=2)
mk_mov(loon_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_loon"," (Correct)"))

# 8. Taylor Swift + Bon Iver "Exile"---------------------------------
# STIMULUS
stimulus_name <- "bon iver"
stimulus_num <- 8

stim8 <- prep_spec(fs::path(stimuli_path,paste0("5 ",stimulus_name),ext = "wav"),ampTrans=1.5,crop=c(1,4.7))
mk_mov(stim8,vidName=paste0(stimulus_num,".",stimulus_name,"_stimulus"))
# HINTS

#hermit warbler
HEWA_url <- "https://xeno-canto.org/570243"
HEWA_spec <- prep_spec(HEWA_url,crop=c(1,5),ampTrans=2)
mk_mov(HEWA_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_HEWA"))

#Nashville Warbler
NAWA_url <- "https://xeno-canto.org/744264"
NAWA_spec <- prep_spec(NAWA_url,ampTrans=3,crop=c(1.5,4.5))
mk_mov(NAWA_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_NAWA"))

#Townsend's Warbler
TOWA_url <- "https://xeno-canto.org/764431"
TOWA_spec <- prep_spec(TOWA_url,crop=c(3,6),ampTrans=2)
mk_mov(TOWA_spec,vidName=paste0(stimulus_num,".",stimulus_name,"_hint_TOWA (MAYBE)"))

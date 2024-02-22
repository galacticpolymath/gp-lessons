pacman::p_load(dynaSpec,dplyr)
pacman::p_load_gh("galacticpolymath/galacticPubs",update=TRUE)
require(galacticPubs)
proj <- "Chipper"
WD <- pick_lesson("?")
stimuli_path <- fs::path(WD=WD,"assets","birdsong stimuli")
list.files(stimuli_path)

stim1_orig <- fs::path(stimuli_path,"exile_HD_clip_orig.wav")
stim1_orig_spec <- prep_static_ggspectro(stim1_orig,specWidth=12,onlyPlotSpec = F,bg = "black",bgFlood = T )
paged_spectro(stim1_orig_spec,vidName = "exile_HD_clip_orig_DynaSpec")


stim1_edit <- fs::path(stimuli_path,"exile_HD_clip_enhanced.wav")
stim1_edit_spec <- prep_static_ggspectro(stim1_edit,specheight = 2)
paged_spectro(stim1_edit_spec,vidName = "exile_HD_clip_enhanced_DynaSpec")

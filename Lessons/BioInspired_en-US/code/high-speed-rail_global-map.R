pacman::p_load(ggplot2, sf, rnaturalearth, ggfx, svglite, dplyr)
require(galacticEdTools)
require(galacticPubs)
WD <- pick_lesson("?")#pick bioinspired_en-us
rail_file <-
  fs::path(WD, "data", "high-speed-rail2_overpass-turbo.geojson")
checkmate::assert_file_exists(rail_file)


# Import map data ---------------------------------------------------------

#Rail data. manually downloaded from overpass-turbo.eu with the following code:
#(
#   way["railway"="rail"]["highspeed"="yes"];
# );
# // print results
# out geom;

#interactive version (slow loading) is here https://overpass-turbo.eu/map.html?Q=%0A%28%0A++way%5B%22railway%22%3D%22rail%22%5D%5B%22highspeed%22%3D%22yes%22%5D%3B%0A%29%3B%0A%2F%2F+print+results%0Aout+geom%3B


#this takes a minute. big file!
rail0 <- sf::read_sf(rail_file)

#which are good columns
data_counts <- rail0 %>% apply(., 2, \(x) sum(!is.na(x)))
#these are robust data
names(which(data_counts > 1000))

#which to keep
noms <-
  c(
    "id",
    "maxspeed_num",
    "layer",
    "loc_name",
    "operator",
    "short_name",
    "source",
    "start_date",
    "wikipedia"
  )

rail0$maxspeed_num <-
  gsub("(\\d*)[ \\w]*$", "\\1", rail0$maxspeed, perl = TRUE) %>% as.numeric()

rail <-
  rail0 %>% dplyr::select(dplyr::all_of(noms)) %>% dplyr::filter(maxspeed_num >=
                                                                   100)
#make maxspeed more cromulent (too many text entries)

View(rail)

rail_simple <- st_simplify(rail)
#World map
world <- ne_countries(returnclass = "sf")



# Plot the map ------------------------------------------------------------
map0 <- ggplot() +
  ylim(-55, 80) +
  geom_sf(data = world, fill = "#3E454D")+
  theme_void() +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "#020A14"),
    plot.margin = ggplot2::unit(c(0, 0, 0, 0), "in"),
    panel.grid = ggplot2::element_line(linewidth = 0.1, colour = "#A0A5AB")
  ) +
  scale_x_continuous(expand = c(0, 0))

map <- map0+
  with_outer_glow(
    geom_sf(
      data = rail_simple,
      col = "#FCF000",
      fill = NA,
      linewidth = 0.5
    ),
    colour = "#FECB29",
    sigma = 10,
    expansion = 5
  )

# Save high res version for video -----------------------------------------


map
ggsave(fs::path(WD, "assets", "highSpeedRail_world.png"),plot = map, dpi = 600)

#Export just the map layer
ggsave(plot = map0,filename=fs::path(WD, "assets", "highSpeedRail_world_justMap.png"), dpi = 600)

#export just the railroad
railmap <- ggplot() +
  ylim(-55, 80) +
  theme_void() +
  ggplot2::theme(
    # panel.background = ggplot2::element_rect(fill = "#020A14"),
    plot.margin = ggplot2::unit(c(0, 0, 0, 0), "in"),
    # panel.grid = ggplot2::element_line(linewidth = 0.1, colour = "#A0A5AB")
  ) +
  scale_x_continuous(limits =c(-180,180), expand = c(0, 0))+
  with_outer_glow(
    geom_sf(
      data = rail_simple,
      col = "#FCF000",
      fill = NA,
      linewidth = 0.5
    ),
    colour = "#FECB29",
    sigma = 10,
    expansion = 5
  )
railmap
ggsave(plot = railmap,filename=fs::path(WD, "assets", "highSpeedRail_world_justRail.png"), dpi = 600)

# Make GP footer version for socializing ----------------------------------

gridFooter <-
  function(caption,
           x,
           y,
           fontsize = 8,
           fillCol = gpColors("galactic black")) {
    grid::grid.rect(
      x = .5,
      y = 0,
      width = 1,
      height = .06,
      just = "bottom",
      gp = grid::gpar(fill = fillCol)
    ) #"#090816"
    grid::grid.lines(
      x = c(0, 1),
      y = c(.06, .06),
      gp = grid::gpar(col = "white")
    )
    grid::grid.text(
      label = caption,
      x = x,
      y = y,
      just = "left",
      gp = grid::gpar(col = "white", fontsize = fontsize)
    )
    grid::grid.raster(
      logoImg,
      x = grid::unit(1, "npc"),
      y = grid::unit(y, "npc"),
      height = grid::unit(.06, "npc"),
      just = c("right", "center")
    )
  }

#import logo
newURL <-
  "https://res.cloudinary.com/galactic-polymath/image/upload/v1594949366/logos/GP_logo_wordmark_horiz_white_transBG_300_kqc3ii.png"
logoImg <-
  png::readPNG(RCurl::getURLContent(newURL), native = T)


# build learningChart -----------------------------------------------------


G <- grid::grid.grabExpr({
  grid::grid.draw(map)

  gridFooter(
    caption = "Global Highspeed Rail (≥100mph)",
    x = 0.01,
    y = .0275,
    fontsize = 9
  )
})
  grDevices::png(
    fs::path(WD, "assets", "highSpeedRail_world_GP.png"),
    width = 5566,
    height = 3924,
    units = "px",
    res = 600
  )

  grid::grid.draw(G)
  grDevices::dev.off()



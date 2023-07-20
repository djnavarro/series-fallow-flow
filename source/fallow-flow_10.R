
library(jasmines)
library(dplyr)

fallow_flow <- function(seed) {

  set.seed(seed)

  sys_id <- "10"
  sys_name <- "fallow-flow"

  # filename
  prefix <- paste0(sys_name, "_", sys_id, "_")
  fname <- paste0(prefix, seed, ".png")
  output <- here::here("image", paste0("sys_", sys_id), fname)

  # palette
  sample_shades <- function() {
    pal <- sample(colorir::colores$palette_name, 1)
    shades <- colorir::colores$colour[colorir::colores$palette_name == pal]
    return(shades)
  }
  pal <- sample_shades()
  pal <- sample(pal)
  bg <- pal[4]
  pal <- pal[1:3]

  # other settings
  gap <- .025
  oct <- 3
  ord <- sample(16)

  # make data
  cat("  making data...\n")
  dat <- use_seed(seed) %>%
    scene_grid((1:4) * 1.2, (1:4) * 1.2, "circle", 1000, 1.2) %>%
    unfold_breeze(
      iterations = 400,
      scale = .0002,
      octaves = oct
    ) %>%
    unfold_inside() %>%
    mutate(val = 1 + (ord[id] + 5*ind/max(ind)) * (inside == 0))

  # specify
  cat("  specifying plot...\n")
  pic <- dat %>%
    style_ribbon(
      background = bg,
      colour = "val",
      palette = colorRampPalette(c(bg, bg, pal[1], bg, pal[2], bg, pal[3])),
      alpha = c(.5, .0025),
      size = .1,
      type = "point"
    )

  # render
  cat("  rendering image...\n")
  suppressMessages(
    pic %>%
      export_image(
        xlim = c(gap, 1 - gap),
        ylim = c(gap, 1 - gap),
        filename = output,
        width = 20,
        height = 20,
        dpi = 200
      )
  )
}


seeds <- 1149:1400
for(seed in seeds) {
  cat("seed:", seed, "\n")
  fallow_flow(seed)
  gc()
}



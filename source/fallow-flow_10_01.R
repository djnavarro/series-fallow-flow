library(magrittr)


# analyse colours ---------------------------------------------------------

files <- list.files(here::here("image/sys_10_01/"))

# fuction to extract the background colour of an image
main_colour <- function(file) {
  cat(file, "\n")
  col <- file %>%
    here::here("image/sys_10", .) %>%
    magick::image_read() %>%
    magick::image_scale("1x1") %>%
    as.raster() %>%
    as.character()
  gc()
  return(col)
}

# get all the background colours
colours <- purrr::map_chr(files, main_colour)

# rescale to [0,1]
rescale <- function(x) {
  x <- x - min(x)
  x <- x / max(x)
  return(x)
}

# two dimensional scaling on the colour space
features <- t(col2rgb(colours))
distances <- dist(features, upper = TRUE) %>% as.matrix()
coord <- cmdscale(distances, 2)
coord[,1] <- rescale(coord[,1])
coord[,2] <- rescale(coord[,2])


# make grid ---------------------------------------------------------------

n_items <- length(files)
x_max <- 7
y_max <- ceiling(n_items/x_max)

# crudely push the coordinates onto a grid
locations <- tibble::tibble(
  id = 1:nrow(coord),
  coord_x = coord[, 1],
  coord_y = coord[, 2],
  ord_x = rank(coord_x),
  ord_y = rank(coord_y),
  int_x = ceiling(ord_x/x_max),
  int_y = ceiling(ord_y/y_max)
)

# place all possible items without duplication
is_first <- !duplicated(paste(locations$int_x, locations$int_y))
grid <- matrix(NA, nrow = y_max, ncol = x_max)
for(i in 1:nrow(locations)) {
  if(is_first[i]) {
    grid[locations$int_x[i], locations$int_y[i]] <- locations$id[i]
  }
}

# place the remainders as best you can
locations <- locations[!is_first,]
locations <- dplyr::arrange(locations, int_y, int_x)
values <- locations$id
slots <- which(is.na(grid))
grid[slots] <- values

# convert to tibble
rownames(grid) <- paste0("y", y_max:1)
colnames(grid) <- paste0("x", 1:x_max)
lookup <- grid %>%
  as.data.frame() %>%
  tibble::rownames_to_column("y") %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("x"), names_to = "x", values_to = "ind") %>%
  dplyr::mutate(pos = dplyr::row_number() + 1000) %>%
  dplyr::arrange(ind)

lookup$file <- files
lookup <- dplyr::select(lookup, pos, file)

copy_across <- function(pos, file) {
  fname <- stringr::str_replace(file, "flow_10_", paste0("flow_10_pos", pos, "_seed"))
  old <- here::here("image/sys_10_01", file)
  new <- here::here("image/sys_10_02", fname)
  cat(new, "\n")
  file.copy(old, new)
}

purrr::pwalk(lookup, copy_across)

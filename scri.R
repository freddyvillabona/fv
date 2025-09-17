
library(ozmaps)
library(sf)



oz_states <- ozmaps::ozmap_states %>% filter(NAME != "Other Territories")
oz_votes <- rmapshaper::ms_simplify(ozmaps::abs_ced)

oz_capitals <- tibble::tribble( 
  ~city,           ~lat,     ~lon,
  "Sydney",    -33.8688, 151.2093,  
  "Melbourne", -37.8136, 144.9631, 
  "Brisbane",  -27.4698, 153.0251, 
  "Adelaide",  -34.9285, 138.6007, 
  "Perth",     -31.9505, 115.8605, 
  "Hobart",    -42.8821, 147.3272, 
  "Canberra",  -35.2809, 149.1300, 
  "Darwin",    -12.4634, 130.8456, 
)




ggplot() + 
  geom_sf(data = oz_votes) + 
  geom_sf(data = oz_states, colour = "black", fill = NA) + 
  geom_sf_label(aes(), label.padding = unit(1, "mm")) +

  geom_point(data = oz_capitals, mapping = aes(x = lon, y = lat), colour = "red") + 
  coord_sf() +
  geom_label(oz_capitals, mapping = aes(fill = factor(city)), colour = "white", fontface = "bold")
  



library(rayshader)

#Here, I load a map with the raster package.
loadzip = tempfile() 
download.file("https://tylermw.com/data/dem_01.tif.zip", loadzip)
localtif = raster::raster(unzip(loadzip, "dem_01.tif"))
unlink(loadzip)

#And convert it to a matrix:
elmat = rayshader::raster_to_matrix(localtif)

#We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "desert") %>%
  rayshader::plot_map()
  


elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
Sys.sleep(0.2)
rayshader::render_snapshot()



# Start a new session
options(rgl.useNULL = TRUE)
library(rgl)


par(mfrow = c(1, 2)) 
montereybay %>% 
  sphere_shade(zscale = 10, texture = "imhof1") %>% 
  add_shadow(montshadow, 0.5) %>%
  add_shadow(montamb, 0) %>%
  rayshader::plot_3d(montereybay, zscale = 50, fov = 0, theta = -45, phi = 45, windowsize = c(1000, 800), zoom = 0.6,
          water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "lightblue",
          waterlinecolor = "white", waterlinealpha = 0.5, baseshape = "circle")

render_snapshot(clear = TRUE)

montereybay %>% 
  sphere_shade(zscale = 10, texture = "imhof1") %>% 
  add_shadow(montshadow, 0.5) %>%
  add_shadow(montamb, 0) %>%
  plot_3d(montereybay, zscale = 50, fov = 0, theta = -45, phi = 45, windowsize = c(1000, 800), zoom = 0.6,
          water = TRUE, waterdepth = 0, wateralpha = 0.5, watercolor = "lightblue",
          waterlinecolor = "white", waterlinealpha = 0.5, baseshape = "hex")

render_snapshot(clear = TRUE)




if(run_documentation()) {
  montereybay %>%
    sphere_shade() %>%
    plot_3d(montereybay,zscale=50,zoom=0.6,theta=-90,phi=30)
}

if(run_documentation()) {
  render_snapshot()
}


if(!require('rayshader')) {
  install.packages('rayshader')
  library('rayshader')
}



montereybay %>%
  sphere_shade() %>%
  plot_3d(montereybay,zscale=50,zoom=0.6,theta=-90) render_snapshot()
rgl::rgl.clear()


library(png)
field_background <- readPNG("field_background.png")

field_background_plot <- function() {
  ggplot() + 
  annotation_raster(field_background,
                    xmin = -278, xmax = 278,
                    ymin = -35, ymax = 469.8) +
  coord_fixed(
    xlim = c(-278, 278),
    ylim = c(-35, 469.8),
    expand = FALSE
  ) +
  theme_void()
}


wall_field <- expand.grid(spray_angle = seq(-0.78, 0.78, by = 0.01))
wall_field <- wall_field %>% mutate(home_dist = predict(wall_ANI_model, newdata = wall_field),
                                    field_x = home_dist * sin(spray_angle),
                                    field_y = home_dist * cos(spray_angle))


field_background_plot() +
geom_point(data = wall_field,
           aes(x = field_x, y = field_y),
           shape = 15)











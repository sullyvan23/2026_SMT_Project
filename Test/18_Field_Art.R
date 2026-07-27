library(png)

### background for field
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

field_background_plot() + geom_point(aes(x = x_1b, y = y_1b))


### background for first base to second base
from_first <- readPNG("from_first.png")
first_base_line_plot <- function() {
  ggplot() + 
  annotation_raster(from_first,
                    xmin = -105.3, xmax = 15.3,
                    ymin = 61, ymax = 115.9) +
  coord_fixed(
    xlim = c(-105.3, 15.3),
    ylim = c(61, 115.9),
    expand = FALSE
  ) +
  theme_void()
}

first_base_line_plot() + geom_point(aes(x = 0, y = 90))


### background for second base to third base
from_second <- readPNG("from_second.png")
second_base_line_plot <- function() {
  ggplot() + 
  annotation_raster(from_second,
                    xmin = -15.3, xmax = 105.3,
                    ymin = 61, ymax = 115.9) +
  coord_fixed(
    xlim = c(-15.3, 105.3),
    ylim = c(61, 115.9),
    expand = FALSE
  ) +
  theme_void()
}

second_base_line_plot() + geom_point(aes(x = 90, y = 90))


### background for third base to home plate
from_third <- readPNG("from_third.png")
third_base_line_plot <- function() {
  ggplot() + 
  annotation_raster(from_third,
                    xmin = -105.3, xmax = 15.3,
                    ymin = -25.9, ymax = 29) +
  coord_fixed(
    xlim = c(-105.3, 15.3),
    ylim = c(-25.9, 29),
    expand = FALSE
  ) +
  theme_void()
}

third_base_line_plot() + geom_point(aes(x = -90, y = 0))

##################################################################################################################################################################################

### process used to put wall on the background based on field and cover foul line past wall
### this process repeated for all 4 fields

wall_field_ARN <- expand.grid(spray_angle = seq(-pi/4, 0, by = 0.01))
wall_field_ARN <- bind_rows(wall_field_ARN, wall_field_ARN %>% mutate(spray_angle = -spray_angle))
wall_field_ARN <- wall_field_ARN %>% mutate(home_dist = predict(wall_ARN_model, newdata = wall_field_ARN),
                                            field_x = home_dist * sin(spray_angle),
                                            field_y = home_dist * cos(spray_angle))
cover_line_ARN <- wall_field_ARN %>% filter(spray_angle == pi/4)
dists <- seq(cover_line_ARN$home_dist[1]+4, 390, by = 1)
cover_line_ARN <- cover_line_ARN %>% slice(rep(1,length(dists)))
cover_line_ARN$home_dist <- dists
temp <- wall_field_ARN %>% filter(spray_angle == -pi/4)
dists <- seq(temp$home_dist[1]+4, 390, by = 1)
temp <- temp %>% slice(rep(1,length(dists)))
temp$home_dist <- dists
cover_line_ARN <- bind_rows(cover_line_ARN, temp)
cover_line_ARN <- cover_line_ARN %>% mutate(field_x = home_dist * sin(spray_angle),
                                            field_y = home_dist * cos(spray_angle))

##################################################################################################################################################################################

### backgrounds for all the fields

ANI_background <- function() {
  field_background_plot() +
  geom_point(data = wall_field_ANI,
             aes(x = field_x, y = field_y),
             shape = 18, size = 1) +
  geom_point(data = cover_line_ANI,
             aes(x = field_x, y = field_y),
             shape = 23, size = 0.5, color = "#1E6432", fill = "#1E6432")
}

ARN_background <- function() {
  field_background_plot() +
  geom_point(data = wall_field_ARN,
             aes(x = field_x, y = field_y),
             shape = 18, size = 1) +
  geom_point(data = cover_line_ARN,
             aes(x = field_x, y = field_y),
             shape = 23, size = 0.5, color = "#1E6432", fill = "#1E6432")
}

PHD_background <- function() {
  field_background_plot() +
  geom_point(data = wall_field_PHD,
             aes(x = field_x, y = field_y),
             shape = 18, size = 1) +
  geom_point(data = cover_line_PHD,
             aes(x = field_x, y = field_y),
             shape = 23, size = 0.5, color = "#1E6432", fill = "#1E6432")
}

VAS_background <- function() {
  field_background_plot() +
  geom_point(data = wall_field_VAS,
             aes(x = field_x, y = field_y),
             shape = 18, size = 1) +
  geom_point(data = cover_line_VAS,
             aes(x = field_x, y = field_y),
             shape = 23, size = 0.5, color = "#1E6432", fill = "#1E6432")
}

##################################################################################################################################################################################








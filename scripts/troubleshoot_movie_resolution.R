library(rayshader)
library(tidyverse)
#devtools::install_github("JoeyStanley/joeyr")
library(joeyr)
library(rgl)


my_iy <- read_csv("http://joeystanley.com/data/joey.csv") %>%
  
  # filter it down
  filter(vowel == "IY", 
         stress == 1, 
         plt_manner %in% c("stop", "fricative", "affricate")) %>%
  
  # Keep just the columns I need
  select(word, F1, F2) %>%
  
  # There were a few bad outliers.
  mutate(mahal_dist = joeyr::tidy_mahalanobis(F2, F1)) %>%
  filter(mahal_dist < 10) %>%
  select(-mahal_dist) %>%
  print()

hex_plot <- ggplot(my_iy, aes(F2, F1)) + 
  geom_hex(bins = 10, size = 1, color = "black") + 
  theme_classic()+
  scale_x_reverse() + scale_y_reverse() + 
  scale_fill_viridis_c(option = "C")


plot_gg(hex_plot,
        width = 5, height = 7)

plot_gg(hex_plot, multicore = TRUE, width = 7, height = 4,
        scale = 300, windowsize = c(1400, 866))


# create custom rotation parameters here
phivechalf = 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
phivecfull = c(phivechalf, rev(phivechalf))
thetavec = -90 + 60 * sin(seq(0,359,length.out = 360) * pi/180)
zoomvec = 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
zoomvecfull = c(zoomvec, rev(zoomvec))

render_movie(filename = "tx_vac_vid", type = "custom", 
             frames = 360,  phi = phivecfull, zoom = zoomvecfull, theta = thetavec)




# Actually render the video.
render_movie(filename = "hex_plot_fancy")


render_depth(focus = 0.5, focallength = 15, filename = "hex_plot.png")




# Set up the camera position and angle
phivechalf = 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
phivecfull = c(phivechalf, rev(phivechalf))
thetavec = 0 + 60 * sin(seq(0,359,length.out = 360) * pi/180)
zoomvec = 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
zoomvecfull = c(zoomvec, rev(zoomvec))


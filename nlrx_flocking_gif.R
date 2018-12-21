library(nlrx)

nl <- nl(nlversion = "6.0.4",
         nlpath = "/home/marco/Documents/rpackages/nlrx_usecase/1_Helper/NetLogo 6.0.4/",
         modelpath = "/home/marco/Documents/rpackages/nlrx_usecase/1_Helper/NetLogo 6.0.4/app/models/Sample Models/Biology/Flocking.nlogo",
         jvmmem = 1024)

nl@experiment <- experiment(expname = "waves",
                            outpath = "/home/marco/Desktop/",
                            repetition = 1,      # If repetitions is > 1, a different random seed will be set for each netlogo run
                            tickmetrics = "true",
                            idsetup = "setup",   # you can define multiple setup procedures with c()
                            idgo = "go",         # you can define multiple go procedures with c()
                            idfinal = NA_character_,  # you can define one or more final commands here
                            idrunnum = NA_character_,
                            runtime = 300,
                            evalticks = seq(1,300),
                            constants = list("population" = 100,
                                             "vision" = 5,
                                             "minimum-separation" = 1, 
                                             "max-align-turn" = 4,
                                             "max-cohere-turn" = 4,
                                             "max-separate-turn" = 4),
                            metrics.turtles = c("who", "xcor", "ycor", "heading", "color"))

# Evaluate if variables and constants are valid:
eval_variables_constants(nl)

## Step3: Add a Simulation Design
nl@simdesign <- simdesign_simple(nl = nl,
                                 nseeds = 1)


# Step4: Run simulations:
results <- run_nl_all(nl = nl)

setsim(nl, "simoutput") <- results

library(tidyverse)
library(gganimate)

nl_spatial <- get_nl_spatial(nl,
                             patches = FALSE,
                             turtle_coords = "x",
                             format = "tibble")

nl_spatial <- nl_spatial %>% 
  dplyr::select(step, who, xcor, ycor, heading, color) 

p1 <- ggplot(nl_spatial, aes(xcor, ycor)) +
  geom_point(aes(color = who)) +
  scale_color_viridis_c() +
  guides(color = FALSE) +
  geom_spoke(aes(angle = heading), radius = 0.7) +
  transition_time(step) +
  coord_equal() +
  theme_void() 


g1 <- gganimate::animate(p1, width=800, height=800, fps = 5)
g1

gganimate::anim_save("flocking.gif", g1)

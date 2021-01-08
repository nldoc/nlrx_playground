
## Load packages
library(nlrx)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(cartography) 
library(rcartocolor)
library(ggthemes) 

# Windows default NetLogo installation path (adjust to your needs!):
netlogopath <- file.path("C:/Program Files/NetLogo 6.0.4")
outpath <- file.path("C:/out")
# Unix default NetLogo installation path (adjust to your needs!):
netlogopath <- file.path("/home/NetLogo 6.0.4")
outpath <- file.path("/home/out")

## Step1: Create a nl obejct:
nl <- nl(nlversion = "6.0.4",
         nlpath = netlogopath,
         modelpath = file.path(netlogopath, "app/models/Sample Models/Biology/Flocking.nlogo"),
         jvmmem = 1024)

## Step2: Add Experiment
nl@experiment <- experiment(expname = "flocking",
                            outpath = outpath,
                            repetition = 1,      
                            tickmetrics = "true",
                            idsetup = "setup",   
                            idgo = "go",         
                            runtime = 300,
                            evalticks = seq(1,300),
                            constants = list("population" = 100,
                                             "vision" = 5,
                                             "minimum-separation" = 1, 
                                             "max-align-turn" = 4,
                                             "max-cohere-turn" = 4,
                                             "max-separate-turn" = 4),
                            metrics.turtles = list("turtles" = c("who", "xcor", "ycor", "heading", "color")))

# Evaluate if variables and constants are valid:
eval_variables_constants(nl)

## Step3: Add a Simulation Design
nl@simdesign <- simdesign_simple(nl = nl,
                                 nseeds = 1)


# Step4: Run simulations:
results <- run_nl_all(nl = nl)

setsim(nl, "simoutput") <- results

nl_spatial <- unnest_simoutput(nl)

## Calculate angles for plotting:
# (a) convert NetLogo degree headings (clockwise with 0 = top) to geom_text degree angle (counter-clockwise with 0 = right)
# (b) convert geom_text degree angle to geom_spoke radians angle
nl_spatial <- nl_spatial %>% 
  dplyr::select(`[step]`, who, xcor, ycor, heading, color) %>% 
  mutate(heading_text = ((heading * -1) + 90)) %>% 
  mutate(heading_radians = ((heading_text * pi) / 180))

# Plot with geom_point and geom_spoke
p1 <- ggplot(nl_spatial, aes(x=xcor, y=ycor)) +
  geom_point(aes(color=color, group=who)) +
  geom_spoke(aes(color=color, group=who, angle=heading_radians), arrow=arrow(length=unit(0.2, "inches")), radius=1) +
  scale_color_viridis_c() +
  guides(color=FALSE) +
  transition_time(`[step]`) +
  coord_equal() +
  theme_void() 

gganimate::animate(p1, nframes=max(nl_spatial$`[step]`), width=600, height=600, fps=20)
gganimate::anim_save("gif/flocking_spoke.gif", g1)


# Plot with geom_text
p2 <- ggplot(nl_spatial, aes(x=xcor, y=ycor)) +
  geom_text(aes(color=color, group=who, angle=heading_text), label="→", size=5) +
  scale_color_viridis_c() +
  guides(color=FALSE) +
  transition_time(`[step]`) +
  coord_equal() +
  theme_void() 

gganimate::animate(p2, nframes=max(nl_spatial$`[step]`), width=600, height=600, fps=20)
gganimate::anim_save("gif/flocking_text.gif", g1)

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
         modelpath = file.path(netlogopath, "app/models/Sample Models/Art/Diffusion Graphics.nlogo"),
         jvmmem = 1024)

## Step2: Add Experiment
nl@experiment <- experiment(expname = "diffusion",
                            outpath = outpath,
                            repetition = 1,   
                            tickmetrics = "true",
                            idsetup = "setup",  
                            idgo = "go",        
                            runtime = 500,
                            evalticks = seq(1,500),
                            constants = list("num-turtles" = 20,
                                             "diffusion-rate" = 1,
                                             "turtle-heat" = 139,
                                             "turtle-speed" = 1.0,
                                             "wander?" = "TRUE"),
                            metrics.turtles = list("turtles" = c("who", "xcor", "ycor")),
                            metrics.patches = c("pxcor", "pycor", "pcolor", "heat"))

# Evaluate if variables and constants are valid:
eval_variables_constants(nl)

## Step3: Add a Simulation Design
nl@simdesign <- simdesign_simple(nl = nl,
                                 nseeds = 1)


# Step4: Run simulations:
results <- run_nl_all(nl = nl)

## Postprocessing:
## Step1: Attach results to nl:
setsim(nl, "simoutput") <- results


# Prepare data for plotting
nl_spatial <- unnest_simoutput(nl)
n <- nl@experiment@runtime

turtles <- nl_spatial %>% dplyr::filter(agent == "turtles" & `[step]` < n) %>% dplyr::select(xcor, ycor, `[step]`, who)
patches <- nl_spatial %>% dplyr::filter(agent == "patches"& `[step]` < n) %>% dplyr::select(pxcor, pycor, pcolor, `[step]`)

## Plot animation:
p1 <- ggplot() +
  geom_tile(data = patches, aes(x=pxcor, y = pycor, fill=pcolor)) +
  geom_point(data = turtles, aes(x = xcor, y = ycor, group = who), size=2) +
  scale_fill_carto_c(palette = "Prism") +
  transition_time(`[step]`) +
  guides(fill="none", color="none") +
  coord_equal() +
  theme_void() 

gganimate::animate(p1, nframes = n, width=800, height=800, duration=6)
gganimate::anim_save("gif/diffusion.gif")


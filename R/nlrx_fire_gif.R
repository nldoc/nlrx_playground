
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
         modelpath = file.path(netlogopath, "app/models/Sample Models/Earth Science/Fire.nlogo"),
         jvmmem = 1024)

## Step2: Add Experiment
nl@experiment <- experiment(expname = "fire",
                            outpath = outpath,
                            repetition = 1,    
                            tickmetrics = "true",
                            idsetup = "setup", 
                            idgo = "go",       
                            runtime = 500,
                            evalticks = seq(1,500),
                            metrics = c("count patches"),
                            metrics.turtles = list("turtles" = c("who", "pxcor", "pycor", "breed", "color")),
                            metrics.patches = c("pxcor", "pycor", "pcolor"),
                            constants = list('density' = 62)
)

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

nl_spatial <- unnest_simoutput(nl)
n <- max(nl_spatial$`[step]`)

embers <- nl_spatial %>% dplyr::filter(breed == "embers" & agent == "turtles" & `[step]` < n) %>% dplyr::select(pxcor, pycor, `[step]`, color, who)
fires <- nl_spatial %>% dplyr::filter(breed == "fires" & agent == "turtles" & `[step]` < n) %>% dplyr::select(pxcor, pycor, `[step]`, color, who)
patches <- nl_spatial %>% dplyr::filter(agent == "patches" & `[step]` < n) %>% dplyr::select(pxcor, pycor, pcolor, `[step]`)

# make same space in the memory
rm(nl)
rm(results)
rm(nl_spatial)
gc()

#----------------------------------
## Plot animation:
p1 <- ggplot(embers) +
  geom_tile(data = patches, aes(x=pxcor, y=pycor, fill=factor(pcolor))) +
  geom_point(data = embers, aes(x = pxcor, y = pycor, color = color, group = who), size=2) +
  scale_color_gradientn(colors = rev(cartography::carto.pal("orange.pal", n1 = 8))) +
  geom_point(data = fires, aes(x = pxcor, y = pycor, color = color, group = who), size=3) +
  scale_fill_manual(values = c("0" = "gray24", "55" = "#83B37D", "11.4" = "#B59A89")) +
  transition_time(`[step]`) +
  guides(fill="none", color="none") +
  coord_equal() +
  theme_void() 

g1 <- gganimate::animate(p1, nframes = n, width=800, height=800, duration = 6)
gganimate::anim_save("gif/fire.gif", g1)

# g2 <- gganimate::animate(p1, nframes = n - 1, width=800, height=800, fps = 20)
# gganimate::anim_save("/home/marco/Downloads/fire2.gif", g2)

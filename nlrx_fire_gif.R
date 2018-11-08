library(nlrx)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Step1: Create a nl obejct:
nl <- nl(nlversion = "6.0.4",
         nlpath = "/home/marco/Documents/NetLogo 6.0.4/",
         modelpath = "/home/marco/Documents/NetLogo 6.0.4/app/models/Sample Models/Earth Science/Fire.nlogo",
         jvmmem = 1024)

## Step2: Add Experiment

# Inspect the model available model parameters:
load_model_parameters(nl)

nl@experiment <- experiment(expname = "firegif",
                            outpath = "/home/marco/Desktop/",
                            repetition = 1,      # If repetitions is > 1, a different random seed will be set for each netlogo run
                            tickmetrics = "true",
                            idsetup = "setup",   # you can define multiple setup procedures with c()
                            idgo = "go",         # you can define multiple go procedures with c()
                            idfinal = NA_character_,  # you can define one or more final commands here
                            idrunnum = NA_character_,
                            runtime = 500,
                            evalticks = seq(1,500),
                            metrics = c("count patches"),
                            metrics.turtles = c("who", "pxcor", "pycor", "breed", "color"),
                            metrics.patches = c("pxcor", "pycor", "pcolor"),
                            constants = list('density' = 62)
)

# Evaluate if variables and constants are valid:
eval_variables_constants(nl)

## Step3: Add a Simulation Design
nl@simdesign <- simdesign_simple(nl = nl,
                                 nseeds = 1)


# Step4: Run simulations:
results <- run_nl_all(nl = nl, cleanup = "all")

## Postprocessing:
## Step1: Attach results to nl:
setsim(nl, "simoutput") <- results

library(tidyverse)
library(ggplot2)
library(gganimate)
library(emojifont)
library(nlrx)

nl_spatial <- get_nl_spatial(nl, format = "tibble")

embers <- nl_spatial %>% dplyr::filter(breed == "embers" & group == "turtles") %>% dplyr::select(pxcor, pycor, step, color, who)
fires <- nl_spatial %>% dplyr::filter(breed == "fires" & group == "turtles") %>% dplyr::select(pxcor, pycor, step, color, who)
patches <- nl_spatial %>% dplyr::filter(group == "patches") %>% dplyr::select(patches_x, patches_y, pcolor, step)

# embers <- embers %>% dplyr::filter(step > 300 & step < 310)
# fires <-  fires %>% dplyr::filter(step > 300 & step < 310)
# patches  <- patches %>%  dplyr::filter(step > 300 & step < 310)

#----------------------------------
## Plot animation:
p1 <- ggplot(embers) +
  geom_tile(data = patches, aes(x=patches_x, y=patches_y, fill=factor(pcolor))) +
  geom_point(data = embers, aes(x = pxcor, y = pycor, color = color, group = who), size=2) +
  # scale_color_viridis_c(option = "A") +
  # scale_color_gradientn(colours = heat.colors(7)) +
  scale_color_gradientn(colors = rev(cartography::carto.pal("orange.pal", n1 = 8))) +
  geom_point(data = fires, aes(x = pxcor, y = pycor, color = color, group = who), size=3) +
  # scale_color_manual(values = "firebrick") +
  # geom_text(aes(x = X, y = Y, group=who, color = breed, label=breed), size=6, family="EmojiOne") +
  scale_fill_manual(values = c("0" = "gray24", "55" = "#83B37D", "11.4" = "#B59A89")) +
  #theme(legend.text=element_text(family='Emojione'))
  #facet_wrap(~`random-seed`) +
  transition_time(step) +
  # shadow_trail(distance = 0.01, size = 0.1) +
  #enter_appear() +
  #exit_disappear() +
  guides(fill="none", color="none") +
  coord_equal() +
  # labs(title = 'Step: {frame_time}') +
  theme_void() 
  
g1 <- gganimate::animate(p1, nframes = 500, width=800, height=800, fps=10)
gganimate::anim_save("/home/marco/Downloads/fire.gif", g1)


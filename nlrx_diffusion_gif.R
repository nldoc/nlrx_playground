library(nlrx)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Step1: Create a nl obejct:
nl <- nl(nlversion = "6.0.4",
         nlpath = "/home/marco/Documents/NetLogo 6.0.4/",
         modelpath = "/home/marco/Documents/NetLogo 6.0.4/app/models/Sample Models/Art/Diffusion Graphics.nlogo",
         jvmmem = 1024)

## Step2: Add Experiment

# Inspect the model available model parameters:
load_model_parameters(nl)

nl@experiment <- experiment(expname = "diffusion",
                            outpath = "/home/marco/Desktop/",
                            repetition = 1,      # If repetitions is > 1, a different random seed will be set for each netlogo run
                            tickmetrics = "true",
                            idsetup = "setup",   # you can define multiple setup procedures with c()
                            idgo = "go",         # you can define multiple go procedures with c()
                            idfinal = NA_character_,  # you can define one or more final commands here
                            idrunnum = NA_character_,
                            runtime = 500,
                            evalticks = seq(1,500),
                            constants = list("num-turtles" = 20,
                                             "diffusion-rate" = 1,
                                             "turtle-heat" = 139,
                                             "turtle-speed" = 1.0,
                                             "wander?" = "TRUE"),
                            metrics.turtles = c("who", "xcor", "ycor"),
                            metrics.patches = c("pxcor", "pycor", "pcolor", "heat"))

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


# Prepare data for plotting
library(tidyverse)
library(ggplot2)
library(gganimate)
library(emojifont)

nl_spatial <- get_nl_spatial(nl, format = "tibble")

n <- 500

turtles <- nl_spatial %>% dplyr::filter(group == "turtles" & step < n) %>% dplyr::select(xcor, ycor, step, who)
patches <- nl_spatial %>% dplyr::filter(group == "patches"& step < n) %>% dplyr::select(patches_x, patches_y, pcolor, step)

## Plot animation:
p1 <- ggplot() +
  geom_tile(data = patches, aes(x=patches_x, y = patches_y, fill=pcolor)) +
  geom_point(data = turtles, aes(x = xcor, y = ycor, group = who), size=2) +
  rcartocolor::scale_fill_carto_c(palette = "Prism") +
  transition_time(step) +
  guides(fill="none", color="none") +
  coord_equal() +
  # labs(title = 'Step: {frame_time}') +
  theme_void() 

gganimate::animate(p1, width=800, height=800, duration=6)
gganimate::anim_save("diffusion.gif")


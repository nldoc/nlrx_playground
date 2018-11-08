## Load nlrx package
library(nlrx)

## Step1: Create a nl obejct:
nl <- nl(nlversion = "6.0.3",
         nlpath = "C:/Program Files/NetLogo 6.0.3/",
         modelpath = "C:/Program Files/NetLogo 6.0.3/app/models/Sample Models/Biology/Ants.nlogo",
         jvmmem = 1024)

## Step2: Add Experiment
nl@experiment <- experiment(expname = "nlrxtest",
                            outpath = "C:/out",
                            repetition = 1,      # If repetitions is > 1, a different random seed will be set for each netlogo run
                            tickmetrics = "true",
                            idsetup = "setup",   # you can define multiple setup procedures with c()
                            idgo = "go",         # you can define multiple go procedures with c()
                            runtime = 1000,
                            evalticks = seq(1,1000),
                            metrics.turtles = c("who", "pxcor", "pycor", "breed"),
                            metrics.patches = c("pxcor", "pycor", "pcolor", "chemical", "food", "food-source-number"),
                            constants = list("population" = 125,
                                             'diffusion-rate' = 50,
                                             'evaporation-rate' = 10))

## Step3: Add a Simulation Design
nl@simdesign <- simdesign_simple(nl = nl,
                                 nseeds = 1)


## Step4: Run simulations:
future::plan(future::multiprocess)
results <- run_nl_all(nl = nl, cleanup = "all")

## Step5: Attach results to nl and reformat spatial data with get_nl_spatial()
setsim(nl, "simoutput") <- results
nl_spatial <- get_nl_spatial(nl, format = "tibble")

## Step6: Prepare data for plotting
library(tidyverse)
nmax <- 900
food_dat <- nl_spatial %>% dplyr::filter(food > 0) %>% dplyr::filter(step %in% 1:nmax) %>% dplyr::select(patches_x, patches_y, step)
chem_dat <- nl_spatial %>% dplyr::filter(step %in% 1:nmax) %>% dplyr::select(patches_x, patches_y, chemical, step)
chem_dat$chemical <- ifelse(chem_dat$chemical <= 0.2, NA, chem_dat$chemical)
turt_dat <- nl_spatial %>% dplyr::filter(!is.na(who)) %>% dplyr::filter(step %in% 1:nmax) %>% dplyr::select(pxcor, pycor, who, step)
nest_dat <- data.frame(x=rep(0, nmax), y=rep(0, nmax), step=1:nmax)

## Step7: Create plot with transition_time
library(emojifont)
library(ggthemes)
library(gganimate)

p1 <- ggplot(food_dat) +
  geom_tile(data=chem_dat, aes(x=patches_x, y=patches_y, fill=sqrt(chemical))) +
  geom_point(data=food_dat, aes(x=patches_x, y=patches_y), color="black", shape=15, size=4.5, alpha=1) +
  geom_text(data=turt_dat, aes(x=pxcor, y=pycor, group=who, color=as.numeric(who)), size=5, alpha=1, label=emoji("ant")) +
  geom_point(data=nest_dat, aes(x=x, y=y), color="brown", fill="white", size=30, stroke=2, shape=21) +
  scale_fill_viridis_c(direction=-1, option="magma", na.value = "white") +
  scale_color_gradient_tableau(palette="Orange") +
  transition_time(step) +
  guides(fill="none", color="none") +
  coord_equal() +
  labs(title = 'Step: {frame_time}') +
  theme_void()

## Step8: Animate the plot and use 1 frame for each step of the model simulations
gganimate::animate(p1, nframes = max(food_dat$step), width=800, height=800, fps=10)
anim_save("ants_world.gif")


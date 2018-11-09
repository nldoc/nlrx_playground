library(nlrx)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Step1: Create a nl obejct:
nl <- nl(nlversion = "6.0.4",
         nlpath = "/home/marco/Documents/NetLogo 6.0.4/",
         modelpath = "/home/marco/Documents/NetLogo 6.0.4/app/models/Sample Models/Chemistry & Physics/Waves/Wave Machine.nlogo",
         jvmmem = 1024)

## Step2: Add Experiment
nl@experiment <- experiment(expname = "waves",
                            outpath = "/home/marco/Desktop/",
                            repetition = 1,      # If repetitions is > 1, a different random seed will be set for each netlogo run
                            tickmetrics = "true",
                            idsetup = "setup",   # you can define multiple setup procedures with c()
                            idgo = "go",         # you can define multiple go procedures with c()
                            idfinal = NA_character_,  # you can define one or more final commands here
                            idrunnum = NA_character_,
                            runtime = 100,
                            evalticks = seq(1,100),
                            variables = list('friction' = list(values = c(5,25,50,90))),
                            constants = list("stiffness" = 20),
                            metrics.turtles = c("who", "xcor", "ycor", "driver?", "edge?", "color"))


# Evaluate if variables and constants are valid:
eval_variables_constants(nl)

## Step3: Add a Simulation Design
nl@simdesign <- simdesign_distinct(nl = nl,
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

nl_spatial <- get_nl_spatial(nl, format = "tibble", patches = FALSE)

nl_spatial$friction <- factor(nl_spatial$friction)
levels(nl_spatial$friction) <- c("Friction = 5",
                                 "Friction = 25",
                                 "Friction = 50",
                                 "Friction = 90")

n <- 100

waves  <- nl_spatial %>% dplyr::filter(`driver?` == "false" & `edge?` == "false" & step < n) %>% dplyr::select(xcor, ycor, step, who, color, friction)
egde   <- nl_spatial %>% dplyr::filter(`driver?` == "false" & `edge?` == "true" & step < n) %>% dplyr::select(xcor, ycor, step, who, friction)
driver <- nl_spatial %>% dplyr::filter(`driver?` == "true" & `edge?` == "false" & step < n) %>% dplyr::select(xcor, ycor, step, who, friction)



p1 <- ggplot(waves) +
  geom_point(data = waves, aes(x = xcor, y = ycor, group = who, color = color), size=2) +
  geom_point(data = driver, aes(x = xcor, y = ycor, group = who), size=2, color = "grey") +
  geom_point(data = egde, aes(x = xcor, y = ycor, group = who), size=2, color = "black") +
  facet_wrap(~friction) +
  transition_time(step) +
  guides(color="none") +
  coord_equal() +
  theme_void() 

gganimate::animate(p1, width=800, height=800, duration = 6)
gganimate::anim_save("waves.gif")

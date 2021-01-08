
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

## The models library filepath pointing to the waves model contains an ampersand which needs to be escaped on Windows but not on Linux.
## Choose the filepath according to your OS:
## Windows:
modelpath <- "app/models/Sample Models/Chemistry ^& Physics/Waves/Wave Machine.nlogo"
## Linux:
modelpath <- "app/models/Sample Models/Chemistry & Physics/Waves/Wave Machine.nlogo"

## Step1: Create a nl obejct:
nl <- nl(nlversion = "6.0.4",
         nlpath = netlogopath,
         modelpath = file.path(netlogopath, modelpath),
         jvmmem = 1024)

## Step2: Add Experiment
nl@experiment <- experiment(expname = "waves",
                            outpath = outpath,
                            repetition = 1,      
                            tickmetrics = "true",
                            idsetup = "setup",  
                            idgo = "go",
                            runtime = 100,
                            evalticks = seq(1,100),
                            variables = list('friction' = list(values = c(5,25,50,90))),
                            constants = list("stiffness" = 20),
                            metrics.turtles = list("turtles" = c("who", "xcor", "ycor", "driver?", "edge?", "color")))

## Step3: Add a Simulation Design
nl@simdesign <- simdesign_distinct(nl = nl,
                                   nseeds = 1)


# Step4: Run simulations:
results <- run_nl_all(nl = nl)

## Postprocessing:
## Step1: Attach results to nl:
setsim(nl, "simoutput") <- results


# Prepare data for plotting
nl_spatial <- unnest_simoutput(nl)

nl_spatial$friction <- factor(nl_spatial$friction)
levels(nl_spatial$friction) <- c("Friction = 5",
                                 "Friction = 25",
                                 "Friction = 50",
                                 "Friction = 90")

n <- nl@experiment@runtime

waves  <- nl_spatial %>% dplyr::filter(`driver?` == "false" & `edge?` == "false" & `[step]` < n) %>% dplyr::select(xcor, ycor, `[step]`, who, color, friction)
egde   <- nl_spatial %>% dplyr::filter(`driver?` == "false" & `edge?` == "true" & `[step]` < n) %>% dplyr::select(xcor, ycor, `[step]`, who, friction)
driver <- nl_spatial %>% dplyr::filter(`driver?` == "true" & `edge?` == "false" & `[step]` < n) %>% dplyr::select(xcor, ycor, `[step]`, who, friction)

p1 <- ggplot(waves) +
  geom_point(data = waves, aes(x = xcor, y = ycor, group = who, color = color), size=2) +
  geom_point(data = driver, aes(x = xcor, y = ycor, group = who), size=2, color = "grey") +
  geom_point(data = egde, aes(x = xcor, y = ycor, group = who), size=2, color = "black") +
  facet_wrap(~friction) +
  transition_time(`[step]`) +
  guides(color="none") +
  coord_equal() +
  theme_void() 

gganimate::animate(p1, width=800, height=800, duration = 6)
gganimate::anim_save("gif/waves.gif")

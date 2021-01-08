
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
         modelpath = file.path(netlogopath, "app/models/Sample Models/Networks/Preferential Attachment.nlogo"),
         jvmmem = 1024)

## Step2: Add Experiment
nl@experiment <- experiment(expname = "networks",
                            outpath = outpath,
                            repetition = 1,    
                            tickmetrics = "true",
                            idsetup = "setup", 
                            idgo = "go",       
                            runtime = 200,
                            evalticks = seq(1,200),
                            constants = list("layout?" = TRUE),
                            metrics.turtles = list("turtles" = c("who", 
                                                                 "xcor",                            
                                                                 "ycor",
                                                                 "color")),
                            metrics.links = list("links" = c("[who] of end1","[who] of end2")))

## Step3: Add simdesign
nl@simdesign <- simdesign_simple(nl=nl, nseeds = 1)

## Run simulation:
results <- run_nl_one(nl = nl,
                      seed = getsim(nl, "simseeds")[1],
                      siminputrow = 1)

## Attach results to nl
setsim(nl, "simoutput") <- results

## Postprocess spatial metrics with get_nl_spatial:
nl_spatial <- unnest_simoutput(nl)

## Subset nl_spatial using the group column:
nl_links <- nl_spatial %>%
  dplyr::filter(agent == "links") %>% 
  dplyr::select(-who, -xcor, -ycor, -color)

nl_turtles <- nl_spatial %>%
  dplyr::filter(agent == "turtles") %>% 
  dplyr::select(`[step]`, who, xcor, ycor, color)

## Reference who numbers of link start (end1) and end (end2) points to actual coordinates:
nl_links <- nl_links %>% 
  dplyr::left_join(nl_turtles, by=c("[step]"="[step]","end1" = "who")) %>% 
  dplyr::left_join(nl_turtles, by=c("[step]"="[step]","end2" = "who"))

## Plot:
p1 <- ggplot() +
  geom_point(data=nl_turtles, aes(x = xcor, y = ycor, group=who), color="red", size=2) +
  geom_segment(data=nl_links, aes(x = xcor.x, y = ycor.x, xend = xcor.y, yend = ycor.y), size=0.5) +
  transition_time(`[step]`) +
  coord_equal() +
  theme_void()

gganimate::animate(p1, nframes = max(nl_turtles$`[step]`), width=400, height=400, fps=8)
gganimate::anim_save("gif/networks.gif")
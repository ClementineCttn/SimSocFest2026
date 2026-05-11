#experiment for reviewer 1

library(tidyverse)

phase <- read.csv("~/GitHub/Paper/Notebook_R_fig_tables/data/phase_48.csv")

long_table <- data.frame()
i <- 0
j <- 1
time <- data.frame(
  day = rep("day",48),
  nd = sort(rep(0:47,3)),
  step = rep("step",48),
  ns = rep(0:2,48)) |>
  mutate(time = paste0(id,day, nd, "_", step, ns)) |>
  select(time)


for(i in 1:nrow(phase)){
  nsenario <- phase[i, "numOfScenario"]
  EII <- t(
    str_split(
      str_replace(
        str_replace( phase[i, "socialInequality"] , c("\\["), ""), 
        c("\\["), ""
      ),
      ",", simplify = T
    )
  )[1:144,]
  healthy <- t(
    str_split(
      str_replace(
        str_replace( phase[i, "numberOfHealthy"] , c("\\["), ""), 
        c("\\["), ""
      ),
      ",", simplify = T
    )
  )[1:144,]
  
  
  long_table[j:(j+143),"time"] <- time
  long_table[j:(j+143),"scenario"] <- rep(nsenario, 144)
  long_table[j:(j+143),"socialInequality"] <- EII
  long_table[j:(j+143),"numberOfHealthy"] <- healthy
  
  j <- j+144
  
}

plot_table <- long_table |>
  mutate(prop_healthy = as.numeric(numberOfHealthy) / 8768898,
         EII = as.numeric(socialInequality),
         scenario = as.factor(ifelse(scenario == 2, "1B", 
                                     ifelse(scenario == 5, "2C", NA))
         ),
         group = paste0(time, scenario),
         id = rep(1:144, 200)
  ) 

ggplot(plot_table, 
       aes(x = id, y = EII,
           colour = scenario, fill = scenario)) +
  geom_violin(aes(group=group), alpha = 0.6) +
  # geom_point() +
  scale_colour_manual(values = c("mediumorchid4", "darkorange2")) +
  scale_fill_manual(values = c("mediumorchid4","darkorange2")) +
  xlab("time") +
  ylab("Social inequality index (EII)")  +
  theme(legend.position = "bottom") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1.0)) +
  theme(axis.text.x.bottom = element_text(margin=margin(t=0, r=0,b=10,l=0)))

ggplot(plot_table, 
       aes(x = id, y = prop_healthy,
           colour = scenario, fill = scenario)) +
  geom_violin(aes(group=group), alpha = 0.6) +
  # geom_point() +
  scale_colour_manual(values = c("mediumorchid4", "darkorange2")) +
  scale_fill_manual(values = c("mediumorchid4","darkorange2")) +
  xlab("time") +
  ylab("Proportion of healthy agents")  +
  theme(legend.position = "bottom") +
 theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1.0)) +
  theme(axis.text.x.bottom = element_text(margin=margin(t=0, r=0,b=10,l=0)))


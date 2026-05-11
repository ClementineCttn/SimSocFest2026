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



sim <- read.csv2("data/results-25042022-10000replications.csv", sep=",", dec=".")
sim$Scenario <- ifelse(sim$numOfScenario == 1, "1A: Random residence / no move",
                       ifelse(sim$numOfScenario == 2, "1B: Random residence / random moves",
                              ifelse(sim$numOfScenario == 3, "2A: Observed residence / no move",
                                     ifelse(sim$numOfScenario == 4, "2B: Observed residence / random moves",
                                            "2C: Observed residence / observed moves"))))

mean_1A <- mean(sim[sim$numOfScenario == 1,"socialInequality"])
mean_1B <- mean(sim[sim$numOfScenario == 2,"socialInequality"])
mean_2A <- mean(sim[sim$numOfScenario == 3,"socialInequality"])
mean_2B <- mean(sim[sim$numOfScenario == 4,"socialInequality"])
mean_2C <- mean(sim[sim$numOfScenario == 5,"socialInequality"])

sim %>%
  ggplot( aes(x=socialInequality, group=Scenario, fill=Scenario)) +
  geom_vline(xintercept=1.405, size=1, color="grey30", linetype = "dashed") +
  geom_density(alpha=0.6) +
  scale_fill_manual(values=c("firebrick2", "mediumorchid4", "dodgerblue2", "darkorange2", "springgreen3"))+
  theme_bw() +
  geom_vline(xintercept=mean_1A, size=0.4, color="firebrick2") +
  geom_vline(xintercept=mean_1B, size=0.4, color="mediumorchid4") +
  geom_vline(xintercept=mean_2A, size=0.4, color="dodgerblue2") +
  geom_vline(xintercept=mean_2B, size=0.4, color="darkorange2") +
  geom_vline(xintercept=mean_2C, size=0.4, color="springgreen3") +
  annotate("text", x=mean_1A - 0.006, y= -8, size=3, label=round(mean_1A,3), color="firebrick2") +
  annotate("text", x=mean_1B - 0.006 , y= -8, size=3, label=round(mean_1B,3), color="mediumorchid4") +
  annotate("text", x=mean_2A - 0.006 , y= -8, size=3, label=round(mean_2A,3), color="dodgerblue2") +
  annotate("text", x=mean_2B + 0.006 , y= -8, size=3, label=round(mean_2B,3), color="darkorange2") +
  annotate("text", x=mean_2C + 0.006, y= -8, size=3, label=round(mean_2C,3), color="springgreen3") +
  annotate("text", x=1.405 + 0.02, y= 115, size=3, label="Value at initialisation", color="grey30") +
  xlab("Social Inequality Index") +
  ylab("Number of simulations") + 
  xlim(1.35,1.6) +
  guides(fill=guide_legend(title="Type of scenario",nrow=3,byrow=TRUE)) +
  theme(legend.position = "bottom") 



mean_1A <- mean(sim[sim$numOfScenario == 1,"numberOfHealthy"]/8778898)
mean_1B <- mean(sim[sim$numOfScenario == 2,"numberOfHealthy"]/8778898)
mean_2A <- mean(sim[sim$numOfScenario == 3,"numberOfHealthy"]/8778898)
mean_2B <- mean(sim[sim$numOfScenario == 4,"numberOfHealthy"]/8778898)
mean_2C <- mean(sim[sim$numOfScenario == 5,"numberOfHealthy"]/8778898)

sim %>%
  ggplot( aes(x=numberOfHealthy/8778898, group=Scenario, fill=Scenario)) +
  geom_vline(xintercept=0.0957, size=1, color="grey30", linetype = "dashed") +
  geom_density(alpha=0.6) +
  scale_fill_manual(values=c("firebrick2", "mediumorchid4", "dodgerblue2", "darkorange2", "springgreen3"))+
  theme_bw() +
  geom_vline(xintercept=mean_1A, size=0.4, color="firebrick2") +
  geom_vline(xintercept=mean_1B, size=0.4, color="mediumorchid4") +
  geom_vline(xintercept=mean_2A, size=0.4, color="dodgerblue2") +
  geom_vline(xintercept=mean_2B, size=0.4, color="darkorange2") +
  geom_vline(xintercept=mean_2C, size=0.4, color="springgreen3") +
  annotate("text", x=mean_1A + 0.0015, y= -300, size=3, label=round(mean_1A,3), color="firebrick2") +
  annotate("text", x=mean_1B - 0.0015 , y= -150, size=3, label=round(mean_1B,3), color="mediumorchid4") +
  annotate("text", x=mean_2A + 0.0015 , y= -150, size=3, label=round(mean_2A,3), color="dodgerblue2") +
  annotate("text", x=mean_2B + 0.0015 , y= -300, size=3, label=round(mean_2B,3), color="darkorange2") +
  annotate("text", x=mean_2C + 0.0015, y= -150, size=3, label=round(mean_2C,3), color="springgreen3") +
  annotate("text", x=0.0957 + 0.007, y= 115, size=3, label="Value at initialisation (0.0957)", color="grey30") +
  xlab("Share of agents with a healthy diet") +
  ylab("Number of simulations") + 
  xlim(0.09,0.15) +
  guides(fill=guide_legend(title="Type of scenario",nrow=3,byrow=TRUE)) +
  theme(legend.position = "bottom") 

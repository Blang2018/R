library("ggplot2")
library("ggpubr")
library("dplyr")

args = commandArgs(trailingOnly = TRUE)
ROOT_PATH <- paste(args[1], "/", sep="")
atks_star <- read.csv(paste(ROOT_PATH, "/", "atks_star.csv", sep=""))
defs_star <- read.csv(paste(ROOT_PATH, "/", "defs_star.csv", sep=""))
home      <- read.csv(paste(ROOT_PATH, "/",  "home.csv", sep=""))
intercept <- read.csv(paste(ROOT_PATH, "/",  "intercept.csv", sep=""))
sd_atk    <- read.csv(paste(ROOT_PATH, "/",  "sd_atk.csv", sep=""))
sd_def    <- read.csv(paste(ROOT_PATH, "/",  "sd_def.csv", sep=""))


atks <- atks_star %>%
  group_by(sample) %>%
  mutate_at(vars(value), funs(subtr_mean = . - mean(.)))

defs <- defs_star %>%
  group_by(sample) %>%
  mutate_at(vars(value), funs(subtr_mean = . - mean(.)))

plot_astar <- ggplot(atks_star, aes(x = value, group = index_0, color = index_0)) + geom_density(size=1) + 
  scale_color_distiller(palette = "Set3") + 
  labs(colour = "Team Index", title = "atks_star") + xlim(-1, 1) + theme(legend.position = "none")

trace_astar <- ggplot(atks_star, aes(x = sample, y = value, group = index_0, color = index_0)) +
  geom_line() + ylim(.6,-.6) + scale_color_distiller(palette = "Set3") + theme(legend.position = "none")

plot_dstar <- ggplot(defs_star, aes(x = value, group = index_0, color = index_0)) + geom_density(size=1) + 
  scale_color_distiller(palette = "Set3") +
  labs(colour = "Team Index", title = "defs_star") + xlim(-1, 1) + theme(legend.position = "none")

trace_dstar <- ggplot(defs_star, aes(x = sample, y = value, group = index_0, color = index_0)) +
  geom_line() + ylim(-1.2, 1.2) + scale_color_distiller(palette = "Set3") + theme(legend.position = "none")

plot_home <- ggplot(home, aes(x = value)) + geom_density(size=1, colour="turquoise") +
  labs(title = "home") + xlim(0,0.33)

trace_home <- ggplot(home, aes(x = sample, y = value)) +
  geom_line(color="turquoise") + ylim(0.0, 0.33) 

plot_sda <- ggplot(sd_atk, aes(x = value)) + geom_density(size = 1, colour="violet") + labs(title = "sd_atk") +
  xlim(0, 1.0)
plot_sdd <- ggplot(sd_def, aes(x = value)) + geom_density(size = 1, colour="violet") + labs(title = "sd_def") +
  xlim(0, 1.4)

trace_sda <- ggplot(sd_atk, aes(x = sample, y = value)) +
  geom_line(color = "violet") + ylim(0, 1)

trace_sdd <- ggplot(sd_def, aes(x = sample, y = value)) +
  geom_line(color = "violet") + ylim(0, 1)

plot_int <- ggplot(intercept, aes(x = value)) + geom_density(size=1, colour="turquoise") + 
  labs(title = "intercept") + xlim(2.8, 3.0)

trace_int <- ggplot(intercept, aes(x = sample, y = value)) +
  geom_line(color="turquoise") + ylim(2.75, 3.1)

plot_atks <- ggplot(atks, aes(x = subtr_mean, group = index_0, color = index_0)) + geom_density(size = 1) +
  scale_color_distiller(palette = "Set3") + labs(colour = "Team Index", title = "atks") + xlim(-0.4,0.4) + theme(legend.position = "none")

trace_atks <- ggplot(atks, aes(x = sample, y = subtr_mean, group = index_0, color = index_0)) +
  geom_line() + scale_color_distiller(palette = "Set3") + ylim(-0.5, 0.5) + theme(legend.position = "none")

plot_defs <- ggplot(defs, aes(x = subtr_mean, group = index_0, color = index_0)) + geom_density(size = 1) +
  scale_color_distiller(palette = "Set3") + labs(colour = "Team Index", title = "defs") + xlim(-.6, 0.8) + theme(legend.position = "none")

trace_defs <- ggplot(defs, aes(x = sample, y = subtr_mean, group = index_0, color = index_0)) +
  geom_line() + scale_color_distiller(palette = "Set3") + ylim(-0.5, 0.7) + theme(legend.position = "none")


ggarrange(plot_astar, trace_astar,
          plot_dstar, trace_dstar,
          plot_home,  trace_home, 
          plot_sda, trace_sda,
          plot_sdd, trace_sdd,
          plot_int, trace_int,
          plot_atks, trace_atks,
          plot_defs, trace_defs,
          ncol=2, nrow=8)

ggsave("blang-rugby-posterior.pdf")

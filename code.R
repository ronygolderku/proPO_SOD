# install.packages('ggplot2')

############################# Figure 4##########################################################
library(ggplot2)
library(readr)
z <- c("2", "4", "6", "2", "4", "6", "2", "4", "6")
n <- bquote(atop(Stocking ~ density ~ (m^-2)))
d2 <- read_csv("d2.csv")
ggplot(d2, aes(x = Pond, y = Mean, fill = z)) +
    geom_bar(stat = "identity", color = "black", position = position_dodge()) +
    geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd),
        width = .2,
        position = position_dodge(.9)
    ) +
    labs(fill = z)
p <- ggplot(d2, aes(x = Pond, y = Mean, fill = z)) +
    geom_bar(stat = "identity", color = "black", position = position_dodge()) +
    geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd),
        width = .2,
        position = position_dodge(.9)
    ) +
    labs(fill = n)
p + geom_text(aes(label = sig, y = (Mean + Sd) + 0.01),
    position = position_dodge(0.9),
    vjust = 0
) + xlab("Probiotics") +
    ylab(expression(paste("pro-PO enzyme activity of   ", italic("M. rosenbergii")))) +
    theme_classic(base_size = 12, base_family = "serif") +
    scale_fill_manual(
        breaks = c("2", "4", "6"),
        values = c("white", "grey", "black")
    ) + theme(legend.position = "top") +
    theme(axis.title.x = element_blank())
ggsave("Figure 4.png")
################################ Figure 2###################################################################
library(ggplot2)
library(readr)
z <- c("2", "4", "6", "2", "4", "6", "2", "4", "6")
n <- bquote(atop(Stocking ~ density ~ (m^-2)))
b1 <- read_csv("b1.csv")
ggplot(b1, aes(x = Pond, y = Mean, fill = z)) +
    geom_bar(stat = "identity", color = "black", position = position_dodge()) +
    geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd),
        width = .2,
        position = position_dodge(.9)
    ) +
    labs(fill = z)
p <- ggplot(b1, aes(x = Pond, y = Mean, fill = z)) +
    geom_bar(stat = "identity", color = "black", position = position_dodge()) +
    geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd),
        width = .2,
        position = position_dodge(.9)
    ) +
    labs(fill = n)
p + geom_text(aes(label = sig, y = (Mean + Sd) + 0.5),
    position = position_dodge(0.9),
    vjust = 0
) + xlab("Probiotics") +
    ylab("Average weight gain (g) in 90 days") +
    theme_classic(base_size = 12, base_family = "serif") +
    scale_fill_manual(
        breaks = c("2", "4", "6"),
        values = c("white", "grey", "black")
    ) + theme(legend.position = "top") +
    theme(axis.title.x = element_blank())
ggsave("Figure2.png")
########################################### Figure 6 ###############################################################
library(ggplot2)
library(readr)
z <- c("2", "4", "6", "2", "4", "6", "2", "4", "6")
n <- bquote(atop(Stocking ~ density ~ (m^-2)))
b2 <- read_csv("b2.csv")
p <- ggplot(b2, aes(x = Pond, y = Mean, fill = z)) +
    geom_bar(stat = "identity", color = "black", position = position_dodge()) +
    geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd),
        width = .2,
        position = position_dodge(.9)
    ) +
    labs(fill = n)
p + geom_text(aes(label = sig, y = (Mean + Sd) + 2),
    position = position_dodge(0.9),
    vjust = 0
) + xlab("Probiotics") +
    ylab("Average weight gain (g) in 90 days") +
    theme_classic(base_size = 12, base_family = "serif") +
    scale_fill_manual(
        breaks = c("2", "4", "6"),
        values = c("white", "grey", "black")
    ) + theme(legend.position = "top") +
    theme(axis.title.x = element_blank())
ggsave("Figur6.png")
######################################### Figure 1###############################################################
library(ggplot2)
library(readr)
d1 <- read_csv("d1.csv")
ggplot(d1, aes(x = tr, y = Mean)) +
    geom_bar(stat = "identity", color = "black") +
    theme_minimal() +
    geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd),
        width = .2,
        position = position_dodge(.9)
    ) +
    geom_text(aes(label = sig, y = (Mean + Sd) + 0.5),
        position = position_dodge(0.9),
        vjust = 0
    ) +
    xlab("Treatment") +
    ylab("Average weight gain (g) in 90 days") +
    theme_classic(base_size = 12, base_family = "serif") +
    theme(legend.position = "none", axis.title.x = element_blank()) +
    scale_fill_manual(values = ("grey"))
ggsave("Figure 1.png")
############################################## Figure 3#####################################################################
library(ggplot2)
library(readr)
c2 <- read_csv("c2.csv")
ggplot(c2, aes(x = tr, y = Mean)) +
    geom_bar(stat = "identity", color = "black") +
    geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd),
        width = .2,
        position = position_dodge(.9)
    ) +
    geom_text(aes(label = sig, y = (Mean + Sd) + 0.008),
        position = position_dodge(0.9),
        vjust = 0
    ) +
    xlab("Treatment") +
    ylab(expression(paste("pro-PO enzyme activity of   ", italic("M. rosenbergii")))) +
    theme_classic(base_size = 12, base_family = "serif") +
    theme(legend.position = "none", axis.title.x = element_blank()) +
    scale_fill_manual(values = c("grey"))
ggsave("Figure 3.png")
####################################### Figure 5 ###########################################################
library(ggplot2)
library(readr)
c1 <- read_csv("c1.csv")
ggplot(c1, aes(x = tr, y = Mean)) +
    geom_bar(stat = "identity", color = "black") +
    theme_minimal() +
    geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd),
        width = .2,
        position = position_dodge(.9)
    ) +
    geom_text(aes(label = sig, y = (Mean + Sd) + 2),
        position = position_dodge(0.9),
        vjust = 0
    ) +
    xlab("Treatment") +
    ylab(expression(paste("SOD enzyme activity (U/mg) in    ", italic("M. rosenbergii")))) +
    theme_classic(base_size = 12, base_family = "serif") +
    theme(legend.position = "none", axis.title.x = element_blank()) +
    scale_fill_manual(values = c("grey"))
ggsave("Figure 5.png")
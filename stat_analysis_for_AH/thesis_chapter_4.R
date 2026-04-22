## new thesis chapter 4 script 
## 03/19/26

rm(list=ls())

library(tidyverse)
library(ggplot2)
library(dplyr)
library(emmeans)
library(AICcmodavg)
library(ggpubr)
library(factoextra)
library(ggsignif)
library(car)

setwd('/Volumes/external/temp_drought_all_062325/data_020206/')

# palettes 

genos_pal <- c("#F56E2F", "#922FF5", "#4AA089", "#2FF5E1", "orchid1", "green2")
genos_pal2 <- c("#F56E2F", "#922FF5", "#4AA089", "#2FF5E1", "orchid1", "green2", "maroon", "steelblue4")
sm_cols <- c("brown", "#C4A484", "black", "lightblue", "darkblue")
sm_cols2 <- c("brown", "#C4A484", "lightblue", "darkblue")
temp_cols <- c("blue", "black", "red")

#
temp_growth <- read.csv('FINAL_temp_sm_growth_agg_101725.csv')


NYC_only <- filter(temp_growth, location == "NYC")
NYC_only <- NYC_only %>%
  group_by(genotype) %>%
  mutate(relative_dap = (DAP / max(DAP)))

write_csv(NYC_only, "growth_data_pub_042226.csv")

#linear modeling temp growth

lm_lfnum <- lm(num_leaves ~ genotype + factor(condition) + genotype:factor(condition), NYC_only)
summary(lm_lfnum)
aov_numlf <- Anova(lm_lfnum)

lm_pw <- lm(plant_width ~ genotype + factor(condition) + genotype:factor(condition), NYC_only)
summary(lm_pw)
aov_pw <- Anova(lm_pw)

lm_LLL <- lm(longest_leaf_length ~ genotype + factor(condition) + genotype:factor(condition), NYC_only)
summary(lm_LLL)
aov_LLL <- Anova(lm_LLL)

# linear modeling plant growth and bolting time 

lm_dap_LN <- lm(num_leaves ~ relative_dap + factor(condition) + relative_dap:factor(condition), NYC_only)
summary(lm_dap_LN)
aov_dap_LN <- Anova(lm_dap_LN)

lm_dap_temp <- lm(relative_dap ~ genotype + factor(condition) + genotype:factor(condition), sm_growth)
summary(lm_dap_temp)
aov_dap_temp <- Anova(lm_dap_temp)

lm_dap_sm <- lm(relative_dap ~ genotype + factor(condition) + genotype:factor(condition), NYC_only)
summary(lm_dap_sm)
aov_dap_sm <- Anova(lm_dap_sm)

# linear modeling leaf shape and size

lm_circ <- lm(circ ~ genotype + factor(condition) + genotype:factor(condition), NYC_leaf)
summary(lm_circ)
aov_circ <- Anova(lm_circ)

lm_ar <- lm(ar ~ genotype + factor(condition) + genotype:factor(condition), NYC_leaf)
summary(lm_ar)
aov_ar <- Anova(lm_ar)

lm_area <- lm(area ~ genotype + factor(condition) + genotype:factor(condition), NYC_leaf)
summary(lm_area)
aov_area <- Anova(lm_area)

# Linear modeling relative node, leaf shape, and size 

lm_rel <- lm(rel_node ~ genotype + factor(condition) + genotype:factor(condition), NYC_leaf)
summary(lm_rel)
aov_rel <- Anova(lm_rel)

lm_circ_rel <- lm(circ ~ rel_node + factor(condition) + rel_node:factor(condition), NYC_leaf)
summary(lm_circ_rel)
aov_circ_rel <- Anova(lm_circ_rel)

lm_ar_rel <- lm(ar ~ rel_node + factor(condition) + rel_node:factor(condition), NYC_leaf)
summary(lm_ar_rel)
aov_ar_rel <- Anova(lm_ar_rel)

lm_area_rel <- lm(area ~ rel_node + factor(condition) + rel_node:factor(condition), NYC_leaf)
summary(lm_area_rel)
aov_area_rel <- Anova(lm_area_rel)

# correlation plant growth
cor_growth <- cor(NYC_only[, c(2, 7:9, 18)], method = "spearman")
r2cor_growth <- (cor_growth)^2

#correlation leaf traits 



png(filename = 'NYC_number_of_leaves_temp_031926.png', res = 300, width= 3200, height = 3200)
ggplot(NYC_only, aes(x = factor(condition), y = num_leaves)) + 
  geom_violin(position = position_dodge(0.9)) + 
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  geom_signif(test =  ,comparisons = list(c("16", "20"), c("16","30"), c("20", "30")),
              map_signif_level = TRUE,step_increase = 0.1, textsize = 6) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30), 
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30)) +
  xlab("Temperature") +
  ylab("Total number of leaves in an individual rosette")
dev.off()

png(filename = 'NYC_plant_width_temp_032526.png', res = 300, width= 3200, height = 3200)
ggplot(NYC_only, aes(x = factor(condition), y = plant_width)) + 
  geom_violin(position = position_dodge(0.9)) + 
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  geom_signif(test =  ,comparisons = list(c("16", "20"), c("16","30"), c("20", "30")),
              map_signif_level = TRUE,step_increase = 0.1, textsize = 6) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30), 
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30)) +
  xlab("Temperature") +
  ylab("Rosette width (cm)")
dev.off()

png(filename = 'NYC_longest_leaf_length_temp_031926.png', res = 300, width= 3200, height = 3200)
ggplot(NYC_only, aes(x = factor(condition), y = longest_leaf_length)) + 
  geom_violin(position = position_dodge(0.9)) + 
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  geom_signif(comparisons = list(c("16", "20"), c("16","30"), c("20", "30")),
              map_signif_level = TRUE,step_increase = 0.1, textsize = 6) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30), 
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30)) +
  xlab("Temperature") +
  ylab("Length of the longest leaf in an individual rosette (cm)") +
  ylim(0, 32)
dev.off()

means_growth <- NYC_only %>%
  group_by(condition, genotype) %>%
  mutate(mean_NL = mean(num_leaves),
         mean_PW = mean(plant_width),
         mean_LLL = mean(longest_leaf_length), 
         sd_NL = sd(num_leaves),
         sd_PW = sd(plant_width),
         sd_LLL = sd(longest_leaf_length))

rc_growth1 <- means_growth %>%
  group_by(genotype) %>%
  mutate(mean_NL_16 = mean_NL[condition == "16"][1], 
         mean_NL_20 = mean_NL[condition == "20"][1],
         mean_NL_30 = mean_NL[condition == "30"][1],
         mean_PW_16 = mean_PW[condition == "16"][1],
         mean_PW_20 = mean_PW[condition == "20"][1],
         mean_PW_30 = mean_PW[condition == "30"][1],
         mean_LLL_16 = mean_LLL[condition == "16"][1],
         mean_LLL_20 = mean_LLL[condition == "20"][1],
         mean_LLL_30 = mean_LLL[condition == "30"][1]) %>%
  reframe(genotype = genotype,
          condition = condition, 
          mean_NL_16 = mean_NL_16,
          mean_NL_20 = mean_NL_20,
          mean_NL_30 = mean_NL_30,
          mean_PW_16 = mean_PW_16,
          mean_PW_20 = mean_PW_20,
          mean_PW_30 = mean_PW_30,
          mean_LLL_16 = mean_LLL_16,
          mean_LLL_20 = mean_LLL_20,
          mean_LLL_30 = mean_LLL_30) %>%
  distinct(genotype,.keep_all = TRUE)

rc_growth2 <- rc_growth1 %>%
  group_by(genotype) %>%
  mutate(rc_NL_cold = (mean_NL_16 - mean_NL_20)/mean_NL_20,
         rc_NL_hot = (mean_NL_30 - mean_NL_20)/mean_NL_20, 
         rc_PW_cold = (mean_PW_16 - mean_PW_20)/mean_PW_20,
         rc_PW_hot = (mean_PW_30 - mean_PW_20)/mean_PW_20,
         rc_LLL_cold = (mean_LLL_16 - mean_LLL_20)/mean_LLL_20,
         rc_LLL_hot = (mean_LLL_30 - mean_LLL_20)/mean_LLL_20) %>%
  reframe(genotype = genotype,
          rc_NL_cold = rc_NL_cold, 
          rc_NL_hot = rc_NL_hot,
          rc_PW_cold = rc_PW_cold,
          rc_PW_hot = rc_PW_hot,
          rc_LLL_cold = rc_LLL_cold,
          rc_LLL_hot = rc_LLL_hot) %>%
  distinct(genotype,.keep_all = TRUE)

rc_growth_long <- rc_growth2 %>%
  pivot_longer(cols = -genotype, names_to = "metric", values_to = "value") %>%
  mutate(
    measurement = case_when(
      str_detect(metric, "NL") ~ "Number of leaves",
      str_detect(metric, "PW") ~ "Rosette width",
      str_detect(metric, "LLL") ~ "Longest leaf length"
    ),
    treatment = case_when(
      str_detect(metric, "cold") ~ "cold",
      str_detect(metric, "hot") ~ "hot"
    )
  )

ext_cols <- c("blue", "red")

growth_rate_change1 <- ggplot(rc_growth_long, aes(x = value, y = genotype, group = interaction(genotype, measurement), shape = treatment)) +
  geom_point(aes(col = factor(treatment)), size = 15) +
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +
  geom_line(linewidth = 1.5) + 
  facet_wrap(~factor(measurement, levels = c("Number of leaves","Rosette width", "Longest leaf length")))

png(filename = 'NYC_growth_rate_change_031926.png', res = 300, width= 6000, height = 5500)
growth_rate_change1 + xlab('Relative Change') + ylab('Genotype') +
  guides(color = guide_legend(title = "Condition")) +
  guides(shape = guide_legend(title = "Condition")) +
  guides(size = "none") +
  scale_colour_manual(values = ext_cols) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))
dev.off()

NYC_only <- NYC_only %>%
  group_by(genotype) %>%
  mutate(relative_dap = (DAP / max(DAP)))

png(filename = 'NYC_relative_DAP_by_temp_031926.png', res = 300, width= 3200, height = 3200)
ggplot(NYC_only, aes(x = factor(condition), y = relative_dap)) +
  geom_boxplot() + ylim(0,1.5) +
  geom_signif(comparisons = list(c("16", "20"), c("16","30"), c("20", "30")),
              map_signif_level = TRUE,step_increase = 0.1, textsize = 6) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30), 
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30)) + 
  xlab("Temperature") +
  ylab("Relative DAP to bolting")
dev.off()

genos <- filter(NYC_only, genotype == "NY_30" | genotype == "NY_65" |
                  genotype == "NY_09" |genotype == "NY_56" | genotype == "NY_64"|
                  genotype == "NY_06" | genotype =="NY_63")

unique(NYC_only$genotype)

png(filename = 'NYC_relative_DAP_genotype_temp_032526.png', res = 300, width= 3200, height = 3200)
ggplot(NYC_only, aes(x = factor(condition), y = relative_dap)) +
  geom_boxplot() + ylim(0,2) +
  geom_signif(comparisons = list(c("16", "20")), y_position = 1.0,
              map_signif_level = TRUE,step_increase = 0.2, textsize = 6) +
  geom_signif(comparisons = list(c("16","30")), y_position = 1.1,
              map_signif_level = TRUE,step_increase = 0.2, textsize = 6) + 
  geom_signif(comparisons = list(c("20", "30")), y_position = 1.2,
              map_signif_level = TRUE,step_increase = 0.2, textsize = 6) + 
  facet_wrap(~factor(genotype, levels = c("NY_30", "NY_65", "NY_09", "NY_56", "NY_64", "NY_06", "NY_63", "NY_18"))) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30), 
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30)) + 
  xlab("Temperature") +
  ylab("Relative DAP to bolting")
dev.off()

### question - is there a difference in leaf number, 
### rosette width, or longest leaf length at time of bolting? 

## take relative_dap and find max for each

max_dap_rel <- means_growth %>%
  group_by(condition, genotype) %>%
  filter(relative_dap == max(relative_dap)) %>%
  mutate(mean_NL = mean(num_leaves),
         mean_PW = mean(plant_width),
         mean_LLL = mean(longest_leaf_length), 
         sd_NL = sd(num_leaves),
         sd_PW = sd(plant_width),
         sd_LLL = sd(longest_leaf_length))

ggplot(max_dap_rel, aes(x = relative_dap, y = mean_NL, col = factor(condition))) +
  geom_point(aes(group = factor(condition))) +
  geom_errorbar(aes(ymin=mean_NL-sd_NL, ymax=mean_NL+sd_NL), width=0.007)

NL_DAP_fig1 <- ggplot(max_dap_rel, aes(x = relative_dap, y = mean_NL, col = factor(genotype))) +
  geom_point(aes(alpha = 0.95), size = 6) +
  geom_errorbar(aes(ymin=mean_NL-sd_NL, ymax=mean_NL+sd_NL), width=0.01) +
  facet_wrap(~factor(condition)) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "blue") +
  ggtitle("A") +
  xlab('Relative bolting date') + ylab('Number of leaves per rosette') +
  guides(color = guide_legend(title = "Genotype")) +
  guides(size = "none") +
  guides(alpha = "none") + 
  scale_colour_manual(values = genos_pal2) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))

PW_DAP_fig1 <- ggplot(max_dap_rel, aes(x = relative_dap, y = mean_PW, col = factor(genotype))) +
  geom_point(aes(alpha = 0.95), size = 6) +
  geom_errorbar(aes(ymin=mean_PW-sd_PW, ymax=mean_PW+sd_PW), width=0.01) +
  facet_wrap(~factor(condition)) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "blue") +
  ggtitle("B") +
  xlab('Relative bolting date') + ylab('Rosette width (cm)') +
  guides(color = guide_legend(title = "Genotype")) +
  guides(size = "none") +
  guides(alpha = "none") + 
  scale_colour_manual(values = genos_pal2) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))

LLL_DAP_fig1 <- ggplot(max_dap_rel, aes(x = relative_dap, y = mean_LLL, col = factor(genotype))) +
  geom_point(aes(alpha = 0.95), size = 6) +
  geom_errorbar(aes(ymin=mean_LLL-sd_LLL, ymax=mean_LLL+sd_LLL), width=0.01) +
  facet_wrap(~factor(condition)) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "blue") +
  ggtitle("C") +
  xlab('Relative bolting date') + ylab('Length of longest leaf (cm)') +
  guides(color = guide_legend(title = "Genotype")) +
  guides(size = "none") +
  guides(alpha = "none") + 
  scale_colour_manual(values = genos_pal2) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))

png(filename = 'NYC_relative_dap_plant_growth_032526.png', res = 300, width= 5500, height = 5500)
ggarrange(NL_DAP_fig1, PW_DAP_fig1, LLL_DAP_fig1, ncol = 1, nrow = 3, common.legend = TRUE, legend = "top")
dev.off()

## relative DAP by temperature 

temp16 <- filter(NYC_only, condition == "16")

png(filename = 'temp_16_SIG_clean_032526.png', res = 300, width= 5500, height = 5500)
ggplot(temp16, aes(x = genotype, y = num_leaves)) + 
  geom_violin(position = position_dodge(0.9)) + 
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  geom_signif(comparisons = list(c("NY_06", "NY_09"),c("NY_06", "NY_18"),c("NY_06", "NY_30"),
                                 c("NY_06", "NY_56"),c("NY_06", "NY_63"),c("NY_06", "NY_64"),
                                 c("NY_06", "NY_65"),c("NY_09", "NY_18"),c("NY_09", "NY_30"),
                                 c("NY_09", "NY_56"),c("NY_09", "NY_63"),c("NY_09", "NY_64"),
                                 c("NY_09", "NY_65"), c("NY_18", "NY_30"),c("NY_18", "NY_56"),
                                 c("NY_18", "NY_63"), c("NY_18", "NY_64"), c("NY_18", "NY_65"),
                                 c("NY_30", "NY_56"), c("NY_30", "NY_63"), c("NY_30", "NY_64"),
                                 c("NY_30", "NY_65"), c("NY_56", "NY_63"), c("NY_56","NY_64"),
                                 c("NY_56", "NY_65"), c("NY_63", "NY_64"), c("NY_63", "NY_65"),
                                 c("NY_64", "NY_65")), y_position = 1.0,
              map_signif_level = TRUE,step_increase = 0.1, textsize = 6) +
  xlab('Genotype') + ylab('Number of leaves in each individual rosette') +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))
dev.off()
  
png(filename = 'temp_16_PW_clean_032526.png', res = 300, width= 5500, height = 5500)
ggplot(temp16, aes(x = genotype, y = plant_width)) + 
  geom_violin(position = position_dodge(0.9)) + 
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  geom_signif(comparisons = list(c("NY_06", "NY_09"),c("NY_06", "NY_18"),c("NY_06", "NY_30"),
                                 c("NY_06", "NY_56"),c("NY_06", "NY_63"),c("NY_06", "NY_64"),
                                 c("NY_06", "NY_65"),c("NY_09", "NY_18"),c("NY_09", "NY_30"),
                                 c("NY_09", "NY_56"),c("NY_09", "NY_63"),c("NY_09", "NY_64"),
                                 c("NY_09", "NY_65"), c("NY_18", "NY_30"),c("NY_18", "NY_56"),
                                 c("NY_18", "NY_63"), c("NY_18", "NY_64"), c("NY_18", "NY_65"),
                                 c("NY_30", "NY_56"), c("NY_30", "NY_63"), c("NY_30", "NY_64"),
                                 c("NY_30", "NY_65"), c("NY_56", "NY_63"), c("NY_56","NY_64"),
                                 c("NY_56", "NY_65"), c("NY_63", "NY_64"), c("NY_63", "NY_65"),
                                 c("NY_64", "NY_65")), y_position = 1.0,
              map_signif_level = TRUE,step_increase = 0.1, textsize = 6) +
  xlab('Genotype') + ylab('Rosette width (cm)') +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))
dev.off()

png(filename = 'temp_16_LLL__032526.png', res = 300, width= 5500, height = 5500)
ggplot(temp16, aes(x = genotype, y = longest_leaf_length)) + 
  geom_violin(position = position_dodge(0.9)) + 
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  xlab('Genotype') + ylab('Length of longest leaf (cm)') +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))
dev.off()

### Correlations - NL, PW, LLL

NL_PW_fig1 <- ggplot(NYC_only, aes(x = num_leaves, y = plant_width)) +
  geom_point() + facet_wrap(~factor(condition)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "blue") +
  ggtitle("A") +
  xlab('Number of leaves in an individual rosette') + ylab('Rosette width (cm)') +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30))

NL_LLL_fig1 <- ggplot(NYC_only, aes(x = num_leaves, y = longest_leaf_length)) +
  geom_point() + facet_wrap(~factor(condition)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "blue") +
  ggtitle("B") +
  xlab('Number of leaves in an individual rosette') + ylab('Length of longest leaf (cm)') +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30))

PW_LLL_fig1 <- ggplot(NYC_only, aes(x = plant_width, y = longest_leaf_length)) +
  geom_point() + facet_wrap(~factor(condition)) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "blue") +
  ggtitle("C") +
  xlab('Rosette width (cm)') + ylab('Length of longest leaf (cm)') +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30))

png(filename = 'NYC_growth_correlations_032526.png', res = 300, width= 5800, height = 6000)
ggarrange(NL_PW_fig1, NL_LLL_fig1, PW_LLL_fig1, ncol = 1, nrow = 3)
dev.off()

############ SHAPE DATA ####################

data_measured <- read.csv('/Volumes/external/temp_drought_all_062325/data_020206/meausured_data_022326.csv')

data_temp <- filter(data_measured, dataset == "temp")

data_temp <- data_temp %>%
  group_by(genotype) %>%
  mutate(rel_node = node/max(node))

write_csv(data_temp, "shape_data_pub_042226.csv")

rel_nodes <- data_temp %>%
  group_by(condition, genotype, node) %>%
  mutate(mean_circ = mean(circ),
         mean_ar = mean(ar),
         mean_sol = mean(solidity),
         mean_asy = mean(asymmetry),
         mean_width = mean(width), 
         mean_length = mean(length),
         mean_area = mean(area)) %>%
  reframe(condition = condition,
          genotype = genotype,
          node = node,
          rel_node = rel_node,
          mean_circ = mean_circ,
          mean_ar = mean_ar,
          mean_sol = mean_sol,
          mean_asy = mean_asy,
          mean_width = mean_width,
          mean_length = mean_length,
          mean_area = mean_area) %>%
  distinct(condition, genotype, node, .keep_all = TRUE)

RN_condition <- data_temp %>%
  group_by(condition, node) %>%
  mutate(mean_circ = mean(circ),
         mean_ar = mean(ar),
         mean_sol = mean(solidity),
         mean_asy = mean(asymmetry),
         mean_width = mean(width), 
         mean_length = mean(length),
         mean_area = mean(area)) %>%
  reframe(condition = condition,
          genotype = genotype,
          node = node,
          rel_node = rel_node,
          mean_circ = mean_circ,
          mean_ar = mean_ar,
          mean_sol = mean_sol,
          mean_asy = mean_asy,
          mean_width = mean_width,
          mean_length = mean_length,
          mean_area = mean_area) %>%
  distinct(condition, genotype, node, .keep_all = TRUE)

NYC_leaf <- filter(data_temp, location == "NYC")

circ1 <- ggplot(NYC_leaf, aes(x = factor(condition), y = circ)) +
  geom_violin(position = position_dodge(0.9)) + 
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  geom_signif(comparisons = list(c("16", "20"), c("16", "30"), c("20", "30")),
              map_signif_level = TRUE,step_increase = 0.1, textsize = 6) +
  ggtitle("A") +
  xlab('Temperature') + ylab('Leaf circularity') +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30), 
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30))

ar1 <- ggplot(NYC_leaf, aes(x = factor(condition), y = ar)) +
  geom_violin(position = position_dodge(0.9)) + 
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  geom_signif(comparisons = list(c("16", "20"), c("16", "30"), c("20", "30")),
              map_signif_level = TRUE,step_increase = 0.1, textsize = 6) +
  ggtitle("B") +
  xlab('Temperature') + ylab('Leaf aspect Ratio') +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30), 
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30))

area1 <- ggplot(data_temp, aes(x = factor(condition), y = area)) +
  geom_violin(position = position_dodge(0.9)) + 
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  geom_signif(comparisons = list(c("16", "20"), c("16", "30"), c("20", "30")),
              map_signif_level = TRUE,step_increase = 0.1, textsize = 6) +
  ggtitle("C") +
  xlab('Temperature') + ylab('Total leaf area (cm)') +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30), 
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30))

png(filename = 'NYC_shape_size_temp_032626.png', res = 300, width= 7000, height = 4000)
ggarrange(circ1,ar1,area1, ncol = 3, nrow = 1)
dev.off()

extreme_mean <- NYC_leaf %>%
  group_by(condition, genotype) %>%
  mutate(mean_circ = mean(circ),
         mean_ar = mean(ar),
         mean_area = mean(area),
         sd_circ = sd(circ),
         sd_ar = sd(ar),
         sd_area = sd(area)) %>%
  reframe(condition = condition,
          genotype = genotype,
          mean_circ = mean_circ,
          mean_ar = mean_ar,
          mean_area = mean_area,
          sd_circ = sd_circ,
          sd_ar = sd_ar,
          sd_area = sd_area) %>%
  distinct(condition, genotype, .keep_all = TRUE)

extreme_mean_filter <- filter(extreme_mean, condition == "16"| condition == "30")


rn_ar1 <- ggplot(extreme_mean_filter, aes(x = factor(condition), y = mean_ar, col = factor(genotype))) +
  geom_point(aes(group = factor(genotype)), size = 10) +
  geom_line(aes(group = factor(genotype))) +
  geom_errorbar(aes(ymin=mean_ar-sd_ar, ymax=mean_ar+sd_ar,width=0.05)) +
  ggtitle("B") +
  xlab('Temperature') + ylab('Mean aspect ratio') +
  guides(color = guide_legend(title = "Genotype")) +
  guides(size = "none") +
  scale_colour_manual(values = genos_pal2) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30))

rn_circ <- ggplot(extreme_mean_filter, aes(x = factor(condition), y = mean_circ, col = factor(genotype))) +
  geom_point(aes(group = factor(genotype)), size = 10) + 
  geom_line(aes(group = factor(genotype))) +
  geom_errorbar(aes(ymin=mean_circ-sd_circ, ymax=mean_circ+sd_circ,width=0.05)) +
  ggtitle("A") + 
  xlab('Temperature') + ylab('Mean Circularity') +
  guides(color = guide_legend(title = "Genotype")) +
  guides(size = "none") +
  scale_colour_manual(values = genos_pal2) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30))

rn_area <- ggplot(extreme_mean_filter, aes(x = factor(condition), y = mean_area, col = factor(genotype))) +
  geom_point(aes(group = factor(genotype)), size = 10) + 
  geom_line(aes(group = factor(genotype))) +
  geom_errorbar(aes(ymin=mean_area-sd_area, ymax=mean_area+sd_area,width=0.05)) +
  ggtitle("C") +
  xlab('Temperature') + ylab('Mean total leaf area') +
  guides(color = guide_legend(title = "Genotype")) +
  guides(size = "none") +
  scale_colour_manual(values = genos_pal2) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30))

png(filename = 'NYC_shape_reaction_norms_032626.png', res = 300, width= 7000, height = 4000)
ggarrange(rn_circ, rn_ar1, rn_area, ncol = 3, nrow = 1, common.legend = TRUE, legend = "top")
dev.off()

NYC_rel_nodes <- data_temp %>%
  group_by(condition, genotype, node) %>%
  mutate(mean_circ = mean(circ),
         mean_ar = mean(ar),
         mean_sol = mean(solidity),
         mean_asy = mean(asymmetry),
         mean_width = mean(width), 
         mean_length = mean(length),
         mean_area = mean(area), 
         sd_circ = sd(circ),
         sd_ar = sd(ar),
         sd_sol = sd(solidity),
         sd_asy = sd(asymmetry),
         sd_width = sd(width),
         sd_length = sd(length),
         sd_area = sd(area)) %>%
  reframe(condition = condition,
          genotype = genotype,
          node = node,
          location = location,
          rel_node = rel_node,
          mean_circ = mean_circ,
          mean_ar = mean_ar,
          mean_sol = mean_sol,
          mean_asy = mean_asy,
          mean_width = mean_width,
          mean_length = mean_length,
          mean_area = mean_area,
          sd_circ = sd_circ,
          sd_ar = sd_ar,
          sd_sol = sd_sol,
          sd_asy = sd_asy,
          sd_width = sd_width,
          sd_length = sd_length,
          sd_area = sd_area) %>%
  distinct(condition, genotype, node, .keep_all = TRUE)

NYC_rel_nodes <- filter(NYC_rel_nodes, location == "NYC")

NYC_rel_extremes <- filter(NYC_rel_nodes, condition == "16" | condition == "30")

png(filename = 'NYC_mean_circularity_relative_node_genotype_032626.png', res = 300, width= 7000, height = 4000)
circ_rn_p1 <- ggplot(NYC_rel_extremes, aes(x = rel_node, y = mean_circ, col = factor(condition))) +
  geom_point(aes(group = factor(condition), shape = factor(condition)), size = 3) + 
  geom_line(aes(group = factor(condition))) + 
  ylim(0, 0.80) +
  facet_wrap(~factor(genotype, levels = c("NY_30", "NY_65", "NY_56", "NY_64", "NY_63","NY_06", "NY_09", "NY_18")), ncol = 4, nrow = 2) +
  xlab('Relative Node') + ylab('Mean circularity per leaf') +
  ggtitle("A") +
  guides(color = guide_legend(title = "Temperature")) +
  guides(shape = guide_legend(title = "Temperature")) +
  scale_colour_manual(values = ext_cols) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))
  
# geom_errorbar(aes(ymin=mean_circ-sd_circ, ymax=mean_circ+sd_circ,width=0.03))

ar_rn_p1 <- ggplot(NYC_rel_extremes, aes(x = rel_node, y = mean_ar, col = factor(condition))) +
  geom_point(aes(group = factor(condition), shape = factor(condition)), size = 3) + 
  geom_line(aes(group = factor(condition))) + 
  ylim(0, 0.80) +
  facet_wrap(~factor(genotype, levels = c("NY_30", "NY_65", "NY_56", "NY_64", "NY_63","NY_06", "NY_09", "NY_18")), ncol = 4, nrow = 2) +
  xlab('Relative Node') + ylab('Mean aspect ratio per leaf') +
  ggtitle("B") +
  guides(color = guide_legend(title = "Temperature")) +
  guides(shape = guide_legend(title = "Temperature")) + 
  scale_colour_manual(values = ext_cols) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))

area_rn_p1 <- ggplot(NYC_rel_extremes, aes(x = rel_node, y = mean_area, col = factor(condition))) +
  geom_point(aes(group = factor(condition), shape = factor(condition)), size = 3) + 
  geom_line(aes(group = factor(condition))) +
  facet_wrap(~factor(genotype, levels = c("NY_30", "NY_65", "NY_56", "NY_64", "NY_63","NY_06", "NY_09", "NY_18")), ncol = 4, nrow = 2) +
  xlab('Relative Node') + ylab('Mean total leaf area') +
  ggtitle("C") +
  guides(color = guide_legend(title = "Temperature")) +
  guides(shape = guide_legend(title = "Temperature")) + 
  scale_colour_manual(values = ext_cols) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))


png(filename = 'NYC_mean_relative_node_genotype_032626.png', res = 300, width= 6000, height = 5500)
ggarrange(circ_rn_p1, ar_rn_p1, area_rn_p1, ncol = 1, nrow = 3, common.legend = TRUE, legend = "right")
dev.off()

circ_temp1 <- ggplot(NYC_rel_extremes, aes(x = rel_node, y = mean_circ, col = factor(condition))) +
  geom_point(aes(group = factor(condition), shape = factor(condition)), size = 3) + 
  geom_line(aes(group = factor(condition))) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "black") +
  facet_wrap(~factor(condition)) + 
  ggtitle("A") + 
  xlab('Relative Node') + ylab('Mean circularity per leaf') +
  guides(color = guide_legend(title = "Temperature")) +
  guides(shape = guide_legend(title = "Temperature")) +
  guides(size = "none") + 
  scale_colour_manual(values = ext_cols) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30), 
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))

ar_temp1 <- ggplot(NYC_rel_extremes, aes(x = rel_node, y = mean_ar, col = factor(condition))) +
  geom_point(aes(group = factor(condition), shape = factor(condition)), size = 3) + 
  geom_line(aes(group = factor(condition))) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "black") +
  facet_wrap(~factor(condition)) + 
  ggtitle("B") +
  xlab('Relative Node') + ylab('Mean aspect ratio per leaf') +
  guides(color = guide_legend(title = "Temperature")) +
  guides(shape = guide_legend(title = "Temperature")) +
  guides(size = "none") + 
  scale_colour_manual(values = ext_cols) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30), 
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))

area_temp1 <- ggplot(NYC_rel_extremes, aes(x = rel_node, y = mean_area, col = factor(condition))) +
  geom_point(aes(group = factor(condition), shape = factor(condition)), size = 3) + 
  geom_line(aes(group = factor(condition))) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "black") +
  facet_wrap(~factor(condition)) + 
  ggtitle("C") +
  xlab('Relative Node') + ylab('Mean total area per leaf') +
  guides(color = guide_legend(title = "Temperature")) +
  guides(shape = guide_legend(title = "Temperature")) +
  guides(size = "none") + 
  scale_colour_manual(values = ext_cols) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30), 
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))

png(filename = 'NYC_mean_relative_only_032626.png', res = 300, width= 6000, height = 5500)
ggarrange(circ_temp1, ar_temp1, area_temp1, ncol = 1, nrow = 3, common.legend = TRUE, legend = "right")
dev.off()

circvsar1 <- ggplot(NYC_leaf, aes(x = circ, y = ar)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "blue") +
  facet_wrap(~factor(condition)) +
  ggtitle("A") + xlab("Circularity per leaf") +
  ylab("Aspect ratio per leaf") + 
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                                       colour = "lightgray"), 
                       panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                                       colour = "lightgray"), 
                       panel.background = element_rect(fill = "white", colour = "black",
                                                       linewidth = 0.3, linetype = "solid"), 
                       axis.line = element_line(colour = "black"), 
                       plot.title = element_text(size = 30, face = "bold"), 
                       legend.text = element_text(size = 30), 
                       legend.title = element_text(size = 30),
                       axis.title = element_text(size = 30), 
                       axis.text = element_text(size = 30),
                       strip.text = element_text(size=30),
                       axis.text.x = element_text(angle = 90,vjust = 0.5))

circvsarea1 <- ggplot(NYC_leaf, aes(x = circ, y = area)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "blue") +
  facet_wrap(~factor(condition)) +
  ggtitle("B") + xlab("Circularity per leaf") +
  ylab("Total area per leaf") + 
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30), 
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))

arvsarea1 <- ggplot(NYC_leaf, aes(x = ar, y = area)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "blue") +
  facet_wrap(~factor(condition)) +
  ggtitle("C") + xlab("Aspect ratio per leaf") +
  ylab("Total area per leaf") + 
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30), 
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))

png(filename = 'NYC_leaf_shape_size_correlations_032626.png', res = 300, width= 6000, height = 6000)
ggarrange(circvsar1, circvsarea1, arvsarea1, ncol = 1, nrow = 3)
dev.off()


####### soil moisture #########
setwd('~/Documents/')

sm_growth <- read.csv('soil_moisture_growth_032626.csv')

# use only final data 
sm_growth <- sm_growth %>%
  group_by(genotype) %>%
  mutate(relative_dap = (DAP / max(DAP)))

sm_growth_edited <- filter(sm_growth, num_leaves > 0, plant_width > 0, longest_leaf_length > 0)

png(filename = 'SM_LN_clear_040226.png', res = 300, width= 4000, height = 4000)
ggplot(sm_growth_edited, aes(x = factor(condition, levels = c("LW2", "LW1", "Control", "HW1", "HW2")), y = num_leaves)) + 
  geom_violin(position = position_dodge(0.9)) + 
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  geom_jitter(width = 0.2) + 
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30), 
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30)) +
  xlab("Soil Moisture") +
  ylab("Total number of leaves in an individual rosette")
dev.off()

png(filename = 'SM_PW_clear_040226.png', res = 300, width= 4000, height = 4000)
ggplot(sm_growth_edited, aes(x = factor(condition, levels = c("LW2", "LW1", "Control", "HW1", "HW2")), y = plant_width)) + 
  geom_violin(position = position_dodge(0.9)) + 
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  geom_jitter(width = 0.2) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30), 
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30)) +
  xlab("Soil Moisture") +
  ylab("Rosette width (cm)")
dev.off()

png(filename = 'SM_LLL_clear_040226.png', res = 300, width= 4000, height = 4000)
ggplot(sm_growth_edited, aes(x = factor(condition, levels = c("LW2", "LW1", "Control", "HW1", "HW2")), y = longest_leaf_length)) + 
  geom_violin(position = position_dodge(0.9)) + 
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  geom_jitter(width = 0.2) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30), 
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30)) +
  xlab("Soil Moisture") +
  ylab("Length of the longest leaf in a rosette (cm)")
dev.off()


#geom_signif(comparisons = list(c("LW2", "LW1"), c("LW2", "Control"), 
                              # c("LW2", "HW1"), c("LW2", "HW2"),
                               #c("LW1","Control"), c("LW1", "HW1"),
                               #c("LW1", "HW2"), c("Control", "HW1"),
                               #c("Control", "HW2"), c("HW1", "HW2")),
            #map_signif_level = c("***"=0.001, "**"=0.01, "*"=0.05, " "=2),
            #step_increase = 0.1, textsize = 6)

NL_PW_fig2 <- ggplot(sm_growth_edited, aes(x = num_leaves, y = plant_width)) +
  geom_point() + facet_wrap(~factor(condition, levels = c("LW2", "LW1", "Control", "HW1", "HW2")), ncol = 5, nrow = 1) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "blue") +
  ggtitle("A") +
  xlab('Number of leaves in an individual rosette') + ylab('Rosette width (cm)') +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))

NL_LLL_fig2 <- ggplot(sm_growth_edited, aes(x = num_leaves, y = longest_leaf_length)) +
  geom_point() + facet_wrap(~factor(condition, levels = c("LW2", "LW1", "Control", "HW1", "HW2")), ncol = 5, nrow = 1) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "blue") +
  ggtitle("B") +
  xlab('Number of leaves in an individual rosette') + ylab('Longest leaf length (cm)') +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))

PW_LLL_fig3 <- ggplot(sm_growth_edited, aes(x = plant_width, y = longest_leaf_length)) +
  geom_point() + facet_wrap(~factor(condition, levels = c("LW2", "LW1", "Control", "HW1", "HW2")), ncol = 5, nrow = 1) +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE, color = "blue") +
  ggtitle("C") +
  xlab('Rosette width (cm)') + ylab('Longest leaf length (cm)') +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))

png(filename = 'SM_growth_correlations_040226.png', res = 300, width= 6000, height = 6000)
ggarrange(NL_PW_fig2, NL_LLL_fig2, PW_LLL_fig3, ncol = 1, nrow = 3)
dev.off()

smmeans_growth <- sm_growth %>%
  group_by(condition, paper_genotype) %>%
  mutate(mean_NL = mean(num_leaves),
         mean_PW = mean(plant_width),
         mean_LLL = mean(longest_leaf_length), 
         sd_NL = sd(num_leaves),
         sd_PW = sd(plant_width),
         sd_LLL = sd(longest_leaf_length))

smrc_growth1 <- smmeans_growth %>%
  group_by(paper_genotype) %>%
  mutate(mean_NL_LW2 = mean_NL[condition == "LW2"][1], 
         mean_NL_LW1 = mean_NL[condition == "LW1"][1],
         mean_NL_C = mean_NL[condition == "Control"][1],
         mean_NL_HW1 = mean_NL[condition == "HW1"][1],
         mean_NL_HW2 = mean_NL[condition == "HW2"][1],
         mean_PW_LW2 = mean_PW[condition == "LW2"][1],
         mean_PW_LW1 = mean_PW[condition == "LW1"][1],
         mean_PW_C = mean_PW[condition == "Control"][1],
         mean_PW_HW1 = mean_PW[condition == "HW1"][1],
         mean_PW_HW2 = mean_PW[condition == "HW2"][1],
         mean_LLL_LW2 = mean_LLL[condition == "LW2"][1],
         mean_LLL_LW1 = mean_LLL[condition == "LW1"][1],
         mean_LLL_C = mean_LLL[condition == "Control"][1],
         mean_LLL_HW1 = mean_LLL[condition == "HW1"][1],
         mean_LLL_HW2 = mean_LLL[condition == "HW2"][1]) %>%
  reframe(genotype = genotype,
          condition = condition, 
          mean_NL_LW2 = mean_NL_LW2,
          mean_NL_LW1 = mean_NL_LW1,
          mean_NL_C = mean_NL_C,
          mean_NL_HW1 = mean_NL_HW1,
          mean_NL_HW2 = mean_NL_HW2,
          mean_PW_LW1 = mean_PW_LW1,
          mean_PW_LW2 = mean_PW_LW2,
          mean_PW_C = mean_PW_C,
          mean_PW_HW1 = mean_PW_HW1,
          mean_PW_HW2 = mean_PW_HW2,
          mean_LLL_LW1 = mean_LLL_LW1,
          mean_LLL_LW2 = mean_LLL_LW2,
          mean_LLL_C = mean_LLL_C,
          mean_LLL_HW1 = mean_LLL_HW1,
          mean_LLL_HW2 = mean_LLL_HW2) %>%
  distinct(paper_genotype,.keep_all = TRUE)

smrc_growth2 <- smrc_growth1 %>%
  group_by(paper_genotype) %>%
  mutate(rc_NL_LW1 = (mean_NL_LW1 - mean_NL_C)/mean_NL_C,
         rc_NL_LW2 = (mean_NL_LW2 - mean_NL_C)/mean_NL_C,
         rc_NL_HW1 = (mean_NL_HW1 - mean_NL_C)/mean_NL_C,
         rc_NL_HW2 = (mean_NL_HW2 - mean_NL_C)/mean_NL_C,
         rc_PW_LW1 = (mean_PW_LW1 - mean_PW_C)/mean_PW_C,
         rc_PW_LW2 = (mean_PW_LW2 - mean_PW_C)/mean_PW_C,
         rc_PW_HW1 = (mean_PW_HW1 - mean_PW_C)/mean_PW_C,
         rc_PW_HW2 = (mean_PW_HW2 - mean_PW_C)/mean_PW_C,
         rc_LLL_LW1 = (mean_LLL_LW1 - mean_LLL_C)/mean_LLL_C,
         rc_LLL_LW2 = (mean_LLL_LW2 - mean_LLL_C)/mean_LLL_C,
         rc_LLL_HW1 = (mean_LLL_HW1 - mean_LLL_C)/mean_LLL_C,
         rc_LLL_HW2 = (mean_LLL_HW2 - mean_LLL_C)/mean_LLL_C) %>%
  reframe(paper_genotype = paper_genotype,
          rc_NL_LW1 = rc_NL_LW1, 
          rc_NL_LW2 = rc_NL_LW2,
          rc_NL_HW1 = rc_NL_HW1,
          rc_NL_HW2 = rc_NL_HW2,
          rc_PW_LW1 = rc_PW_LW1,
          rc_PW_LW2 = rc_PW_LW2,
          rc_PW_HW1 = rc_PW_HW1,
          rc_PW_HW2 = rc_PW_HW2,
          rc_LLL_LW1 = rc_LLL_LW1,
          rc_LLL_LW2 = rc_LLL_LW2,
          rc_LLL_HW1 = rc_LLL_HW1,
          rc_LLL_HW2 = rc_LLL_HW2) %>%
  distinct(paper_genotype,.keep_all = TRUE)

smrc_growth_long <- smrc_growth2 %>%
  pivot_longer(cols = -paper_genotype, names_to = "metric", values_to = "value") %>%
  mutate(
    measurement = case_when(
      str_detect(metric, "NL") ~ "Number of leaves",
      str_detect(metric, "PW") ~ "Rosette width",
      str_detect(metric, "LLL") ~ "Longest leaf length"
    ),
    treatment = case_when(
      str_detect(metric, "LW2") ~ "LW2",
      str_detect(metric, "LW1") ~ "LW1",
      str_detect(metric, "HW1") ~ "HW1",
      str_detect(metric, "HW2") ~ "HW2"
    )
  )

smrc_growth_long <- na.omit(smrc_growth_long)
smrc_growth_long_clean <- smrc_growth_long %>% filter(if_all(where(is.numeric), is.finite))

png(filename = 'SM_reaction_growth_040226.png', res = 300, width= 6000, height = 6000)
ggplot(smrc_growth_long, aes(x = value, y = paper_genotype, group = interaction(paper_genotype, measurement), 
                             shape = factor(treatment, levels = c("LW2", "LW1", "HW1","HW2")))) +
  geom_point(aes(col = factor(treatment, levels = c("LW2", "LW1", "HW1","HW2"))), size = 15) +
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +
  geom_line(linewidth = 1.5) + 
  xlim(-2, 4) + 
  facet_wrap(~factor(measurement, 
                     levels = c("Number of leaves","Rosette width", "Longest leaf length")),
             ncol = 3, nrow = 1) +
  xlab('Relative Change') + ylab('Genotype') +
  guides(color = guide_legend(title = "Condition")) +
  guides(shape = guide_legend(title = "Condition")) +
  guides(size = "none") +
  scale_colour_manual(values = sm_cols2) +
  scale_shape_manual(values = c(15,16,17,18)) + 
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30), 
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30),
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5))
dev.off()

png(filename = 'SM_relative_DAP_by_temp_040226.png', res = 300, width= 3200, height = 3200)
ggplot(sm_growth, aes(x = factor(condition, levels = c("LW2", "LW1", "Control", "HW1", "HW2")), y = relative_dap)) +
  geom_boxplot() + ylim(0,1.5) +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30), 
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30)) + 
  xlab("Soil Moisture") +
  ylab("Relative DAP to bolting")
dev.off()


png(filename = 'SM_bolting_genotype_clear_040226.png', res = 300, width= 6000, height = 6000)
ggplot(sm_growth, aes(x = factor(condition, levels = c("LW2", "LW1", "Control", "HW1", "HW2")),
                     y = relative_dap)) +
  geom_boxplot() +
  facet_wrap(~factor(paper_genotype), ncol = 3, nrow = 4) + 
theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30), 
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30),
        axis.text.x = element_text(angle = 90,vjust = 0.5)) + 
  xlab("Soil Moisture") +
  ylab("Relative DAP to bolting")
dev.off()


######### SOil Moisture leaf shape #############

sm_leaf_shape <- filter(data_measured, dataset == "drought")

sm_leaf_shape <- sm_leaf_shape %>%
  group_by(genotype) %>%
  mutate(rel_node = node/max(node))

smrel_nodes <- sm_leaf_shape %>%
  group_by(condition, genotype, node) %>%
  mutate(mean_circ = mean(circ),
         mean_ar = mean(ar),
         mean_sol = mean(solidity),
         mean_asy = mean(asymmetry),
         mean_width = mean(width), 
         mean_length = mean(length),
         mean_area = mean(area)) %>%
  reframe(condition = condition,
          genotype = genotype,
          node = node,
          rel_node = rel_node,
          mean_circ = mean_circ,
          mean_ar = mean_ar,
          mean_sol = mean_sol,
          mean_asy = mean_asy,
          mean_width = mean_width,
          mean_length = mean_length,
          mean_area = mean_area) %>%
  distinct(condition, genotype, node, .keep_all = TRUE)

smRN_condition <- sm_leaf_shape %>%
  group_by(condition, node) %>%
  mutate(mean_circ = mean(circ),
         mean_ar = mean(ar),
         mean_sol = mean(solidity),
         mean_asy = mean(asymmetry),
         mean_width = mean(width), 
         mean_length = mean(length),
         mean_area = mean(area)) %>%
  reframe(condition = condition,
          genotype = genotype,
          node = node,
          rel_node = rel_node,
          mean_circ = mean_circ,
          mean_ar = mean_ar,
          mean_sol = mean_sol,
          mean_asy = mean_asy,
          mean_width = mean_width,
          mean_length = mean_length,
          mean_area = mean_area) %>%
  distinct(condition, genotype, node, .keep_all = TRUE)

circ2 <- ggplot(sm_leaf_shape, aes(x = factor(condition), y = circ)) +
  geom_violin(position = position_dodge(0.9)) + 
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  ylim(0,1.5) +
  ggtitle("A") +
  xlab('Soil Moisture') + ylab('Leaf circularity') +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30), 
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30))

ar2 <- ggplot(sm_leaf_shape, aes(x = factor(condition), y = ar)) +
  geom_violin(position = position_dodge(0.9)) + 
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  ylim(0,1.5) +
  ggtitle("B") +
  xlab('Soil Moisture') + ylab('Leaf aspect Ratio') +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30), 
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30))

area2 <- ggplot(sm_leaf_shape, aes(x = factor(condition), y = area)) +
  geom_violin(position = position_dodge(0.9)) +
  ylim(0,9) +
  geom_boxplot(width = 0.1, position = position_dodge(0.9)) +
  ggtitle("C") +
  xlab('Soil Mositure') + ylab('Total leaf area (cm)') +
  theme(panel.grid.major = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.grid.minor = element_line(linewidth = 0.3, linetype = 'solid',
                                        colour = "lightgray"), 
        panel.background = element_rect(fill = "white", colour = "black",
                                        linewidth = 0.3, linetype = "solid"), 
        axis.line = element_line(colour = "black"), 
        plot.title = element_text(size = 30, face = "bold"), 
        legend.text = element_text(size = 30), 
        axis.title = element_text(size = 30), 
        axis.text = element_text(size = 30),
        strip.text = element_text(size=30))

png(filename = 'shape_size_SM_clear_040326.png', res = 300, width= 7000, height = 4000)
ggarrange(circ2,ar2,area2, ncol = 3, nrow = 1)
dev.off()



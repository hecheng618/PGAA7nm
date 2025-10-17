########### delta ug calculation #################################
library(ggplot2)
library(patchwork)
setwd("/path/to/data") # Ensure this path is correct for your system
it_df_dug1 <- read.table("./mathematica/alldug_1_1.txt")

df_dug1 <- data.frame(
  vgs = as.numeric(it_df_dug1[, 1]),
  dibl = as.numeric(it_df_dug1[, 2]),
  ndibl = as.numeric(it_df_dug1[, 3]),
  all = as.numeric(it_df_dug1[, 4]))

base_plot_1 <- ggplot() +
  # Draw Combined line (red solid line)
  geom_line(data = df_dug1,
            aes(x = vgs,
                y = all,
                color = "Combined",
                linetype = "Combined"),
            size = 1) +
  # Draw DIBL line (dashed, color based on scale_color_manual)
  geom_line(data = df_dug1,
            aes(x = vgs,
                y = dibl,
                color = "DIBL",
                linetype = "DIBL"),
            size = 1) +
  # Draw w/o DIBL (NDIBL) line (dashed, color based on scale_color_manual)
  geom_line(data = df_dug1,
            aes(x = vgs,
                y = ndibl,
                color = "w/o DIBL",
                linetype = "w/o DIBL"),
            size = 1) +
  scale_y_continuous(limits = c(-0.025, 0.01)) +
  scale_x_continuous(limits = c(-0.601, 0)) +
  labs(
    x = expression(italic("V")[GS] ~ "[V]"),
    y = expression(italic(Delta)*italic(U)[G] ~ "[V]")
  ) +
  # Configure colors
  scale_color_manual(name = " ",
                     values = c("Combined" = "red",
                                "DIBL" = "blue",
                                "w/o DIBL" = "green"),
                     #                     labels = c(expression(Delta*italic(U)[G]^all), # Changed to superscript 'all'
                     #                                expression(Delta*italic(U)[G]^DIBL), # Changed to superscript 'dibl'
                     #                                expression(Delta*italic(U)[G]^(1)))) + # Changed to superscript '1'
                     labels = c(expression((28)), # Changed to superscript 'all'
                                expression((21)), # Changed to superscript 'dibl'
                                expression((24)))) +
  # Configure line types
  scale_linetype_manual(name = " ",
                        values = c("Combined" = "solid",
                                   "DIBL" = "dashed",
                                   "w/o DIBL" = "dashed"),
                        labels = c(expression((28)), # Changed to superscript 'all'
                                   expression((21)), # Changed to superscript 'dibl'
                                   expression((24)))) + # Changed to superscript '1'
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(0.8, 0.513), # Legend position
    legend.text = element_text(size = 16, hjust = 0),
    legend.box = "vertical",
    legend.spacing.y = unit(0.01, "cm"),
    legend.box.spacing = unit(0.01, "cm"),
    legend.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit = "cm"),
    legend.key.size = unit(0.8, "cm"),
    aspect.ratio = 1) +
  # Left-aligned annotations
  annotate("text",
           x = -0.6,
           y = -0.02,
           label = expression(italic("R")*"= 1.5 nm"),
           hjust = 0, # Left align
           vjust = 0,
           size = 7,
           family = "Times New Roman") +
  annotate("text",
           x = -0.6,
           y = -0.024,
           label = expression(italic("L")*"= 7 nm"),
           hjust = 0,
           vjust = 0,
           size = 7,
           family = "Times New Roman")

# --- You need to apply the same changes to base_plot_2 through base_plot_6 ---

# Example for base_plot_2 (modify similarly for base_plot_3, 4, 5, 6)
it_df_dug2 <- read.table("./mathematica/alldug_1_2.txt")
df_dug2 <- data.frame(
  vgs = as.numeric(it_df_dug2[, 1]),
  dibl = as.numeric(it_df_dug2[, 2]),
  ndibl = as.numeric(it_df_dug2[, 3]),
  all = as.numeric(it_df_dug2[, 4]))

base_plot_2 <- ggplot() +
  geom_line(data = df_dug2, aes(x = vgs, y = all, color = "Combined", linetype = "Combined"), size = 1) +
  geom_line(data = df_dug2, aes(x = vgs, y = dibl, color = "DIBL", linetype = "DIBL"), size = 1) +
  geom_line(data = df_dug2, aes(x = vgs, y = ndibl, color = "w/o DIBL", linetype = "w/o DIBL"), size = 1) +
  scale_y_continuous(limits = c(-0.025, 0.01)) +
  scale_x_continuous(limits = c(-0.601, 0)) +
  labs(
    x = expression(italic("V")[GS] ~ "[V]"),
    y = expression(italic(Delta)*italic(U)[G] ~ "[V]")
  ) +
  scale_color_manual(name = " ",
                     values = c("Combined" = "red", "DIBL" = "blue", "w/o DIBL" = "green"),
                     labels = c(expression(Delta*italic(U)[G]^all),
                                expression(Delta*italic(U)[G]^dibl),
                                expression(Delta*italic(U)[G]^(1)))) +
  scale_linetype_manual(name = " ",
                        values = c("Combined" = "solid", "DIBL" = "dashed", "w/o DIBL" = "dashed"),
                        labels = c(expression(Delta*italic(U)[G]^all),
                                   expression(Delta*italic(U)[G]^dibl),
                                   expression(Delta*italic(U)[G]^(1)))) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(5, 5), # Moved legend off-plot for combined plot, will be added to p_1a
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.spacing = unit(0.01, "cm"),
    legend.box.spacing = unit(0.05, "cm"),
    legend.key.size = unit(0.8, "cm"),
    aspect.ratio = 1) +
  annotate("text", x = -0.6, y = -0.008, label = expression(italic("R")*"= 1.5 nm"), hjust = 0, vjust = 0, size = 7, family = "Times New Roman") +
  annotate("text", x = -0.6, y = -0.012, label = expression(italic("L")*"= 9 nm"), hjust = 0, vjust = 0, size = 7, family = "Times New Roman") +
  annotate("text", x = -0.6, y = -0.016, label = expression(italic("V")[DS]*"= -0.6 V"), hjust = 0, vjust = 0, size = 7, family = "Times New Roman") +
  annotate("text", x = -0.6, y = -0.02, label = expression(italic(phi)[GC]*"-"*italic(w)[FB]*"= 0.63 V"), hjust = 0, vjust = 0, size = 7, family = "Times New Roman") +
  annotate("text", x = -0.6, y = -0.024, label = expression(italic(V)[bi]*"= -1.35 V"), hjust = 0, vjust = 0, size = 7, family = "Times New Roman")


# Replicate for base_plot_3, base_plot_4, base_plot_5, base_plot_6 with appropriate annotations and limits

it_df_dug3 <- read.table("./mathematica/alldug_2_1.txt")
df_dug3 <- data.frame(
  vgs = as.numeric(it_df_dug3[, 1]),
  dibl = as.numeric(it_df_dug3[, 2]),
  ndibl = as.numeric(it_df_dug3[, 3]),
  all = as.numeric(it_df_dug3[, 4]))

base_plot_3 <- ggplot() +
  geom_line(data = df_dug3, aes(x = vgs, y = all, color = "Combined", linetype = "Combined"), size = 1) +
  geom_line(data = df_dug3, aes(x = vgs, y = dibl, color = "DIBL", linetype = "DIBL"), size = 1) +
  geom_line(data = df_dug3, aes(x = vgs, y = ndibl, color = "w/o DIBL", linetype = "w/o DIBL"), size = 1) +
  scale_y_continuous(limits = c(-0.05, 0.03)) +
  scale_x_continuous(limits = c(-0.601, 0)) +
  labs(
    x = expression(italic("V")[GS] ~ "[V]"),
    y = expression(italic(Delta)*italic(U)[G] ~ "[V]")
  ) +
  scale_color_manual(name = " ",
                     values = c("Combined" = "red", "DIBL" = "blue", "w/o DIBL" = "green"),
                     labels = c(expression(Delta*italic(U)[G]^all),
                                expression(Delta*italic(U)[G]^dibl),
                                expression(Delta*italic(U)[G]^(1)))) +
  scale_linetype_manual(name = " ",
                        values = c("Combined" = "solid", "DIBL" = "dashed", "w/o DIBL" = "dashed"),
                        labels = c(expression(Delta*italic(U)[G]^all),
                                   expression(Delta*italic(U)[G]^dibl),
                                   expression(Delta*italic(U)[G]^(1)))) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(5, 5), # Moved legend off-plot for combined plot
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.spacing = unit(0.01, "cm"),
    legend.box.spacing = unit(0.05, "cm"),
    legend.key.size = unit(0.8, "cm"),
    aspect.ratio = 1) +
  annotate("text", x = -0.6, y = -0.04, label = expression(italic("R")*"= 2 nm"), hjust = 0, vjust = 0, size = 7, family = "Times New Roman") +
  annotate("text", x = -0.6, y = -0.048, label = expression(italic("L")*"= 7 nm"), hjust = 0, vjust = 0, size = 7, family = "Times New Roman")

it_df_dug4 <- read.table("./mathematica/alldug_2_2.txt")
df_dug4 <- data.frame(
  vgs = as.numeric(it_df_dug4[, 1]),
  dibl = as.numeric(it_df_dug4[, 2]),
  ndibl = as.numeric(it_df_dug4[, 3]),
  all = as.numeric(it_df_dug4[, 4]))

base_plot_4 <- ggplot() +
  geom_line(data = df_dug4, aes(x = vgs, y = all, color = "Combined", linetype = "Combined"), size = 1) +
  geom_line(data = df_dug4, aes(x = vgs, y = dibl, color = "DIBL", linetype = "DIBL"), size = 1) +
  geom_line(data = df_dug4, aes(x = vgs, y = ndibl, color = "w/o DIBL", linetype = "w/o DIBL"), size = 1) +
  scale_y_continuous(limits = c(-0.05, 0.03)) +
  scale_x_continuous(limits = c(-0.601, 0)) +
  labs(
    x = expression(italic("V")[GS] ~ "[V]"),
    y = expression(italic(Delta)*italic(U)[G] ~ "[V]")
  ) +
  scale_color_manual(name = " ",
                     values = c("Combined" = "red", "DIBL" = "blue", "w/o DIBL" = "green"),
                     labels = c(expression(Delta*italic(U)[G]^all),
                                expression(Delta*italic(U)[G]^dibl),
                                expression(Delta*italic(U)[G]^(1)))) +
  scale_linetype_manual(name = " ",
                        values = c("Combined" = "solid", "DIBL" = "dashed", "w/o DIBL" = "dashed"),
                        labels = c(expression(Delta*italic(U)[G]^all),
                                   expression(Delta*italic(U)[G]^dibl),
                                   expression(Delta*italic(U)[G]^(1)))) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(5, 5), # Moved legend off-plot for combined plot
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.spacing = unit(0.01, "cm"),
    legend.box.spacing = unit(0.05, "cm"),
    legend.key.size = unit(0.8, "cm"),
    aspect.ratio = 1) +
  annotate("text", x = -0.6, y = -0.04, label = expression(italic("R")*"= 2 nm"), hjust = 0, vjust = 0, size = 7, family = "Times New Roman") +
  annotate("text", x = -0.6, y = -0.048, label = expression(italic("L")*"= 9 nm"), hjust = 0, vjust = 0, size = 7, family = "Times New Roman")

it_df_dug5 <- read.table("./mathematica/alldug_3_1.txt")
df_dug5 <- data.frame(
  vgs = as.numeric(it_df_dug5[, 1]),
  dibl = as.numeric(it_df_dug5[, 2]),
  ndibl = as.numeric(it_df_dug5[, 3]),
  all = as.numeric(it_df_dug5[, 4]))

base_plot_5 <- ggplot() +
  geom_line(data = df_dug5, aes(x = vgs, y = all, color = "Combined", linetype = "Combined"), size = 1) +
  geom_line(data = df_dug5, aes(x = vgs, y = dibl, color = "DIBL", linetype = "DIBL"), size = 1) +
  geom_line(data = df_dug5, aes(x = vgs, y = ndibl, color = "w/o DIBL", linetype = "w/o DIBL"), size = 1) +
  scale_y_continuous(limits = c(-0.08, 0.03)) +
  scale_x_continuous(limits = c(-0.601, 0)) +
  labs(
    x = expression(italic("V")[GS] ~ "[V]"),
    y = expression(italic(Delta)*italic(U)[G] ~ "[V]")
  ) +
  scale_color_manual(name = " ",
                     values = c("Combined" = "red", "DIBL" = "blue", "w/o DIBL" = "green"),
                     labels = c(expression(Delta*italic(U)[G]^all),
                                expression(Delta*italic(U)[G]^dibl),
                                expression(Delta*italic(U)[G]^(1)))) +
  scale_linetype_manual(name = " ",
                        values = c("Combined" = "solid", "DIBL" = "dashed", "w/o DIBL" = "dashed"),
                        labels = c(expression(Delta*italic(U)[G]^all),
                                   expression(Delta*italic(U)[G]^dibl),
                                   expression(Delta*italic(U)[G]^(1)))) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(5, 5), # Moved legend off-plot for combined plot
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.spacing = unit(0.01, "cm"),
    legend.box.spacing = unit(0.05, "cm"),
    legend.key.size = unit(0.8, "cm"),
    aspect.ratio = 1) +
  annotate("text", x = -0.6, y = -0.06, label = expression(italic("R")*"= 2.5 nm"), hjust = 0, vjust = 0, size = 7, family = "Times New Roman") +
  annotate("text", x = -0.6, y = -0.072, label = expression(italic("L")*"= 7 nm"), hjust = 0, vjust = 0, size = 7, family = "Times New Roman")

it_df_dug6 <- read.table("./mathematica/alldug_3_2.txt")
df_dug6 <- data.frame(
  vgs = as.numeric(it_df_dug6[, 1]),
  dibl = as.numeric(it_df_dug6[, 2]),
  ndibl = as.numeric(it_df_dug6[, 3]),
  all = as.numeric(it_df_dug6[, 4]))

base_plot_6 <- ggplot() +
  geom_line(data = df_dug6, aes(x = vgs, y = all, color = "Combined", linetype = "Combined"), size = 1) +
  geom_line(data = df_dug6, aes(x = vgs, y = dibl, color = "DIBL", linetype = "DIBL"), size = 1) +
  geom_line(data = df_dug6, aes(x = vgs, y = ndibl, color = "w/o DIBL", linetype = "w/o DIBL"), size = 1) +
  scale_y_continuous(limits = c(-0.08, 0.03)) +
  scale_x_continuous(limits = c(-0.601, 0)) +
  labs(
    x = expression(italic("V")[GS] ~ "[V]"),
    y = expression(italic(Delta)*italic(U)[G] ~ "[V]")
  ) +
  scale_color_manual(name = " ",
                     values = c("Combined" = "red", "DIBL" = "blue", "w/o DIBL" = "green"),
                     labels = c(expression(Delta*italic(U)[G]^all),
                                expression(Delta*italic(U)[G]^dibl),
                                expression(Delta*italic(U)[G]^(1)))) +
  scale_linetype_manual(name = " ",
                        values = c("Combined" = "solid", "DIBL" = "dashed", "w/o DIBL" = "dashed"),
                        labels = c(expression(Delta*italic(U)[G]^all),
                                   expression(Delta*italic(U)[G]^dibl),
                                   expression(Delta*italic(U)[G]^(1)))) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(5, 5), # Moved legend off-plot for combined plot
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.spacing = unit(0.01, "cm"),
    legend.box.spacing = unit(0.05, "cm"),
    legend.key.size = unit(0.8, "cm"),
    aspect.ratio = 1) +
  annotate("text", x = -0.6, y = -0.06, label = expression(italic("R")*"= 2.5 nm"), hjust = 0, vjust = 0, size = 7, family = "Times New Roman") +
  annotate("text", x = -0.6, y = -0.072, label = expression(italic("L")*"= 9 nm"), hjust = 0, vjust = 0, size = 7, family = "Times New Roman")

# Patchwork arrangement
p_1a <- base_plot_1 +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  annotate("text",
           x = -0.6,
           y = 0.01,
           label = "(a)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_1b <- base_plot_2 +
  theme(#axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank()
    ) +
  annotate("text",
           x = -0.6,
           y = 0.01,
           label = "(d)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_2a <- base_plot_3 +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  annotate("text",
           x = -0.6,
           y = 0.03,
           label = "(b)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_2b <- base_plot_4 +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank()
    ) +
  annotate("text",
           x = -0.6,
           y = 0.03,
           label = "(e)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_3a <- base_plot_5 +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  annotate("text",
           x = -0.6,
           y = 0.03,
           label = "(c)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_3b <- base_plot_6 +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
    ) +
  annotate("text",
           x = -0.6,
           y = 0.03,
           label = "(f)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

final_plot <- (p_1a | p_2a | p_3a )/
  (p_1b | p_2b | p_3b)
#/
#  (p_3a | p_3b )

print(final_plot)

ggsave("/path/to/data/fig/dug_yoko.png", 
       final_plot, width = 16, height = 10, dpi = 300)


######################## potential #############################################

setwd("/path/to/data/")
temp1_englvlvg00 <- read.csv("./silvaco/Nanowire_r1.5_l7_Vd06Vg00.str", 
                             header=FALSE, sep=" ")
temp1_englvlvg02 <- read.csv("./silvaco/Nanowire_r1.5_l7_Vd06Vg02.str", 
                             header=FALSE, sep=" ")
temp1_englvlvg05 <- read.csv("./silvacoNanowire_r1.5_l7_Vd06Vg05.str", 
                             header=FALSE, sep=" ")
temp4_englvlvg00 <- read.csv("./silvaco/Nanowire_r1.5_l9_Vd06Vg00.str", 
                             header=FALSE, sep=" ")
temp4_englvlvg02 <- read.csv("./silvaco/Nanowire_r1.5_l9_Vd06Vg02.str", 
                             header=FALSE, sep=" ")
temp4_englvlvg05 <- read.csv("./silvaco/Nanowire_r1.5_l9_Vd06Vg05.str", 
                             header=FALSE, sep=" ")
temp2_englvlvg00 <- read.csv("./silvaco/Nanowire_r2_l7_Vd06Vg00.str", 
                             header=FALSE, sep=" ")
temp2_englvlvg02 <- read.csv("./silvaco/Nanowire_r2_l7_Vd06Vg02.str", 
                             header=FALSE, sep=" ")
temp2_englvlvg05 <- read.csv("./silvaco/Nanowire_r2_l7_Vd06Vg05.str", 
                             header=FALSE, sep=" ")
temp3_englvlvg00 <- read.csv("./silvaco/Nanowire_r2.5_l7_Vd06Vg00.str", 
                             header=FALSE, sep=" ")
temp3_englvlvg02 <- read.csv("./silvaco/Nanowire_r2.5_l7_Vd06Vg02.str", 
                             header=FALSE, sep=" ")
temp3_englvlvg05 <- read.csv("./silvaco/Nanowire_r2.5_l7_Vd06Vg05.str", 
                             header=FALSE, sep=" ")
temp5_englvlvg00 <- read.csv("./silvaco/Nanowire_r2_l9_Vd06Vg00.str", 
                             header=FALSE, sep=" ")
temp5_englvlvg02 <- read.csv("./silvaco/Nanowire_r2_l9_Vd06Vg02.str", 
                             header=FALSE, sep=" ")
temp5_englvlvg05 <- read.csv("./silvaco/Nanowire_r2_l9_Vd06Vg05.str", 
                             header=FALSE, sep=" ")
temp6_englvlvg00 <- read.csv("./silvaco/Nanowire_r2.5_l9_Vd06Vg00.str", 
                             header=FALSE, sep=" ")
temp6_englvlvg02 <- read.csv("./silvaco/Nanowire_r2.5_l9_Vd06Vg02.str", 
                             header=FALSE, sep=" ")
temp6_englvlvg05 <- read.csv("./silvaco/Nanowire_r2.5_l9_Vd06Vg05.str", 
                             header=FALSE, sep=" ")
############ fig 1 #############
df1_englvl <- data.frame(
  z = seq(from = 0, to = 27, by = 0.1),
  simvg00 = as.numeric(temp1_englvlvg00[17347:17617,11]),
  simvg02 = as.numeric(temp1_englvlvg02[17347:17617,11]),
  simvg05 = as.numeric(temp1_englvlvg05[17347:17617,11])
)
colnames(df1_englvl) <- c("z", "simvg00", "simvg02", "simvg05")
#df1_englvl
df1_englvlmdl <- data.frame(read.table("/path/to/data/mathematica/anapot_1_1.txt"))
colnames(df1_englvlmdl) <- c("z", "mdlvg00", "mdlvg02", "mdlvg05")

base_plot_1 <- ggplot() +
  geom_line(data = df1_englvl, 
            aes(x = z, 
                y = simvg00, 
                color = "VGS = 0.0 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df1_englvl, 
            aes(x = z, 
                y = simvg02, 
                color = "VGS = 0.2 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df1_englvl, 
            aes(x = z, 
                y = simvg05, 
                color = "VGS = 0.5 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_point(data = df1_englvlmdl, 
             aes(x = z, 
                 y = mdlvg00, 
                 color = "VGS = 0.0 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df1_englvlmdl, 
             aes(x = z, 
                 y = mdlvg02, 
                 color = "VGS = 0.2 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df1_englvlmdl, 
             aes(x = z, 
                 y = mdlvg05, 
                 color = "VGS = 0.5 V", 
                 shape = "Model"), 
             size = 3) +
  scale_y_continuous(limits = c(-1.6, 0)) +
  scale_x_continuous(limits = c(0,30), breaks = seq(0, 30, 5)) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 17, linetype = "dashed", color = "black", linewidth = 0.5) +
  labs(
    x = expression(italic("z") ~ "[nm]"),
    y = expression(italic(w)[0] ~ "[V]")
  ) +
  scale_color_manual(name = " ",
                     values = c("VGS = 0.0 V" = "red",
                                "VGS = 0.2 V" = "green",
                                "VGS = 0.5 V" = "blue"),
                     labels = c(expression(italic("V")[GS]*" = 0.0 V"),
                                expression(italic("V")[GS]*" = -0.2 V"),
                                expression(italic("V")[GS]*" = -0.5 V"))) +
  scale_linetype_manual(name = " ", 
                        values = c("Simulation" = "solid")) +
  scale_shape_manual(name = " ", 
                     values = c("Model" = 16)) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(0.79,0.73),
    legend.text = element_text(size =16,hjust=0),
    legend.box = "vertical",
    legend.spacing.y = unit(0.001, "cm"),
    legend.box.spacing = unit(0.001,"cm"),
    #legend.margin = margin(unit(0.001,"cm")),
    legend.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit = "cm"),
    legend.key.height = unit(0.3, "lines"),
    legend.key.size = unit(0.8, "cm"),) +
  annotate("text", 
           x=0, 
           y=-1.08, 
           label=expression(italic("R")*"= 1.5 nm"), 
           hjust=0,
           vjust=0, 
           size=7, 
           family = "Times New Roman") +
  annotate("text", 
           x=0, 
           y=-1.21, 
           label=expression(italic("L")*"= 7 nm"), 
           hjust=0, 
           vjust=0, 
           size=7, 
           family = "Times New Roman") +
  annotate("text", 
           x=0, 
           y=-1.34, 
           label=expression(italic("V")[DS]*"= -0.6 V"), 
           hjust=0, 
           vjust=0, 
           size=7, 
           family = "Times New Roman") +
  annotate("text", 
           x=0, 
           y=-1.47, 
           label=expression(italic(phi)[GC]*"-"*italic(w)[FB]*"= 0.63 V"), 
           hjust=0, 
           vjust=0, 
           size=7, 
           family = "Times New Roman") +
  annotate("text", 
           x=0, 
           y=-1.6, 
           label=expression(italic(V)[bi]*"= -1.35 V"), 
           hjust=0, 
           vjust=0, 
           size=7, 
           family = "Times New Roman")
############ fig 2 #############
df2_englvl <- data.frame(
  z = seq(from = 0, to = 27, by = 0.1),
  simvg00 = as.numeric(temp2_englvlvg00[21439:21709,11]),
  simvg02 = as.numeric(temp2_englvlvg02[21439:21709,11]),
  simvg05 = as.numeric(temp2_englvlvg05[21439:21709,11])
)
colnames(df2_englvl) <- c("z", "simvg00", "simvg02", "simvg05")

df2_englvlmdl <- data.frame(read.table("/path/to/data/mathematica/anapot_2_1.txt"))
colnames(df2_englvlmdl) <- c("z", "mdlvg00", "mdlvg02", "mdlvg05")

base_plot_2 <- ggplot() +
  geom_line(data = df2_englvl, 
            aes(x = z, 
                y = simvg00, 
                color = "VGS = 0.0 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df2_englvl, 
            aes(x = z, 
                y = simvg02, 
                color = "VGS = 0.2 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df2_englvl, 
            aes(x = z, 
                y = simvg05, 
                color = "VGS = 0.5 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_point(data = df2_englvlmdl, 
             aes(x = z, 
                 y = mdlvg00, 
                 color = "VGS = 0.0 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df2_englvlmdl, 
             aes(x = z, 
                 y = mdlvg02, 
                 color = "VGS = 0.2 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df2_englvlmdl, 
             aes(x = z, 
                 y = mdlvg05, 
                 color = "VGS = 0.5 V", 
                 shape = "Model"), 
             size = 3) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 17, linetype = "dashed", color = "black", linewidth = 0.5) +
  scale_y_continuous(limits = c(-1.6, 0)) +
  scale_x_continuous(limits = c(0,30),breaks = seq(0, 30, 5)) +
  labs(
    x = expression(italic("z") ~ "[nm]"),
    y = expression(italic(w)[0] ~ "[V]")
  ) +
  scale_color_manual(name = " ",
                     values = c("VGS = 0.0 V" = "red",
                                "VGS = 0.2 V" = "green",
                                "VGS = 0.5 V" = "blue"),
                     labels = c(expression(italic("V")[GS]*" = 0.0 V"),
                                expression(italic("V")[GS]*" = -0.2 V"),
                                expression(italic("V")[GS]*" = -0.5 V"))) +
  scale_linetype_manual(name = " ", 
                        values = c("Simulation" = "solid")) +
  scale_shape_manual(name = " ", 
                     values = c("Model" = 16)) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(5,5),
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.spacing = unit(0.01, "cm"),
    legend.box.spacing = unit(0.05,"cm")) +
  annotate("text", 
           x=0, 
           y=-1.1, 
           label=expression(italic("R")*"= 2 nm"), 
           hjust=0,
           vjust=0, 
           size=7, 
           family = "Times New Roman") +
  annotate("text", 
           x=0, 
           y=-1.23, 
           label=expression(italic("L")*"= 7 nm"), 
           hjust=0, 
           vjust=0, 
           size=7, 
           family = "Times New Roman") #+
# annotate("text", 
#          x=0, 
#          y=-1.29, 
#          label=expression(italic("V")[DS]*"= -0.6 V"), 
#          hjust=0, 
#          vjust=0, 
#          size=7, 
#          family = "Times New Roman") +
# annotate("text", 
#          x=0, 
#          y=-1.42, 
#          label=expression(italic(phi)[GC]*"-"*italic(w)[FB]*"= 0.63 V"), 
#          hjust=0, 
#          vjust=0, 
#          size=7, 
#          family = "Times New Roman") +
# annotate("text", 
#          x=0, 
#          y=-1.55, 
#          label=expression(italic(V)[bi]*"= -1.35 V"), 
#          hjust=0, 
#          vjust=0, 
#          size=7, 
#          family = "Times New Roman")
############ fig 3 #############
df3_englvl <- data.frame(
  z = seq(from = 0, to = 27, by = 0.1),
  simvg00 = as.numeric(temp3_englvlvg00[25534:25804,11]),
  simvg02 = as.numeric(temp3_englvlvg02[25534:25804,11]),
  simvg05 = as.numeric(temp3_englvlvg05[25534:25804,11])
)
colnames(df3_englvl) <- c("z", "simvg00", "simvg02", "simvg05")

df3_englvlmdl <- data.frame(read.table("/path/to/data/mathematica/anapot_3_1.txt"))
colnames(df3_englvlmdl) <- c("z", "mdlvg00", "mdlvg02", "mdlvg05")

base_plot_3 <- ggplot() +
  geom_line(data = df3_englvl, 
            aes(x = z, 
                y = simvg00, 
                color = "VGS = 0.0 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df3_englvl, 
            aes(x = z, 
                y = simvg02, 
                color = "VGS = 0.2 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df3_englvl, 
            aes(x = z, 
                y = simvg05, 
                color = "VGS = 0.5 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_point(data = df3_englvlmdl, 
             aes(x = z, 
                 y = mdlvg00, 
                 color = "VGS = 0.0 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df3_englvlmdl, 
             aes(x = z, 
                 y = mdlvg02, 
                 color = "VGS = 0.2 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df3_englvlmdl, 
             aes(x = z, 
                 y = mdlvg05, 
                 color = "VGS = 0.5 V", 
                 shape = "Model"), 
             size = 3) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 17, linetype = "dashed", color = "black", linewidth = 0.5) +
  scale_y_continuous(limits = c(-1.6, 0)) +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  labs(
    x = expression(italic("z") ~ "[nm]"),
    y = expression(italic(w)[0] ~ "[V]")
  ) +
  scale_color_manual(name = " ",
                     values = c("VGS = 0.0 V" = "red",
                                "VGS = 0.2 V" = "green",
                                "VGS = 0.5 V" = "blue"),
                     labels = c(expression(italic("V")[GS]*" = 0.0 V"),
                                expression(italic("V")[GS]*" = -0.2 V"),
                                expression(italic("V")[GS]*" = -0.5 V"))) +
  scale_linetype_manual(name = " ", 
                        values = c("Simulation" = "solid")) +
  scale_shape_manual(name = " ", 
                     values = c("Model" = 16)) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(5,5),
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.spacing = unit(0.01, "cm"),
    legend.box.spacing = unit(0.05,"cm")) +
  annotate("text", 
           x=0, 
           y=-1.1, 
           label=expression(italic("R")*"= 2.5 nm"), 
           hjust=0,
           vjust=0, 
           size=7, 
           family = "Times New Roman") +
  annotate("text", 
           x=0, 
           y=-1.23, 
           label=expression(italic("L")*"= 7 nm"), 
           hjust=0, 
           vjust=0, 
           size=7, 
           family = "Times New Roman") #+
# annotate("text", 
#          x=0, 
#          y=-1.29, 
#          label=expression(italic("V")[DS]*"= -0.6 V"), 
#          hjust=0, 
#          vjust=0, 
#          size=7, 
#          family = "Times New Roman") +
# annotate("text", 
#          x=0, 
#          y=-1.42, 
#          label=expression(italic(phi)[GC]*"-"*italic(w)[FB]*"= 0.63 V"), 
#          hjust=0, 
#          vjust=0, 
#          size=7, 
#          family = "Times New Roman") +
# annotate("text", 
#          x=0, 
#          y=-1.55, 
#          label=expression(italic(V)[bi]*"= -1.35 V"), 
#          hjust=0, 
#          vjust=0, 
#          size=7, 
#          family = "Times New Roman")
############ fig 4 #############
df4_englvl <- data.frame(
  z = seq(from = 0, to = 29, by = 0.1),
  simvg00 = as.numeric(temp4_englvlvg00[18647:18937,11]),
  simvg02 = as.numeric(temp4_englvlvg02[18647:18937,11]),
  simvg05 = as.numeric(temp4_englvlvg05[18647:18937,11])
)
colnames(df4_englvl) <- c("z", "simvg00", "simvg02", "simvg05")

df4_englvlmdl <- data.frame(read.table("/path/to/data/mathematica/anapot_1_2.txt"))
colnames(df4_englvlmdl) <- c("z", "mdlvg00", "mdlvg02", "mdlvg05")

base_plot_4 <- ggplot() +
  geom_line(data = df4_englvl, 
            aes(x = z, 
                y = simvg00, 
                color = "VGS = 0.0 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df4_englvl, 
            aes(x = z, 
                y = simvg02, 
                color = "VGS = 0.2 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df4_englvl, 
            aes(x = z, 
                y = simvg05, 
                color = "VGS = 0.5 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_point(data = df4_englvlmdl, 
             aes(x = z, 
                 y = mdlvg00, 
                 color = "VGS = 0.0 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df4_englvlmdl, 
             aes(x = z, 
                 y = mdlvg02, 
                 color = "VGS = 0.2 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df4_englvlmdl, 
             aes(x = z, 
                 y = mdlvg05, 
                 color = "VGS = 0.5 V", 
                 shape = "Model"), 
             size = 3) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 19, linetype = "dashed", color = "black", linewidth = 0.5) +
  scale_y_continuous(limits = c(-1.6, 0)) +
  scale_x_continuous(limits = c(0,30), breaks = seq(0, 30, 5)) +
  labs(
    x = expression(italic("z") ~ "[nm]"),
    y = expression(italic(w)[0] ~ "[V]")) +
  scale_color_manual(name = " ",
                     values = c("VGS = 0.0 V" = "red",
                                "VGS = 0.2 V" = "green",
                                "VGS = 0.5 V" = "blue"),
                     labels = c(expression(italic("V")[GS]*" = 0.0 V"),
                                expression(italic("V")[GS]*" = -0.2 V"),
                                expression(italic("V")[GS]*" = -0.5 V"))) +
  scale_linetype_manual(name = " ", 
                        values = c("Simulation" = "solid")) +
  scale_shape_manual(name = " ", 
                     values = c("Model" = 16)) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(5,5),
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.spacing = unit(0.01, "cm"),
    legend.box.spacing = unit(0.05,"cm")) +
  annotate("text", 
           x=0, 
           y=-1.1, 
           label=expression(italic("R")*"= 1.5 nm"), 
           hjust=0,
           vjust=0, 
           size=7, 
           family = "Times New Roman") +
  annotate("text", 
           x=0, 
           y=-1.23, 
           label=expression(italic("L")*"= 9 nm"), 
           hjust=0, 
           vjust=0, 
           size=7, 
           family = "Times New Roman") #+
# annotate("text", 
#          x=0, 
#          y=-1.29, 
#          label=expression(italic("V")[DS]*"= -0.6 V"), 
#          hjust=0, 
#          vjust=0, 
#          size=7, 
#          family = "Times New Roman") +
# annotate("text", 
#          x=0, 
#          y=-1.42, 
#          label=expression(italic(phi)[GC]*"-"*italic(w)[FB]*"= 0.63 V"), 
#          hjust=0, 
#          vjust=0, 
#          size=7, 
#          family = "Times New Roman") +
# annotate("text", 
#          x=0, 
#          y=-1.55, 
#          label=expression(italic(V)[bi]*"= -1.35 V"), 
#          hjust=0, 
#          vjust=0, 
#          size=7, 
#          family = "Times New Roman")
############ fig 5 #############
df5_englvl <- data.frame(
  z = seq(from = 0, to = 29, by = 0.1),
  simvg00 = as.numeric(temp5_englvlvg00[23042:23332,11]),
  simvg02 = as.numeric(temp5_englvlvg02[23042:23332,11]),
  simvg05 = as.numeric(temp5_englvlvg05[23042:23332,11])
)
colnames(df5_englvl) <- c("z", "simvg00", "simvg02", "simvg05")

df5_englvlmdl <- data.frame(read.table("/path/to/data/mathematica/anapot_2_2.txt"))
colnames(df5_englvlmdl) <- c("z", "mdlvg00", "mdlvg02", "mdlvg05")

base_plot_5 <- ggplot() +
  geom_line(data = df5_englvl, 
            aes(x = z, 
                y = simvg00, 
                color = "VGS = 0.0 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df5_englvl, 
            aes(x = z, 
                y = simvg02, 
                color = "VGS = 0.2 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df5_englvl, 
            aes(x = z, 
                y = simvg05, 
                color = "VGS = 0.5 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_point(data = df5_englvlmdl, 
             aes(x = z, 
                 y = mdlvg00, 
                 color = "VGS = 0.0 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df5_englvlmdl, 
             aes(x = z, 
                 y = mdlvg02, 
                 color = "VGS = 0.2 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df5_englvlmdl, 
             aes(x = z, 
                 y = mdlvg05, 
                 color = "VGS = 0.5 V", 
                 shape = "Model"), 
             size = 3) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 19, linetype = "dashed", color = "black", linewidth = 0.5) +
  scale_y_continuous(limits = c(-1.6, 0)) +
  scale_x_continuous(limits = c(0,30), breaks = seq(0, 30, 5)) +
  labs(
    x = expression(italic("z") ~ "[nm]"),
    y = expression(italic(w)[0] ~ "[V]")) +
  scale_color_manual(name = " ",
                     values = c("VGS = 0.0 V" = "red",
                                "VGS = 0.2 V" = "green",
                                "VGS = 0.5 V" = "blue"),
                     labels = c(expression(italic("V")[GS]*" = 0.0 V"),
                                expression(italic("V")[GS]*" = -0.2 V"),
                                expression(italic("V")[GS]*" = -0.5 V"))) +
  scale_linetype_manual(name = " ", 
                        values = c("Simulation" = "solid")) +
  scale_shape_manual(name = " ", 
                     values = c("Model" = 16)) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(5,5),
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.spacing = unit(0.01, "cm"),
    legend.box.spacing = unit(0.05,"cm")) +
  annotate("text", 
           x=0, 
           y=-1.1, 
           label=expression(italic("R")*"= 2 nm"), 
           hjust=0,
           vjust=0, 
           size=7, 
           family = "Times New Roman") +
  annotate("text", 
           x=0, 
           y=-1.23, 
           label=expression(italic("L")*"= 9 nm"), 
           hjust=0, 
           vjust=0, 
           size=7, 
           family = "Times New Roman") #+
# annotate("text", 
#          x=0, 
#          y=-1.29, 
#          label=expression(italic("V")[DS]*"= -0.6 V"), 
#          hjust=0, 
#          vjust=0, 
#          size=7, 
#          family = "Times New Roman") +
# annotate("text", 
#          x=0, 
#          y=-1.42, 
#          label=expression(italic(phi)[GC]*"-"*italic(w)[FB]*"= 0.63 V"), 
#          hjust=0, 
#          vjust=0, 
#          size=7, 
#          family = "Times New Roman") +
# annotate("text", 
#          x=0, 
#          y=-1.55, 
#          label=expression(italic(V)[bi]*"= -1.35 V"), 
#          hjust=0, 
#          vjust=0, 
#          size=7, 
#          family = "Times New Roman")
############ fig 6 #############
df6_englvl <- data.frame(
  z = seq(from = 0, to = 29, by = 0.1),
  simvg00 = as.numeric(temp6_englvlvg00[27437:27727,11]),
  simvg02 = as.numeric(temp6_englvlvg02[27437:27727,11]),
  simvg05 = as.numeric(temp6_englvlvg05[27437:27727,11])
)
colnames(df6_englvl) <- c("z", "simvg00", "simvg02", "simvg05")

df6_englvlmdl <- data.frame(read.table("/path/to/data/mathematica/anapot_3_2.txt"))
colnames(df6_englvlmdl) <- c("z", "mdlvg00", "mdlvg02", "mdlvg05")

base_plot_6 <- ggplot() +
  geom_line(data = df6_englvl, 
            aes(x = z, 
                y = simvg00, 
                color = "VGS = 0.0 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df6_englvl, 
            aes(x = z, 
                y = simvg02, 
                color = "VGS = 0.2 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df6_englvl, 
            aes(x = z, 
                y = simvg05, 
                color = "VGS = 0.5 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_point(data = df6_englvlmdl, 
             aes(x = z, 
                 y = mdlvg00, 
                 color = "VGS = 0.0 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df6_englvlmdl, 
             aes(x = z, 
                 y = mdlvg02, 
                 color = "VGS = 0.2 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df6_englvlmdl, 
             aes(x = z, 
                 y = mdlvg05, 
                 color = "VGS = 0.5 V", 
                 shape = "Model"), 
             size = 3) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 19, linetype = "dashed", color = "black", linewidth = 0.5) +
  scale_y_continuous(limits = c(-1.6, 0)) +
  scale_x_continuous(limits = c(0,30), breaks = seq(0, 30, 5)) +
  labs(
    x = expression(italic("z") ~ "[nm]"),
    y = expression(italic(w)[0] ~ "[V]")) +
  scale_color_manual(name = " ",
                     values = c("VGS = 0.0 V" = "red",
                                "VGS = 0.2 V" = "green",
                                "VGS = 0.5 V" = "blue"),
                     labels = c(expression(italic("V")[GS]*" = 0.0 V"),
                                expression(italic("V")[GS]*" = -0.2 V"),
                                expression(italic("V")[GS]*" = -0.5 V"))) +
  scale_linetype_manual(name = " ", 
                        values = c("Simulation" = "solid")) +
  scale_shape_manual(name = " ", 
                     values = c("Model" = 16)) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(5,5),
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.spacing = unit(0.01, "cm"),
    legend.box.spacing = unit(0.05,"cm")) +
  annotate("text", 
           x=0, 
           y=-1.1, 
           label=expression(italic("R")*"= 2.5 nm"), 
           hjust=0,
           vjust=0, 
           size=7, 
           family = "Times New Roman") +
  annotate("text", 
           x=0, 
           y=-1.23, 
           label=expression(italic("L")*"= 9 nm"), 
           hjust=0, 
           vjust=0, 
           size=7, 
           family = "Times New Roman") #+
# annotate("text", 
#          x=0, 
#          y=-1.29, 
#          label=expression(italic("V")[DS]*"= -0.6 V"), 
#          hjust=0, 
#          vjust=0, 
#          size=7, 
#          family = "Times New Roman") +
# annotate("text", 
#          x=0, 
#          y=-1.42, 
#          label=expression(italic(phi)[GC]*"-"*italic(w)[FB]*"= 0.63 V"), 
#          hjust=0, 
#          vjust=0, 
#          size=7, 
#          family = "Times New Roman") +
# annotate("text", 
#          x=0, 
#          y=-1.55, 
#          label=expression(italic(V)[bi]*"= -1.35 V"), 
#          hjust=0, 
#          vjust=0, 
#          size=7, 
#          family = "Times New Roman")
############ plot #############################################
p_1a <- base_plot_1 +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  annotate("text",
           x = 0,
           y = 0,
           label = "(a)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_1b <- base_plot_4 +
  theme(#axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank()
    ) +
  annotate("text",
           x = 0,
           y = 0,
           label = "(d)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_2a <- base_plot_2 +
  theme(axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) +
  annotate("text",
           x = 0,
           y = 0,
           label = "(b)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_2b <- base_plot_5 +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank()
        ) +
  annotate("text",
           x = 0,
           y = 0,
           label = "(e)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_3a <- base_plot_3 +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  annotate("text",
           x = 0,
           y = 0,
           label = "(c)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_3b <- base_plot_6 +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  annotate("text",
           x = 0,
           y = 0,
           label = "(f)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

final_plot <- (p_1a | p_2a | p_3a)/  
  (p_1b | p_2b | p_3b) 
print(final_plot)

ggsave("/path/to/data/fig/combined_potential_comparison_yoko.png", 
       final_plot, width = 16, height = 10, dpi = 300)


######################### energy level min #####################################

library(readr)
setwd("/path/to/data/silvaco/")


# -----------------------------
temp1_englvlvg00 <- read.csv("./Nanowire_r1.5_l7_Vd06Vg00.str", header=FALSE, sep=" ")
temp1_englvlvg01 <- read.csv("./Nanowire_r1.5_l7_Vd06Vg01.str", header=FALSE, sep=" ")
temp1_englvlvg02 <- read.csv("./Nanowire_r1.5_l7_Vd06Vg02.str", header=FALSE, sep=" ")
temp1_englvlvg03 <- read.csv("./Nanowire_r1.5_l7_Vd06Vg03.str", header=FALSE, sep=" ")
temp1_englvlvg04 <- read.csv("./Nanowire_r1.5_l7_Vd06Vg04.str", header=FALSE, sep=" ")
temp1_englvlvg05 <- read.csv("./Nanowire_r1.5_l7_Vd06Vg05.str", header=FALSE, sep=" ")
temp1_englvlvg06 <- read.csv("./Nanowire_r1.5_l7_Vd06Vg06.str", header=FALSE, sep=" ")
temp1_englvlvg07 <- read.csv("./Nanowire_r1.5_l7_Vd06Vg07.str", header=FALSE, sep=" ")


temp1_englvlmdl <- read.table("/path/to/data/mathematica/englvlmin_1_1.txt")

df1_englvlsim <- data.frame(
  vgs = seq(from = 0, to = -0.7, by = -0.1),
  englvlsim = c(min(temp1_englvlvg00[17351:17621,33]),
                min(temp1_englvlvg01[17351:17621,33]),
                min(temp1_englvlvg02[17351:17621,33]),
                min(temp1_englvlvg03[17351:17621,33]),
                min(temp1_englvlvg04[17351:17621,33]),
                min(temp1_englvlvg05[17351:17621,33]),
                min(temp1_englvlvg06[17351:17621,33]),
                min(temp1_englvlvg07[17351:17621,33])))

df1_englvlmdl <- data.frame(vgs = seq(from = 0, to = -0.7, by = -0.01),
                            englvlmdl = as.numeric(temp1_englvlmdl[, 2]))

base_plot_1 <- ggplot() +
 
  geom_line(data = df1_englvlmdl,
            aes(x = vgs,
                y = englvlmdl,
                color = "Model"),  
            size = 1.5) + 
  
  
  geom_point(data = df1_englvlsim,
             aes(x = vgs,
                 y = englvlsim,
                 color = "Simulation"), 
             size = 3, 
             shape = 16) + 
  
 
  scale_color_manual(
    name = " ",
    values = c("Simulation" = "red",
               "Model" = "black"),
    labels = c(expression("Model"),
               expression("Simulation")),
    
    guide = guide_legend(
      override.aes = list(
        linetype = c("solid", "blank"),  
        shape = c(NA, 16)  
      ),
      keyheight = unit(2.5,"lines")
    )
  ) +
  
 
  scale_y_continuous(limits = c(-0.7, 0.), breaks = seq(-0.7, 0.0, 0.1),
                     labels = scales::number_format(accuracy = 0.1)) +

  scale_x_continuous(limits = c(-0.601, 0), breaks = seq(-0.6, 0, 0.1)) +
  labs(
    x = expression(italic("V")[GS] ~ "[V]"),
    y = expression(italic("E")["0,1,MIN"] ~ "[eV]")) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size =20),
        axis.title.y = element_text(size =20),
        legend.position = c(0.8, 0.8), 
        legend.text = element_text(size = 16,hjust=0),
        legend.box = "vertical",
        #legend.margin = margin(unit(0.01, "cm")),
        legend.spacing = unit(0.01, "mm"),
        legend.box.spacing = unit(0.01,"mm"),
        legend.title = element_text(size = 18),
        legend.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit = "cm"),
        legend.key.size = unit(0.8, "cm"),
        legend.key.height = unit(0.3, "lines")) + 
  
  
  annotate("text", x=-0.55, y=-0.35,
           label=expression(italic("R")*"= 1.5 nm"),
           hjust=0, vjust=0,
           size=7, 
           family = "Times New Roman")+
  annotate("text", x=-0.55, y=-0.42, 
           label=expression(italic("L")*"= 7 nm"),
           hjust=0, vjust=0,
           size=7,
           family = "Times New Roman")+
  annotate("text", x=-0.55, y=-0.49, 
           label=expression(italic("V")[DS]*"= -0.6 V"),
           hjust=0, vjust=0,
           size=7,
           family = "Times New Roman")+
  annotate("text", x=-0.55, y=-0.56, 
           label=expression(italic(phi)[GC]*"-"*italic(w)[FB]*"= 0.63 V"),
           hjust=0, vjust=0,
           size=7,
           family = "Times New Roman")+
  annotate("text",
           x = -0.55,
           y = -0.63, 
           label = expression(italic(V)[bi]*"= -1.35 V"),
           hjust = 0,
           vjust = 0,
           size = 7,
           family = "Times New Roman")

# -----------------------------
temp2_englvlvg00 <- read.csv("./Nanowire_r1.5_l9_Vd06Vg00.str", header=FALSE, sep=" ")
temp2_englvlvg01 <- read.csv("./Nanowire_r1.5_l9_Vd06Vg01.str", header=FALSE, sep=" ")
temp2_englvlvg02 <- read.csv("./Nanowire_r1.5_l9_Vd06Vg02.str", header=FALSE, sep=" ")
temp2_englvlvg03 <- read.csv("./Nanowire_r1.5_l9_Vd06Vg03.str", header=FALSE, sep=" ")
temp2_englvlvg04 <- read.csv("./Nanowire_r1.5_l9_Vd06Vg04.str", header=FALSE, sep=" ")
temp2_englvlvg05 <- read.csv("./Nanowire_r1.5_l9_Vd06Vg05.str", header=FALSE, sep=" ")
temp2_englvlvg06 <- read.csv("./Nanowire_r1.5_l9_Vd06Vg06.str", header=FALSE, sep=" ")
temp2_englvlvg07 <- read.csv("./Nanowire_r1.5_l9_Vd06Vg07.str", header=FALSE, sep=" ")


temp2_englvlmdl <- read.table("/path/to/data/mathematica/englvlmin_1_2.txt")

df2_englvlsim <- data.frame(
  vgs = seq(from = 0, to = -0.7, by = -0.1),
  englvlsim = c(min(temp2_englvlvg00[23042:23332,33]),
                min(temp2_englvlvg01[23042:23332,33]),
                min(temp2_englvlvg02[23042:23332,33]),
                min(temp2_englvlvg03[23042:23332,33]),
                min(temp2_englvlvg04[23042:23332,33]),
                min(temp2_englvlvg05[23042:23332,33]),
                min(temp2_englvlvg06[23042:23332,33]),
                min(temp2_englvlvg07[23042:23332,33])))

df2_englvlmdl <- data.frame(vgs = seq(from = 0, to = -0.7, by = -0.01),
                            englvlmdl = as.numeric(temp2_englvlmdl[, 2]))

base_plot_2 <- ggplot() +
  
  geom_line(data = df2_englvlmdl,
            aes(x = vgs,
                y = englvlmdl,
                color = "Model"),   
            size = 1.5) + 
  geom_point(data = df2_englvlsim,
             aes(x = vgs,
                 y = englvlsim,
                 color = "Simulation"), 
             size = 3, 
             shape = 16) + 
  
  
  scale_color_manual(
    name = " ", 
    values = c("Simulation" = "red",
               "Model" = "black"),
    labels = c(expression("Model"),
               expression("Simulation")),
    
    guide = guide_legend(
      override.aes = list(
        linetype = c("solid", "blank"), 
        shape = c(NA, 16) 
      ),
      keyheight = unit(2.5,"lines")
    )
  ) +
  
  
  scale_y_continuous(limits = c(-0.7, 0.), breaks = seq(-0.7, 0.0, 0.1),
                     labels = scales::number_format(accuracy = 0.1)) +
  
  scale_x_continuous(limits = c(-0.601, 0), breaks = seq(-0.6, 0, 0.1)) +
  labs(
    x = expression(italic("V")[GS] ~ "[V]"),
    y = expression(italic("E")["0,1,MIN"] ~ "[eV]")) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size =20),
        axis.title.y = element_text(size =20),
        legend.position = c(5, 5), # Í¼ÀýÎ»ÖÃ
        legend.text = element_text(size = 16,hjust=0),
        legend.box = "vertical",
        #legend.margin = margin(unit(0.01, "cm")),
        legend.spacing = unit(0.01, "mm"),
        legend.box.spacing = unit(0.01,"mm"),
        legend.title = element_text(size = 18),
        legend.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit = "cm"),
        legend.key.size = unit(0.8, "cm"),
        legend.key.height = unit(0.3, "lines")) + 
  annotate("text", x=-0.55, y=-0.35,
           label=expression(italic("R")*"= 1.5 nm"),
           hjust=0, vjust=0,
           size=7, family = "Times New Roman")+
  annotate("text", x=-0.55, y=-0.4, 
           label=expression(italic("L")*"= 9 nm"),
           hjust=0, vjust=0,
           size=7, family = "Times New Roman")

# ----------------------------
temp3_englvlvg00 <- read.csv("./Nanowire_r2_l7_Vd06Vg00.str", header=FALSE, sep=" ")
temp3_englvlvg01 <- read.csv("./Nanowire_r2_l7_Vd06Vg01.str", header=FALSE, sep=" ")
temp3_englvlvg02 <- read.csv("./Nanowire_r2_l7_Vd06Vg02.str", header=FALSE, sep=" ")
temp3_englvlvg03 <- read.csv("./Nanowire_r2_l7_Vd06Vg03.str", header=FALSE, sep=" ")
temp3_englvlvg04 <- read.csv("./Nanowire_r2_l7_Vd06Vg04.str", header=FALSE, sep=" ")
temp3_englvlvg05 <- read.csv("./Nanowire_r2_l7_Vd06Vg05.str", header=FALSE, sep=" ")
temp3_englvlvg06 <- read.csv("./Nanowire_r2_l7_Vd06Vg06.str", header=FALSE, sep=" ")
temp3_englvlvg07 <- read.csv("./Nanowire_r2_l7_Vd06Vg07.str", header=FALSE, sep=" ")


temp3_englvlmdl <- read.table("/path/to/data/mathematica/englvlmin_2_1.txt")

df3_englvlsim <- data.frame(
  vgs = seq(from = 0, to = -0.7, by = -0.1),
  englvlsim = c(min(temp3_englvlvg00[23846:24146,33]),
                min(temp3_englvlvg01[23846:24146,33]),
                min(temp3_englvlvg02[23846:24146,33]),
                min(temp3_englvlvg03[23846:24146,33]),
                min(temp3_englvlvg04[23846:24146,33]),
                min(temp3_englvlvg05[23846:24146,33]),
                min(temp3_englvlvg06[23846:24146,33]),
                min(temp3_englvlvg07[23846:24146,33])))

df3_englvlmdl <- data.frame(vgs = seq(from = 0, to = -0.7, by = -0.01),
                            englvlmdl = temp3_englvlmdl[,2])

base_plot_3 <- ggplot() +
 
  geom_line(data = df3_englvlmdl,
            aes(x = vgs,
                y = englvlmdl,
                color = "Model"),    
            size = 1.5) + 
  
  geom_point(data = df3_englvlsim,
             aes(x = vgs,
                 y = englvlsim,
                 color = "Simulation"), 
             size = 3, 
             shape = 16) + 
  
  
  scale_color_manual(
    name = " ", 
    values = c("Simulation" = "red",
               "Model" = "black"),
    labels = c(expression("Simulation"),
               expression("Model")),
   
    guide = guide_legend(
      override.aes = list(
        linetype = c("solid", "blank"), 
        shape = c(NA, 16) 
      ),
      keyheight = unit(2.5,"lines")
    )
  ) +
  
  
  scale_y_continuous(limits = c(-0.6, 0.1), breaks = seq(-0.6, 0.1, 0.1),
                     labels = scales::number_format(accuracy = 0.1)) +
  
  scale_x_continuous(limits = c(-0.601, 0), breaks = seq(-0.6, 0, 0.1)) +
  labs(
    x = expression(italic("V")[GS] ~ "[V]"),
    y = expression(italic("E")["0,1,MIN"] ~ "[eV]")) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size =20),
        axis.title.y = element_text(size =20),
        legend.position = c(5, 5), 
        legend.text = element_text(size = 20),
        legend.box = "vertical",
        legend.margin = margin(unit(0.01, "cm")),
        legend.title = element_text(size = 18)) + 
  
  annotate("text", x=-0.55, y=-0.42,
           label=expression(italic("R")*"= 2 nm"),
           hjust=0, vjust=0,
           size=7, 
           family = "Times New Roman")+
  annotate("text", x=-0.55, y=-0.49, 
           label=expression(italic("L")*"= 7 nm"),
           hjust=0, vjust=0,
           size=7,
           family = "Times New Roman")#+
# annotate("text", x=-0.55, y=-0.39, # µ÷Õû y ×ø±ê
#          label=expression(italic("V")[DS]*"= -0.6 V"),
#          hjust=0, vjust=0,
#          size=7,
#          family = "Times New Roman")+
# annotate("text", x=-0.55, y=-0.46, # µ÷Õû y ×ø±ê
#          label=expression(italic(phi)[GC]*"-"*italic(w)[FB]*"= 0.63 V"),
#          hjust=0, vjust=0,
#          size=7,
#          family = "Times New Roman")+
# annotate("text",
#          x = -0.55,
#          y = -0.53, # µ÷Õû y ×ø±ê
#          label = expression(italic(V)[bi]*"= -1.35 V"),
#          hjust = 0,
#          vjust = 0,
#          size = 7,
#          family = "Times New Roman")

# ----------------------------
#temp4_englvlvg00 <- read_table(file = "./Nanowire_r2_l9_Vd06Vg00.str", 
#                               col_names = str_col_names, skip = 0)
#temp4_englvlvg01 <- read_table(file = "./Nanowire_r2_l9_Vd06Vg01.str", 
#                               col_names = str_col_names, skip = 0)
#temp4_englvlvg02 <- read_table(file = "./Nanowire_r2_l9_Vd06Vg02.str", 
#                               col_names = str_col_names, skip = 0)
#temp4_englvlvg03 <- read_table(file = "./Nanowire_r2_l9_Vd06Vg03.str", 
#                               col_names = str_col_names, skip = 0)
#temp4_englvlvg04 <- read_table(file = "./Nanowire_r2_l9_Vd06Vg04.str", 
#                               col_names = str_col_names, skip = 0)
#temp4_englvlvg05 <- read_table(file = "./Nanowire_r2_l9_Vd06Vg05.str", 
#                               col_names = str_col_names, skip = 0)
#temp4_englvlvg06 <- read_table(file = "./Nanowire_r2_l9_Vd06Vg06.str", 
#                               col_names = str_col_names, skip = 0)
#temp4_englvlvg07 <- read_table(file = "./Nanowire_r2_l9_Vd06Vg07.str", 
#                               col_names = str_col_names, skip = 0)
temp4_englvlvg00 <- read.csv("./Nanowire_r2_l9_Vd06Vg00.str", header=FALSE, sep=" ")
temp4_englvlvg01 <- read.csv("./Nanowire_r2_l9_Vd06Vg01.str", header=FALSE, sep=" ")
temp4_englvlvg02 <- read.csv("./Nanowire_r2_l9_Vd06Vg02.str", header=FALSE, sep=" ")
temp4_englvlvg03 <- read.csv("./Nanowire_r2_l9_Vd06Vg03.str", header=FALSE, sep=" ")
temp4_englvlvg04 <- read.csv("./Nanowire_r2_l9_Vd06Vg04.str", header=FALSE, sep=" ")
temp4_englvlvg05 <- read.csv("./Nanowire_r2_l9_Vd06Vg05.str", header=FALSE, sep=" ")
temp4_englvlvg06 <- read.csv("./Nanowire_r2_l9_Vd06Vg06.str", header=FALSE, sep=" ")
temp4_englvlvg07 <- read.csv("./Nanowire_r2_l9_Vd06Vg07.str", header=FALSE, sep=" ")


temp4_englvlvg00$V33 <- as.numeric(temp4_englvlvg00$V33)
temp4_englvlvg01$V33 <- as.numeric(temp4_englvlvg01$V33)
temp4_englvlvg02$V33 <- as.numeric(temp4_englvlvg02$V33)
temp4_englvlvg03$V33 <- as.numeric(temp4_englvlvg03$V33)
temp4_englvlvg04$V33 <- as.numeric(temp4_englvlvg04$V33)
temp4_englvlvg05$V33 <- as.numeric(temp4_englvlvg05$V33)
temp4_englvlvg06$V33 <- as.numeric(temp4_englvlvg06$V33)
temp4_englvlvg07$V33 <- as.numeric(temp4_englvlvg07$V33)


temp4_englvlmdl <- read.table("/path/to/data/mathematica/englvlmin_2_2.txt")



start_row_4 <- 27437
end_row_4 <- 27727
target_col_4 <- 33

df4_englvlsim <- data.frame(
  vgs = seq(from = 0, to = -0.7, by = -0.1),
  englvlsim = c(min(temp4_englvlvg00[start_row_4:end_row_4, target_col_4], na.rm = TRUE),
                min(temp4_englvlvg01[start_row_4:end_row_4, target_col_4], na.rm = TRUE),
                min(temp4_englvlvg02[start_row_4:end_row_4, target_col_4], na.rm = TRUE),
                min(temp4_englvlvg03[start_row_4:end_row_4, target_col_4], na.rm = TRUE),
                min(temp4_englvlvg04[start_row_4:end_row_4, target_col_4], na.rm = TRUE),
                min(temp4_englvlvg05[start_row_4:end_row_4, target_col_4], na.rm = TRUE),
                min(temp4_englvlvg06[start_row_4:end_row_4, target_col_4], na.rm = TRUE),
                min(temp4_englvlvg07[start_row_4:end_row_4, target_col_4], na.rm = TRUE)))


df4_englvlmdl <- data.frame(vgs = seq(from = 0, to = -0.7, by = -0.01),
                            englvlmdl = as.numeric(temp4_englvlmdl[,2])) 
base_plot_4 <- ggplot() +

  geom_line(data = df4_englvlmdl,
            aes(x = vgs, y = englvlmdl, color = "Model"), 
            size = 1.5) +
  
  
  geom_point(data = df4_englvlsim,
             aes(x = vgs, y = englvlsim, color = "Simulation"), 
             size = 3, shape = 16) +
  
  
  scale_color_manual(
    name = " ", 
    values = c("Simulation" = "red", "Model" = "black"), 
    labels = c(expression("Simulation"), expression("Model")), 
    guide = guide_legend(
      
      override.aes = list(
      
        linetype = c("blank", "solid"),
      
        shape = c(16, NA)
      ),
      keyheight = unit(2.5, "lines") 
    )
  ) +
  

  scale_y_continuous(limits = c(-0.6, 0.1), breaks = seq(-0.6, 0.1, 0.1),
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_x_continuous(limits = c(-0.601, 0), breaks = seq(-0.6, 0, 0.1)) +
  

  labs(
    x = expression(italic("V")[GS] ~ "[V]"),
    y = expression(italic("E")["0,1,MIN"] ~ "[eV]")) +
  

  theme_bw() + 
  theme(text = element_text(family = "Times New Roman"), 
        axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 
        
        legend.position = c(5, 5), 
        legend.text = element_text(size = 20), 
        legend.box = "vertical", 
        legend.margin = margin(unit(0.01, "cm")), 
        legend.title = element_text(size = 16)) + 
  
  annotate("text", x=-0.55, y=-0.42,
           label=expression(italic("R")*"= 2 nm"),
           hjust=0, vjust=0,
           size=7, family = "Times New Roman")+
  annotate("text", x=-0.55, y=-0.49, 
           label=expression(italic("L")*"= 9 nm"),
           hjust=0, vjust=0,
           size=7, family = "Times New Roman")

# ----------------------------
temp5_englvlvg00 <- read.csv("./Nanowire_r2.5_l7_Vd06Vg00.str", header=FALSE, sep=" ")
temp5_englvlvg01 <- read.csv("./Nanowire_r2.5_l7_Vd06Vg01.str", header=FALSE, sep=" ")
temp5_englvlvg02 <- read.csv("./Nanowire_r2.5_l7_Vd06Vg02.str", header=FALSE, sep=" ")
temp5_englvlvg03 <- read.csv("./Nanowire_r2.5_l7_Vd06Vg03.str", header=FALSE, sep=" ")
temp5_englvlvg04 <- read.csv("./Nanowire_r2.5_l7_Vd06Vg04.str", header=FALSE, sep=" ")
temp5_englvlvg05 <- read.csv("./Nanowire_r2.5_l7_Vd06Vg05.str", header=FALSE, sep=" ")
temp5_englvlvg06 <- read.csv("./Nanowire_r2.5_l7_Vd06Vg06.str", header=FALSE, sep=" ")
temp5_englvlvg07 <- read.csv("./Nanowire_r2.5_l7_Vd06Vg07.str", header=FALSE, sep=" ")


temp5_englvlmdl <- read.table("/path/to/data/mathematica/englvlmin_3_1.txt")

df5_englvlsim <- data.frame(
  vgs = seq(from = 0, to = -0.7, by = -0.1),
  englvlsim = c(min(temp5_englvlvg00[28387:28687,33]),
                min(temp5_englvlvg01[28387:28687,33]),
                min(temp5_englvlvg02[28387:28687,33]),
                min(temp5_englvlvg03[28387:28687,33]),
                min(temp5_englvlvg04[28387:28687,33]),
                min(temp5_englvlvg05[28387:28687,33]),
                min(temp5_englvlvg06[28387:28687,33]),
                min(temp5_englvlvg07[28387:28687,33])))
df5_englvlsim
df5_englvlmdl <- data.frame(vgs = seq(from = 0, to = -0.7, by = -0.01),
                            englvlmdl = temp5_englvlmdl[,2])

base_plot_5 <- ggplot() +

  geom_line(data = df5_englvlmdl,
            aes(x = vgs,
                y = englvlmdl,
                color = "Model"),    
            size = 1.5) + 
  
  
  geom_point(data = df5_englvlsim,
             aes(x = vgs,
                 y = englvlsim,
                 color = "Simulation"), 
             size = 3, 
             shape = 16) + 
  
  
  scale_color_manual(
    name = " ", 
    values = c("Simulation" = "red",
               "Model" = "black"),
    labels = c(expression("Simulation"),
               expression("Model")),
    
    guide = guide_legend(
      override.aes = list(
        linetype = c("solid", "blank"), 
        shape = c(NA, 16) 
      ),
      keyheight = unit(2.5,"lines")
    )
  ) +
  
  
  scale_y_continuous(limits = c(-0.5, 0.1), breaks = seq(-0.5, 0.1, 0.1),
                     labels = scales::number_format(accuracy = 0.1)) +
  
  scale_x_continuous(limits = c(-0.601, 0), breaks = seq(-0.6, 0, 0.1)) +
  labs(
    x = expression(italic("V")[GS] ~ "[V]"),
    y = expression(italic("E")["0,1,MIN"] ~ "[eV]")) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size =20),
        axis.title.y = element_text(size =20),
        legend.position = c(5, 5), 
        legend.text = element_text(size = 20),
        legend.box = "vertical",
        legend.margin = margin(unit(0.01, "cm")),
        legend.spacing = unit(0.01, "cm"),
        legend.box.spacing = unit(0.01,"cm"),
        legend.title = element_text(size = 18)) + 
  
  
  annotate("text", x=-0.55, y=-0.35,
           label=expression(italic("R")*"= 2.5 nm"),
           hjust=0, vjust=0,
           size=7, 
           family = "Times New Roman")+
  annotate("text", x=-0.55, y=-0.4, 
           label=expression(italic("L")*"= 7 nm"),
           hjust=0, vjust=0,
           size=7,
           family = "Times New Roman")
# ----------------------------
max_str_cols <- 60 

str_col_names <- paste0("V", 1:max_str_cols)



temp6_englvlvg00 <- read_table(file = "./Nanowire_r2.5_l9_Vd06Vg00.str", col_names = str_col_names, skip = 0)
temp6_englvlvg01 <- read_table(file = "./Nanowire_r2.5_l9_Vd06Vg01.str", col_names = str_col_names, skip = 0)
temp6_englvlvg02 <- read_table(file = "./Nanowire_r2.5_l9_Vd06Vg02.str", col_names = str_col_names, skip = 0)
temp6_englvlvg03 <- read_table(file = "./Nanowire_r2.5_l9_Vd06Vg03.str", col_names = str_col_names, skip = 0)
temp6_englvlvg04 <- read_table(file = "./Nanowire_r2.5_l9_Vd06Vg04.str", col_names = str_col_names, skip = 0)
temp6_englvlvg05 <- read_table(file = "./Nanowire_r2.5_l9_Vd06Vg05.str", col_names = str_col_names, skip = 0)
temp6_englvlvg06 <- read_table(file = "./Nanowire_r2.5_l9_Vd06Vg06.str", col_names = str_col_names, skip = 0)
temp6_englvlvg07 <- read_table(file = "./Nanowire_r2.5_l9_Vd06Vg07.str", col_names = str_col_names, skip = 0)


temp6_englvlvg00$V33 <- as.numeric(temp6_englvlvg00$V33)
temp6_englvlvg01$V33 <- as.numeric(temp6_englvlvg01$V33)
temp6_englvlvg02$V33 <- as.numeric(temp6_englvlvg02$V33)
temp6_englvlvg03$V33 <- as.numeric(temp6_englvlvg03$V33)
temp6_englvlvg04$V33 <- as.numeric(temp6_englvlvg04$V33)
temp6_englvlvg05$V33 <- as.numeric(temp6_englvlvg05$V33)
temp6_englvlvg06$V33 <- as.numeric(temp6_englvlvg06$V33)
temp6_englvlvg07$V33 <- as.numeric(temp6_englvlvg07$V33)


temp6_englvlmdl <- read.table("/path/to/data/mathematica/englvlmin_3_2.txt")


start_row_6 <- 27437
end_row_6 <- 27727
target_col_6 <- 33

df6_englvlsim <- data.frame(
  vgs = seq(from = 0, to = -0.7, by = -0.1),
  englvlsim = c(min(temp6_englvlvg00[start_row_6:end_row_6, target_col_6], na.rm = TRUE),
                min(temp6_englvlvg01[start_row_6:end_row_6, target_col_6], na.rm = TRUE),
                min(temp6_englvlvg02[start_row_6:end_row_6, target_col_6], na.rm = TRUE),
                min(temp6_englvlvg03[start_row_6:end_row_6, target_col_6], na.rm = TRUE),
                min(temp6_englvlvg04[start_row_6:end_row_6, target_col_6], na.rm = TRUE),
                min(temp6_englvlvg05[start_row_6:end_row_6, target_col_6], na.rm = TRUE),
                min(temp6_englvlvg06[start_row_6:end_row_6, target_col_6], na.rm = TRUE),
                min(temp6_englvlvg07[start_row_6:end_row_6, target_col_6], na.rm = TRUE)))


df6_englvlmdl <- data.frame(vgs = seq(from = 0, to = -0.7, by = -0.01),
                            englvlmdl = as.numeric(temp6_englvlmdl[,2])) 



base_plot_6 <- ggplot() +

  geom_line(data = df6_englvlmdl,
            aes(x = vgs, y = englvlmdl, color = "Model"), 
            size = 1.5) +
  

  geom_point(data = df6_englvlsim,
             aes(x = vgs, y = englvlsim, color = "Simulation"), 
             size = 3, shape = 16) +
  

  scale_color_manual(
    name = " ", 
    values = c("Simulation" = "red", "Model" = "black"), 
    labels = c(expression("Simulation"), expression("Model")), 
    guide = guide_legend(

      override.aes = list(

        linetype = c("blank", "solid"),

        shape = c(16, NA)
      ),
      keyheight = unit(2.5, "lines") 
    )
  ) +
  

  scale_y_continuous(limits = c(-0.5, 0.1), breaks = seq(-0.6, 0.1, 0.1),
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_x_continuous(limits = c(-0.601, 0), breaks = seq(-0.6, 0, 0.1)) +
  

  labs(
    x = expression(italic("V")[GS] ~ "[V]"),
    y = expression(italic("E")["0,1,MIN"] ~ "[eV]")) +
  

  theme_bw() + 
  theme(text = element_text(family = "Times New Roman"), 
        axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 20), 
        axis.title.y = element_text(size = 20), 

        legend.position = c(5, 5), 
        legend.text = element_text(size = 20), 
        legend.box = "vertical",
        legend.margin = margin(unit(0.01, "cm")), 
        legend.title = element_text(size = 16)) + 
  
  annotate("text", x=-0.55, y=-0.35,
           label=expression(italic("R")*"= 2.5 nm"),
           hjust=0, vjust=0,
           size=7, family = "Times New Roman")+
  annotate("text", x=-0.55, y=-0.4, 
           label=expression(italic("L")*"= 9 nm"),
           hjust=0, vjust=0,
           size=7, family = "Times New Roman")

p_1a <- base_plot_1 +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  annotate("text",
           x = -0.6,
           y = 0,
           label = "(a)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_1b <- base_plot_2 +
  theme(
    ) +
  annotate("text",
           x = -0.6,
           y = 0.,
           label = "(d)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_2a <- base_plot_3 +
  theme(axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    ) +
  annotate("text",
           x = -0.6,
           y = 0.1,
           label = "(b)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_2b <- base_plot_4 +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        ) +
  annotate("text",
           x = -0.6,
           y = 0.1,
           label = "(e)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_3a <- base_plot_5 +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  annotate("text",
           x = -0.6,
           y = 0.1,
           label = "(c)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_3b <- base_plot_6 +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  annotate("text",
           x = -0.6,
           y = 0.1,
           label = "(f)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

final_plot <- (p_1a | p_2a | p_3a)/  
  (p_1b | p_2b | p_3b)
print(final_plot)
ggsave("/path/to/data/fig/englvlmin_yoko.png",
       final_plot, width = 16, height = 10, dpi = 300)

###############################################################################
############## PLOT MULTIPLE SUBBAND ENERGY LEVELS vs. GATE VOLTAGE ############

setwd("/path/to/data/")
temp1_englvlvg00 <- read.csv("./silvaco/Nanowire_r1.5_l7_Vd06Vg00.str", 
                             header=FALSE, sep=" ")
temp1_englvlvg02 <- read.csv("./silvaco/Nanowire_r1.5_l7_Vd06Vg02.str", 
                             header=FALSE, sep=" ")
temp1_englvlvg05 <- read.csv("./silvaco/Nanowire_r1.5_l7_Vd06Vg05.str", 
                             header=FALSE, sep=" ")
temp4_englvlvg00 <- read.csv("./silvaco/Nanowire_r1.5_l9_Vd06Vg00.str", 
                             header=FALSE, sep=" ")
temp4_englvlvg02 <- read.csv("./silvaco/Nanowire_r1.5_l9_Vd06Vg02.str", 
                             header=FALSE, sep=" ")
temp4_englvlvg05 <- read.csv("./silvaco/Nanowire_r1.5_l9_Vd06Vg05.str", 
                             header=FALSE, sep=" ")
temp2_englvlvg00 <- read.csv("./silvaco/Nanowire_r2_l7_Vd06Vg00.str", 
                             header=FALSE, sep=" ")
temp2_englvlvg02 <- read.csv("./silvaco/Nanowire_r2_l7_Vd06Vg02.str", 
                             header=FALSE, sep=" ")
temp2_englvlvg05 <- read.csv("./silvaco/Nanowire_r2_l7_Vd06Vg05.str", 
                             header=FALSE, sep=" ")
temp3_englvlvg00 <- read.csv("./silvaco/Nanowire_r2.5_l7_Vd06Vg00.str", 
                             header=FALSE, sep=" ")
temp3_englvlvg02 <- read.csv("./silvaco/Nanowire_r2.5_l7_Vd06Vg02.str", 
                             header=FALSE, sep=" ")
temp3_englvlvg05 <- read.csv("./silvaco/Nanowire_r2.5_l7_Vd06Vg05.str", 
                             header=FALSE, sep=" ")
temp5_englvlvg00 <- read.csv("./silvaco/Nanowire_r2_l9_Vd06Vg00.str", 
                             header=FALSE, sep=" ")
temp5_englvlvg02 <- read.csv("./silvaco/Nanowire_r2_l9_Vd06Vg02.str", 
                             header=FALSE, sep=" ")
temp5_englvlvg05 <- read.csv("./silvaco/Nanowire_r2_l9_Vd06Vg05.str", 
                             header=FALSE, sep=" ")
temp6_englvlvg00 <- read.csv("./silvaco/Nanowire_r2.5_l9_Vd06Vg00.str", 
                             header=FALSE, sep=" ")
temp6_englvlvg02 <- read.csv("./silvaco/Nanowire_r2.5_l9_Vd06Vg02.str", 
                             header=FALSE, sep=" ")
temp6_englvlvg05 <- read.csv("./silvaco/Nanowire_r2.5_l9_Vd06Vg05.str", 
                             header=FALSE, sep=" ")
############ fig 1 #############
df1_englvl <- data.frame(
  z = seq(from = 0, to = 27, by = 0.1),
  simvg00 = c(temp1_englvlvg00[17347:17617,33]),
  simvg02 = c(temp1_englvlvg02[17347:17617,33]),
  simvg05 = c(temp1_englvlvg05[17347:17617,33])
)
colnames(df1_englvl) <- c("z", "simvg00", "simvg02", "simvg05")
#df1_englvl
df1_englvlmdl <- data.frame(read.table("/path/to/data/mathematica/anaEnglvl_1_1.txt"))
colnames(df1_englvlmdl) <- c("z", "mdlvg00", "mdlvg02", "mdlvg05")

base_plot_1 <- ggplot() +
  geom_line(data = df1_englvl, 
            aes(x = z, 
                y = simvg00, 
                color = "VGS = 0.0 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df1_englvl, 
            aes(x = z, 
                y = simvg02, 
                color = "VGS = 0.2 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df1_englvl, 
            aes(x = z, 
                y = simvg05, 
                color = "VGS = 0.5 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_point(data = df1_englvlmdl, 
             aes(x = z, 
                 y = mdlvg00, 
                 color = "VGS = 0.0 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df1_englvlmdl, 
             aes(x = z, 
                 y = mdlvg02, 
                 color = "VGS = 0.2 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df1_englvlmdl, 
             aes(x = z, 
                 y = mdlvg05, 
                 color = "VGS = 0.5 V", 
                 shape = "Model"), 
             size = 3) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 17, linetype = "dashed", color = "black", linewidth = 0.5) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(0,30), breaks = seq(0, 30, 5)) +
  labs(
    x = expression(italic("z") ~ "[nm]"),
    y = expression(italic("E")["0,1"]^{"H"} ~ "[eV]")
  ) +
  scale_color_manual(name = " ",
                     values = c("VGS = 0.0 V" = "red",
                                "VGS = 0.2 V" = "green",
                                "VGS = 0.5 V" = "blue"),
                     labels = c(expression(italic("V")[GS]*" = 0.0 V"),
                                expression(italic("V")[GS]*" = -0.2 V"),
                                expression(italic("V")[GS]*" = -0.5 V"))) +
  scale_linetype_manual(name = " ", 
                        values = c("Simulation" = "solid")) +
  scale_shape_manual(name = " ", 
                     values = c("Model" = 16)) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(0.79,0.29),
    legend.text = element_text(size = 16),
    legend.box = "vertical",
    legend.spacing.y = unit(0.0, "cm"),
    legend.box.spacing = unit(0.0,"cm"),
    legend.box.margin = margin(0, 0, 0, 0, unit="pt"),
    #legend.margin = margin(unit(0.001, "cm")),
    legend.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit = "cm"),
    legend.key.size = unit(0.8, "cm"),
    legend.key.height = unit(0.1, "lines"),
    legend.key.width = unit(0.4, "cm")) +
  annotate("text", 
           x=0, 
           y=-0.40, 
           label=expression(italic("R")*"= 1.5 nm"), 
           hjust=0,
           vjust=0, 
           size=7, 
           family = "Times New Roman") +
  annotate("text", 
           x=0, 
           y=-0.55, 
           label=expression(italic("L")*"= 7 nm"), 
           hjust=0, 
           vjust=0, 
           size=7, 
           family = "Times New Roman") +
  annotate("text", 
           x=0, 
           y=-0.7, 
           label=expression(italic("V")[DS]*"= -0.6 V"), 
           hjust=0, 
           vjust=0, 
           size=7, 
           family = "Times New Roman") +
  annotate("text", 
           x=0, 
           y=-0.85, 
           label=expression(italic(phi)[GC]*"-"*italic(w)[FB]*"= 0.63 V"), 
           hjust=0, 
           vjust=0, 
           size=7, 
           family = "Times New Roman") +
  annotate("text", 
           x=0, 
           y=-1, 
           label=expression(italic(V)[bi]*"= -1.35 V"), 
           hjust=0, 
           vjust=0, 
           size=7, 
           family = "Times New Roman")
############ fig 2 #############
df2_englvl <- data.frame(
  z = seq(from = 0, to = 27, by = 0.1),
  simvg00 = c(temp2_englvlvg00[21442:21712,33]),
  simvg02 = c(temp2_englvlvg02[21442:21712,33]),
  simvg05 = c(temp2_englvlvg05[21442:21712,33])
)
colnames(df2_englvl) <- c("z", "simvg00", "simvg02", "simvg05")

df2_englvlmdl <- data.frame(read.table("/path/to/data/mathematica/anaEnglvl_2_1.txt"))
colnames(df2_englvlmdl) <- c("z", "mdlvg00", "mdlvg02", "mdlvg05")

base_plot_2 <- ggplot() +
  geom_line(data = df2_englvl, 
            aes(x = z, 
                y = simvg00, 
                color = "VGS = 0.0 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df2_englvl, 
            aes(x = z, 
                y = simvg02, 
                color = "VGS = 0.2 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df2_englvl, 
            aes(x = z, 
                y = simvg05, 
                color = "VGS = 0.5 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_point(data = df2_englvlmdl, 
             aes(x = z, 
                 y = mdlvg00, 
                 color = "VGS = 0.0 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df2_englvlmdl, 
             aes(x = z, 
                 y = mdlvg02, 
                 color = "VGS = 0.2 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df2_englvlmdl, 
             aes(x = z, 
                 y = mdlvg05, 
                 color = "VGS = 0.5 V", 
                 shape = "Model"), 
             size = 3) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 17, linetype = "dashed", color = "black", linewidth = 0.5) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(0,30),breaks = seq(0, 30, 5)) +
  labs(
    x = expression(italic("z") ~ "[nm]"),
    y = expression(italic("E")["0,1"]^{"H"} ~ "[eV]")
  ) +
  scale_color_manual(name = " ",
                     values = c("VGS = 0.0 V" = "red",
                                "VGS = 0.2 V" = "green",
                                "VGS = 0.5 V" = "blue"),
                     labels = c(expression(italic("V")[GS]*" = 0.0 V"),
                                expression(italic("V")[GS]*" = -0.2 V"),
                                expression(italic("V")[GS]*" = -0.5 V"))) +
  scale_linetype_manual(name = " ", 
                        values = c("Simulation" = "solid")) +
  scale_shape_manual(name = " ", 
                     values = c("Model" = 16)) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(5,5),
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.spacing = unit(0.01, "cm"),
    legend.box.spacing = unit(0.05,"cm")) +
  annotate("text", 
           x=0, 
           y=-0.40, 
           label=expression(italic("R")*"= 2 nm"), 
           hjust=0,
           vjust=0, 
           size=7, 
           family = "Times New Roman") +
  annotate("text", 
           x=0, 
           y=-0.55, 
           label=expression(italic("L")*"= 7 nm"), 
           hjust=0, 
           vjust=0, 
           size=7, 
           family = "Times New Roman") 
############ fig 3 #############
df3_englvl <- data.frame(
  z = seq(from = 0, to = 27, by = 0.1),
  simvg00 = c(temp3_englvlvg00[25537:25807,33]),
  simvg02 = c(temp3_englvlvg02[25537:25807,33]),
  simvg05 = c(temp3_englvlvg05[25537:25807,33])
)
colnames(df3_englvl) <- c("z", "simvg00", "simvg02", "simvg05")

df3_englvlmdl <- data.frame(read.table("/path/to/data/mathematica/anaEnglvl_3_1.txt"))
colnames(df3_englvlmdl) <- c("z", "mdlvg00", "mdlvg02", "mdlvg05")

base_plot_3 <- ggplot() +
  geom_line(data = df3_englvl, 
            aes(x = z, 
                y = simvg00, 
                color = "VGS = 0.0 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df3_englvl, 
            aes(x = z, 
                y = simvg02, 
                color = "VGS = 0.2 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df3_englvl, 
            aes(x = z, 
                y = simvg05, 
                color = "VGS = 0.5 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_point(data = df3_englvlmdl, 
             aes(x = z, 
                 y = mdlvg00, 
                 color = "VGS = 0.0 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df3_englvlmdl, 
             aes(x = z, 
                 y = mdlvg02, 
                 color = "VGS = 0.2 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df3_englvlmdl, 
             aes(x = z, 
                 y = mdlvg05, 
                 color = "VGS = 0.5 V", 
                 shape = "Model"), 
             size = 3) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 17, linetype = "dashed", color = "black", linewidth = 0.5) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  labs(
    x = expression(italic("z") ~ "[nm]"),
    y = expression(italic("E")["0,1"]^{"H"} ~ "[eV]")
  ) +
  scale_color_manual(name = " ",
                     values = c("VGS = 0.0 V" = "red",
                                "VGS = 0.2 V" = "green",
                                "VGS = 0.5 V" = "blue"),
                     labels = c(expression(italic("V")[GS]*" = 0.0 V"),
                                expression(italic("V")[GS]*" = -0.2 V"),
                                expression(italic("V")[GS]*" = -0.5 V"))) +
  scale_linetype_manual(name = " ", 
                        values = c("Simulation" = "solid")) +
  scale_shape_manual(name = " ", 
                     values = c("Model" = 16)) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(5,5),
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.spacing = unit(0.01, "cm"),
    legend.box.spacing = unit(0.05,"cm")) +
  annotate("text", 
           x=0, 
           y=-0.40, 
           label=expression(italic("R")*"= 2.5 nm"), 
           hjust=0,
           vjust=0, 
           size=7, 
           family = "Times New Roman") +
  annotate("text", 
           x=0, 
           y=-0.55, 
           label=expression(italic("L")*"= 7 nm"), 
           hjust=0, 
           vjust=0, 
           size=7, 
           family = "Times New Roman") 
############ fig 4 #############
df4_englvl <- data.frame(
  z = seq(from = 0, to = 29, by = 0.1),
  simvg00 = c(temp4_englvlvg00[18647:18937,33]),
  simvg02 = c(temp4_englvlvg02[18647:18937,33]),
  simvg05 = c(temp4_englvlvg05[18647:18937,33])
)
colnames(df4_englvl) <- c("z", "simvg00", "simvg02", "simvg05")

df4_englvlmdl <- data.frame(read.table("/path/to/data/mathematica/anaEnglvl_1_2.txt"))
colnames(df4_englvlmdl) <- c("z", "mdlvg00", "mdlvg02", "mdlvg05")

base_plot_4 <- ggplot() +
  geom_line(data = df4_englvl, 
            aes(x = z, 
                y = simvg00, 
                color = "VGS = 0.0 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df4_englvl, 
            aes(x = z, 
                y = simvg02, 
                color = "VGS = 0.2 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df4_englvl, 
            aes(x = z, 
                y = simvg05, 
                color = "VGS = 0.5 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_point(data = df4_englvlmdl, 
             aes(x = z, 
                 y = mdlvg00, 
                 color = "VGS = 0.0 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df4_englvlmdl, 
             aes(x = z, 
                 y = mdlvg02, 
                 color = "VGS = 0.2 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df4_englvlmdl, 
             aes(x = z, 
                 y = mdlvg05, 
                 color = "VGS = 0.5 V", 
                 shape = "Model"), 
             size = 3) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 19, linetype = "dashed", color = "black", linewidth = 0.5) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(0,30), breaks = seq(0, 30, 5)) +
  labs(
    x = expression(italic("z") ~ "[nm]"),
    y = expression(italic("E")["0,1"]^{"H"} ~ "[eV]")) +
  scale_color_manual(name = " ",
                     values = c("VGS = 0.0 V" = "red",
                                "VGS = 0.2 V" = "green",
                                "VGS = 0.5 V" = "blue"),
                     labels = c(expression(italic("V")[GS]*" = 0.0 V"),
                                expression(italic("V")[GS]*" = -0.2 V"),
                                expression(italic("V")[GS]*" = -0.5 V"))) +
  scale_linetype_manual(name = " ", 
                        values = c("Simulation" = "solid")) +
  scale_shape_manual(name = " ", 
                     values = c("Model" = 16)) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(5,5),
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.spacing = unit(0.01, "cm"),
    legend.box.spacing = unit(0.05,"cm")) +
  annotate("text", 
           x=0, 
           y=-0.40, 
           label=expression(italic("R")*"= 1.5 nm"), 
           hjust=0,
           vjust=0, 
           size=7, 
           family = "Times New Roman") +
  annotate("text", 
           x=0, 
           y=-0.55, 
           label=expression(italic("L")*"= 9 nm"), 
           hjust=0, 
           vjust=0, 
           size=7, 
           family = "Times New Roman") 
############ fig 5 #############
df5_englvl <- data.frame(
  z = seq(from = 0, to = 29, by = 0.1),
  simvg00 = c(temp5_englvlvg00[23042:23332,33]),
  simvg02 = c(temp5_englvlvg02[23042:23332,33]),
  simvg05 = c(temp5_englvlvg05[23042:23332,33])
)
colnames(df5_englvl) <- c("z", "simvg00", "simvg02", "simvg05")

df5_englvlmdl <- data.frame(read.table("/path/to/data/mathematica/anaEnglvl_2_2.txt"))
colnames(df5_englvlmdl) <- c("z", "mdlvg00", "mdlvg02", "mdlvg05")

base_plot_5 <- ggplot() +
  geom_line(data = df5_englvl, 
            aes(x = z, 
                y = simvg00, 
                color = "VGS = 0.0 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df5_englvl, 
            aes(x = z, 
                y = simvg02, 
                color = "VGS = 0.2 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df5_englvl, 
            aes(x = z, 
                y = simvg05, 
                color = "VGS = 0.5 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_point(data = df5_englvlmdl, 
             aes(x = z, 
                 y = mdlvg00, 
                 color = "VGS = 0.0 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df5_englvlmdl, 
             aes(x = z, 
                 y = mdlvg02, 
                 color = "VGS = 0.2 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df5_englvlmdl, 
             aes(x = z, 
                 y = mdlvg05, 
                 color = "VGS = 0.5 V", 
                 shape = "Model"), 
             size = 3) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 19, linetype = "dashed", color = "black", linewidth = 0.5) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(0,30), breaks = seq(0, 30, 5)) +
  labs(
    x = expression(italic("z") ~ "[nm]"),
    y = expression(italic("E")["0,1"]^{"H"} ~ "[eV]")) +
  scale_color_manual(name = " ",
                     values = c("VGS = 0.0 V" = "red",
                                "VGS = 0.2 V" = "green",
                                "VGS = 0.5 V" = "blue"),
                     labels = c(expression(italic("V")[GS]*" = 0.0 V"),
                                expression(italic("V")[GS]*" = -0.2 V"),
                                expression(italic("V")[GS]*" = -0.5 V"))) +
  scale_linetype_manual(name = " ", 
                        values = c("Simulation" = "solid")) +
  scale_shape_manual(name = " ", 
                     values = c("Model" = 16)) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(5,5),
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.spacing = unit(0.01, "cm"),
    legend.box.spacing = unit(0.05,"cm")) +
  annotate("text", 
           x=0, 
           y=-0.40, 
           label=expression(italic("R")*"= 2 nm"), 
           hjust=0,
           vjust=0, 
           size=7, 
           family = "Times New Roman") +
  annotate("text", 
           x=0, 
           y=-0.55, 
           label=expression(italic("L")*"= 9 nm"), 
           hjust=0, 
           vjust=0, 
           size=7, 
           family = "Times New Roman") 

############ fig 6 #############
df6_englvl <- data.frame(
  z = seq(from = 0, to = 29, by = 0.1),
  simvg00 = c(temp6_englvlvg00[27437:27727,33]),
  simvg02 = c(temp6_englvlvg02[27437:27727,33]),
  simvg05 = c(temp6_englvlvg05[27437:27727,33])
)
colnames(df6_englvl) <- c("z", "simvg00", "simvg02", "simvg05")

df6_englvlmdl <- data.frame(read.table("/path/to/data/mathematica/anaEnglvl_3_2.txt"))
colnames(df6_englvlmdl) <- c("z", "mdlvg00", "mdlvg02", "mdlvg05")

base_plot_6 <- ggplot() +
  geom_line(data = df6_englvl, 
            aes(x = z, 
                y = simvg00, 
                color = "VGS = 0.0 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df6_englvl, 
            aes(x = z, 
                y = simvg02, 
                color = "VGS = 0.2 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_line(data = df6_englvl, 
            aes(x = z, 
                y = simvg05, 
                color = "VGS = 0.5 V", 
                linetype = "Simulation"), 
            size = 1) +
  geom_point(data = df6_englvlmdl, 
             aes(x = z, 
                 y = mdlvg00, 
                 color = "VGS = 0.0 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df6_englvlmdl, 
             aes(x = z, 
                 y = mdlvg02, 
                 color = "VGS = 0.2 V", 
                 shape = "Model"), 
             size = 3) +
  geom_point(data = df6_englvlmdl, 
             aes(x = z, 
                 y = mdlvg05, 
                 color = "VGS = 0.5 V", 
                 shape = "Model"), 
             size = 3) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 19, linetype = "dashed", color = "black", linewidth = 0.5) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(0,30), breaks = seq(0, 30, 5)) +
  labs(
    x = expression(italic("z") ~ "[nm]"),
    y = expression(italic("E")["0,1"]^{"H"} ~ "[eV]")) +
  scale_color_manual(name = " ",
                     values = c("VGS = 0.0 V" = "red",
                                "VGS = 0.2 V" = "green",
                                "VGS = 0.5 V" = "blue"),
                     labels = c(expression(italic("V")[GS]*" = 0.0 V"),
                                expression(italic("V")[GS]*" = -0.2 V"),
                                expression(italic("V")[GS]*" = -0.5 V"))) +
  scale_linetype_manual(name = " ", 
                        values = c("Simulation" = "solid")) +
  scale_shape_manual(name = " ", 
                     values = c("Model" = 16)) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(5,5),
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.spacing = unit(0.01, "cm"),
    legend.box.spacing = unit(0.05,"cm")) +
  annotate("text", 
           x=0, 
           y=-0.40, 
           label=expression(italic("R")*"= 2.5 nm"), 
           hjust=0,
           vjust=0, 
           size=7, 
           family = "Times New Roman") +
  annotate("text", 
           x=0, 
           y=-0.55, 
           label=expression(italic("L")*"= 9 nm"), 
           hjust=0, 
           vjust=0, 
           size=7, 
           family = "Times New Roman") 

############ plot #############################################
p_1a <- base_plot_1 +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  annotate("text",
           x = 0,
           y = 1.0,
           label = "(a)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_1b <- base_plot_4 +
  theme(#axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank()
    ) +
  annotate("text",
           x = 0,
           y = 1.0,
           label = "(d)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_2a <- base_plot_2 +
  theme(axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) +
  annotate("text",
           x = 0,
           y = 1.0,
           label = "(b)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_2b <- base_plot_5 +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank()
        ) +
  annotate("text",
           x = 0,
           y = 1.0,
           label = "(e)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_3a <- base_plot_3 +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  annotate("text",
           x = 0,
           y = 1.0,
           label = "(c)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_3b <- base_plot_6 +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  annotate("text",
           x = 0,
           y = 1.0,
           label = "(f)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

final_plot <- (p_1a | p_2a | p_3a )/  
  (p_1b | p_2b | p_3b )
print(final_plot)

ggsave("/path/to/data/fig/combined_englvl_comparison_yoko.png", 
       final_plot, width = 16, height = 10, dpi = 300)
########################## IDVG #############################################

setwd("/path/to/data/silvaco/")


temp1_idvg <- read.csv("IdVg_Nanowire_r1.5_l7.log", header = FALSE, sep=" ")
temp2_idvg <- read.csv("IdVg_Nanowire_r2_l7.log", header = FALSE, sep=" ")
temp3_idvg <- read.csv("IdVg_Nanowire_r2.5_l7.log", header = FALSE, sep=" ")
temp4_idvg <- read.csv("IdVg_Nanowire_r1.5_l9.log", header = FALSE, sep=" ")
temp5_idvg <- read.csv("IdVg_Nanowire_r2_l9.log", header = FALSE, sep=" ")
temp6_idvg <- read.csv("IdVg_Nanowire_r2.5_l9.log", header = FALSE, sep=" ")

primary_min_log <- -17
primary_max_log <- -5
secondary_min_linear <- 0
secondary_max_linear <- 6e-6 # <-- CHANGED TO 5e-7
# Calculate slope (m_val) and intercept (c_val) for: secondary_value = m_val * primary_value + c_val
m_val <- (secondary_max_linear - secondary_min_linear) / (primary_max_log - primary_min_log)
c_val <- secondary_min_linear - m_val * primary_min_log

sec_axis_def_final <- sec_axis(
  trans = ~ ( . * m_val + c_val ), # Transforms primary (log10) value to secondary (linear) value
  name = expression(italic("I")[SD] ~ "[A]"),
  breaks = seq(secondary_min_linear, secondary_max_linear, by =1e-6) # Breaks for the new linear range
)
################## fig 1
df1_idsim <- data.frame(vgs = as.numeric(temp1_idvg[36:96,8]), 
                        simidvg = as.numeric(temp1_idvg[36:96,4]))
colnames(df1_idsim) <- c("vgs", "id_sim")
df1_idsim
temp_df1_idmdl <- read.table("/path/to/data/mathematica/anaisd_1_1.txt", 
                             header = FALSE)
df1_idmdl <- data.frame(vgs =as.numeric(temp_df1_idmdl[ ,1]), 
                        mdlidvg = as.numeric(temp_df1_idmdl[ ,2]))
colnames(df1_idmdl) <- c("vgs", "id_mdl")
df1_idmdl
df1_idsim$log_id_sim <- log10(df1_idsim$id_sim)
df1_idmdl$log_id_mdl <- log10(df1_idmdl$id_mdl)
df1_idsim$mapped_linear_sim <- (df1_idsim$id_sim - c_val) / m_val
df1_idmdl$mapped_linear_mdl <- (df1_idmdl$id_mdl - c_val) / m_val

base_plot_1 <- ggplot() +
  # Plot log-scale data
  geom_line(data = df1_idsim, aes(x = vgs, y = log_id_sim, color = "Log Sim", linetype = "Simulation"), size = 1.5) +
  geom_point(data = df1_idmdl, aes(x = vgs, y = log_id_mdl, color = "Log Model", shape = "Model"), size = 4) +
  
  # Plot linear-scale data, mapped to the primary log10 axis for correct alignment
  geom_line(data = df1_idsim, aes(x = vgs, y = mapped_linear_sim, color = "Linear Sim"), size = 1.5) +
  geom_point(data = df1_idmdl, aes(x = vgs, y = mapped_linear_mdl, color = "Linear Model", shape = "Model"), size = 4) +
  
  # Manual scales for colors, shapes, and linetypes
  scale_color_manual(name = "",
                     values = c("Log Sim" = "black", "Log Model" = "black",
                                "Linear Sim" = "red", "Linear Model" = "red")) +
  scale_shape_manual(name = "", values = c("Model" = 16)) +
  scale_linetype_manual(name = "", values = c("Simulation" = "solid")) +
  
  # Y-axis definitions
  scale_y_continuous(name = expression(log[10]~(italic("I")[SD])~"[A]"),
                     sec.axis = sec_axis_def_final, # Apply the carefully crafted secondary axis
                     # Explicit primary axis breaks for alignment.
                     # These should correspond to the secondary axis breaks via the inverse transformation.
                     breaks = c(
                       (0 - c_val) / m_val,        # maps 0 A to log scale (-17)
                       (1e-6 - c_val) / m_val,       # maps 1e-7 A to log scale (-15)
                       (2e-6 - c_val) / m_val,       # maps 2e-7 A to log scale (-13)
                       (3e-6 - c_val) / m_val,       # maps 3e-7 A to log scale (-11)
                       (4e-6 - c_val) / m_val,       # maps 4e-7 A to log scale (-9)
                       (5e-6 - c_val) / m_val,      # maps 5e-7 A to log scale (-7)
                       (6e-6 - c_val) / m_val        # maps 5e-7 A to log scale (-5)
                     )
  ) +
  coord_cartesian(ylim = c(primary_min_log, primary_max_log)) + # Set the visible range for the primary y-axis
  
  # Labels and Theme
  labs(x = expression(italic("V")[GS] ~ "[V]")) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size =20),
        axis.title.y = element_text(size =20),
        legend.position = c(0.77, 0.728),
        legend.text = element_text(size = 16,hjust=0),
        legend.box = "vertical",
        #legend.margin = margin(unit(0.01, "cm")),
        #legend.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit = "cm"),
        #legend.key.size = unit(0.8, "cm"),
        #legend.spacing.y = unit(0.01, "cm"),
        #legend.box.spacing = unit(0.01,"cm"),
        #legend.key.height = unit(0.3, "lines"),
        #legend.box = "vertical",
        legend.spacing.y = unit(0.0, "cm"),
        legend.box.spacing = unit(0.0,"cm"),
        legend.box.margin = margin(0, 0, 0, 0, unit="pt"),
        #legend.margin = margin(unit(0.001, "cm")),
        legend.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit = "cm"),
        legend.key.size = unit(0.8, "cm"),
        legend.key.height = unit(0.2, "lines"),
        legend.key.width = unit(0.5, "cm"),
        axis.line.y.right = element_blank(), # Remove secondary axis line
        axis.ticks.y.right = element_blank(), # Remove secondary axis ticks
        axis.text.y.right = element_blank(), # Remove secondary axis labels
        axis.title.y.right = element_blank() # Remove secondary axis title
  ) +
  # Annotations (unchanged)
  annotate("text", x=-0.5, y=-12, label=expression(italic("R")*"= 1.5 nm"), 
           hjust=0,vjust=0, size=7, family = "Times New Roman")+
  annotate("text", x=-0.5, y=-13, label=expression(italic("L")*"= 7 nm"), 
           hjust=0., vjust=0, size=7, family = "Times New Roman") +
  annotate("text", x=-0.5, y=-14, label=expression(italic("V")[DS]*"= -0.6 V"),
           hjust=0., vjust=0, size=7, family = "Times New Roman")+
  annotate("text", x=-0.5, y=-15, label=expression(italic(phi)[GC]*"-"*italic(w)[FB]*"= 0.63 V"),
           hjust=0., vjust=0, size=7, family = "Times New Roman") +
  annotate("text", x=-0.5, y=-16, label=expression(italic(V)[bi]*"= -1.35 V"),
           hjust=0, vjust=0, size=7, family = "Times New Roman")

################## fig 2
df2_idsim <- data.frame(vgs = as.numeric(temp2_idvg[36:96,8]), 
                        simidvg = as.numeric(temp2_idvg[36:96,4]))
df2_idsim
colnames(df2_idsim) <- c("vgs", "id_sim")
temp_df2_idmdl <- read.table("/path/to/data/mathematica/anaisd_2_1.txt", header = FALSE)
df2_idmdl <- data.frame(vgs =as.numeric(temp_df2_idmdl[ ,1]), mdlidvg = as.numeric(temp_df2_idmdl[ ,2]))
colnames(df2_idmdl) <- c("vgs", "id_mdl")
df2_idsim$log_id_sim <- log10(df2_idsim$id_sim)
df2_idmdl$log_id_mdl <- log10(df2_idmdl$id_mdl)
df2_idsim$mapped_linear_sim <- (df2_idsim$id_sim - c_val) / m_val
df2_idmdl$mapped_linear_mdl <- (df2_idmdl$id_mdl - c_val) / m_val

base_plot_2 <- ggplot() +
  # Plot log-scale data
  geom_line(data = df2_idsim, aes(x = vgs, y = log_id_sim, color = "Log Sim", linetype = "Simulation"), size = 1.5) +
  geom_point(data = df2_idmdl, aes(x = vgs, y = log_id_mdl, color = "Log Model", shape = "Model"), size = 4) +
  
  # Plot linear-scale data, mapped to the primary log10 axis for correct alignment
  geom_line(data = df2_idsim, aes(x = vgs, y = mapped_linear_sim, color = "Linear Sim"), size = 1.5) +
  geom_point(data = df2_idmdl, aes(x = vgs, y = mapped_linear_mdl, color = "Linear Model", shape = "Model"), size = 4) +
  
  # Manual scales for colors, shapes, and linetypes
  scale_color_manual(name = "",
                     values = c("Log Sim" = "black", "Log Model" = "black",
                                "Linear Sim" = "red", "Linear Model" = "red")) +
  scale_shape_manual(name = "", values = c("Model" = 16)) +
  scale_linetype_manual(name = "", values = c("Simulation" = "solid")) +
  
  # Y-axis definitions
  scale_y_continuous(name = expression(log[10]~(italic("I")[SD])~"[A]"),
                     sec.axis = sec_axis_def_final, # Apply the carefully crafted secondary axis
                     # Explicit primary axis breaks for alignment.
                     # These should correspond to the secondary axis breaks via the inverse transformation.
                     breaks = c(
                       (0 - c_val) / m_val,        # maps 0 A to log scale (-17)
                       (1e-6 - c_val) / m_val,       # maps 1e-7 A to log scale (-15)
                       (2e-6 - c_val) / m_val,       # maps 2e-7 A to log scale (-13)
                       (3e-6 - c_val) / m_val,       # maps 3e-7 A to log scale (-11)
                       (4e-6 - c_val) / m_val,       # maps 4e-7 A to log scale (-9)
                       (5e-6 - c_val) / m_val,      # maps 5e-7 A to log scale (-7)
                       (6e-6 - c_val) / m_val        # maps 5e-7 A to log scale (-5)
                     )
  ) +
  coord_cartesian(ylim = c(primary_min_log, primary_max_log)) + # Set the visible range for the primary y-axis
  
  # Labels and Theme
  labs(x = expression(italic("V")[GS] ~ "[V]")) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size =20),
        axis.title.y = element_text(size =20),
        legend.position = c(5, 5),
        legend.text = element_text(size = 20),
        legend.box = "vertical",
        legend.margin = margin(unit(0.1, "cm")),
        axis.line.y.left = element_blank(), # Remove primary axis line
        axis.ticks.y.left = element_blank(), # Remove primary axis ticks
        axis.text.y.left = element_blank(), # Remove primary axis labels
        axis.title.y.left = element_blank(), # Remove primary axis title
        axis.line.y.right = element_blank(), # Remove secondary axis line
        axis.ticks.y.right = element_blank(), # Remove secondary axis ticks
        axis.text.y.right = element_blank(), # Remove secondary axis labels
        axis.title.y.right = element_blank() # Remove secondary axis title
  ) +
  # Annotations (unchanged)
  annotate("text", x=-0.5, y=-13, label=expression(italic("R")*"= 2 nm"), 
           hjust=0,vjust=0, size=7, family = "Times New Roman")+
  annotate("text", x=-0.5, y=-14, label=expression(italic("L")*"= 7 nm"), 
           hjust=0., vjust=0, size=7, family = "Times New Roman") #+
# annotate("text", x=-0.45, y=-13, label=expression(italic("V")[DS]*"= -0.6 V"), 
#          hjust=0., vjust=0, size=7, family = "Times New Roman")+
# annotate("text", x=-0.45, y=-14, label=expression(italic(phi)[GC]*"-"*italic(w)[FB]*"= 0.63 V"), 
#          hjust=0., vjust=0, size=7, family = "Times New Roman") +
# annotate("text", x=-0.45, y=-15, label=expression(italic(V)[bi]*"= -1.35 V"), 
#          hjust=0., vjust=0, size=7, family = "Times New Roman")
################## fig 3
df3_idsim <- data.frame(vgs = as.numeric(temp3_idvg[36:96,8]), 
                        simidvg = as.numeric(temp3_idvg[36:96,4]))
colnames(df3_idsim) <- c("vgs", "id_sim")
df3_idsim
temp_df3_idmdl <- read.table("/path/to/data/mathematica/anaisd_3_1.txt", header = FALSE)
df3_idmdl <- data.frame(vgs =as.numeric(temp_df3_idmdl[ ,1]), 
                        mdlidvg = as.numeric(temp_df3_idmdl[ ,2]))
colnames(df3_idmdl) <- c("vgs", "id_mdl")
df3_idmdl
df3_idsim$log_id_sim <- log10(df3_idsim$id_sim)
df3_idmdl$log_id_mdl <- log10(df3_idmdl$id_mdl)
df3_idsim$mapped_linear_sim <- (df3_idsim$id_sim - c_val) / m_val
df3_idmdl$mapped_linear_mdl <- (df3_idmdl$id_mdl - c_val) / m_val

base_plot_3 <- ggplot() +
  # Plot log-scale data
  geom_line(data = df3_idsim, aes(x = vgs, y = log_id_sim, color = "Log Sim", linetype = "Simulation"), size = 1.5) +
  geom_point(data = df3_idmdl, aes(x = vgs, y = log_id_mdl, color = "Log Model", shape = "Model"), size = 4) +
  
  # Plot linear-scale data, mapped to the primary log10 axis for correct alignment
  geom_line(data = df3_idsim, aes(x = vgs, y = mapped_linear_sim, color = "Linear Sim"), size = 1.5) +
  geom_point(data = df3_idmdl, aes(x = vgs, y = mapped_linear_mdl, color = "Linear Model", shape = "Model"), size = 4) +
  
  # Manual scales for colors, shapes, and linetypes
  scale_color_manual(name = "",
                     values = c("Log Sim" = "black", "Log Model" = "black",
                                "Linear Sim" = "red", "Linear Model" = "red")) +
  scale_shape_manual(name = "", values = c("Model" = 16)) +
  scale_linetype_manual(name = "", values = c("Simulation" = "solid")) +
  
  # Y-axis definitions
  scale_y_continuous(name = expression(log[10]~(italic("I")[SD])~"[A]"),
                     sec.axis = sec_axis_def_final, # Apply the carefully crafted secondary axis
                     # Explicit primary axis breaks for alignment.
                     # These should correspond to the secondary axis breaks via the inverse transformation.
                     breaks = c(
                       (0 - c_val) / m_val,        # maps 0 A to log scale (-17)
                       (1e-6 - c_val) / m_val,       # maps 1e-7 A to log scale (-15)
                       (2e-6 - c_val) / m_val,       # maps 2e-7 A to log scale (-13)
                       (3e-6 - c_val) / m_val,       # maps 3e-7 A to log scale (-11)
                       (4e-6 - c_val) / m_val,       # maps 4e-7 A to log scale (-9)
                       (5e-6 - c_val) / m_val,      # maps 5e-7 A to log scale (-7)
                       (6e-6 - c_val) / m_val        # maps 5e-7 A to log scale (-5)
                     )
  ) +
  coord_cartesian(ylim = c(primary_min_log, primary_max_log)) + # Set the visible range for the primary y-axis
  
  # Labels and Theme
  labs(x = expression(italic("V")[GS] ~ "[V]")) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size =20),
        axis.title.y = element_text(size =20),
        axis.title.y.left = element_blank(), # Remove primary (left) Y-axis title
        axis.ticks.y.left = element_blank(), # Remove primary (left) Y-axis ticks
        axis.text.y.left = element_blank(), # Remove primary (left) Y-axis labels
        axis.line.y.left = element_blank(), # Remove primary axis line
        #axis.line.y.right = element_blank(), # Remove secondary axis line
        #axis.ticks.y.right = element_blank(), # Remove secondary axis ticks
        #axis.text.y.right = element_blank(), # Remove secondary axis labels
        #axis.title.y.right = element_blank(), # Remove secondary axis title
        legend.position = c(5, 5),
        legend.text = element_text(size = 20),
        legend.box = "vertical",
        legend.margin = margin(unit(0.1, "cm"))
  ) +
  # Annotations (unchanged)
  annotate("text", x=-0.5, y=-13, label=expression(italic("R")*"= 2.5 nm"), 
           hjust=0,vjust=0, size=7, family = "Times New Roman")+
  annotate("text", x=-0.5, y=-14, label=expression(italic("L")*"= 7 nm"), 
           hjust=0., vjust=0, size=7, family = "Times New Roman") #+
# annotate("text", x=-0.35, y=-13, label=expression(italic("V")[DS]*"= -0.6 V"), 
#          hjust=0., vjust=0, size=7, family = "Times New Roman")+
# annotate("text", x=-0.35, y=-14, label=expression(italic(phi)[GC]*"-"*italic(w)[FB]*"= 0.63 V"), 
#          hjust=0., vjust=0, size=7, family = "Times New Roman") +
# annotate("text", x=-0.35, y=-15, label=expression(italic(V)[bi]*"= -1.35 V"), 
#          hjust=0., vjust=0, size=7, family = "Times New Roman")
################## fig 4
df4_idsim <- data.frame(vgs = as.numeric(temp4_idvg[36:96,8]), 
                        simidvg = as.numeric(temp4_idvg[36:96,4]))
colnames(df4_idsim) <- c("vgs", "id_sim")
df4_idsim
temp_df4_idmdl <- read.table("/path/to/data/mathematica/anaisd_1_2.txt", header = FALSE)
df4_idmdl <- data.frame(vgs =as.numeric(temp_df4_idmdl[ ,1]), mdlidvg = as.numeric(temp_df4_idmdl[ ,2]))
colnames(df4_idmdl) <- c("vgs", "id_mdl")
df4_idsim$log_id_sim <- log10(df4_idsim$id_sim)
df4_idmdl$log_id_mdl <- log10(df4_idmdl$id_mdl)
df4_idsim$mapped_linear_sim <- (df4_idsim$id_sim - c_val) / m_val
df4_idmdl$mapped_linear_mdl <- (df4_idmdl$id_mdl - c_val) / m_val

base_plot_4 <- ggplot() +
  # Plot log-scale data
  geom_line(data = df4_idsim, 
            aes(x = vgs, y = log_id_sim, 
                color = "Log Sim", linetype = "Simulation"), 
            size = 1.5) +
  geom_point(data = df4_idmdl, 
             aes(x = vgs, y = log_id_mdl, color = "Log Model", shape = "Model"), 
             size = 4) +
  
  # Plot linear-scale data, mapped to the primary log10 axis for correct alignment
  geom_line(data = df4_idsim, 
            aes(x = vgs, y = mapped_linear_sim, color = "Linear Sim"), 
            size = 1.5) +
  geom_point(data = df4_idmdl, 
             aes(x = vgs, y = mapped_linear_mdl, 
                 color = "Linear Model", shape = "Model"), 
             size = 4) +
  
  # Manual scales for colors, shapes, and linetypes
  scale_color_manual(name = "",
                     values = c("Log Sim" = "black", "Log Model" = "black",
                                "Linear Sim" = "red", "Linear Model" = "red")) +
  scale_shape_manual(name = "", values = c("Model" = 16)) +
  scale_linetype_manual(name = "", values = c("Simulation" = "solid")) +
  
  # Y-axis definitions
  scale_y_continuous(name = expression(log[10]~(italic("I")[SD])~"[A]"),
                     sec.axis = sec_axis_def_final, # Apply the carefully crafted secondary axis
                     # Explicit primary axis breaks for alignment.
                     # These should correspond to the secondary axis breaks via the inverse transformation.
                     breaks = c(
                       (0 - c_val) / m_val,        # maps 0 A to log scale (-17)
                       (1e-6 - c_val) / m_val,       # maps 1e-7 A to log scale (-15)
                       (2e-6 - c_val) / m_val,       # maps 2e-7 A to log scale (-13)
                       (3e-6 - c_val) / m_val,       # maps 3e-7 A to log scale (-11)
                       (4e-6 - c_val) / m_val,       # maps 4e-7 A to log scale (-9)
                       (5e-6 - c_val) / m_val,      # maps 5e-7 A to log scale (-7)
                       (6e-6 - c_val) / m_val        # maps 5e-7 A to log scale (-5)
                     )
  ) +
  coord_cartesian(ylim = c(primary_min_log, primary_max_log)) + # Set the visible range for the primary y-axis
  
  # Labels and Theme
  labs(x = expression(italic("V")[GS] ~ "[V]")) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size =20),
        axis.title.y = element_text(size =20),
        legend.position = c(5, 5),
        legend.text = element_text(size = 20),
        legend.box = "vertical",
        legend.margin = margin(unit(0.1, "cm")),
        #axis.line.y.right = element_blank(), # Remove secondary axis line
        #axis.ticks.y.right = element_blank(), # Remove secondary axis ticks
        #axis.text.y.right = element_blank(), # Remove secondary axis labels
        #axis.title.y.right = element_blank() # Remove secondary axis title
        #axis.line.y.left = element_blank(), # Remove primary axis line
        #axis.ticks.y.left = element_blank(), # Remove primary axis ticks
        #axis.text.y.left = element_blank(), # Remove primary axis labels
        #axis.title.y.left = element_blank(), # Remove primary axis title
        axis.line.y.right = element_blank(), # Remove secondary axis line
        axis.ticks.y.right = element_blank(), # Remove secondary axis ticks
        axis.text.y.right = element_blank(), # Remove secondary axis labels
        axis.title.y.right = element_blank() # Remove secondary axis title
  ) +
  # Annotations (unchanged)
  annotate("text", x=-0.5, y=-13, label=expression(italic("R")*"= 1.5 nm"), 
           hjust=0,vjust=0, size=7, family = "Times New Roman")+
  annotate("text", x=-0.5, y=-14, label=expression(italic("L")*"= 9 nm"), 
           hjust=0., vjust=0, size=7, family = "Times New Roman") #+
# annotate("text", x=-0.55, y=-13, label=expression(italic("V")[DS]*"= -0.6 V"), 
#          hjust=0., vjust=0, size=7, family = "Times New Roman")+
# annotate("text", x=-0.55, y=-14, label=expression(italic(phi)[GC]*"-"*italic(w)[FB]*"= 0.63 V"), 
#          hjust=0., vjust=0, size=7, family = "Times New Roman") +
# annotate("text", x=-0.55, y=-15, label=expression(italic(V)[bi]*"= -1.35 V"), 
#          hjust=0., vjust=0, size=7, family = "Times New Roman")
################## fig 5
df5_idsim <- data.frame(vgs = as.numeric(temp5_idvg[36:96,8]), 
                        simidvg = as.numeric(temp5_idvg[36:96,4]))
colnames(df5_idsim) <- c("vgs", "id_sim")
temp_df5_idmdl <- read.table("/path/to/data/mathematica/anaisd_2_2.txt", header = FALSE)
df5_idmdl <- data.frame(vgs =as.numeric(temp_df5_idmdl[ ,1]), mdlidvg = as.numeric(temp_df5_idmdl[ ,2]))
colnames(df5_idmdl) <- c("vgs", "id_mdl")
df5_idsim$log_id_sim <- log10(df5_idsim$id_sim)
df5_idmdl$log_id_mdl <- log10(df5_idmdl$id_mdl)
df5_idsim$mapped_linear_sim <- (df5_idsim$id_sim - c_val) / m_val
df5_idmdl$mapped_linear_mdl <- (df5_idmdl$id_mdl - c_val) / m_val

base_plot_5 <- ggplot() +
  # Plot log-scale data
  geom_line(data = df5_idsim, 
            aes(x = vgs, y = log_id_sim, color = "Log Sim", linetype = "Simulation"), 
            size = 1.5) +
  geom_point(data = df5_idmdl, 
             aes(x = vgs, y = log_id_mdl, color = "Log Model", shape = "Model"), 
             size = 4) +
  
  # Plot linear-scale data, mapped to the primary log10 axis for correct alignment
  geom_line(data = df5_idsim, 
            aes(x = vgs, y = mapped_linear_sim, color = "Linear Sim"), 
            size = 1.5) +
  geom_point(data = df5_idmdl, 
             aes(x = vgs, y = mapped_linear_mdl, color = "Linear Model", shape = "Model"), 
             size = 4) +
  
  # Manual scales for colors, shapes, and linetypes
  scale_color_manual(name = "",
                     values = c("Log Sim" = "black", "Log Model" = "black",
                                "Linear Sim" = "red", "Linear Model" = "red")) +
  scale_shape_manual(name = "", values = c("Model" = 16)) +
  scale_linetype_manual(name = "", values = c("Simulation" = "solid")) +
  
  # Y-axis definitions
  scale_y_continuous(name = expression(log[10]~(italic("I")[SD])~"[A]"),
                     sec.axis = sec_axis_def_final, # Apply the carefully crafted secondary axis
                     # Explicit primary axis breaks for alignment.
                     # These should correspond to the secondary axis breaks via the inverse transformation.
                     breaks = c(
                       (0 - c_val) / m_val,        # maps 0 A to log scale (-17)
                       (1e-6 - c_val) / m_val,       # maps 1e-7 A to log scale (-15)
                       (2e-6 - c_val) / m_val,       # maps 2e-7 A to log scale (-13)
                       (3e-6 - c_val) / m_val,       # maps 3e-7 A to log scale (-11)
                       (4e-6 - c_val) / m_val,       # maps 4e-7 A to log scale (-9)
                       (5e-6 - c_val) / m_val,      # maps 5e-7 A to log scale (-7)
                       (6e-6 - c_val) / m_val        # maps 5e-7 A to log scale (-5)
                     )
  ) +
  coord_cartesian(ylim = c(primary_min_log, primary_max_log)) + # Set the visible range for the primary y-axis
  
  # Labels and Theme
  labs(x = expression(italic("V")[GS] ~ "[V]")) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size =20),
        axis.title.y = element_text(size =20),
        legend.position = c(5, 5),
        legend.text = element_text(size = 20),
        legend.box = "vertical",
        legend.margin = margin(unit(0.1, "cm")),
        axis.line.y.left = element_blank(), # Remove primary axis line
        axis.ticks.y.left = element_blank(), # Remove primary axis ticks
        axis.text.y.left = element_blank(), # Remove primary axis labels
        axis.title.y.left = element_blank(), # Remove primary axis title
        axis.line.y.right = element_blank(), # Remove secondary axis line
        axis.ticks.y.right = element_blank(), # Remove secondary axis ticks
        axis.text.y.right = element_blank(), # Remove secondary axis labels
        axis.title.y.right = element_blank() # Remove secondary axis title
  ) +
  # Annotations (unchanged)
  annotate("text", x=-0.5, y=-13, label=expression(italic("R")*"= 2 nm"), 
           hjust=0,vjust=0, size=7, family = "Times New Roman")+
  annotate("text", x=-0.5, y=-14, label=expression(italic("L")*"= 9 nm"), 
           hjust=0., vjust=0, size=7, family = "Times New Roman") #+
# annotate("text", x=-0.45, y=-13, label=expression(italic("V")[DS]*"= -0.6 V"), 
#          hjust=0., vjust=0, size=7, family = "Times New Roman")+
# annotate("text", x=-0.45, y=-14, label=expression(italic(phi)[GC]*"-"*italic(w)[FB]*"= 0.63 V"), 
#          hjust=0., vjust=0, size=7, family = "Times New Roman") +
# annotate("text", x=-0.45, y=-15, label=expression(italic(V)[bi]*"= -1.35 V"), 
#          hjust=0., vjust=0, size=7, family = "Times New Roman")
################## fig 6
df6_idsim <- data.frame(vgs = as.numeric(temp6_idvg[36:96,8]),
                        simidvg = as.numeric(temp6_idvg[36:96,4]))
colnames(df6_idsim) <- c("vgs", "id_sim")
temp_df6_idmdl <- read.table("/path/to/data/mathematica/anaisd_3_2.txt", header = FALSE)
df6_idmdl <- data.frame(vgs =as.numeric(temp_df6_idmdl[ ,1]), mdlidvg = as.numeric(temp_df6_idmdl[ ,2]))
colnames(df6_idmdl) <- c("vgs", "id_mdl")
df6_idsim$log_id_sim <- log10(df6_idsim$id_sim)
df6_idmdl$log_id_mdl <- log10(df6_idmdl$id_mdl)
df6_idsim$mapped_linear_sim <- (df6_idsim$id_sim - c_val) / m_val
df6_idmdl$mapped_linear_mdl <- (df6_idmdl$id_mdl - c_val) / m_val

base_plot_6 <- ggplot() +
  # Plot log-scale data
  geom_line(data = df6_idsim, 
            aes(x = vgs, y = log_id_sim, color = "Log Sim", linetype = "Simulation"), 
            size = 1.5) +
  geom_point(data = df6_idmdl, 
             aes(x = vgs, y = log_id_mdl, color = "Log Model", shape = "Model"), 
             size = 4) +
  
  # Plot linear-scale data, mapped to the primary log10 axis for correct alignment
  geom_line(data = df6_idsim, 
            aes(x = vgs, y = mapped_linear_sim, color = "Linear Sim"), 
            size = 1.5) +
  geom_point(data = df6_idmdl, 
             aes(x = vgs, y = mapped_linear_mdl, color = "Linear Model", shape = "Model"), 
             size = 4) +
  
  # Manual scales for colors, shapes, and linetypes
  scale_color_manual(name = "",
                     values = c("Log Sim" = "black", "Log Model" = "black",
                                "Linear Sim" = "red", "Linear Model" = "red")) +
  scale_shape_manual(name = "", values = c("Model" = 16)) +
  scale_linetype_manual(name = "", values = c("Simulation" = "solid")) +
  
  # Y-axis definitions
  scale_y_continuous(name = expression(log[10]~(italic("I")[SD])~"[A]"),
                     sec.axis = sec_axis_def_final, # Apply the carefully crafted secondary axis
                     # Explicit primary axis breaks for alignment.
                     # These should correspond to the secondary axis breaks via the inverse transformation.
                     breaks = c(
                       (0 - c_val) / m_val,        # maps 0 A to log scale (-17)
                       (1e-6 - c_val) / m_val,       # maps 1e-7 A to log scale (-15)
                       (2e-6 - c_val) / m_val,       # maps 2e-7 A to log scale (-13)
                       (3e-6 - c_val) / m_val,       # maps 3e-7 A to log scale (-11)
                       (4e-6 - c_val) / m_val,       # maps 4e-7 A to log scale (-9)
                       (5e-6 - c_val) / m_val,      # maps 5e-7 A to log scale (-7)
                       (6e-6 - c_val) / m_val        # maps 5e-7 A to log scale (-5)
                     )
  ) +
  coord_cartesian(ylim = c(primary_min_log, primary_max_log)) + # Set the visible range for the primary y-axis
  
  # Labels and Theme
  labs(x = expression(italic("V")[GS] ~ "[V]")) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size =20),
        axis.title.y = element_text(size =20),
        axis.title.y.left = element_blank(), # Remove primary (left) Y-axis title
        axis.ticks.y.left = element_blank(), # Remove primary (left) Y-axis ticks
        axis.text.y.left = element_blank(),# Remove primary (left) Y-axis labels
        #axis.line.y.left = element_blank(), # Remove primary axis line
        #axis.ticks.y.left = element_blank(), # Remove primary axis ticks
        #axis.text.y.left = element_blank(), # Remove primary axis labels
        #axis.title.y.left = element_blank(), # Remove primary axis title
        #axis.line.y.right = element_blank(), # Remove secondary axis line
        #axis.ticks.y.right = element_blank(), # Remove secondary axis ticks
        #axis.text.y.right = element_blank(), # Remove secondary axis labels
        #axis.title.y.right = element_blank(), # Remove secondary axis title
        legend.position = c(5, 5),
        legend.text = element_text(size = 20),
        legend.box = "vertical",
        legend.margin = margin(unit(0.1, "cm"))
  ) +
  # Annotations (unchanged)
  annotate("text", x=-0.5, y=-13, label=expression(italic("R")*"= 2.5 nm"), 
           hjust=0, vjust=0, size=7, family = "Times New Roman")+
  annotate("text", x=-0.5, y=-14, label=expression(italic("L")*"= 9 nm"), 
           hjust=0., vjust=0, size=7, family = "Times New Roman") #+
# annotate("text", x=-0.4, y=-13, label=expression(italic("V")[DS]*"= -0.6 V"), 
#          hjust=0., vjust=0, size=7, family = "Times New Roman")+
# annotate("text", x=-0.4, y=-14, label=expression(italic(phi)[GC]*"-"*italic(w)[FB]*"= 0.63 V"), 
#          hjust=0., vjust=0, size=7, family = "Times New Roman") +
# annotate("text", x=-0.4, y=-15, label=expression(italic(V)[bi]*"= -1.35 V"), 
#          hjust=0., vjust=0, size=7, family = "Times New Roman")

p_1a <- base_plot_1 +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  annotate("text",
           x = -0.6,
           y = -5,
           label = "(a)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_1b <- base_plot_4 +
  theme(#axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    #axis.ticks.y = element_blank(),
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank()
    ) +
  annotate("text",
           x = -0.6,
           y = -5,
           label = "(d)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_2a <- base_plot_2 +
  theme(axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()) +
  annotate("text",
           x = -0.6,
           y = -5,
           label = "(b)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_2b <- base_plot_5 +
  theme(axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    #axis.title.x = element_blank(),
    #axis.text.x = element_blank(),
    #axis.ticks.x = element_blank()
    ) +
  annotate("text",
           x = -0.6,
           y = -5,
           label = "(e)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_3a <- base_plot_3 +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  annotate("text",
           x = -0.6,
           y = -5,
           label = "(c)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_3b <- base_plot_6 +
  theme(#axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    #axis.ticks.y = element_blank()
  ) +
  annotate("text",
           x = -0.6,
           y = -5,
           label = "(f)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

final_plot <- (p_1a | p_2a | p_3a )  
  (p_1b | p_2b | p_3b)
print(final_plot)
ggsave("/path/to/data/fig/combined_idvg_comparison_yoko.png",
       final_plot, width = 16, height = 10, dpi = 300)

######################### IDVD ##############################################


setwd("/path/to/data/silvaco/")

# --- Êý¾Ý¶ÁÈ¡ ---
temp1_idvd050 <- read.csv("./IdVd_Nanowire_r1.5_l7050.log",
                          header=FALSE, sep=" ")
temp1_idvd055 <- read.csv("./IdVd_Nanowire_r1.5_l7055.log",
                          header=FALSE, sep=" ")
temp1_idvd060 <- read.csv("./IdVd_Nanowire_r1.5_l7060.log",
                          header=FALSE, sep=" ")
temp4_idvd050 <- read.csv("./IdVd_Nanowire_r1.5_l9050.log",
                          header=FALSE, sep=" ")
temp4_idvd055 <- read.csv("./IdVd_Nanowire_r1.5_l9055.log",
                          header=FALSE, sep=" ")
temp4_idvd060 <- read.csv("./IdVd_Nanowire_r1.5_l9060.log",
                          header=FALSE, sep=" ")
temp2_idvd050 <- read.csv("./IdVd_Nanowire_r2_l7050.log",
                          header=FALSE, sep=" ")
temp2_idvd055 <- read.csv("./IdVd_Nanowire_r2_l7055.log",
                          header=FALSE, sep=" ")
temp2_idvd060 <- read.csv("./IdVd_Nanowire_r2_l7060.log",
                          header=FALSE, sep=" ")
temp3_idvd050 <- read.csv("./IdVd_Nanowire_r2.5_l7050.log",
                          header=FALSE, sep=" ")
temp3_idvd055 <- read.csv("./IdVd_Nanowire_r2.5_l7055.log",
                          header=FALSE, sep=" ")
temp3_idvd060 <- read.csv("./IdVd_Nanowire_r2.5_l7060.log",
                          header=FALSE, sep=" ")
temp5_idvd050 <- read.csv("./IdVd_Nanowire_r2_l9050.log",
                          header=FALSE, sep=" ")
temp5_idvd055 <- read.csv("./IdVd_Nanowire_r2_l9055.log",
                          header=FALSE, sep=" ")
temp5_idvd060 <- read.csv("./IdVd_Nanowire_r2_l9060.log",
                          header=FALSE, sep=" ")
temp6_idvd050 <- read.csv("./IdVd_Nanowire_r2.5_l9050.log",
                          header=FALSE, sep=" ")
temp6_idvd055 <- read.csv("./IdVd_Nanowire_r2.5_l9055.log",
                          header=FALSE, sep=" ")
temp6_idvd060 <- read.csv("./IdVd_Nanowire_r2.5_l9060.log",
                          header=FALSE, sep=" ")

############ fig 1 #############
df1_idsim <- data.frame(
  #  vds = seq(from = 0, to = -0.6, by = -0.01),
  vds = as.numeric(temp1_idvd050[36:96,5]),
  simvg50 = as.numeric(temp1_idvd050[36:96,4]),
  simvg55 = as.numeric(temp1_idvd055[36:96,4]),
  simvg60 = abs(as.numeric(temp1_idvd060[36:96,4])))
df1_idsim
#colnames(df1_idsim) <- c("vds", "mdlvg50", "mdlvg55", "mdlvg60")
temp_df1_idmdl <- data.frame(read.table("/path/to/data/mathematica/anaisd_1_1.txt"))
df1_idmdl <- data.frame(vds =as.numeric(temp_df1_idmdl[ ,1]),
                        mdlvg50 = as.numeric(temp_df1_idmdl[ ,3]),
                        mdlvg55 = as.numeric(temp_df1_idmdl[ ,4]),
                        mdlvg60 = as.numeric(temp_df1_idmdl[ ,5]))
#colnames(df1_idmdl) <- c("vds", "mdlvg50", "mdlvg55", "mdlvg60") # ÕâÐÐ¶ÔÓÚ mdlvg ÊÇÕýÈ·µÄ
df1_idmdl
base_plot_1 <- ggplot() +
  geom_line(data = df1_idsim,
            aes(x = vds,
                y = simvg50,
                color = "VGS = 0.50 V",
                linetype = "Simulation"),
            size = 1.5) +
  geom_line(data = df1_idsim,
            aes(x = vds,
                y = simvg55,
                color = "VGS = 0.55 V",
                linetype = "Simulation"),
            size = 1.5) +
  geom_line(data = df1_idsim,
            aes(x = vds,
                y = simvg60,
                color = "VGS = 0.60 V",
                linetype = "Simulation"),
            size = 1.5) +
  geom_point(data = df1_idmdl,
             aes(x = vds,
                 y = mdlvg50,
                 color = "VGS = 0.50 V",
                 shape = "Model"),
             size = 4) +
  geom_point(data = df1_idmdl,
             aes(x = vds,
                 y = mdlvg55,
                 color = "VGS = 0.55 V",
                 shape = "Model"),
             size = 4) +
  geom_point(data = df1_idmdl,
             aes(x = vds,
                 y = mdlvg60,
                 color = "VGS = 0.60 V",
                 shape = "Model"),
             size = 4) +
  scale_y_continuous(limits = c(0, 6e-6)) +
  scale_x_continuous(limits = c(-0.601, -0.)) +
  labs(
    x = expression(italic("V")[DS] ~ "[V]"),
    y = expression(italic("I")[SD] ~ "[A]")
  ) +
  scale_color_manual(name = " ",
                     values = c("VGS = 0.50 V" = "red",
                                "VGS = 0.55 V" = "green",
                                "VGS = 0.60 V" = "blue"),
                     labels = c(expression(italic("V")[GS]*" = -0.50 V"),
                                expression(italic("V")[GS]*" = -0.55 V"),
                                expression(italic("V")[GS]*" = -0.60 V"))) +
  scale_linetype_manual(name = " ",
                        values = c("Simulation" = "solid")) +
  scale_shape_manual(name = " ",
                     values = c("Model" = 18)) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    #legend.spacing = unit(0.01, "mm"),
    #legend.box.spacing = unit(0.01,"mm"),
    legend.position = c(3,3),
    legend.text = element_text(size = 16, hjust=0),
    legend.box = "vertical",
    #legend.margin = margin(unit(0.01, "cm")),
    legend.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit = "cm"),
    legend.key.size = unit(0.8, "cm"),
    legend.spacing.y = unit(0.01, "cm"),
    legend.box.spacing = unit(0.01,"cm"),
    legend.key.height = unit(0.3, "lines"),
    aspect.ratio = 1) +
  annotate("text",
           x=-0.6,
           y=5e-6,
           label=expression(italic("R")*"= 1.5 nm"),
           hjust=0,
           vjust=0,
           size=7,
           family = "Times New Roman") +
  annotate("text",
           x=-0.6,
           y=4.56e-6,
           label=expression(italic("L")*"= 7 nm"),
           hjust=0,
           vjust=0,
           size=7,
           family = "Times New Roman") +
  annotate("text",
           x=-0.6,
           y=4.e-6,
           label=expression(italic("V")[DS]*"= -0.6 V"),
           hjust=0,
           vjust=0,
           size=7,
           family = "Times New Roman") +
  annotate("text",
           x=-0.6,
           y=3.5e-6,
           label=expression(italic(phi)[GC]*"-"*italic(w)[FB]*"= 0.63 V"),
           hjust=0,
           vjust=0,
           size=7,
           family = "Times New Roman") +
  annotate("text",
           x=-0.6,
           y=3.e-6,
           label=expression(italic(V)[bi]*"= -1.35 V"),
           hjust=0,
           vjust=0,
           size=7,
           family = "Times New Roman")

############ fig 2 #############
df2_idsim <- data.frame(
  vds = as.numeric(temp2_idvd050[36:96,5]),
  simvg50 = as.numeric(temp2_idvd050[36:96,4]),
  simvg55 = as.numeric(temp2_idvd055[36:96,4]),
  simvg60 = as.numeric(temp2_idvd060[36:96,4]))

#colnames(df2_idsim) <- c("vds", "mdlvg50", "mdlvg55", "mdlvg60")

temp_df2_idmdl <- data.frame(read.table("/path/to/data/mathematica/anaisd_2_1.txt"))
df2_idmdl <- data.frame(vds =as.numeric(temp_df2_idmdl[ ,1]),
                        mdlvg50 = as.numeric(temp_df2_idmdl[ ,3]),
                        mdlvg55 = as.numeric(temp_df2_idmdl[ ,4]),
                        mdlvg60 = as.numeric(temp_df2_idmdl[ ,5]))
#colnames(df2_idmdl) <- c("vds", "mdlvg50", "mdlvg55", "mdlvg60")

base_plot_2 <- ggplot() +
  geom_line(data = df2_idsim,
            aes(x = vds,
                y = simvg50,
                color = "VGS = 0.50 V",
                linetype = "Simulation"),
            size = 1.5) +
  geom_line(data = df2_idsim,
            aes(x = vds,
                y = simvg55,
                color = "VGS = 0.55 V",
                linetype = "Simulation"),
            size = 1.5) +
  geom_line(data = df2_idsim,
            aes(x = vds,
                y = simvg60,
                color = "VGS = 0.60 V",
                linetype = "Simulation"),
            size = 1.5) +
  geom_point(data = df2_idmdl,
             aes(x = vds,
                 y = mdlvg50,
                 color = "VGS = 0.50 V",
                 shape = "Model"),
             size = 4) +
  geom_point(data = df2_idmdl,
             aes(x = vds,
                 y = mdlvg55,
                 color = "VGS = 0.55 V",
                 shape = "Model"),
             size = 4) +
  geom_point(data = df2_idmdl,
             aes(x = vds,
                 y = mdlvg60,
                 color = "VGS = 0.60 V",
                 shape = "Model"),
             size = 4) +
  scale_y_continuous(limits = c(0, 6e-6)) +
  scale_x_continuous(limits = c(-0.601, -0.)) +
  labs(
    x = expression(italic("V")[DS] ~ "[V]"),
    y = expression(italic("I")[SD] ~ "[A]")) +
  scale_color_manual(name = " ",
                     values = c("VGS = 0.50 V" = "red", 
                                "VGS = 0.55 V" = "green",
                                "VGS = 0.60 V" = "blue"),
                     labels = c(expression(italic("V")[GS]*" = -0.50 V"),
                                expression(italic("V")[GS]*" = -0.55 V"),
                                expression(italic("V")[GS]*" = -0.60 V"))) +
  scale_linetype_manual(name = " ",
                        values = c("Simulation" = "solid")) +
  scale_shape_manual(name = " ",
                     values = c("Model" = 16)) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(3,3),
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.spacing = unit(0.1, "mm"),
    legend.box.spacing = unit(0.1,"mm"),
    aspect.ratio = 1) +
  annotate("text",
           x=-0.6,
           y=5e-6,
           label=expression(italic("R")*"= 2 nm"),
           hjust=0,
           vjust=0,
           size=7,
           family = "Times New Roman") +
  annotate("text",
           x=-0.6,
           y=4.5e-6,
           label=expression(italic("L")*"= 7 nm"),
           hjust=0,
           vjust=0,
           size=7,
           family = "Times New Roman") 

############ fig 3 #############
df3_idsim <- data.frame(
  vds = as.numeric(temp3_idvd050[36:96,5]),
  simvg50 = as.numeric(temp3_idvd050[36:96,4]),
  simvg55 = as.numeric(temp3_idvd055[36:96,4]),
  simvg60 = abs(as.numeric(temp3_idvd060[36:96,4])))


temp_df3_idmdl <- data.frame(read.table("/path/to/data/mathematica/anaisd_3_1.txt"))
df3_idmdl <- data.frame(vds =as.numeric(temp_df3_idmdl[ ,1]),
                        mdlvg50 = as.numeric(temp_df3_idmdl[ ,3]),
                        mdlvg55 = as.numeric(temp_df3_idmdl[ ,4]),
                        mdlvg60 = as.numeric(temp_df3_idmdl[ ,5]))


base_plot_3 <- ggplot() +
  geom_line(data = df3_idsim,
            aes(x = vds,
                y = simvg50,
                color = "VGS = 0.50 V",
                linetype = "Simulation"),
            size = 1.5) +
  geom_line(data = df3_idsim,
            aes(x = vds,
                y = simvg55,
                color = "VGS = 0.55 V",
                linetype = "Simulation"),
            size = 1.5) +
  geom_line(data = df3_idsim,
            aes(x = vds,
                y = simvg60,
                color = "VGS = 0.60 V",
                linetype = "Simulation"),
            size = 1.5) +
  geom_point(data = df3_idmdl,
             aes(x = vds,
                 y = mdlvg50,
                 color = "VGS = 0.50 V",
                 shape = "Model"),
             size = 4) +
  geom_point(data = df3_idmdl,
             aes(x = vds,
                 y = mdlvg55,
                 color = "VGS = 0.55 V",
                 shape = "Model"),
             size = 4) +
  geom_point(data = df3_idmdl,
             aes(x = vds,
                 y = mdlvg60,
                 color = "VGS = 0.60 V",
                 shape = "Model"),
             size = 4) +
  scale_y_continuous(limits = c(0, 6e-6)) +
  scale_x_continuous(limits = c(-0.601, -0.)) +
  labs(
    x = expression(italic("V")[DS] ~ "[V]"),
    y = expression(italic("I")[SD] ~ "[A]")
  ) +
  scale_color_manual(name = " ",
                     values = c("VGS = 0.50 V" = "red",
                                "VGS = 0.55 V" = "green",
                                "VGS = 0.60 V" = "blue"),
                     labels = c(expression(italic("V")[GS]*" = -0.50 V"),
                                expression(italic("V")[GS]*" = -0.55 V"),
                                expression(italic("V")[GS]*" = -0.60 V"))) +
  scale_linetype_manual(name = " ",
                        values = c("Simulation" = "solid")) +
  scale_shape_manual(name = " ",
                     values = c("Model" = 16)) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(5,5),
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.spacing = unit(0.01, "cm"),
    legend.box.spacing = unit(0.05,"cm"),
    aspect.ratio = 1) +
  annotate("text",
           x=-0.6,
           y=5e-6,
           label=expression(italic("R")*"= 2.5 nm"),
           hjust=0,
           vjust=0,
           size=7,
           family = "Times New Roman") +
  annotate("text",
           x=-0.6,
           y=4.5e-6,
           label=expression(italic("L")*"= 7 nm"),
           hjust=0,
           vjust=0,
           size=7,
           family = "Times New Roman") 

############ fig 4 #############
df4_idsim <- data.frame(
  vds = as.numeric(temp4_idvd050[36:96,5]),
  simvg50 = as.numeric(temp4_idvd050[36:96,4]),
  simvg55 = as.numeric(temp4_idvd055[36:96,4]),
  simvg60 = abs(as.numeric(temp4_idvd060[36:96,4])))


temp_df4_idmdl <- data.frame(read.table("/path/to/data/mathematica/anaisd_1_2.txt"))
df4_idmdl <- data.frame(vds =as.numeric(temp_df4_idmdl[ ,1]),
                        mdlvg50 = as.numeric(temp_df4_idmdl[ ,3]),
                        mdlvg55 = as.numeric(temp_df4_idmdl[ ,4]),
                        mdlvg60 = as.numeric(temp_df4_idmdl[ ,5]))
colnames(df4_idmdl) <- c("vds", "mdlvg50", "mdlvg55", "mdlvg60")

base_plot_4 <- ggplot() +
  geom_line(data = df4_idsim,
            aes(x = vds,
                y = simvg50,
                color = "VGS = 0.50 V",
                linetype = "Simulation"),
            size = 1.5) +
  geom_line(data = df4_idsim,
            aes(x = vds,
                y = simvg55,
                color = "VGS = 0.55 V",
                linetype = "Simulation"),
            size = 1.5) +
  geom_line(data = df4_idsim,
            aes(x = vds,
                y = simvg60,
                color = "VGS = 0.60 V",
                linetype = "Simulation"),
            size = 1.5) +
  geom_point(data = df4_idmdl,
             aes(x = vds,
                 y = mdlvg50,
                 color = "VGS = 0.50 V",
                 shape = "Model"),
             size = 4) +
  geom_point(data = df4_idmdl,
             aes(x = vds,
                 y = mdlvg55,
                 color = "VGS = 0.55 V",
                 color = "VGS = 0.55 V", 
                 shape = "Model"),
             size = 4) +
  geom_point(data = df4_idmdl,
             aes(x = vds,
                 y = mdlvg60,
                 color = "VGS = 0.60 V",
                 shape = "Model"),
             size = 4) +
  scale_y_continuous(limits = c(0, 6e-6)) +
  scale_x_continuous(limits = c(-0.601, -0.)) +
  labs(
    x = expression(italic("V")[DS] ~ "[V]"),
    y = expression(italic("I")[SD] ~ "[A]")) +
  scale_color_manual(name = " ",
                     values = c("VGS = 0.50 V" = "red",
                                "VGS = 0.55 V" = "green",
                                "VGS = 0.60 V" = "blue"),
                     labels = c(expression(italic("V")[GS]*" = -0.50 V"),
                                expression(italic("V")[GS]*" = -0.55 V"),
                                expression(italic("V")[GS]*" = -0.60 V"))) +
  scale_linetype_manual(name = " ",
                        values = c("Simulation" = "solid")) +
  scale_shape_manual(name = " ",
                     values = c("Model" = 16)) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(0.7,0.7),
    legend.text = element_text(size = 16, hjust=0),
    legend.box = "vertical",
    legend.margin = margin(unit(0.01, "cm")),
    legend.spacing.y = unit(0.01, "cm"),
    legend.box.spacing = unit(0.01,"cm"),
    aspect.ratio = 1) +
  annotate("text",
           x=-0.6,
           y=5e-6,
           label=expression(italic("R")*"= 1.5 nm"),
           hjust=0,
           vjust=0,
           size=7,
           family = "Times New Roman") +
  annotate("text",
           x=-0.6,
           y=4.5e-6,
           label=expression(italic("L")*"= 9 nm"),
           hjust=0,
           vjust=0,
           size=7,
           family = "Times New Roman") 

############ fig 5 #############
df5_idsim <- data.frame(
  vds = as.numeric(temp5_idvd050[36:96,5]),
  simvg50 = as.numeric(temp5_idvd050[36:96,4]),
  simvg55 = as.numeric(temp5_idvd055[36:96,4]),
  simvg60 = as.numeric(temp5_idvd060[36:96,4]))


temp_df5_idmdl <- data.frame(read.table("/path/to/data/mathematica/anaisd_2_2.txt"))
df5_idmdl <- data.frame(vds =as.numeric(temp_df5_idmdl[ ,1]),
                        mdlvg50 = as.numeric(temp_df5_idmdl[ ,3]),
                        mdlvg55 = as.numeric(temp_df5_idmdl[ ,4]),
                        mdlvg60 = as.numeric(temp_df5_idmdl[ ,5]))
colnames(df5_idmdl) <- c("vds", "mdlvg50", "mdlvg55", "mdlvg60")
df5_idsim
base_plot_5 <- ggplot() +
  geom_line(data = df5_idsim,
            aes(x = vds,
                y = simvg50,
                color = "VGS = 0.50 V",
                linetype = "Simulation"),
            size = 1.5) +
  geom_line(data = df5_idsim,
            aes(x = vds,
                y = simvg55,
                color = "VGS = 0.55 V",
                linetype = "Simulation"),
            size = 1.5) +
  geom_line(data = df5_idsim,
            aes(x = vds,
                y = simvg60,
                color = "VGS = 0.60 V",
                linetype = "Simulation"),
            size = 1.5) +
  geom_point(data = df5_idmdl,
             aes(x = vds,
                 y = mdlvg50,
                 color = "VGS = 0.50 V",
                 shape = "Model"),
             size = 4) +
  geom_point(data = df5_idmdl,
             aes(x = vds,
                 y = mdlvg55,
                 color = "VGS = 0.55 V",
                 shape = "Model"),
             size = 4) +
  geom_point(data = df5_idmdl,
             aes(x = vds,
                 y = mdlvg60,
                 color = "VGS = 0.60 V",
                 shape = "Model"),
             size = 4) +
  scale_y_continuous(limits = c(0, 6e-6)) +
  scale_x_continuous(limits = c(-0.601, -0.)) +
  labs(
    x = expression(italic("V")[DS] ~ "[V]"),
    y = expression(italic("I")[SD] ~ "[A]")) +
  scale_color_manual(name = " ",
                     values = c("VGS = 0.50 V" = "red",
                                "VGS = 0.55 V" = "green",
                                "VGS = 0.60 V" = "blue"),
                     labels = c(expression(italic("V")[GS]*" = -0.50 V"),
                                expression(italic("V")[GS]*" = -0.55 V"),
                                expression(italic("V")[GS]*" = -0.60 V"))) +
  scale_linetype_manual(name = " ",
                        values = c("Simulation" = "solid")) +
  scale_shape_manual(name = " ",
                     values = c("Model" = 16)) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(4,4),
    legend.text = element_text(size = 18),
    legend.box = "vertical",
    legend.spacing = unit(0.01, "cm"),
    legend.box.spacing = unit(0.05,"cm"),
    aspect.ratio = 1) +
  annotate("text",
           x=-0.6,
           y=5e-6,
           label=expression(italic("R")*"= 2 nm"),
           hjust=0,
           vjust=0,
           size=7,
           family = "Times New Roman") +
  annotate("text",
           x=-0.6,
           y=4.5e-6,
           label=expression(italic("L")*"= 9 nm"),
           hjust=0,
           vjust=0,
           size=7,
           family = "Times New Roman") 

df6_idsim <- data.frame(
  vds = as.numeric(temp6_idvd050[36:96,5]),
  simvg50 = as.numeric(temp6_idvd050[36:96,4]),
  simvg55 = as.numeric(temp6_idvd055[36:96,4]),
  simvg60 = as.numeric(temp6_idvd060[36:96,4]))


temp_df6_idmdl <- data.frame(read.table("/path/to/data/mathematica/anaisd_3_2.txt"))
df6_idmdl <- data.frame(vds =as.numeric(temp_df6_idmdl[ ,1]),
                        mdlvg50 = as.numeric(temp_df6_idmdl[ ,3]),
                        mdlvg55 = as.numeric(temp_df6_idmdl[ ,4]),
                        mdlvg60 = as.numeric(temp_df6_idmdl[ ,5]))
colnames(df6_idmdl) <- c("vds", "mdlvg50", "mdlvg55", "mdlvg60")
df6_idmdl
base_plot_6 <- ggplot() +
  geom_line(data = df6_idsim,
            aes(x = vds,
                y = simvg50,
                color = "VGS = 0.50 V",
                linetype = "Simulation"),
            size = 1.5) +
  geom_line(data = df6_idsim,
            aes(x = vds,
                y = simvg55,
                color = "VGS = 0.55 V",
                linetype = "Simulation"),
            size = 1.5) +
  geom_line(data = df6_idsim,
            aes(x = vds,
                y = simvg60,
                color = "VGS = 0.60 V",
                linetype = "Simulation"),
            size = 1.5) +
  geom_point(data = df6_idmdl,
             aes(x = vds,
                 y = mdlvg50,
                 color = "VGS = 0.50 V",
                 shape = "Model"),
             size = 4) +
  geom_point(data = df6_idmdl,
             aes(x = vds,
                 y = mdlvg55,
                 color = "VGS = 0.55 V",
                 shape = "Model"),
             size = 4) +
  geom_point(data = df6_idmdl,
             aes(x = vds,
                 y = mdlvg60,
                 color = "VGS = 0.60 V",
                 shape = "Model"),
             size = 4) +
  scale_y_continuous(limits = c(0, 6e-6)) +
  scale_x_continuous(limits = c(-0.601, -0.)) +
  labs(
    x = expression(italic("V")[DS] ~ "[V]"),
    y = expression(italic("I")[SD] ~ "[A]")) +
  scale_color_manual(name = " ",
                     values = c("VGS = 0.50 V" = "red",
                                "VGS = 0.55 V" = "green",
                                "VGS = 0.60 V" = "blue"),
                     labels = c(expression(italic("V")[GS]*" = -0.50 V"),
                                expression(italic("V")[GS]*" = -0.55 V"),
                                expression(italic("V")[GS]*" = -0.60 V"))) +
  scale_linetype_manual(name = " ",
                        values = c("Simulation" = "solid")) +
  scale_shape_manual(name = " ",
                     values = c("Model" = 16)) +
  theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.position = c(3.,3.),
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.spacing = unit(0.01, "cm"),
    legend.box.spacing = unit(0.05,"cm"),
    aspect.ratio = 1) +
  annotate("text",
           x=-0.6,
           y=5e-6,
           label=expression(italic("R")*"= 2.5 nm"),
           hjust=0,
           vjust=0,
           size=7,
           family = "Times New Roman") +
  annotate("text",
           x=-0.6,
           y=4.5e-6,
           label=expression(italic("L")*"= 9 nm"),
           hjust=0,
           vjust=0,
           size=7,
           family = "Times New Roman") 

############ plot #############################################
p_1a <- base_plot_1 +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  annotate("text",
           x = -0.6,
           y = 6e-6,
           label = "(a)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_2a <- base_plot_2 +
  theme(axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
    ) +
  annotate("text",
           x = -0.6,
           y = 6e-6,
           label = "(b)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_3a <- base_plot_3 +
  theme(axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  annotate("text",
           x = -0.6,
           y = 6e-6,
           label = "(c)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_1b <- base_plot_4 +
  theme(#axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank()
    ) +
  annotate("text",
           x = -0.6,
           y = 6e-6,
           label = "(d)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_2b <- base_plot_5 +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        #axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank()
        ) +
  annotate("text",
           x = -0.6,
           y = 6e-6,
           label = "(e)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

p_3b <- base_plot_6 +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  annotate("text",
           x = -0.6,
           y = 6e-6,
           label = "(f)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")

final_plot <- (p_1a | p_2a | p_3a)/  
  (p_1b | p_2b | p_3b )
print(final_plot)

ggsave("/path/to/data/fig/combined_idvd_comparison_yoko.png",
       final_plot, width = 16, height = 10, dpi = 300)

########################### current error ##################################
setwd("/path/to/data/silvaco/") # Set this to your actual directory


calc_error <- function(sim, mdl, xidx_sim, yidx_sim, xidx_mdl, yidx_mdl) {
  df_sim <- data.frame(x = round(as.numeric(sim[36:96, xidx_sim]),4),
                       y_sim = as.numeric(sim[36:96, yidx_sim]))
  df_mdl <- data.frame(x = round(as.numeric(mdl[, xidx_mdl]),4),
                       y_mdl = as.numeric(mdl[, yidx_mdl]))
  df_merged <- merge(df_sim, df_mdl, by = "x", all = FALSE) # Use all=FALSE for inner join behavior
  eps <- 1e-18
  df_merged$rel_err <- abs((df_merged$y_sim - df_merged$y_mdl) / (df_merged$y_sim + eps)) * 100
  return(df_merged)
}


group_info <- list(
  list( # Group 1: R=1.5nm, L=7nm
    idvg_sim = "IdVg_Nanowire_r1.5_l7.log",
    idvg_mdl = "/path/to/data/mathematica/anaisd_1_1.txt",
    idvd_sim = c("IdVd_Nanowire_r1.5_l7050.log",
                 "IdVd_Nanowire_r1.5_l7055.log",
                 "IdVd_Nanowire_r1.5_l7060.log"),
    idvd_mdl = "/path/to/data/mathematica/anaisd_1_1.txt",
    letter = "(a)",
    r_val = 1.5,
    l_val = 7
  ),
  list( # Group 2: R=2nm, L=7nm
    idvg_sim = "IdVg_Nanowire_r2_l7.log",
    idvg_mdl = "/path/to/data/mathematica/anaisd_2_1.txt",
    idvd_sim = c("IdVd_Nanowire_r2_l7050.log",
                 "IdVd_Nanowire_r2_l7055.log",
                 "IdVd_Nanowire_r2_l7060.log"),
    idvd_mdl = "/path/to/data/mathematica/anaisd_2_1.txt",
    letter = "(c)",
    r_val = 2,
    l_val = 7
  ),
  list( # Group 3: R=2.5nm, L=7nm
    idvg_sim = "IdVg_Nanowire_r2.5_l7.log",
    idvg_mdl = "/path/to/data/mathematica/anaisd_3_1.txt",
    idvd_sim = c("IdVd_Nanowire_r2.5_l7050.log",
                 "IdVd_Nanowire_r2.5_l7055.log",
                 "IdVd_Nanowire_r2.5_l7060.log"),
    idvd_mdl = "/path/to/data/mathematica/anaisd_3_1.txt",
    letter = "(e)",
    r_val = 2.5,
    l_val = 7
  ),
  list( # Group 4: R=1.5nm, L=10nm
    idvg_sim = "IdVg_Nanowire_r1.5_l9.log",
    idvg_mdl = "/path/to/data/mathematica/anaisd_1_2.txt",
    idvd_sim = c("IdVd_Nanowire_r1.5_l9050.log",
                 "IdVd_Nanowire_r1.5_l9055.log",
                 "IdVd_Nanowire_r1.5_l9060.log"),
    idvd_mdl = "/path/to/data/mathematica/anaisd_1_2.txt",
    letter = "(b)",
    r_val = 1.5,
    l_val = 9
  ),
  list( # Group 5: R=2nm, L=10nm
    idvg_sim = "IdVg_Nanowire_r2_l9.log",
    idvg_mdl = "/path/to/data/mathematica/anaisd_2_2.txt",
    idvd_sim = c("IdVd_Nanowire_r2_l9050.log",
                 "IdVd_Nanowire_r2_l9055.log",
                 "IdVd_Nanowire_r2_l9060.log"),
    idvd_mdl = "/path/to/data/mathematica/anaisd_2_2.txt",
    letter = "(d)",
    r_val = 2,
    l_val = 9
  ),
  list( # Group 6: R=2.5nm, L=10nm
    idvg_sim = "IdVg_Nanowire_r2.5_l9.log",
    idvg_mdl = "/path/to/data/mathematica/anaisd_3_2.txt",
    idvd_sim = c("IdVd_Nanowire_r2.5_l9050.log",
                 "IdVd_Nanowire_r2.5_l9055.log",
                 "IdVd_Nanowire_r2.5_l9060.log"),
    idvd_mdl = "/path/to/data/mathematica/anaisd_3_2.txt",
    letter = "(f)",
    r_val = 2.5,
    l_val = 9
  )
)


curve_labels <- c(
  "IdVg" = expression(italic("I")[SD]~"- "~italic("V")[GS]),
  "IdVd@VGS=0.50V" = expression(italic("I")[SD]~"- "~italic("V")[DS]~" ("*italic("V")[GS]*" = -0.50 V)"),
  "IdVd@VGS=0.55V" = expression(italic("I")[SD]~"- "~italic("V")[DS]~" ("*italic("V")[GS]*" = -0.55 V)"),
  "IdVd@VGS=0.60V" = expression(italic("I")[SD]~"- "~italic("V")[DS]~" ("*italic("V")[GS]*" = -0.60 V)")
)


vgs_colors <- c("IdVg"="black", "IdVd@VGS=0.50V"="red", "IdVd@VGS=0.55V"="green", "IdVd@VGS=0.60V"="blue")
vgs_shapes <- c("IdVg"=16, "IdVd@VGS=0.50V"=17, "IdVd@VGS=0.55V"=15, "IdVd@VGS=0.60V"=18)
vgs_linetypes <- c("IdVg"="solid", "IdVd@VGS=0.50V"="dashed", "IdVd@VGS=0.55V"="dashed", "IdVd@VGS=0.60V"="dashed")



all_relerr <- c()
df_list <- list() # To store data and metadata for each subplot

for(i in 1:length(group_info)) {
  info <- group_info[[i]]
  

  temp_sim_idvg <- read.csv(info$idvg_sim, header=FALSE, sep=" ")
  temp_mdl_idvg <- read.table(info$idvg_mdl, header=FALSE)
  
  df_idvg <- data.frame(vgs = round(as.numeric(temp_sim_idvg[36:96,8]),4),
                        sim = as.numeric(temp_sim_idvg[36:96,4]))
  df_idvg_mdl <- data.frame(vgs = round(as.numeric(temp_mdl_idvg[,1]),4),
                            mdl = as.numeric(temp_mdl_idvg[,2]))
  
  df_idvg_merged <- merge(df_idvg, df_idvg_mdl, by = "vgs", all = FALSE)
  epsilon <- 1e-18
  df_idvg_merged$rel_err <- abs((df_idvg_merged$sim - df_idvg_merged$mdl) / (df_idvg_merged$sim + epsilon)) * 100
  df_idvg_merged$curve <- "IdVg"
  df_idvg_merged$x_plot <- df_idvg_merged$vgs
  all_relerr <- c(all_relerr, df_idvg_merged$rel_err)
  

  temp_sim_idvd1 <- read.csv(info$idvd_sim[1], header=FALSE, sep=" ")
  temp_sim_idvd2 <- read.csv(info$idvd_sim[2], header=FALSE, sep=" ")
  temp_sim_idvd3 <- read.csv(info$idvd_sim[3], header=FALSE, sep=" ")
  temp_mdl_idvd <- read.table(info$idvd_mdl, header=FALSE)
  
  df_idvd_50 <- calc_error(temp_sim_idvd1, temp_mdl_idvd, 5, 4, 1, 3)
  df_idvd_55 <- calc_error(temp_sim_idvd2, temp_mdl_idvd, 5, 4, 1, 4)
  df_idvd_60 <- calc_error(temp_sim_idvd3, temp_mdl_idvd, 5, 4, 1, 5)
  
  df_idvd_50$curve <- "IdVd@VGS=0.50V"; df_idvd_50$x_plot <- df_idvd_50$x
  df_idvd_55$curve <- "IdVd@VGS=0.55V"; df_idvd_55$x_plot <- df_idvd_55$x
  df_idvd_60$curve <- "IdVd@VGS=0.60V"; df_idvd_60$x_plot <- df_idvd_60$x
  all_relerr <- c(all_relerr, df_idvd_50$rel_err, df_idvd_55$rel_err, df_idvd_60$rel_err)
  

  plot_df <- rbind(
    data.frame(x_plot = df_idvg_merged$x_plot, rel_err = df_idvg_merged$rel_err, curve = "IdVg"),
    data.frame(x_plot = df_idvd_50$x_plot, rel_err = df_idvd_50$rel_err, curve = "IdVd@VGS=0.50V"),
    data.frame(x_plot = df_idvd_55$x_plot, rel_err = df_idvd_55$rel_err, curve = "IdVd@VGS=0.55V"),
    data.frame(x_plot = df_idvd_60$x_plot, rel_err = df_idvd_60$rel_err, curve = "IdVd@VGS=0.60V")
  )
  
  df_list[[i]] <- list(plot_df = plot_df, letter = info$letter, r = info$r_val, l = info$l_val)
}


max_err <- max(all_relerr, na.rm=TRUE)
max_y <- ceiling(max(10, max_err) / 10) * 10
y_limits <- c(0, max_y)


make_plot <- function(dflist, letter, r_val, l_val,
                      show_xaxis_title = TRUE, show_xaxis_text = TRUE,
                      show_yaxis_title = TRUE, show_yaxis_text = TRUE) {
  
  p <- ggplot(dflist, aes(x = x_plot, y = rel_err, color = curve, shape = curve, linetype = curve)) +
    geom_hline(yintercept = 40, linetype = "dashed", color = "black", size = 0.8) +
    geom_line(size=1.5) +
    geom_point(size=3.5) +
    

    labs(x = if(show_xaxis_title) "Bias [V]" else NULL,
         y = if(show_yaxis_title) "Relative Error [%]" else NULL) +
    
    scale_color_manual(values = vgs_colors, labels = curve_labels) +
    scale_shape_manual(values = vgs_shapes, labels = curve_labels) +
    scale_linetype_manual(values = vgs_linetypes, labels = curve_labels) +
    scale_y_continuous(limits = y_limits, breaks = seq(0, max_y, by = 20)) +
    scale_x_continuous(limits = c(-0.6, 0), breaks = seq(-0.6, 0, by = 0.1)) +
    theme_bw() +
    theme(
      text = element_text(family = "Times New Roman"),
      aspect.ratio = 1, # Keep aspect ratio square
      plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"),
      

      axis.text.x = if(show_xaxis_text) element_text(size = 14) else element_blank(),
      axis.title.x = if(show_xaxis_title) element_text(size = 16) else element_blank(),
      axis.ticks.x = if(show_xaxis_text) element_line() else element_blank(), # Hide ticks if text is hidden
      
      axis.text.y = if(show_yaxis_text) element_text(size = 14) else element_blank(),
      axis.title.y = if(show_yaxis_title) element_text(size = 16) else element_blank(),
      axis.ticks.y = if(show_yaxis_text) element_line() else element_blank(), # Hide ticks if text is hidden
      
      legend.title = element_blank(),
      legend.text = element_text(size = 18, margin = margin(r = 10, unit = "pt")),

      legend.position = "none"
    ) +
    

    annotate("text", x = -0.6, y = y_limits[2], label = letter, size = 6, family = "Times New Roman", hjust=0) +
    annotate("text", x = -0.6, y = y_limits[2]*0.9, label = bquote(italic(R)~"="~.(r_val)~" nm"), hjust=0, size=7, family = "Times New Roman") +
    annotate("text", x = -0.6, y = y_limits[2]*0.8, label = bquote(italic(L)~"="~.(l_val)~" nm"), hjust=0, size=7, family = "Times New Roman")
  
  return(p)
}


plot_list <- list()
for(i in 1:length(df_list)) {

  plot_list[[i]] <- make_plot(
    df_list[[i]]$plot_df,
    df_list[[i]]$letter,
    df_list[[i]]$r,
    df_list[[i]]$l
  )
}


p_a <- plot_list[[1]] + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                              legend.position = c(0.85, 0.45), # Position legend at 95% from left, 25% from bottom
                              legend.justification = c("right", "bottom"), # Anchor legend to its bottom-right corner
                              # Removed 'colour="black", size=0.5' to remove the bounding box
                              legend.background = element_rect(fill="white", colour=NA), # Set colour=NA for no border
                              legend.box.margin = margin(1, 1, 1, 1, unit="pt"), # Adjust margin around legend box
                              legend.key.height = unit(0.8, "lines"), # Adjust legend key size
                              legend.key.width = unit(1.2, "lines"),
                              legend.margin = margin(unit(0.01, "cm")),
                              legend.spacing.y = unit(0.01, "cm"),
                              legend.box.spacing = unit(0.01,"cm"))
# (b) hide Y-axis, hide X-axis, AND SHOW LEGEND at bottom-right
p_b <- plot_list[[4]] +
  theme(#axis.title.y = element_blank(), 
        #axis.text.y = element_blank(), 
        #axis.ticks.y = element_blank(),
        #axis.title.x = element_blank(), 
        #axis.text.x = element_blank(), 
        #axis.ticks.x = element_blank()
        
  )

# Row 2: (c) and (d)
# (c) show Y-axis, hide X-axis
p_c <- plot_list[[2]] + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
# (d) hide Y-axis, hide X-axis
p_d <- plot_list[[5]] + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        #axis.title.x = element_blank(), 
        #axis.text.x = element_blank(), 
        #axis.ticks.x = element_blank()
        )

# Row 3: (e) and (f)
# (e) show Y-axis, show X-axis (This one will be the anchor for its column's X-axis)
p_e <- plot_list[[3]] + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
# (f) hide Y-axis, show X-axis (This one will be the anchor for its column's X-axis)
p_f <- plot_list[[6]] + 
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())


# Combine them with patchwork
final_plot <- (p_a | p_c | p_e) /
  (p_b | p_d | p_f)

# Print the final combined plot
print(final_plot)

# Save the plot to a file
ggsave("/path/to/data/fig/combined_idvg_idvd_error_yoko.png",
       final_plot, width = 16, height = 10, dpi = 300)
######################### spice simulation ####################################

library(cowplot)
library(png)
library(magick)
setwd("/path/to/data/spice/")

temp_df_cinv <- read.csv("cinv.csv", header=TRUE)
temp_df_ninv <- read.csv("ninv.csv", header=TRUE)
temp_df_pinv <- read.csv("pinv.csv", header=TRUE)

df_cinv <- data.frame(vin = as.numeric(temp_df_cinv[,1]),
                      vout = as.numeric(temp_df_cinv[,2]),
                      type = "cinv") # Add type column
df_ninv <- data.frame(vin = as.numeric(temp_df_ninv[,1]),
                      vout = as.numeric(temp_df_ninv[,2]),
                      type = "ninv") # Add type column
df_pinv <- data.frame(vin = as.numeric(temp_df_pinv[,1]),
                      vout = as.numeric(temp_df_pinv[,2]),
                      type = "pinv") # Add type column
df_cinv

df_all <- rbind(df_cinv, df_ninv, df_pinv)

my_plot <- ggplot(data = df_all,
                  aes(x = vin, y = vout, color = type)) + # Now 'type' column exists in df_all
  geom_line(size = 1.5) +
  scale_color_manual(name = "type",
                     values = c("cinv" = "red",
                                "ninv" = "green",
                                "pinv" = "blue"),
                     labels = c("CMOS inverter","NMOS inverter","PMOS inverter")) +
  scale_y_continuous(limits = c(0, 0.7)) +
  scale_x_continuous(limits = c(0, 0.6)) +
  labs(x = expression(italic(V)["in"] ~ "[V]"),
       y = expression(italic(V)["out"] ~ "[V]")) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        legend.position = c(0.8, 0.9),
        legend.text = element_text(size = 16),
        legend.box = "vertical",
        legend.margin = margin(unit(0.1, "cm"))) +
  # You had guides(color = "none") which hides the legend.
  # If you want the legend to show the colors based on 'type', remove this line.
  # If you want to show the legend, but want to remove the title of the legend, use:
  guides(color = guide_legend(title = NULL)) + # This will show the legend but remove the "type" title
  
  annotate("text", x = 0.01, y = 0.45,
           label = expression(italic(R)[NMOS] * "= 2 nm"),
           hjust = 0, vjust = 0, size = 7,
           family = "Times New Roman") +
  annotate("text", x = 0.01, y = 0.4,
           label = expression(italic(L)[NMOS] * "= 9 nm"),
           hjust = 0, vjust = 0, size = 7,
           family = "Times New Roman") +
  annotate("text", x = 0.01, y = 0.35,
           label = expression(italic(R)[PMOS] * "= 2 nm"),
           hjust = 0, vjust = 0, size = 7,
           family = "Times New Roman") +
  annotate("text", x = 0.01, y = 0.3,
           label = expression(italic(L)[PMOS] * "= 9 nm"),
           hjust = 0, vjust = 0, size = 7,
           family = "Times New Roman") +
  annotate("text", x = 0.01, y = 0.25,
           label = expression(italic(r) * "= 5M" * Omega), # Note: r for resistance often uses R. If 'r' is radius, then use the appropriate unit.
           hjust = 0, vjust = 0, size = 7,
           family = "Times New Roman") +
  annotate("text", x = 0.01, y = 0.2,
           label = expression(italic(V)[DD] * "= 0.6 V"),
           hjust = 0, vjust = 0, size = 7,
           family = "Times New Roman")
print(my_plot)

image_path <- "/path/to/data/fig/invschem.png"

# Read image using magick
my_image_magick <- image_read(image_path)

final_plot <- ggdraw(my_plot) +
  draw_image(my_image_magick, # Use the image read by magick
             x = 0.98, y = 0.5,
             hjust = 1, vjust = 0.38,
             width = 0.45, height = 0.45) +# Adjust these as needed after testing
  annotate("text",
           x = 0.13,
           y = 0.95,
           label = "(b)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman") +
  annotate("text",
           x = 0.54,
           y = 0.74,
           label = "(a)",
           size = 7,
           fontface = "plain",
           family = "Times New Roman")
print(final_plot)
ggsave("/path/to/data/fig/comparison_of_inverters_yoko.png", 
       plot=final_plot, width = 10, height = 6, dpi = 300)


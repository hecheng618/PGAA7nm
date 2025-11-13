library(ggplot2)
library(cowplot)
library(png)
library(magick)
setwd("D:/onedrive/2025_pgaa_cheng_rev/new_data/")

# ===============================================================
# 定义统一主题
# ===============================================================
theme_common <- theme_bw() +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.box = "vertical",
    legend.margin = margin(unit(0.1, "cm")),
    plot.tag.position = c(0.01, 0.98),
    plot.tag = element_text(size = 20, face = "bold", family = "Times New Roman")
  )

# ===============================================================
# 图 (a)
# ===============================================================
temp_df_a <- read.table("inv_model_bsim.txt", header = FALSE, skip = 1)

df_sram_a1 <- data.frame(vin = as.numeric(temp_df_a[,1]),
                         vout = as.numeric(temp_df_a[,3]),
                         type = "sram1")
df_sram_a2 <- data.frame(vin = as.numeric(temp_df_a[,1]),
                         vout = as.numeric(temp_df_a[,2]),
                         type = "sram2")
df_all_a <- rbind(df_sram_a1, df_sram_a2)

my_plot_a <- ggplot(df_all_a, aes(x = vin, y = vout, color = type)) +
  geom_line(size = 1.5) +
  scale_color_manual(values = c("sram1" = "green", "sram2" = "red"),
                     labels = c("BSIM-CMG", "This work")) +
  scale_y_continuous(limits = c(0, 0.6)) +
  scale_x_continuous(limits = c(0, 0.6)) +
  labs(
    x = expression(italic(V)["in"] ~ "[V]"),
    y = expression(italic(V)["out"] ~ "[V]"),
    tag = "(a)"
  ) +
  theme_common +
  theme(legend.position = c(0.8, 0.8)) +
  guides(color = guide_legend(title = NULL)) +
  annotate("text", x = 0.35, y = 0.35, label = "italic(R)[NMOS] == ' 2 nm'",
           parse = TRUE, hjust = 0, vjust = 0, size = 7) +
  annotate("text", x = 0.35, y = 0.30, label = "italic(L)[NMOS] == ' 9 nm'",
           parse = TRUE, hjust = 0, vjust = 0, size = 7) +
  annotate("text", x = 0.35, y = 0.25, label = "italic(R)[PMOS] == ' 2 nm'",
           parse = TRUE, hjust = 0, vjust = 0, size = 7) +
  annotate("text", x = 0.35, y = 0.20, label = "italic(L)[PMOS] == ' 9 nm'",
           parse = TRUE, hjust = 0, vjust = 0, size = 7) +
  annotate("text", x = 0.35, y = 0.15, label = "italic(V)[DD] == ' 0.6 V'",
           parse = TRUE, hjust = 0, vjust = 0, size = 7)

# ===============================================================
# 图 (b)
# ===============================================================
df1 <- read.table("sram_output1.txt", header = TRUE)
df2 <- read.table("sram_output2.txt", header = TRUE)

this_curve1 <- data.frame(v1 = df1[,1], v2 = df1[,2], set = "This work", curve = "c1")
this_curve2 <- data.frame(v1 = df1[,3], v2 = df1[,4], set = "This work", curve = "c2")
bsim_curve1 <- data.frame(v1 = df2[,1], v2 = df2[,2], set = "BSIM-CMG", curve = "c1")
bsim_curve2 <- data.frame(v1 = df2[,3], v2 = df2[,4], set = "BSIM-CMG", curve = "c2")
df_all_b <- rbind(this_curve1, this_curve2, bsim_curve1, bsim_curve2)

my_plot_b <- ggplot(df_all_b, aes(x = v1, y = v2, color = set, group = interaction(set, curve))) +
  geom_line(linewidth = 1.5) +
  scale_color_manual(values = c("This work" = "red", "BSIM-CMG" = "green")) +
  scale_x_continuous(limits = c(0, 0.6)) +
  scale_y_continuous(limits = c(0, 0.6)) +
  labs(
    x = expression(italic(V)[Q] ~ "[V]"),
    y = expression(italic(V)[Qb] ~ "[V]"),
    tag = "(b)"
  ) +
  theme_common +
  theme(legend.position = c(5, 5)) +
  guides(color = guide_legend(title = NULL)) #+
#  annotate("text", x = 0.35, y = 0.55,
#           label = "italic(R)[NMOS] == ' 2 nm'", parse = TRUE,
#           hjust = 0, vjust = 0, size = 7) +
#  annotate("text", x = 0.35, y = 0.50,
#           label = "italic(L)[NMOS] == ' 10 nm'", parse = TRUE,
#           hjust = 0, vjust = 0, size = 7) +
#  annotate("text", x = 0.35, y = 0.45,
#           label = "italic(R)[PMOS] == ' 2 nm'", parse = TRUE,
#           hjust = 0, vjust = 0, size = 7) +
#  annotate("text", x = 0.35, y = 0.40,
#           label = "italic(L)[PMOS] == ' 10 nm'", parse = TRUE,
#           hjust = 0, vjust = 0, size = 7) +
#  annotate("text", x = 0.35, y = 0.35,
#           label = "italic(V)[DD] == ' 0.6 V'", parse = TRUE,
#           hjust = 0, vjust = 0, size = 7)

# ===============================================================
# 3. 上下排列并保存
# ===============================================================
image_path_a <- "D:/onedrive/MPDI/data/fig/inverter.png"
my_image_magick_a <- image_read(image_path_a)
my_plot_a1 <- ggdraw(my_plot_a) +
  draw_image(my_image_magick_a, # Use the image read by magick
             x = 0.5, y = 0.5,
             hjust = 1, vjust = 0.38,
             width = 0.35, height = 0.4)
image_path_b <- "D:/onedrive/MPDI/data/fig/sram.png"

# Read image using magick

my_image_magick_b <- image_read(image_path_b)


my_plot_b1 <- ggdraw(my_plot_b) +
  draw_image(my_image_magick_b, # Use the image read by magick
             x = 0.98, y = 0.7,
             hjust = 1, vjust = 0.38,
             width = 0.38, height = 0.5)
combined_plot <- plot_grid(my_plot_a1, my_plot_b1, ncol = 2, align = "v")
print(combined_plot)

ggsave("./output_of_sram_combined.png",
       plot = combined_plot, width = 12, height = 6, dpi = 300)

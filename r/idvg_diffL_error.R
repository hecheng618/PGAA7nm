# --- 0. 加载程序包 ---
# install.packages("tidyr") 
library(ggplot2)
library(patchwork) # <--- 用于组合图表
library(tidyr) 

# --- 1. 设置工作目录和加载数据 ---
setwd("D:/onedrive/2025_pgaa_cheng_rev/new_data")

temp1_idvg <- read.table("idvg_model.txt", header = FALSE, skip = 1) 
temp2_idvg <- read.table("idvg_sim.txt", header = FALSE, skip = 1)

# (修改) 首先加载为 "raw" (原始) 数据框
idvg_mdl_raw <- data.frame(vgs =as.numeric(temp1_idvg[ ,1]),
                           L7 = as.numeric(temp1_idvg[ ,2]),
                           L9 = as.numeric(temp1_idvg[ ,3]),
                           L12 = as.numeric(temp1_idvg[ ,4]),
                           L20 = as.numeric(temp1_idvg[ ,5]))
idvg_sim_raw <- data.frame(vgs =as.numeric(temp2_idvg[ ,1]),
                           L7 = as.numeric(temp2_idvg[ ,2]),
                           L9 = as.numeric(temp2_idvg[ ,3]),
                           L12 = as.numeric(temp2_idvg[ ,4]),
                           L20 = as.numeric(temp2_idvg[ ,5]))

# (新增) 通过 'vgs' 列合并数据
merged_data <- merge(idvg_mdl_raw, idvg_sim_raw, by = "vgs", suffixes = c(".mdl", ".sim"))

# (新增) 如果合并后没有数据，则停止并报错
if (nrow(merged_data) == 0) {
  stop("The 'vgs' columns in your model and sim files have no matching values. Cannot calculate error.")
}

# (修改) 
# 现在，我们从这个已合并、已对齐的 "merged_data" 重新创建 idvg_mdl 和 idvg_sim
idvg_mdl <- merged_data[, c("vgs", "L7.mdl", "L9.mdl", "L12.mdl", "L20.mdl")]
idvg_sim <- merged_data[, c("vgs", "L7.sim", "L9.sim", "L12.sim", "L20.sim")]

# (修改) 必须重命名列，以匹配后续代码 (去掉 .mdl 和 .sim 后缀)
colnames(idvg_mdl) <- c("vgs", "L7", "L9", "L12", "L20")
colnames(idvg_sim) <- c("vgs", "L7", "L9", "L12", "L20")


# --- 1b. (修改) 计算相对误差 ---
mdl_data <- idvg_mdl[, 2:5]
sim_data <- idvg_sim[, 2:5]

relative_error_matrix <- abs(mdl_data - sim_data) / abs(sim_data)

# --- (修改) 处理特殊值 ---
relative_error_matrix[] <- lapply(relative_error_matrix, function(col) {
  
  if (!is.numeric(col)) {
    col <- as.numeric(col)
  }
  
  # 1. Inf -> NA
  col[is.infinite(col)] <- NA
  
  # 2. NaN -> 0
  col[is.nan(col)] <- 0
  
  return(col)
})

error_df <- data.frame(vgs = idvg_mdl$vgs, relative_error_matrix)


# --- 2. 双坐标轴设置 (保持不变) ---
primary_min_log <- -17
primary_max_log <- -5
secondary_min_linear <- 0
secondary_max_linear <- 3.0e-6
m_val <- (secondary_max_linear - secondary_min_linear) / (primary_max_log - primary_min_log)
c_val <- secondary_min_linear - m_val * primary_min_log

my_sec_breaks <- seq(secondary_min_linear, secondary_max_linear, by = 0.5e-6)
my_sec_labels <- c("0", "0.5e-6", "1.0e-6", "1.5e-6", "2.0e-6", "2.5e-6", "3.0e-6")

sec_axis_def_final <- sec_axis(
  trans = ~ ( . * m_val + c_val ), 
  name = expression(italic("I")[SD] ~ "[A]"),
  breaks = my_sec_breaks, 
  labels = my_sec_labels
)

# --- 3. 重塑数据 (保持不变) ---
# 3a. 处理 Model (mdl) 数据，用于 geom_point
idvg_mdl_long <- pivot_longer(idvg_mdl, 
                              cols = c("L7", "L9", "L12", "L20"), 
                              names_to = "Length", 
                              values_to = "Id") 
idvg_mdl_long$log_Id <- log10(idvg_mdl_long$Id)
idvg_mdl_long$mapped_linear_Id <- (idvg_mdl_long$Id - c_val) / m_val
idvg_mdl_final <- pivot_longer(idvg_mdl_long,
                               cols = c("log_Id", "mapped_linear_Id"),
                               names_to = "Scale", 
                               values_to = "Plot_Y") 

# 3b. 处理 Sim (sim) 数据，用于 geom_line
idvg_sim_long <- pivot_longer(idvg_sim, 
                              cols = c("L7", "L9", "L12", "L20"), 
                              names_to = "Length", 
                              values_to = "Id") 
idvg_sim_long$log_Id <- log10(idvg_sim_long$Id)
idvg_sim_long$mapped_linear_Id <- (idvg_sim_long$Id - c_val) / m_val
idvg_sim_final <- pivot_longer(idvg_sim_long,
                               cols = c("log_Id", "mapped_linear_Id"),
                               names_to = "Scale", 
                               values_to = "Plot_Y") 

# 3c. (保持不变) 确保 Length 列的顺序
idvg_mdl_final$Length <- factor(idvg_mdl_final$Length, levels = c("L7", "L9", "L12", "L20"))
idvg_sim_final$Length <- factor(idvg_sim_final$Length, levels = c("L7", "L9", "L12", "L20"))


################## 4. 绘制比较图 (Sim=线, Mdl=点) ##################
# (此部分代码保持不变)
base_plot_1 <- ggplot() + 
  geom_line(data = idvg_sim_final, 
            aes(x = vgs, y = Plot_Y, color = Scale, linetype = Scale, group = interaction(Scale, Length)), 
            size = 1.0) + 
  geom_point(data = idvg_mdl_final, 
             aes(x = vgs, y = Plot_Y, color = Scale, shape = Length),
             size = 3) + 
  scale_color_manual(name = NULL, 
                     values = c("log_Id" = "black", "mapped_linear_Id" = "red"),
                     labels = c("log_Id" = "Log Scale", "mapped_linear_Id" = "Linear Scale")) +
  scale_linetype_manual(name = NULL, 
                        values = c("log_Id" = "solid", "mapped_linear_Id" = "dashed"), 
                        labels = c("log_Id" = "Log Scale", "mapped_linear_Id" = "Linear Scale")) +
  scale_shape_manual(name = NULL, 
                     values = c("L7" = 16, "L9" = 17, "L12" = 15, "L20" = 18), 
                     labels = c("L7" = bquote(italic(L) ~ "= 7 nm"), 
                                "L9" = bquote(italic(L) ~ "= 9 nm"),
                                "L12" = bquote(italic(L) ~ "= 12 nm"),
                                "L20" = bquote(italic(L) ~ "= 20 nm"))) +
  scale_y_continuous(name = expression(log[10]~(italic("I")[SD])~"[A]"),
                     sec.axis = sec_axis_def_final, 
                     breaks = c(
                       (0 - c_val) / m_val, (0.5e-6 - c_val) / m_val, (1e-6 - c_val) / m_val, 
                       (1.5e-6 - c_val) / m_val, (2e-6 - c_val) / m_val, (2.5e-6 - c_val) / m_val,
                       (3e-6 - c_val) / m_val 
                     )
  ) +
  coord_cartesian(ylim = c(primary_min_log, primary_max_log)) + 
  labs(x = expression(italic("V")[GS] ~ "[V]")) +
  theme_bw() +
  theme(text = element_text(family = "Times New Roman"),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.title.x = element_text(size =20),
        axis.title.y = element_text(size =20),
        axis.text.y.right = element_text(size = 18), 
        axis.title.y.right = element_text(size = 20),
        legend.position = c(0.75, 0.8), 
        legend.text = element_text(size = 20,hjust=0),
        legend.box = "vertical", 
        legend.spacing.y = unit(0.0, "cm"), 
        legend.box.spacing = unit(0.0,"cm"),
        legend.box.margin = margin(0, 0, 0, 0, unit="pt"),
        legend.margin = margin(t = 0.0, r = 0.0, b = 0.0, l = 0.0, unit = "cm"),
        legend.key.size = unit(0.8, "cm"),
        legend.key.height = unit(0.6, "lines"), 
        legend.key.width = unit(1.5, "cm") 
  ) +
  annotate("text", x=-0.5, y=-12, label = "italic(R) == ' 2 nm'", parse = TRUE, 
           hjust=0,vjust=0, size=7, family = "Times New Roman") +
  annotate("text", x=-0.5, y=-13, label = "italic(V)[DS] == ' -0.6 V'", parse = TRUE, 
           hjust=0., vjust=0, size=7, family = "Times New Roman") +
  annotate("text", x=-0.5, y=-14, label = "italic(phi)[GC] - italic(w)[FB] == ' 0.63 V'", parse = TRUE, 
           hjust=0., vjust=0, size=7, family = "Times New Roman") + 
  annotate("text", x=-0.5, y=-15, label = "italic(V)[bi] == ' -1.35 V'", parse = TRUE, 
           hjust=0, vjust=0, size=7, family = "Times New Roman")

# --- 5. (修改) 此处不再单独打印或保存电流图 ---


################## 6. (修改) 绘制相对误差图 (0-120%) ##################

# 6a. 重塑误差数据以便绘图
error_long <- pivot_longer(error_df, 
                           cols = c("L7", "L9", "L12", "L20"), 
                           names_to = "Length", 
                           values_to = "RelError")

error_long$Length <- factor(error_long$Length, levels = c("L7", "L9", "L12", "L20"))

# 将相对误差转换为百分比
error_long$RelError_Percent <- error_long$RelError * 100

# 6b. 绘制误差图
error_plot <- ggplot(error_long, aes(x = vgs, y = RelError_Percent, color = Length)) +
  geom_line(size = 1.) + # (修改) 将线条加粗
  geom_point(aes(shape = Length), size = 3) + # (新增) 添加点
  
  # (修改) 关键：使用线性 Y 轴 (0-120%)
  scale_y_continuous(
    name = "Relative Error [%]",
    limits = c(0, 120),                     # 强制 Y 轴范围
    breaks = seq(0, 120, by = 20),         # 定义刻度位置 (0, 20, ...)
    labels = c("0%", "20%", "40%", "60%", "80%", "100%", "120%") # 定义刻度标签
  ) + 
  
  # (修改) 添加 40% 的虚线，不加文字
  geom_hline(yintercept = 40, linetype = "dashed", color = "black", size = 0.8) + 
  
  # (修改) L 变为斜体 (现在需要同时为 color 和 shape 定义图例)
  scale_color_discrete(name = NULL, 
                       labels = c("L7" = bquote(italic(L) ~ "= 7 nm"), 
                                  "L9" = bquote(italic(L) ~ "= 9 nm"),
                                  "L12" = bquote(italic(L) ~ "= 12 nm"),
                                  "L20" = bquote(italic(L) ~ "= 20 nm"))) + 
  
  # (新增) 为点添加形状图例，与颜色图例保持一致
  scale_shape_manual(name = NULL, 
                     values = c("L7" = 16, "L9" = 17, "L12" = 15, "L20" = 18), # 与 (a) 图的点形状保持一致
                     labels = c("L7" = bquote(italic(L) ~ "= 7 nm"), 
                                "L9" = bquote(italic(L) ~ "= 9 nm"),
                                "L12" = bquote(italic(L) ~ "= 12 nm"),
                                "L20" = bquote(italic(L) ~ "= 20 nm"))) +
  
  # (修改) 移除 title
  labs(title = NULL, 
       x = expression(italic("V")[GS] ~ "[V]")) +
  
  theme_bw() + 
  # (修改) 移除标题设置，并将图例移入图中
  theme(text = element_text(family = "Times New Roman"),
        axis.text = element_text(size = 18), 
        axis.title = element_text(size = 20), 
        plot.title = element_blank(), # 确保标题被移除
        legend.text = element_text(size = 18), # 调整图例文字大小
        legend.title = element_blank(), # 确保图例标题被移除
        legend.position = c(0.1, 0.9),  # (x, y) 坐标 (右上角)
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = "white", colour = NA) # (修改) 移除图例边框
  ) 


# --- 6c. (修改) 此处不再单独打印或保存误差图 ---


# --- 7. (新增) 组合两个图并添加 (a) (b) 标签 ---
# (修改) 添加 tag_prefix 和 tag_suffix
combined_plot <- (base_plot_1 + error_plot) + 
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
  theme(plot.tag = element_text(size = 22, face = 'bold', family = "Times New Roman")) 

# 显式打印组合图
print(combined_plot)

# 保存组合图
# 注意：宽度需要调整以容纳两个并排的图，大约是单个图宽度的两倍
ggsave("D:/onedrive/2025_pgaa_cheng_rev/new_data/combined_idvg_error.png",
       combined_plot, width = 15, height = 6, dpi = 300) # 宽度设为 20 以容纳两个图
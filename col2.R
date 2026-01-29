# 加载必要的包
library(tidyverse)
library(rstatix)
library(ggsignif)

# 数据预处理（假设数据框名为df）
df_processed <- df %>%
  # 添加统计检验列
  group_by(groups, data) %>%
  mutate(
    test_result = list(rstatix::wilcox_test(value ~ y, data = cur_data())),
    pvalue = map_dbl(test_result, "p"),
    significance = case_when(
      pvalue < 0.001 ~ "***",
      pvalue < 0.01 ~ "**",
      pvalue < 0.05 ~ "*",
      TRUE ~ "NS"
    )
  ) %>%
  ungroup()

# 数据汇总（计算均值和标准差）
df_summary <- df_processed %>%
  group_by(groups, data, y) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

# 绘制图形
ggplot(df_summary, aes(x = data, y = mean_value, fill = y)) +
  # 柱形图主体
  geom_col(position = position_dodge(0.9), width = 0.8) +
  # 误差线
  geom_errorbar(
    aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
    width = 0.2,
    position = position_dodge(0.9)
  ) +
  # 显著性标注
  geom_signif(
    comparisons = list(c("Y", "O")),
    map_signif_level = TRUE,
    test = "wilcox.test",
    tip_length = 0.01,
    y_position = df_summary %>% 
      group_by(groups, data) %>%
      summarise(max_mean = max(mean_value)) %>%
      pull(max_mean) + 0.5,
    .groups = "drop"
  ) +
  # 分面显示不同基因
  facet_wrap(~groups, scales = "free_y", ncol = 2) +
  # 颜色方案
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  # 坐标轴和标签
  labs(
    title = "基因表达分布比较",
    x = "数据组",
    y = "表达值（均值±标准差）",
    fill = "分组"
  ) +
  # 主题优化
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank()
  ) +
  # 基线线
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")

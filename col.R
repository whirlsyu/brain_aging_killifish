df_all<-NULL
df <- reshape2::melt(
  data.frame(t(train[importance_feature_gene,]),y = group_for_train,data = "train"),  
  id.vars = c("y","data"),    # 保留的主字段
  variable.name = "groups",           # 分类列名
  value.name = "value"         # 值列名
)

df_all<-rbind(df_all,df)

df <- reshape2::melt(
  data.frame(t(test[importance_feature_gene,]),y = group_for_test,data = "test"),  
  id.vars = c("y","data"),    # 保留的主字段
  variable.name = "groups",           # 分类列名
  value.name = "value"         # 值列名
)

df_all<-rbind(df_all,df)

df <- reshape2::melt(
  data.frame(external_test[,importance_feature_gene],y = external_group,data = "external"),  
  id.vars = c("y","data"),    # 保留的主字段
  variable.name = "groups",           # 分类列名
  value.name = "value"         # 值列名
)

df_all<-rbind(df_all,df)

df_all$value<-as.numeric(df_all$value)


# 加载必要的包
library(tidyverse)
library(ggpubr)
library(rstatix)

# 创建示例数据（请替换为您的实际数据）
df <- df_all
df$data <- factor(df$data, levels = c("train", "test", "external"))
df$value<-log2(df$value + 1)
# 数据整理：转换为长格式并添加统计检验结果
df_processed <- df %>%
  # 添加统计检验列
  group_by(groups, data) %>%
  mutate(
    test_result = list(rstatix::wilcox_test(formula = value ~ y,data=cur_data())),
    pvalue = map_dbl(test_result, "p"),
    significance = case_when(
      pvalue < 0.001 ~ "***",
      pvalue < 0.01 ~ "**",
      pvalue < 0.05 ~ "*",
      TRUE ~ ""
    )
  ) %>% 
  mutate(
    FacetLabel = case_when(
      groups == "gene.G4P62_010996" ~ "ankle1",
      groups == "gene.G4P62_009132" ~ "gpr148",
      groups == "gene.G4P62_015839" ~ "LOC107386469",
      groups == "gene.G4P62_018805" ~ "LOC107396841",
      TRUE ~ "其他"
    )
  ) %>%
  ungroup()

df_summary <- df_processed %>%
  group_by(FacetLabel, data, y) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    sd_value = sd(value, na.rm = TRUE),
    .FacetLabel = "drop"
  )


# 修正后的绘图代码
 ggplot(df_processed, aes(x = data, y = value, fill = y)) +
  geom_violin(trim=TRUE,
              scale="width",        # 统一宽度
              adjust=0.5,            # 降低带宽
              alpha = 0.5) +
  geom_signif(
    comparisons = list(c("Y", "O")),
    map_signif_level = TRUE,
    test = "wilcox.test",
    tip_length = 0.01,
    y_position = max(df_processed$value) + 0.5
  ) + scale_x_discrete(limits = c("train", "test", "external")) + 
  facet_wrap(~FacetLabel, scales = "free_y", ncol = 4,) +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e")) +
  labs(
    title = "Gene Expressions",
    x = "groups",
    y = "log2(TPM)",
    fill = "groups"
  ) +
   theme(
     panel.background = element_rect(fill = "white"),
     panel.grid.major = element_line(color = "gray90"),
     panel.grid.minor = element_line(color = "gray95"),
     plot.background = element_rect(fill = "white"),
     axis.line = element_line(color = "black"),  
     axis.text = element_text(color = "black",size = 15),
     axis.title = element_text(color = "black"),
     axis.text.x = element_text(angle = 45, hjust = 1),
   ) +
  # 修正后的显著性标注层
  geom_text(
    data = df_processed %>%
      group_by(FacetLabel, data) %>%
      summarise(
        pos = max(value) - 0.1,
        sig = unique(significance) %>%
          paste(collapse = "") %>%
          str_trim(),
        .FacetLabel = "drop"  # 显式取消分组
      ),
    aes(x = data, y = pos, label = sig),
    size = 8, 
    vjust = 0,
    inherit.aes = FALSE  # 关键修正：禁用继承原始aes
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray")

 
 

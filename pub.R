setwd("/data/fish/killifishes/RNA/analysis/")
# 加载必要的包
library(data.table)  # 高效数据处理

# 1. 获取所有匹配的_gene_abundance.txt文件路径
file_list <- list.files(
  path = ".",          # 当前工作目录（即analysis文件夹）
  pattern = "*_gene_abundance.txt",  # 文件名匹配模式
  recursive = TRUE,    # 递归搜索子文件夹
  full.names = TRUE    # 返回完整路径
)


# 3. 读取并合并所有文件
merged_data <- rbindlist(
  lapply(file_list, function(file_path){
    # 读取文件（自动检测分隔符，支持txt/csv）
    dt <- fread(file_path, header = TRUE)
    
    # 可选：添加样本ID列（根据文件名提取）
    sample_id <- basename(file_path)  # 获取文件名（如SRR1030817_gene_abundance.txt）
    sample_id <- sub("_gene_abundance.txt", "", sample_id)  # 提取SRR1030817部分
    dt[, SampleID := sample_id]
    
    return(dt)
  }),
  use.names = TRUE,  # 保持列名一致
  fill = TRUE        # 自动填充缺失列（列顺序不同时有效）
)


# 加载必要包
library(data.table)  # 高效数据处理
library(tidyr)       # 数据重塑

# 方法1：使用data.table的dcast函数（推荐大数据处理）
# 转换为data.table格式
dt <- as.data.table(merged_data)

# 创建基因-样本TPM矩阵（自动处理重复基因-样本组合）
tpm_matrix <- dcast(
  dt,
  `Gene ID` ~ SampleID,  # 行=基因ID，列=样本ID
  value.var = "TPM",  # 使用TPM列的值
  fill = 0,           # 缺失值填充为0
  sep = "_"           # 列名分隔符（避免样本ID含特殊字符）
)

write.xlsx(list(
"Filtered_Expression" = expr_filtered,
"Filtered_Metadata" = meta_filtered
), "matched_data.xlsx",rowNames=TRUE)

meta_filtered_fill <- meta_filtered
meta_filtered_fill$GSE <- ave(
  meta_filtered_fill$GSE, 
  cumsum(!is.na(meta_filtered_fill$GSE)), 
  FUN = function(x) x[1]
)



tpm_matrix_f<-tpm_matrix[,-1]

library(tidyverse)
library(ggplot2)
library(reshape2)

# 转换为长格式（基因为行，样本为列 → 长格式：基因+样本+表达量）
merged_tpm_matrix<-as.matrix(merged_tpm)
rownames(merged_tpm_matrix)<-merged_tpm_matrix[,1]
merged_tpm_matrix<-merged_tpm_matrix[,-c(1,12)]
tpm_long <- melt(as.numeric_matrix(merged_tpm_matrix), 
                 variable.name = "sample", 
                 value.name = "tpm",
                 varnames = c("gene", "sample"))

# 可选：对TPM值进行log2转换（消除右偏）
tpm_long <- tpm_long %>%
  mutate(tpm_log = ifelse(tpm > 0, log2(tpm + 1), 0))


p_density <- ggplot(tpm_long, aes(x = tpm_log, color = sample)) +
  geom_density(alpha = 0.3) +                        # 透明度设置
  scale_color_viridis_d(option = "plasma") +         # 连续颜色映射
  labs(
    title = "Gene Expression Density",
    x = "log2(TPM+1)",
    y = "Density"
  ) +
  theme_minimal(base_size = 12)
p_density

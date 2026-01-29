if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("rtracklayer", "dplyr", "tidyr"))
library(rtracklayer)
library(dplyr)
library(tidyr)


# 读取GTF文件为GRanges对象
gtf_gr <- import("/data/ref/furzeri/furzeri_ncbi/data/GCA_014300015.1/genomic.gff")

# 转换为数据框
gtf_df <- as.data.frame(gtf_gr, stringsAsFactors = FALSE)

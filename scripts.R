library(purrr)
library(dplyr)

# 读取文件并提取TPM列

file_list <- list.files(
  path = "./data",
  pattern = "\\.txt$",  # 正则匹配路径结构
  full.names = TRUE,
  recursive = TRUE                                    # 允许递归搜索
)

tpm_data <- map(file_list, ~ {
  read.table(.x, header=TRUE, sep="\t") %>%
    select(Gene.ID, TPM)  # 假设TPM列名为"TPM"
})


# 合并所有TPM数据
merged_tpm <- reduce(tpm_data, full_join, by="Gene.ID")
colnames(merged_tpm)<-c("gene_name",file_list)
rownames(merged_tpm)<-merged_tpm[,1]
merged_tpm<-merged_tpm[,-1]
# 保存结果
write.csv(merged_tpm, "merged_tpm.csv", row.names=FALSE)
# log2转换（+1避免log(0)）
log2_tpm <- log2(merged_tpm + 1)

# 加载依赖包
library(limma)
library(edgeR)

library("limma",warn.conflicts=F,quietly =T,verbose=F)
external_group<-factor(c("Y","Y","Y","O","Y","O","Y","O","O","O"))
design<-model.matrix(~external_group)

fitlim<-lmFit(log2_tpm,design)
fiteb<-eBayes(fitlim)
DEG<-topTable(fiteb,coef=2,adjust="BH",n=Inf,lfc=0,p.value=0.05)

DEG<-topTable(fiteb,p.value=1,n=21000,adjust="fdr")
DEG$GeneName<-merged_12[rownames(DEG),]$`Gene Name`
for (variable in rownames(DEG)) {
  if (DEG[variable,]$GeneName=="-"){
    DEG[variable,]$GeneName = gtf_df[gtf_df$ID == variable,"Note"]
  }
}



merged_12 <- merge(B102[,1:6], DEG, by="Gene ID", all=TRUE)

library(ballgown)

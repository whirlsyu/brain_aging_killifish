# 安装并加载包
install.packages("ggVennDiagram")
library(ggVennDiagram)

# 生成示例数据
Supplemental_tables_Relaxed_excluding_CMD_Nothobranchius <- read_excel("cell_Relaxed_Selection/Supplemental_tables.xlsx",
                                                                       sheet = "TableS22", skip = 1)
Supplemental_tables_Positive_selected_excluding_CMD_Nothobranchius <- read_excel("cell_Relaxed_Selection/Supplemental_tables.xlsx",
                                                                       sheet = "TableS28", skip = 1)

# 假设df1和df2是原始DataFrame
common_genes_Relaxed_Positive <- intersect(Supplemental_tables_Relaxed_excluding_CMD_Nothobranchius$GeneSymbol, 
                          Supplemental_tables_Positive_selected_excluding_CMD_Nothobranchius$GeneSymbol)

# 从df1中删除共有基因的行
Supplemental_tables_Relaxed_excluding_CMD_Nothobranchius_filtered <- Supplemental_tables_Relaxed_excluding_CMD_Nothobranchius %>%
  filter(!GeneSymbol %in% common_genes_Relaxed_Positive)

# 从df2中删除共有基因的行
Supplemental_tables_Positive_selected_excluding_CMD_Nothobranchius_filtered <- Supplemental_tables_Positive_selected_excluding_CMD_Nothobranchius %>%
  filter(!GeneSymbol %in% common_genes_Relaxed_Positive)



venn_object2<-venn.diagram(
  x = list(x1= DEG_INFO$`Gene Name`,x2 = (Supplemental_tables_Relaxed_excluding_CMD_Nothobranchius_filtered %>% filter(P < 0.1))$GeneSymbol,
           x3 = (Supplemental_tables_Positive_selected_excluding_CMD_Nothobranchius_filtered %>% filter(P < 0.1))$GeneSymbol),
  filename = NULL,
  category.names = c("", "Positive_selected","Relaxed"), # Optional: specify names
  fill = c("skyblue", "pink","#440154"),      # Optional: specify fill colors
  main = "Two-Sets Venn Diagram"             # Optional: add a title
)
grid::grid.draw(venn_object2)



ggVennDiagram(  x = list(DEG= as.character(DEG_INFO$`Gene Name`),Relaxed = (Supplemental_tables_Relaxed_excluding_CMD_Nothobranchius_filtered %>% filter(P < 0.1))$GeneSymbol,
                         Positive_selected = (Supplemental_tables_Positive_selected_excluding_CMD_Nothobranchius_filtered %>% filter(P < 0.1))$GeneSymbol))


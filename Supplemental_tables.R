write.xlsx(list(
  "S1 gene_expression" = expr_filtered,
  "S2 expression_Metadata" = meta_filtered_fill,
  "S3 external validation" = merged_tpm_matrix,
  "S5 prediction results" = agg_long,
  "S6 KEGG pathway enrichment" = kk_readable@result,
  "S7 GO BP enrichment" = ego@result,
  "S8 GO MF enrichment" = ego_MF@result,
  "S9 GO MF enrichment" = ego_CC@result,
  "S10 glmnet model beta" = betas
), "Supplemental_tables.xlsx",rowNames=TRUE)


write.xlsx(list(
  "S1 DEG" = DEG_res_INFO
), "Supplemental_tables_deg1.xlsx",rowNames=TRUE)

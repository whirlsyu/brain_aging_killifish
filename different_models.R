#different models
options(java.parameters = "-Xmx32000m")


library(mlr3)
library(mlr3learners)
library(mlr3verse)
library(mlr3benchmark)

tainingset<-data.frame(t(train[RES_GENE,]),group_for_train)
testset<-data.frame(t(test[RES_GENE,]),group_for_train=y_test)

tasks<-as_task_classif(group_for_train ~ ., data = tainingset, positive = "O")
task_test<-as_task_classif(group_for_train ~ ., data = testset, positive = "O")



library(paradox)
library(mlr3verse)



# 创建每个学习器并设置参数
learners <- list(
  classif.ranger = lrn("classif.ranger", predict_type = "prob"),
  classif.kknn = lrn("classif.kknn", 
                     predict_type = "prob"),
  classif.cv_glmnet = lrn("classif.cv_glmnet", 
                      alpha = 0.1,
                      predict_type = "prob"),
  classif.svm = lrn("classif.svm", type = "C-classification", predict_type = "prob"),
  classif.xgboost = lrn("classif.xgboost", predict_type = "prob")
)

set.seed(111)
resamplings = rsmp("cv", folds = 3)

design <- benchmark_grid(
  tasks = tasks,
  learners = learners,
  resampling = resamplings
)
bmr = benchmark(design)
measures <- msrs(c("classif.acc", "classif.auc", "classif.bbrier"))
# 聚合结果
agg_res <- bmr$aggregate(measures)
# 按准确率排序
agg_res[order(-agg_res$classif.acc), ]

# 绘制所有模型的ROC曲线
p <- autoplot(bmr, type = "roc") +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "comparesion of ROC for multiple models")

auc_values <- bmr$aggregate(msr("classif.auc"))# 创建ROC曲线图


# 添加AUC值到右下角
p + 
  geom_text(
    data = auc_values,
    aes(
      x = 0.7,  # 水平分散
      y = (as.numeric(as.factor(learner_id)) - 1) * 0.1,
      label = paste0(gsub("^classif\\.", "", learner_id)," AUC: ", round(classif.auc, 3))
    )
  )

# 提取AUC值
auc_values <- bmr$aggregate(msr("classif.auc"))

# 绘制基础ROC曲线
p <- autoplot(bmr, type = "roc") +
  labs(title = "Comparison of ROC for Multiple Models")

# 添加颜色比例尺（仅一次）
p <- p + scale_color_brewer(palette = "Dark2")

# 添加AUC文本标签（修正坐标参数）
n_models <- nrow(auc_values)
p + 
  geom_text(
    data = auc_values,
    aes(
      x = 0.9,  # 水平分散
      y = 0.05 + (1:n_models) * 0.05,  # 垂直分散
      label = paste0(gsub("^classif\\.", "", learner_id), " AUC: ", sprintf("%.4f", round(classif.auc, 4)) )
    ),
    hjust = 1,
    vjust = 0,
    size = 4,
    color = "black",
    inherit.aes = FALSE
  )


# 提取准确率和AUC
agg_res <- agg_res[, .(learner_id, classif.acc, classif.auc)]

# 转换为长格式
agg_long <- melt(agg_res, id.vars = "learner_id")
agg_long_processed <- agg_long %>%
  mutate(
    learner_id = gsub("^classif\\.", "", learner_id),  # 移除前缀
    variable = toupper(variable)                      # 字母大写
  )

# 创建图形对象
 ggplot(agg_long_processed, aes(
  x = learner_id, 
  y = value, 
  fill = variable
)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = sprintf("%.4f", round(value, 4))),  # 保留3位小数
    position = position_dodge(width = 0.9),  # 与柱状图对齐
    vjust = -0.5,  # 标签上移
    size = 5       # 标签字体大小
  ) +
  coord_cartesian(ylim = c(0.75, 1.0)) +  # 设置y轴范围
  scale_fill_manual(
    values = c("#1f77b4", "#ff7f0e"),
    name = "types",  # 图例标题
    labels = c("ACC", "AUC")  # 图例标签大写
  ) +
  labs(x = "models", y = "measures", fill = "types",size = 25 ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15 ),  # x轴标签倾斜
    axis.text.y  = element_text( size = 15 ),
    legend.position = "top"  # 图例位置
  )



# 根据AUC选择最佳模型
best_model <- agg_res[which.max(agg_res$classif.acc), ]
cat("最佳模型：", best_model$learner_id, "\n")

instance = ti(
  task = tasks,
  learner = lrn("classif.kknn", 
                k = to_tune(seq(1,20)),
                distance = to_tune(1, 5),
                kernel = to_tune(c("optimal","rectangular", "epanechnikov", "gaussian")),
                predict_type = "prob"),
  resampling = resamplings,
  measures = msr("classif.auc"),
  terminator = trm("none")
)

tuner = tnr("grid_search", resolution = 11)
tuner$optimize(instance)

best_params <- instance$result_learner_param_vals
best_learner <- learners[[best_model$learner_id]]
best_learner$param_set$values <- best_params
final_model<-best_learner$train(tasks)

train_pred <- final_model$predict(tasks)
train_acc <- train_pred$score(measures)
cat("训练集准确率：", train_acc, "\n")
autoplot(train_pred, type = "roc")+
  annotate("text", x = 0.7, y = 0.3, 
           label = sprintf("AUC = %.4f", train_pred$score(msr("classif.auc"))),
           color = "steelblue", size = 4) 



test_pred <- final_model$predict(task_test)
test_acc <- test_pred$score(measures)
cat("测试集准确率：", test_acc, "\n")
autoplot(test_pred, type = "roc")+
  annotate("text", x = 0.7, y = 0.3, 
           label = sprintf("AUC = %.4f", test_pred$score(msr("classif.auc"))),
           color = "steelblue", size = 4) 


library(iml)
predictor <- Predictor$new(best_learner, data = tasks$data(), y = tasks$target_names)
importance_it_res <-NULL
for (variable in 1:100) {
  importance_it_res[[variable]]<-FeatureImp$new(predictor, loss = "ce")
}

significant_features <- unlist(
  lapply(importance_it_res, function(df) {
    df<-df$results
    unique(df$feature[df$importance > 1])
  })
)
feature_counts_deg<-table(significant_features)

importance_feature_gene<-DEG_INFO[gsub('\\.', '-', names(feature_counts_deg[feature_counts_deg>60])),"Gene ID"]
importance_feature_gene_name<-DEG_INFO[importance_feature_gene,"Gene Name"]
# importance <- as.data.table(importance$results)
# importance <- importance[importance.95>1,]
# importance$feature<-DEG_INFO[gsub('\\.', '-', importance$feature[importance$importance>1]),"Gene ID"]
# importance$"GeneName"<-DEG_INFO[importance$feature,"Gene Name"]

ggplot(importance, aes(x = reorder(GeneName, importance.95), y = importance.95)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "Feature", y = "importance", title = "Feature Importance") +
  theme_minimal()


plot(model_cv$glmnet.fit, xvar = "lambda", label = TRUE)
# Add vertical lines for optimal lambda values
abline(v = log(model_cv$lambda.min), col = "blue", lty = 2) # lambda that gives min error
abline(v = log(model_cv$lambda.1se), col = "red", lty = 2) # largest lambda within 1 SE of min

betas = as.matrix(model_cv$glmnet.fit$beta)
lambdas = model_cv$lambda
names(lambdas) = colnames(betas)
betas<-betas[importance_feature_gene,]
rownames(betas)<-importance_feature_gene_name
# Source - https://stackoverflow.com/a/64408574
# Posted by StupidWolf
# Retrieved 2026-01-12, License - CC BY-SA 4.0

library(ggplot2)
library(tidyr)
library(dplyr)
library(ggrepel)

as.data.frame(betas) %>% 
  tibble::rownames_to_column("variable") %>% 
  pivot_longer(-variable) %>% 
  mutate(lambda=lambdas[name]) %>% 
  ggplot(aes(x=lambda,y=value,col=variable)) + 
  geom_line(size = 3) +
  geom_label_repel(data=~subset(.x,lambda==min(lambda)),
                   aes(label=variable),nudge_x=-0.5,size = 5) +
  scale_x_log10() + theme_minimal() + theme(axis.text = element_text(size = 14),axis.title = element_text(size = 16))


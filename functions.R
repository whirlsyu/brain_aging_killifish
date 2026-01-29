# 总体环境参数
# platform       x86_64-redhat-linux-gnu
# arch           x86_64
# os             linux-gnu
# system         x86_64, linux-gnu
# status
# major          3
# minor          5.0
# year           2018
# month          04
# day            23
# svn rev        74626
# language       R
# version.string R version 3.5.0 (2018-04-23)
# nickname       Joy in Playing

####################自有函数func##################
#作者：于昕 author: Yu Xin
#email: yuxin@qlnu.edu.cn
#License: GPL-2
#GPL-2开源协议网址
#https://www.gnu.org/licenses/old-licenses/gpl-2.0.html
#简单的说：开源使用，可复制，可使用
#但是在使用的软件必须也开源包含#GPL-2开源协议并且包含原作者的信息
#发表文章和任何使用过程中也必须注明原作者信息

make_eset<-function(matrix,pData){
  colnames<-rownames(pData)

  commdata<-intersect(colnames,colnames(matrix))

  pData<-pData[rownames(pData) %in% commdata,]

  matrix<-matrix[,colnames(matrix) %in% commdata]

  if (nrow(pData)!=ncol(matrix)){stop("matrix colnames do not match the pData rownames")}

  #pData<-pData[colnames(matrix),]
  matrix<-matrix[,rownames(pData)]

  metadata<-data.frame(labelDescription=colnames(pData),stringsAsFactors = F)

  adf<-new("AnnotatedDataFrame",data=pData,varMetadata=metadata)

  eSet<-new("ExpressionSet",exprs=as.matrix(matrix),phenoData=adf)

  return(eSet)
}

limma_DEG<-function(x,y,p.value=0.01,FDRmethod = "none",log_transform=F){
  library(limma)
  if (log_transform == T) x<-log2(x+1)
  y=as.character(y) == unique(as.character(y))[1]
  group<-factor(y)
  design<-model.matrix(~group)
  fitlim<-lmFit(x,design)
  fiteb<-eBayes(fitlim)
  DEG<-topTable(fiteb,coef=2,adjust=FDRmethod,number = 25000,p.value=p.value)
  DEG$label<-rownames(DEG)
  DEG
}

is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

as.numeric_matrix<-function(met_matrixf){
  met_matrixff<-matrix(as.numeric(met_matrixf),
                       nrow = NROW(met_matrixf),
                       ncol = NCOL(met_matrixf))
  rownames(met_matrixff)<-rownames(met_matrixf)
  colnames(met_matrixff)<-colnames(met_matrixf)
  met_matrixff
}

draw_prediction_map<-function(Test,true_y,title_text=NULL,label_names=c("O","Y"),mean=F,save_name=NA){
  require(ggplot2)
  best_threshold = ROC_threshold(predict =as.numeric(Test) ,response = as.numeric(true_y))
  if (mean == T) best_threshold = mean(as.numeric(Test))
  data_risk_score<-data.frame(Samples=1:length(Test),Risk_Score=(Test-best_threshold)[order(Test-best_threshold,decreasing = T)])
  color_label<-as.numeric(true_y)[order(Test-best_threshold,decreasing = T)]
  
  Groups<-factor(color_label+2,labels = label_names)
  print(as.data.frame(table(Groups)))
  #print(data.frame(data_risk_score))
  g<-ggplot(data = data_risk_score,aes(x=Samples,y=Risk_Score,fill=Groups,color = Groups)) + 
    geom_bar( stat="identity" , width = 0.4) + labs(x="Cases",y="Risk score",title = title_text)+
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"))+
    scale_fill_manual(values = c("#00BFC4","#F8766D")) +scale_color_manual(values = c("#00BFC4","#F8766D"))
  if (!is.na(save_name)){
    ggsave(filename = paste(save_name,".jpeg",sep=""),plot = g,device ="jpeg" ,
           path = getwd(),dpi = 300,units = "in",width = 8, height = 4,
           limitsize=F)
  }
}

ROC_threshold <- function(predict, response) {
  perf <-prediction(as.numeric(predict),as.numeric(response))
  
  return(give_me_the_sen_spc(perf,print = F)) 
}

give_me_the_sen_spc<-function(pred,print=T) {
  tpr<-(pred@tp[[1]]/(pred@tp[[1]]+pred@fn[[1]])) #sensitivity
  fpr<-(pred@fp[[1]]/(pred@tn[[1]]+pred@fp[[1]])) #specificity 
  best_one<-which.min(sqrt( (1-tpr)^2+ fpr^2 ))
  if (print == T)  cat(paste(" sensitivity:",tpr[best_one],"\n","specificity:",1-fpr[best_one],"\n"))
  pred@cutoffs[[1]][best_one]#specificity 
}
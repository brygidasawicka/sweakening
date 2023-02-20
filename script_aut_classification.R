source("D:/CloudDrives/DropBox/Dropbox/Scripts/libraries_TJ_basic.R")
source("D:/CloudDrives/DropBox/Dropbox/Scripts/libraries_TJ_packages.R")
setwd("D:/CloudDrives/DropBox/Dropbox/Projects/spanish_vowels/")

source("../Dropbox/Scripts/libraries_TJ_basic.R")
source("..//Dropbox/Scripts/libraries_TJ_packages.R")
setwd("../Dropbox/Projects/spanish_vowels/")
library(GGally)
library(gridExtra)
library(grid)
library(nnet)
library(glmnet)

formatSc<-function(x){
  format(x,digits=2,scientific = TRUE)
}
round2<-function(x){
  round(x,digits=2)
}
round2Sc<-function(x){
  if ((x>0.2)|x<(-0.2)){
    round(x,digits=2)    
  } else {
    format(x,scientific=TRUE,digits=2)
  }
}
round4<-function(x){
  round(x,digits=4)
}
sampling_partition<-function(data,dataDiv,partition_trainfrac){
  dataNew=list()
  
  #   colnames(data)[colnames(data)==signal]="signal"
  #   signal="signal"
  
  group_partition=by(data,list(dataDiv),function(x){ 
    outx=list()
    sample_num=nrow(x)
    idsample<-sample(1:sample_num,floor(partition_trainfrac*sample_num))
    outx$datatrain=x[idsample,]
    outx$datatest=x[-idsample,]
    outx
  })
  
  dataNew$test=do.call(rbind,lapply(group_partition,function(x) x$datatest ))
  dataNew$train=do.call(rbind,lapply(group_partition,function(x) x$datatrain ))
  
  if (class(data)=="matrix") {
    dataNew$test=as.matrix(dataNew$test)
    dataNew$train=as.matrix(dataNew$train)}
  
  dataNew
}

undersampling_partition<-function(data,dataDiv,partition_frac){
  
  num_sample=floor(nrow(data)*partition_frac)
  
  group_partition=by(data,list(dataDiv),function(x){ 
    idsample<-sample(1:nrow(x),num_sample,replace = FALSE)
    outx=x[idsample,]
    outx
  })
  dataNew=do.call(rbind,group_partition)
  
  dataNew
}
# 
# temp_frac=table(data_classify_assessed_melted$PERC)
# temp_frac=min(temp_frac/sum(temp_frac)  )
# temp_df_undersampled=undersampling_partition(temp_df,temp_df$PERC,temp_frac)
# 
# modelListUS=list()
# for (itype in unique(data_classify_assessed_melted$variable)){
#   modelListUS[[itype]]=tj_calculate_classifier(data_main=temp_df_undersampled,
#                                                d_variable="PERC",
#                                                ind_variables = itype,maxit=5000,MaxNWts=30000)
# }

dir.create("plots/")
dir.create("tables/")

main_data=fread("data/main_data.txt")

#### word frequency analysis ####
word_freq=fread("data/word_freq.txt",encoding = "UTF-8")
sapply(word_freq,class)
word_freq$word_length=str_length(word_freq$Word)
colnames(word_freq)=c("word","count","countPerMil","logFreq","word_length")

word_freqAgg=do.call(rbind,by(word_freq,word_freq$word_length,function(x){
  data.frame(length=x$word_length[1],
             count_sum=sum(x$count),
             logCount_sum=sum(x$logFreq),stringsAsFactors = FALSE)
}))

word_freqAgg$cumSum=cumsum(word_freqAgg$count_sum)
word_freqAgg$cumLogSum=cumsum(word_freqAgg$logCount_sum)
word_freqAgg$fracSum=(word_freqAgg$count_sum)/sum(word_freqAgg$count_sum)
word_freqAgg$fracLogSum=(word_freqAgg$logCount_sum)/sum(word_freqAgg$logCount_sum)
word_freqAgg$cumFracSum=cumsum(word_freqAgg$fracSum)
word_freqAgg$cumFracLogSum=cumsum(word_freqAgg$fracLogSum)

ggplot(word_freqAgg,aes(x=length,y=fracSum))+geom_col()+theme_bw()
plot=ggplot(word_freqAgg,aes(x=length,y=fracLogSum))+geom_col()+theme_bw()
ggsave(plot,filename = "plots/wordLength_Frequncey.png",width = 6,height=4)

## Based on logFreq:
# krotkie: 1-6
# srednie: 7-9
# dlugie: 10-21

#### preliminary ####
main_data$length_class="short"
main_data$length_class[main_data$LENGTH>6]="average"
main_data$length_class[main_data$LENGTH>9]="long"
colnames(main_data)[colnames(main_data)=="3RDMOMENT"]="thirdMoment"
main_data$logKurtosis=main_data$KURTOSIS
main_data$logKurtosis=log(main_data$logKurtosis+3)

#### main analysis ####

## Classificator data ####
data_classify=main_data[,c("ID","id_type","PERC","perc_type","DUR",	"COG",	"STDEV",	"SKEWNESS",	"KURTOSIS",	"thirdMoment","logKurtosis")]
data_classify=data_classify[data_classify$id_type=="normal",]
data_classify_assessed=data_classify[data_classify$perc_type=="assessed",]
data_classify_toPredict=data_classify[!data_classify$perc_type=="assessed",]

data_classify_melted=melt(data_classify,id.vars = c("ID","id_type","PERC","perc_type"))
data_classify_assessed_melted=melt(data_classify_assessed,id.vars = c("ID","id_type","PERC","perc_type"))


# check correlation between acoustic ####
library(corrplot)
cor_all=cor(data_classify[,c("DUR",	"COG",	"STDEV",	"SKEWNESS",	"KURTOSIS","logKurtosis","thirdMoment")])
cor_assessed=cor(data_classify_assessed[,c("DUR",	"COG",	"STDEV",	"SKEWNESS",	"KURTOSIS","logKurtosis",	"thirdMoment")])
dir.create("plots/acoustic_description/",recursive = TRUE)
dir.create("tables/acoustic_description/",recursive = TRUE)

png("plots/acoustic_description/corr_acoustic_all.png",width=800,height=800)
corrplot.mixed(cor_all,tl.pos = 'lt')
dev.off()
png("plots/acoustic_description/corr_acoustic_assesded.png",width=800,height=800)
corrplot.mixed(cor_assessed,tl.pos = 'lt')
dev.off()



# pairs plots ####

plot=ggpairs(data_classify[,c("DUR",	"COG",	"STDEV",	"SKEWNESS",	"KURTOSIS",	"logKurtosis",	"thirdMoment")])
ggsave(plot,filename="plots/acoustic_description/pairs.png",width=10,height=10)



#pca plots ####
pca_data=prcomp(data_classify[,c("DUR",	"COG",	"STDEV",	"SKEWNESS",	"KURTOSIS",	"logKurtosis",	"thirdMoment")],center=TRUE,scale. = TRUE)

tab=data.frame(type=c("sdev","sdev_frac","var_frac"),rbind(pca_data$sdev,
cumsum(pca_data$sdev)/sum(pca_data$sdev),
cumsum(pca_data$sdev^2)/sum(pca_data$sdev^2)))
tj_write.table(tab,"tables/acoustic_description/pca.txt")

df_temp=data.frame(pca_data$rotation)
df_temp$variable=row.names(df_temp)
tj_write.table(df_temp,"tables/acoustic_description/pca_rotations.txt")


png("plots/acoustic_description/pca_1_3.png",width=800,height=800)
pairs(pca_data$x[,c("PC1","PC2","PC3")])
dev.off()

plot_data=data.frame(pca_data$x,PERC=data_classify$PERC,perc_type=data_classify$perc_type)
plot1=ggplot(data=plot_data[plot_data$perc_type=="assessed",],
       aes(x=PC1,y=PC2,colour=PERC))+geom_point(alpha=0.2)+theme_bw()
plot2=ggplot(data=plot_data[plot_data$perc_type=="assessed",],
            aes(x=PC2,y=PC3,colour=PERC))+geom_point(alpha=0.2)+theme_bw()
plot3=ggplot(data=plot_data[plot_data$perc_type=="assessed",],
            aes(x=PC1,y=PC3,colour=PERC))+geom_point(alpha=0.2)+theme_bw()
ggsave(grid.arrange(plot1,plot2,plot3,ncol=3),filename="plots/acoustic_description/pca_1_3assessed.png",width=15,height=10)



#tsne plots ####
#install.packages("Rtsne")
# library(Rtsne)
# tsne_2=Rtsne(data_classify[,c("DUR",	"COG",	"STDEV",	"SKEWNESS",	"KURTOSIS",	"logKurtosis",	"thirdMoment")],dims=2)
# tsne_2_40=Rtsne(data_classify[,c("DUR",	"COG",	"STDEV",	"SKEWNESS",	"KURTOSIS",	"logKurtosis",	"thirdMoment")],dims=2,perplexity = 40)
# tsne_2_50=Rtsne(data_classify[,c("DUR",	"COG",	"STDEV",	"SKEWNESS",	"KURTOSIS",	"logKurtosis",	"thirdMoment")],dims=2,perplexity = 50)
# tsne_3=Rtsne(data_classify[,c("DUR",	"COG",	"STDEV",	"SKEWNESS",	"KURTOSIS",	"logKurtosis",	"thirdMoment")],dims=3)
# tsne_3_40=Rtsne(data_classify[,c("DUR",	"COG",	"STDEV",	"SKEWNESS",	"KURTOSIS",	"logKurtosis",	"thirdMoment")],dims=3,perplexity = 40)
# tsne_3_50=Rtsne(data_classify[,c("DUR",	"COG",	"STDEV",	"SKEWNESS",	"KURTOSIS",	"logKurtosis",	"thirdMoment")],dims=3,perplexity = 50)
# 
# tsne_3_30_10000=Rtsne(data_classify[,c("DUR",	"COG",	"STDEV",	"SKEWNESS",	"KURTOSIS",	"logKurtosis",	"thirdMoment")],
#                       dims=3,perplexity = 50,max_iter=10000)
# 
# tsne_2_10_5000=Rtsne(data_classify[,c("DUR",	"COG",	"STDEV",	"SKEWNESS",	"KURTOSIS",	"thirdMoment")],
#                       dims=2,perplexity = 10,max_iter=5000)
# 
# plot(tsne_2$Y)
# plot(tsne_2_40$Y)
# plot(tsne_2_50$Y)
# plot(tsne_3$Y)
# plot(tsne_3_40$Y)
# plot(tsne_3_50$Y)
# plot(tsne_3_30_10000$Y)
# 
# 
# 
# #install.packages("tsne")
# library(tsne)
# 
# perp_vec=c(2,2,2,10,10,10,30,30,30,50,50,50)
# iter_vec=c(100,1000,5000,100,1000,5000,100,1000,5000,100,1000,5000)
# 
# tsneList=list()
# for (ii in 1:length(perp_vec)){
#   tsneList[[i]]=tsne(data_classify[,c("DUR",	"COG",	"STDEV",	"SKEWNESS",	"KURTOSIS",	"logKurtosis",	"thirdMoment")],
#                      k = 2,perplexity = perp_vec[ii],max_iter=iter_vec[ii])
# }



# check if distribution of acoustinc is the same between groups ####
plot1=ggplot(data=data_classify_melted,aes(x=perc_type,y=value,group=perc_type))+geom_boxplot()+
  facet_grid(variable~.,scales = "free_y")+theme_bw()
plot2=ggplot(data=data_classify_melted,aes(x=perc_type,y=value,group=perc_type))+geom_violin()+
  facet_grid(variable~.,scales = "free_y")+theme_bw()
dir.create("plots/distrib_acoustic_groups/")

ggsave(plot1,filename = "plots/distrib_acoustic_groups/plot_1.png",width = 8,height=12)
ggsave(plot2,filename = "plots/distrib_acoustic_groups/plot_2.png",width = 8,height=12)



tab1=do.call(rbind,by(data_classify_melted,data_classify_melted$variable,function(x){
  xx=x$value[x$perc_type=="assessed"]
  yy=x$value[x$perc_type=="not_assessed"]
  data.frame(variable=x$variable[1],
             medians=paste0(round(median(xx),digits=2)," vs. ", round(median(yy),digits=2) ),
             means=paste0(round(mean(xx),digits=2)," vs. ", round(mean(yy),digits=2) ),
             sds=paste0(round(sd(xx),digits=2)," vs. ", round(sd(yy),digits=2) ),
             ks_test_pv=format(ks.test(xx,yy)$p.value,scientific=TRUE,digits=2),
             t_test_pv=format(t.test(xx,yy)$p.value,scientific=TRUE,digits=2),
             u_test_pv=format(wilcox.test(xx,yy)$p.value,scientific=TRUE,digits=2),stringsAsFactors = FALSE)
}))
tj_write.table(tab1,paste0("plots/distrib_acoustic_groups.txt"))


for (itype in unique(data_classify_melted$variable)){
  plot3=ggplot(data=data_classify_melted[data_classify_melted$variable==itype,],
               aes(x=value,fill=perc_type))+geom_density(alpha=0.6)+theme_bw()+
    ggtitle(itype)
  
  itab=tab1[tab1$variable==itype,]
  ggsave(grid.arrange(plot3,tableGrob(itab),layout_matrix=matrix(c(1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2),4,5)),
         filename = paste0("plots/distrib_acoustic_groups/plot_3_",itype,".png"),width = 10,height=6)
}



# check distributions with resepct to assessed ####
plot1=ggplot(data=data_classify_melted,aes(x=PERC,y=value,group=PERC))+geom_boxplot()+
  facet_grid(variable~.,scales = "free_y")+theme_bw()
plot2=ggplot(data=data_classify_melted,aes(x=PERC,y=value,group=PERC))+geom_violin()+
  facet_grid(variable~.,scales = "free_y")+theme_bw()

dir.create("plots/distrib_acoustic_PERC/")
ggsave(plot1,filename = "plots/distrib_acoustic_PERC/allObsv_plot_1.png",width = 8,height=19)
ggsave(plot2,filename = "plots/distrib_acoustic_PERC/allObsv_plot_2.png",width = 8,height=10)

plot1=ggplot(data=data_classify_assessed_melted,aes(x=PERC,y=value,group=PERC))+geom_boxplot()+
  facet_grid(variable~.,scales = "free_y")+theme_bw()
plot2=ggplot(data=data_classify_assessed_melted,aes(x=PERC,y=value,group=PERC))+geom_violin()+
  facet_grid(variable~.,scales = "free_y")+theme_bw()

ggsave(plot1,filename = "plots/distrib_acoustic_PERC/OnlyAssessed_plot_1.png",width = 8,height=10)
ggsave(plot2,filename = "plots/distrib_acoustic_PERC/OnlyAssessed_plot_2.png",width = 8,height=10)


tab1=do.call(rbind,by(data_classify_assessed_melted,data_classify_assessed_melted$variable,function(x){
  xx=x$value[x$PERC=="s"]
  yy=x$value[x$PERC=="h"]
  data.frame(variable=x$variable[1],
             medians=paste0(round(median(xx),digits=2)," vs. ", round(median(yy),digits=2)),
             means=paste0(round(mean(xx),digits=2)," vs. ", round(mean(yy),digits=2)),
             sds=paste0(round(sd(xx),digits=2)," vs. ", round(sd(yy),digits=2)),
             ks_test_pv=format(ks.test(xx,yy)$p.value,scientific=TRUE,digits=2),
             t_test_pv=format(t.test(xx,yy)$p.value,scientific=TRUE,digits=2),
             u_test_pv=format(wilcox.test(xx,yy)$p.value,scientific=TRUE,digits=2),stringsAsFactors = FALSE)
}))
tj_write.table(tab1,paste0("plots/distrib_acoustic_PERC/distrib_acoustic_PERC.txt"))

for (itype in unique(data_classify_assessed_melted$variable)){
  plot3=ggplot(data=data_classify_assessed_melted[data_classify_assessed_melted$variable==itype,],
               aes(x=value,fill=PERC))+geom_density(alpha=0.6)+theme_bw()+
    ggtitle(itype)
  
  itab=tab1[tab1$variable==itype,]
  ggsave(grid.arrange(plot3,tableGrob(itab),layout_matrix=matrix(c(1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2),4,5)),
         filename = paste0("plots/distrib_acoustic_PERC/plot_3_",itype,".png"),width = 10,height=6)
}

tab2=table(data_classify_assessed$PERC)/nrow(data_classify_assessed)
tj_write.table(tab2,paste0("plots/distrib_acoustic_PERC/distrib_PERC.txt"))




# calculate models for clasificators ####
dir.create("plots/correlationModel",recursive = TRUE)
temp_df=data_classify_assessed
temp_df$PERC=factor(temp_df$PERC)
modelList=list()
for (itype in unique(data_classify_assessed_melted$variable)){
  modelList[[itype]]=tj_calculate_classifier(data_main=temp_df,
                                              d_variable="PERC",
                                             ind_variables = itype,maxit=5000,MaxNWts=30000)
}

df_summary=do.call(rbind,lapply(modelList,function(x){
  temp_coeffs=coefficients(x$model)
  data.frame(threshold=-temp_coeffs[1]/temp_coeffs[2],
             coeff_b=temp_coeffs[2],coeff_bsd=temp_coeffs[2]/x$coeff_importance$Importance[1],
             importance=x$coeff_importance$Importance[1],
             accuracy=x$confusionMatrix$overall[1],
             accuracyPV=x$confusionMatrix$overall[6],
             mcnemarPValue=x$confusionMatrix$overall[7],
             sensitivity=x$confusionMatrix$byClass[1],
             specificity=x$confusionMatrix$byClass[2],
             precision=x$confusionMatrix$byClass[3],
             recall=x$confusionMatrix$byClass[4],
             balanced_accuracy=x$confusionMatrix$byClass[11],stringsAsFactors = FALSE)
}))
df_summary$variable=row.names(df_summary)

df_summary$threshold=format(df_summary$threshold,digits=2,scientific=TRUE)
df_summary$coeff_b=format(df_summary$coeff_b,digits=2,scientific=TRUE)
df_summary$coeff_bsd=format(df_summary$coeff_bsd,digits=2,scientific=TRUE)
df_summary$importance=round(df_summary$importance,digits=2)
df_summary$accuracy=round(df_summary$accuracy,digits=2)
df_summary$accuracyPV=format(df_summary$accuracyPV,digits=2,scientific=TRUE)
df_summary$mcnemarPValue=format(df_summary$mcnemarPValue,digits=2,scientific=TRUE)
df_summary$sensitivity=round(df_summary$sensitivity,digits=2)
df_summary$specificity=round(df_summary$specificity,digits=2)
df_summary$precision=round(df_summary$precision,digits=2)
df_summary$recall=round(df_summary$recall,digits=2)
df_summary$balanced_accuracy=round(df_summary$balanced_accuracy,digits=2)

df_summary=df_summary[,c("variable","threshold","balanced_accuracy","accuracy","accuracyPV","mcnemarPValue",
                         "coeff_b","coeff_bsd","importance","sensitivity","specificity","precision","recall")]

tj_write.table(df_summary,"plots/correlationModel/corr_models_single.txt")
ggsave(tableGrob(df_summary),filename = "plots/correlationModel/corr_models_single.pdf",width=15)

for (itype in unique(data_classify_assessed_melted$variable)){
  plot3=ggplot(data=data_classify_assessed_melted[data_classify_assessed_melted$variable==itype,],
               aes(x=value,fill=PERC))+geom_density(alpha=0.6)+theme_bw()+
    geom_vline(xintercept = as.numeric(df_summary$threshold[df_summary$variable==itype]),size=1.5,linetype=2)+
    ggtitle(itype)
  
  itab=df_summary[df_summary$variable==itype,]
  
  ggsave(grid.arrange(plot3,tableGrob(itab),layout_matrix=matrix(c(1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2),4,5)),
         filename = paste0("plots/correlationModel/plot_single_",itype,".png"),width = 10,height=6)
}


## Undersampling ####


df_summaryUS=do.call(rbind,lapply(modelListUS,function(x){
  temp_coeffs=coefficients(x$model)
  data.frame(threshold=-temp_coeffs[1]/temp_coeffs[2],
             coeff_b=temp_coeffs[2],coeff_bsd=temp_coeffs[2]/x$coeff_importance$Importance[1],
             importance=x$coeff_importance$Importance[1],
             accuracy=x$confusionMatrix$overall[1],
             accuracyPV=x$confusionMatrix$overall[6],
             mcnemarPValue=x$confusionMatrix$overall[7],
             sensitivity=x$confusionMatrix$byClass[1],
             specificity=x$confusionMatrix$byClass[2],
             precision=x$confusionMatrix$byClass[3],
             recall=x$confusionMatrix$byClass[4],
             balanced_accuracy=x$confusionMatrix$byClass[11],stringsAsFactors = FALSE)
}))
df_summaryUS$variable=row.names(df_summaryUS)

df_summaryUS$threshold=format(df_summaryUS$threshold,digits=2,scientific=TRUE)
df_summaryUS$coeff_b=format(df_summaryUS$coeff_b,digits=2,scientific=TRUE)
df_summaryUS$coeff_bsd=format(df_summaryUS$coeff_bsd,digits=2,scientific=TRUE)
df_summaryUS$importance=round(df_summaryUS$importance,digits=2)
df_summaryUS$accuracy=round(df_summaryUS$accuracy,digits=2)
df_summaryUS$accuracyPV=format(df_summaryUS$accuracyPV,digits=2,scientific=TRUE)
df_summaryUS$mcnemarPValue=format(df_summaryUS$mcnemarPValue,digits=2,scientific=TRUE)
df_summaryUS$sensitivity=round(df_summaryUS$sensitivity,digits=2)
df_summaryUS$specificity=round(df_summaryUS$specificity,digits=2)
df_summaryUS$precision=round(df_summaryUS$precision,digits=2)
df_summaryUS$recall=round(df_summaryUS$recall,digits=2)
df_summaryUS$balanced_accuracy=round(df_summaryUS$balanced_accuracy,digits=2)

df_summaryUS=df_summaryUS[,c("variable","threshold","balanced_accuracy","accuracy","accuracyPV","mcnemarPValue",
                         "coeff_b","coeff_bsd","importance","sensitivity","specificity","precision","recall")]

tj_write.table(df_summaryUS,"plots/correlationModel/corr_modelsUS_single.txt")
ggsave(tableGrob(df_summaryUS),filename = "plots/correlationModel/corr_modelsUS_single.pdf",width=15)

for (itype in unique(data_classify_assessed_melted$variable)){
  plot3=ggplot(data=data_classify_assessed_melted[data_classify_assessed_melted$variable==itype,],
               aes(x=value,fill=PERC))+geom_density(alpha=0.6)+theme_bw()+
    geom_vline(xintercept = as.numeric(df_summaryUS$threshold[df_summaryUS$variable==itype]),size=1.5,linetype=2)+
    ggtitle(itype)
  
  itab=df_summaryUS[df_summaryUS$variable==itype,]
  
  ggsave(grid.arrange(plot3,tableGrob(itab),layout_matrix=matrix(c(1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2,1,1,1,2),4,5)),
         filename = paste0("plots/correlationModel/plotUS_single_",itype,".png"),width = 10,height=6)
}


  


# full_model=tj_calculate_classifier(data_main=temp_df,
#                                    d_variable="PERC",
#                                    ind_variables = as.character(unique(data_classify_assessed_melted$variable)),
#                                    maxit=5000,MaxNWts=30000)
# temp_coeffs=coefficients(full_model$model)
# 
# sink("tables/corr_models_full_confusion.txt")
# full_model$confusionMatrix
# sink()
# 
# tab=merge(full_model$coeff_importance,data.frame(coeff=temp_coeffs,variable=names(temp_coeffs)),by="variable")
# tj_write.table(tab,"tables/corr_models_full_coeffs.txt")




# full_model=tj_calculate_classifier(data_main=temp_df,
#                                    d_variable="PERC",
#                                    ind_variables = c("COG","DUR","thirdMoment"),
#                                    maxit=5000,MaxNWts=30000)
# temp_coeffs=coefficients(full_model$model)
# 
# sink("tables/corr_modelsTrunc_full_confusion.txt")
# full_model$confusionMatrix
# sink()
# 
# tab=merge(full_model$coeff_importance,data.frame(coeff=temp_coeffs,variable=names(temp_coeffs)),by="variable")
# tj_write.table(tab,"tables/corr_modelsTrunc_full_coeffs.txt")





# temp_df_scaled=data.frame(temp_df[,c("ID","id_type","PERC","perc_type")],
#                                      scale(temp_df[,-c(1:4)]))
# full_model_scaled=tj_calculate_classifier(data_main=temp_df_scaled,
#                                    d_variable="PERC",
#                                    ind_variables = as.character(unique(data_classify_assessed_melted$variable)),
#                                    maxit=5000,MaxNWts=30000)
# temp_coeffs=coefficients(full_model_scaled$model)
# 
# sink("tables/corr_models_fullScaled_confusion.txt")
# full_model_scaled$confusionMatrix
# sink()
# 
# tab=merge(full_model_scaled$coeff_importance,
#           data.frame(coeff=temp_coeffs,variable=names(temp_coeffs)),by="variable")
# tj_write.table(tab,"tables/corr_models_fullScaled_coeffs.txt")




# SVM Models  ####
# library(e1071)
# library(rpart)
# svm.model <- svm(PERC ~ DUR+COG+STDEV+SKEWNESS+KURTOSIS+thirdMoment, data = temp_df, cost = 100, gamma = 1)
# obs<-temp_df$PERC
# svm.pred <- predict(svm.model, temp_df)
# sink("tables/classif_SVM_confusion.txt")
# caret::confusionMatrix(factor(svm.pred),obs)
# sink()
# 
# 
# svm.model <- svm(PERC ~ DUR, data = temp_df, cost = 100, gamma = 1)
# obs<-temp_df$PERC
# svm.pred <- predict(svm.model, temp_df)
# sink("tables/classif_SVM_DUR_confusion.txt")
# caret::confusionMatrix(factor(svm.pred),obs)
# sink()
# 
# svm.model <- svm(PERC ~ COG, data = temp_df, cost = 100, gamma = 1)
# obs<-temp_df$PERC
# svm.pred <- predict(svm.model, temp_df)
# sink("tables/classif_SVM_COG_confusion.txt")
# caret::confusionMatrix(factor(svm.pred),obs)
# sink()
# 
# svm.model <- svm(PERC ~ SKEWNESS, data = temp_df, cost = 100, gamma = 1)
# obs<-temp_df$PERC
# svm.pred <- predict(svm.model, temp_df)
# sink("tables/classif_SVM_SKEW_confusion.txt")
# caret::confusionMatrix(factor(svm.pred),obs)
# sink()
# 
# svm.model <- svm(PERC ~ KURTOSIS, data = temp_df, cost = 100, gamma = 1)
# obs<-temp_df$PERC
# svm.pred <- predict(svm.model, temp_df)
# sink("tables/classif_SVM_KURT_confusion.txt")
# caret::confusionMatrix(factor(svm.pred),obs)
# sink()
# 
# svm.model <- svm(PERC ~ STDEV, data = temp_df, cost = 100, gamma = 1)
# obs<-temp_df$PERC
# svm.pred <- predict(svm.model, temp_df)
# sink("tables/classif_SVM_stdev_confusion.txt")
# caret::confusionMatrix(factor(svm.pred),obs)
# sink()
# 
# svm.model <- svm(PERC ~ thirdMoment, data = temp_df, cost = 100, gamma = 1)
# obs<-temp_df$PERC
# svm.pred <- predict(svm.model, temp_df)
# sink("tables/classif_SVM_thrdM_confusion.txt")
# caret::confusionMatrix(factor(svm.pred),obs)
# sink()
# 
# 
# rpart.model <- rpart(PERC ~ DUR+COG+STDEV+SKEWNESS+KURTOSIS+thirdMoment, data = temp_df)
# obs<-temp_df$PERC
# rpart.pred <- predict(rpart.model, temp_df, type = "class")
# sink("tables/classif_rpart_confusion.txt")
# caret::confusionMatrix(factor(rpart.pred),obs)
# sink()



# GLM Net Cross-validate Models  ####
#install.packages("glmnet")
dir.create("plots/correlationModelFinal/",recursive = TRUE)
library(glmnet)
temp_df=data_classify_assessed
temp_df$PERC=factor(temp_df$PERC)
temp_matrix=as.matrix(temp_df[,c("DUR","COG","STDEV","SKEWNESS","KURTOSIS","logKurtosis","thirdMoment")])
cvfit = cv.glmnet(temp_matrix, 
                  temp_df$PERC, family = "binomial", type.measure = "class")
plot(cvfit)
cvfit$lambda.min
cvfit$lambda.1se
coef(cvfit, s = "lambda.min")
pred=predict(cvfit, newx = temp_matrix, s = "lambda.min", type = "class")
obs=temp_df$PERC
sink("plots/correlationModelFinal/classifAll_glmnetCvfit_confusion.txt")
caret::confusionMatrix(factor(pred),obs)
cat("\n")
coef(cvfit, s = "lambda.min")
sink()


temp_df=data_classify_assessed
temp_df$PERC=factor(temp_df$PERC)
temp_frac=table(data_classify_assessed$PERC)
temp_frac=min(temp_frac/sum(temp_frac)  )
temp_df_undersampled=undersampling_partition(temp_df,temp_df$PERC,temp_frac)

temp_matrix=as.matrix(temp_df_undersampled[,c("DUR","COG","STDEV","SKEWNESS","KURTOSIS","logKurtosis","thirdMoment")])
cvfit = cv.glmnet(temp_matrix, 
                  temp_df_undersampled$PERC, family = "binomial", type.measure = "class")
plot(cvfit)
cvfit$lambda.min
cvfit$lambda.1se
coef(cvfit, s = "lambda.min")
pred=predict(cvfit, newx = temp_matrix, s = "lambda.min", type = "class")
obs=temp_df_undersampled$PERC
sink("plots/correlationModelFinal/classifAll_US_glmnetCvfit_confusion.txt")
caret::confusionMatrix(factor(pred),obs)
cat("\n")
coef(cvfit, s = "lambda.min")
sink()













# train-test check ####



# ## basic log reg 3 variables ##
# nn=100
# trainTest_list=list()
# for (ii in 1:nn){
#   trainTest_list[[ii]]=list()
#   temp_dfTT=sampling_partition(temp_df,temp_df$PERC,partition_trainfrac=0.6)
#   
#   tmp_full_model=tj_calculate_classifier(data_main=temp_dfTT$train,
#                                      d_variable="PERC",
#                                      ind_variables = c("COG","DUR","STDEV"),
#                                      maxit=5000,MaxNWts=30000)
#   pred=predict(tmp_full_model$model, newdata = temp_dfTT$test,  type = "class")
#   obs=temp_dfTT$test$PERC
#   
#   trainTest_list[[ii]]$coeffs=coefficients(tmp_full_model$model)
#   trainTest_list[[ii]]$confMatrTrain=tmp_full_model$confusionMatrix
#   trainTest_list[[ii]]$confMatrTest=caret::confusionMatrix(pred,obs)
# }
# 
#  
# tab=data.frame(COG_m=median(sapply(trainTest_list,function(x) x$coeffs[2])),
#            COG_sd=sd(sapply(trainTest_list,function(x) x$coeffs[2])),
#            DUR_m=median(sapply(trainTest_list,function(x) x$coeffs[3])),
#            DUR_sd=sd(sapply(trainTest_list,function(x) x$coeffs[3])),
#            STDEV_m=median(sapply(trainTest_list,function(x) x$coeffs[4])),
#            STDEV_sd=sd(sapply(trainTest_list,function(x) x$coeffs[4])),
#            acc_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$overall[1])),
#            acc_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$overall[1])),
#            sens_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$byClass[1])),
#            sens_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$byClass[1])),
#            spec_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$byClass[2])),
#            spec_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$byClass[2])),
#            balAcc_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$byClass[11])),
#            balAcc_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$byClass[11])),
#            acc_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$overall[1])),
#            acc_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$overall[1])),
#            sens_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[1])),
#            sens_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[1])),
#            spec_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[2])),
#            spec_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[2])),
#            balAcc_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[11])),
#            balAcc_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[11])))
# tj_write.table(tab,"tables/trainTestResults_3vars.txt")
# 
# 
# ## 1 variable ##
# nn=100
# trainTest_list=list()
# for (ii in 1:nn){
#   trainTest_list[[ii]]=list()
#   temp_dfTT=sampling_partition(temp_df,temp_df$PERC,partition_trainfrac=0.6)
#   
#   tmp_full_model=tj_calculate_classifier(data_main=temp_dfTT$train,
#                                          d_variable="PERC",
#                                          ind_variables = c("COG"),
#                                          maxit=5000,MaxNWts=30000)
#   pred=predict(tmp_full_model$model, newdata = temp_dfTT$test,  type = "class")
#   obs=temp_dfTT$test$PERC
#   
#   trainTest_list[[ii]]$coeffs=coefficients(tmp_full_model$model)
#   trainTest_list[[ii]]$confMatrTrain=tmp_full_model$confusionMatrix
#   trainTest_list[[ii]]$confMatrTest=caret::confusionMatrix(pred,obs)
# }
# 
# 
# tab=data.frame(COG_m=median(sapply(trainTest_list,function(x) x$coeffs[2])),
#                COG_sd=sd(sapply(trainTest_list,function(x) x$coeffs[2])),
#                DUR_m=median(sapply(trainTest_list,function(x) x$coeffs[3])),
#                DUR_sd=sd(sapply(trainTest_list,function(x) x$coeffs[3])),
#                STDEV_m=median(sapply(trainTest_list,function(x) x$coeffs[4])),
#                STDEV_sd=sd(sapply(trainTest_list,function(x) x$coeffs[4])),
#                acc_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$overall[1])),
#                acc_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$overall[1])),
#                sens_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$byClass[1])),
#                sens_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$byClass[1])),
#                spec_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$byClass[2])),
#                spec_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$byClass[2])),
#                balAcc_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$byClass[11])),
#                balAcc_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$byClass[11])),
#                acc_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$overall[1])),
#                acc_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$overall[1])),
#                sens_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[1])),
#                sens_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[1])),
#                spec_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[2])),
#                spec_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[2])),
#                balAcc_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[11])),
#                balAcc_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[11])))
# tj_write.table(tab,"tables/trainTestResults_COG.txt")
# 
# 
# 
# ## basic log reg all variables ##
# nn=100
# trainTest_list=list()
# for (ii in 1:nn){
#   trainTest_list[[ii]]=list()
#   temp_dfTT=sampling_partition(temp_df,temp_df$PERC,partition_trainfrac=0.6)
#   
#   tmp_full_model=tj_calculate_classifier(data_main=temp_dfTT$train,
#                                          d_variable="PERC",
#                                          ind_variables = c("DUR","COG","STDEV","SKEWNESS","KURTOSIS","thirdMoment"),
#                                          maxit=5000,MaxNWts=30000)
#   pred=predict(tmp_full_model$model, newdata = temp_dfTT$test,  type = "class")
#   obs=temp_dfTT$test$PERC
#   
#   trainTest_list[[ii]]$coeffs=coefficients(tmp_full_model$model)
#   trainTest_list[[ii]]$confMatrTrain=tmp_full_model$confusionMatrix
#   trainTest_list[[ii]]$confMatrTest=caret::confusionMatrix(pred,obs)
# }
# 
# 
# tab=data.frame(COG_m=median(sapply(trainTest_list,function(x) x$coeffs[3])),
#                COG_sd=sd(sapply(trainTest_list,function(x) x$coeffs[3])),
#                DUR_m=median(sapply(trainTest_list,function(x) x$coeffs[2])),
#                DUR_sd=sd(sapply(trainTest_list,function(x) x$coeffs[2])),
#                STDEV_m=median(sapply(trainTest_list,function(x) x$coeffs[4])),
#                STDEV_sd=sd(sapply(trainTest_list,function(x) x$coeffs[4])),
#                SKEW_m=median(sapply(trainTest_list,function(x) x$coeffs[5])),
#                SKEW_sd=sd(sapply(trainTest_list,function(x) x$coeffs[5])),
#                KURT_m=median(sapply(trainTest_list,function(x) x$coeffs[6])),
#                KURT_sd=sd(sapply(trainTest_list,function(x) x$coeffs[6])),
#                thrMom_m=median(sapply(trainTest_list,function(x) x$coeffs[7])),
#                thrMom_sd=sd(sapply(trainTest_list,function(x) x$coeffs[7])),
#                acc_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$overall[1])),
#                acc_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$overall[1])),
#                sens_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$byClass[1])),
#                sens_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$byClass[1])),
#                spec_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$byClass[2])),
#                spec_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$byClass[2])),
#                balAcc_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$byClass[11])),
#                balAcc_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$byClass[11])),
#                acc_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$overall[1])),
#                acc_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$overall[1])),
#                sens_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[1])),
#                sens_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[1])),
#                spec_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[2])),
#                spec_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[2])),
#                balAcc_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[11])),
#                balAcc_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[11])))
# tj_write.table(tab,"tables/trainTestResults_allVars.txt")





# GLM Net Cross-validate Models  ####
#install.packages("glmnet")
library(glmnet)

nn=100
trainTest_list=list()
for (ii in 1:nn){
  cat(paste0(ii,".."))
  trainTest_list[[ii]]=list()
  temp_dfTT=sampling_partition(temp_df,temp_df$PERC,partition_trainfrac=0.6)
  temp_matrix=as.matrix(temp_dfTT$train[,c("DUR","COG","STDEV","SKEWNESS",
                                           "KURTOSIS","logKurtosis","thirdMoment")])
  cvfit = cv.glmnet(temp_matrix, 
                    temp_dfTT$train$PERC, family = "binomial", type.measure = "class")

  predTrain=predict(cvfit, newx = as.matrix(temp_dfTT$train[,c("DUR","COG","STDEV","SKEWNESS",
                                                               "KURTOSIS","logKurtosis","thirdMoment")]), 
                    s = "lambda.min", type = "class")
  obsTrain=temp_dfTT$train$PERC
  predTest=predict(cvfit, newx = as.matrix(temp_dfTT$test[,c("DUR","COG","STDEV","SKEWNESS",
                                                             "KURTOSIS","logKurtosis","thirdMoment")]), 
                   s = "lambda.min", type = "class")
  obsTest=temp_dfTT$test$PERC

  trainTest_list[[ii]]$coeffs=coef(cvfit, s = "lambda.min")
  trainTest_list[[ii]]$confMatrTrain=caret::confusionMatrix(factor(predTrain),obsTrain)
  trainTest_list[[ii]]$confMatrTest=caret::confusionMatrix(factor(predTest),obsTest)
}


tab=data.frame(COG_m=median(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="COG"] )),
               COG_sd=sd(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="COG"] )),
               DUR_m=median(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="DUR"] )),
               DUR_sd=sd(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="DUR"] )),
               STDEV_m=median(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="STDEV"] )),
               STDEV_sd=sd(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="STDEV"] )),
               SKEW_m=median(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="SKEWNESS"] )),
               SKEW_sd=sd(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="SKEWNESS"] )),
               KURT_m=median(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="KURTOSIS"] )),
               KURT_sd=sd(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="KURTOSIS"] )),
               logKURT_m=median(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="logKurtosis"] )),
               logKURT_sd=sd(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="logKurtosis"] )),
               thrdMom_m=median(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="thirdMoment"] )),
               thrdMom_sd=sd(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="thirdMoment"] )),
               acc_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$overall[1])),
               acc_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$overall[1])),
               sens_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$byClass[1])),
               sens_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$byClass[1])),
               spec_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$byClass[2])),
               spec_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$byClass[2])),
               balAcc_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$byClass[11])),
               balAcc_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$byClass[11])),
               acc_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$overall[1])),
               acc_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$overall[1])),
               sens_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[1])),
               sens_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[1])),
               spec_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[2])),
               spec_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[2])),
               balAcc_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[11])),
               balAcc_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[11])))

tabF=tab
tabF$coeff_COG=paste0(formatSc(tabF$COG_m) ,"(SD: ", formatSc(tabF$COG_sd) ,")")
tabF$coeff_DUR=paste0(formatSc(tabF$DUR_m) ,"(SD: ", formatSc(tabF$DUR_sd) ,")")
tabF$coeff_SKEW=paste0(formatSc(tabF$SKEW_m) ,"(SD: ", formatSc(tabF$SKEW_sd) ,")")
tabF$coeff_KURT=paste0(formatSc(tabF$KURT_m) ,"(SD: ", formatSc(tabF$KURT_sd) ,")")
tabF$coeff_logKURT=paste0(formatSc(tabF$logKURT_m) ,"(SD: ", formatSc(tabF$logKURT_sd) ,")")
tabF$coeff_STDEV=paste0(formatSc(tabF$STDEV_m) ,"(SD: ", formatSc(tabF$STDEV_sd) ,")")
tabF$coeff_thrdMom=paste0(formatSc(tabF$thrdMom_m) ,"(SD: ", formatSc(tabF$thrdMom_sd) ,")")
tabF$Test_Acc_Bal=paste0(round2(tabF$balAcc_test_med) ,"(SD: ", round4(tabF$balAcc_test_sd) ,")")
tabF$Test_Acc=paste0(round2(tabF$acc_test_med) ,"(SD: ", round4(tabF$acc_test_sd ) ,")")
tabF$Train_Acc_Bal=paste0(round2(tabF$balAcc_train_med) ,"(SD: ", round4(tabF$balAcc_train_sd) ,")")
tabF$Train_Acc=paste0(round2(tabF$acc_train_med) ,"(SD: ", round4(tabF$acc_train_sd ) ,")")

tj_write.table(tabF[,31:41],"plots/correlationModelFinal/trainTestResults_GlmnetAllVars.txt")





nn=100
trainTest_list=list()
for (ii in 1:nn){
  cat(paste0(ii,".."))
  trainTest_list[[ii]]=list()
  temp_dfTT=sampling_partition(temp_df_undersampled,temp_df_undersampled$PERC,partition_trainfrac=0.6)
  temp_matrix=as.matrix(temp_dfTT$train[,c("DUR","COG","STDEV","SKEWNESS",
                                           "KURTOSIS","logKurtosis","thirdMoment")])
  cvfit = cv.glmnet(temp_matrix, 
                    temp_dfTT$train$PERC, family = "binomial", type.measure = "class")
  
  predTrain=predict(cvfit, newx = as.matrix(temp_dfTT$train[,c("DUR","COG","STDEV","SKEWNESS",
                                                               "KURTOSIS","logKurtosis","thirdMoment")]), 
                    s = "lambda.min", type = "class")
  obsTrain=temp_dfTT$train$PERC
  predTest=predict(cvfit, newx = as.matrix(temp_dfTT$test[,c("DUR","COG","STDEV","SKEWNESS",
                                                             "KURTOSIS","logKurtosis","thirdMoment")]), 
                   s = "lambda.min", type = "class")
  obsTest=temp_dfTT$test$PERC
  
  trainTest_list[[ii]]$coeffs=coef(cvfit, s = "lambda.min")
  trainTest_list[[ii]]$confMatrTrain=caret::confusionMatrix(factor(predTrain),obsTrain)
  trainTest_list[[ii]]$confMatrTest=caret::confusionMatrix(factor(predTest),obsTest)
}


tab=data.frame(COG_m=median(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="COG"] )),
               COG_sd=sd(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="COG"] )),
               DUR_m=median(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="DUR"] )),
               DUR_sd=sd(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="DUR"] )),
               STDEV_m=median(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="STDEV"] )),
               STDEV_sd=sd(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="STDEV"] )),
               SKEW_m=median(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="SKEWNESS"] )),
               SKEW_sd=sd(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="SKEWNESS"] )),
               KURT_m=median(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="KURTOSIS"] )),
               KURT_sd=sd(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="KURTOSIS"] )),
               logKURT_m=median(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="logKurtosis"] )),
               logKURT_sd=sd(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="logKurtosis"] )),
               thrdMom_m=median(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="thirdMoment"] )),
               thrdMom_sd=sd(sapply(trainTest_list,function(x) x$coeffs[row.names(x$coeffs)=="thirdMoment"] )),
               acc_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$overall[1])),
               acc_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$overall[1])),
               sens_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$byClass[1])),
               sens_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$byClass[1])),
               spec_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$byClass[2])),
               spec_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$byClass[2])),
               balAcc_test_med=median(sapply(trainTest_list,function(x) x$confMatrTest$byClass[11])),
               balAcc_test_sd=sd(sapply(trainTest_list,function(x) x$confMatrTest$byClass[11])),
               acc_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$overall[1])),
               acc_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$overall[1])),
               sens_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[1])),
               sens_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[1])),
               spec_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[2])),
               spec_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[2])),
               balAcc_train_med=median(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[11])),
               balAcc_train_sd=sd(sapply(trainTest_list,function(x) x$confMatrTrain$byClass[11])))

tabF=tab
tabF$coeff_COG=paste0(formatSc(tabF$COG_m) ,"(SD: ", formatSc(tabF$COG_sd) ,")")
tabF$coeff_DUR=paste0(formatSc(tabF$DUR_m) ,"(SD: ", formatSc(tabF$DUR_sd) ,")")
tabF$coeff_SKEW=paste0(formatSc(tabF$SKEW_m) ,"(SD: ", formatSc(tabF$SKEW_sd) ,")")
tabF$coeff_KURT=paste0(formatSc(tabF$KURT_m) ,"(SD: ", formatSc(tabF$KURT_sd) ,")")
tabF$coeff_logKURT=paste0(formatSc(tabF$logKURT_m) ,"(SD: ", formatSc(tabF$logKURT_sd) ,")")
tabF$coeff_STDEV=paste0(formatSc(tabF$STDEV_m) ,"(SD: ", formatSc(tabF$STDEV_sd) ,")")
tabF$coeff_thrdMom=paste0(formatSc(tabF$thrdMom_m) ,"(SD: ", formatSc(tabF$thrdMom_sd) ,")")
tabF$Test_Acc_Bal=paste0(round2(tabF$balAcc_test_med) ,"(SD: ", round4(tabF$balAcc_test_sd) ,")")
tabF$Test_Acc=paste0(round2(tabF$acc_test_med) ,"(SD: ", round4(tabF$acc_test_sd ) ,")")
tabF$Train_Acc_Bal=paste0(round2(tabF$balAcc_train_med) ,"(SD: ", round4(tabF$balAcc_train_sd) ,")")
tabF$Train_Acc=paste0(round2(tabF$acc_train_med) ,"(SD: ", round4(tabF$acc_train_sd ) ,")")

tj_write.table(tabF[,c(31:41)],"plots/correlationModelFinal/trainTestResults_USGlmnetAllVars.txt")





#### Main Model ####
#install.packages("glmnet")
library(glmnet)
temp_df=data_classify_assessed
temp_df$PERC=factor(temp_df$PERC)
temp_frac=table(temp_df$PERC)
temp_frac=min(temp_frac/sum(temp_frac)  )
temp_df=undersampling_partition(temp_df,temp_df$PERC,temp_frac)

temp_matrix=as.matrix(temp_df[,c("DUR","COG","STDEV","SKEWNESS","KURTOSIS","logKurtosis","thirdMoment")])
cvfit = cv.glmnet(temp_matrix, 
                  temp_df$PERC, family = "binomial", type.measure = "class")

png("plots/correlationModelFinal/main_model_cv.png",width=800,height=640)
plot(cvfit)
dev.off()

coeffs_relative=coef(cvfit, s = "lambda.min")[,1]*c(1,apply(temp_matrix,2,function(x) mean(abs(x)) ))
sink(paste0("plots/correlationModelFinal/main_model.txt"))
cat(paste0("lambda: ",cvfit$lambda.min,"\n"))
cat("\n Coeffs Absolute\n")
print(coef(cvfit, s = "lambda.min"))
cat("\n Coeffs Relative\n")
print(coeffs_relative[-1])
cat("\n Relative Importance\n")
abs(coeffs_relative[-1])/max(abs(coeffs_relative[-1]))
sink()



#Prediction of vowels
main_data_predicted=main_data
matrix_to_predict=as.matrix(main_data_predicted[,c("DUR","COG","STDEV","SKEWNESS",
                                                   "KURTOSIS","logKurtosis","thirdMoment")])
main_data_predicted$PERC_predicted=predict(cvfit, newx = matrix_to_predict, s = "lambda.min", type = "class")
main_data_predicted$PERC_predicted[main_data_predicted$perc_type=="assessed"]=main_data_predicted$PERC[main_data_predicted$perc_type=="assessed"]

main_data_predicted2=main_data_predicted
main_data_predicted2$PERC_predicted[is.na(main_data_predicted2$PERC_predicted)]="elision"
main_data_predicted=main_data_predicted[!is.na(main_data_predicted$PERC_predicted),]

main_data_predicted$PERC_predicted=as.character(main_data_predicted$PERC_predicted)
main_data_predicted2$PERC_predicted=as.character(main_data_predicted2$PERC_predicted)

saveRDS(main_data_predicted,"data/main_data_predicted.rds")
saveRDS(main_data_predicted2,"data/main_data_predicted_withElision.rds")
tj_write.table(main_data_predicted,"data/main_data_predicted.txt")
tj_write.table(main_data_predicted2,"data/main_data_predicted_withElision.txt")


main_data_predicted=readRDS("data/main_data_predicted.rds")
main_data_predicted2=readRDS("data/main_data_predicted_withElision.rds.")

#### Exploration of variables ####
library(corrplot)
library(grid)
library(gridExtra)
library(nonpar)
library(DescTools)

colsG1=c("#ef8a62","#67a9cf","#af8dc3")

tj_explore_NumVsCategory<-function(x,y,dir_output,nname,nnameGroup){
  
  cols1=colsG1[1:length(unique(y))]
  dir.create(dir_output,recursive = TRUE)
  temp_df=data.frame(x=x,y=y)
  
  df_summary=do.call(rbind,by(temp_df,temp_df$y,function(x){
    tj_summary_numericAux(x$x)
  }))
  df_summary$var=row.names(df_summary)
 
  anova_test=oneway.test(data = temp_df,x~y)
  kruskal_test=kruskal.test(data = temp_df,x~y)
  
  if (length(unique(temp_df$y))==2){
    effect_size=round2(abs(mean(temp_df$x[temp_df$y==unique(temp_df$y)[1]])-
                      mean(temp_df$x[temp_df$y==unique(temp_df$y)[2]]))/sd(temp_df$x))
  } else {
    effect_size="" 
  }
  
  df_summary$param_testPV=formatSc(anova_test$p.value)
  df_summary$nonparam_testPV=formatSc(kruskal_test$p.value)
  df_print=df_summary[,c("var","q50","mean","sd","min","max","nsample")]
  colnames(df_print)<-c("Group","median","mean","SD","min","max","nsample")
  
  tj_write.table(df_summary,paste0(dir_output,"/summaryTable_",nname,".txt"))
  
  textGG=textGrob(nname)
  emptyGG=textGrob("")
  plot1=ggplot(data=temp_df,aes(y=x,x=y,group=y,colour=y))+geom_boxplot()+theme_bw()+
    scale_x_discrete(nnameGroup)+scale_y_continuous(nname)+scale_color_manual(nnameGroup,values = cols1)
  plot2=ggplot(data=temp_df,aes(y=x,x=y,group=y,colour=y))+geom_violin(size=2)+theme_bw()+
    scale_x_discrete(nnameGroup)+scale_y_continuous(nname)+scale_color_manual(nnameGroup,values = cols1)
  plot3=ggplot(data=temp_df,aes(y=..density..,x=x,fill=y,colour=y))+geom_density(alpha=0.4)+theme_bw()+
    scale_fill_manual(nnameGroup,values = cols1)+scale_x_continuous(nname)+scale_color_manual(nnameGroup,values = cols1)
  tableGG=tableGrob(df_print)
  ggpv1=textGrob(paste0("Parametric Test: ", df_summary$param_testPV))
  ggpv2=textGrob(paste0("Non-Parametric Test: ", df_summary$nonparam_testPV))
  gges=textGrob(paste0("Effect Size: ", effect_size))
  
  plotAll=grid.arrange(textGG,plot1,plot2,plot3,tableGG,ggpv1,ggpv2,emptyGG,gges,
                       layout_matrix=matrix(c(1,2,2,2,2,5,5,5,
                                              1,2,2,2,2,5,5,5,
                                              1,2,2,2,2,5,5,5,
                                              1,2,2,2,2,5,5,5,
                                              8,3,3,3,3,5,5,5,
                                              8,3,3,3,3,5,5,5,
                                              8,3,3,3,3,5,5,5,
                                              8,3,3,3,3,5,5,5,
                                              8,4,4,4,4,6,7,9,
                                              8,4,4,4,4,6,7,9,
                                              8,4,4,4,4,6,7,9,
                                              8,4,4,4,4,6,7,9),
                                            8,12))
  ggsave(plotAll,filename =paste0(dir_output,"/mainPlot_",nname,".png"),width = 12,height=8)
  
  1
}



tj_explore_CatVsCategory<-function(x,y,dir_output,nname,nnameGroup){
  
  cols1=colsG1[1:length(unique(y))]
  dir.create(dir_output,recursive = TRUE)
  temp_df=data.frame(x=x,y=y)
  
  temp_table=table(temp_df$x,temp_df$y)
  temp_tableDF=dcast(data.frame(temp_table),Var1~Var2,value.var="Freq")
  colnames(temp_tableDF)[1]=nname
  colnames(temp_tableDF)[-1]=paste0(nnameGroup,"_",colnames(temp_tableDF)[-1])
  tj_write.table(temp_tableDF,paste0(dir_output,"/countTable_",nname,".txt"))
  
  if (nrow(temp_table)>1&ncol(temp_table)>1){
    fisher_test=formatSc(fisher.test(temp_table,simulate.p.value = TRUE)$p.value)
    Mantel_Haenszel_test=formatSc(MHChisqTest(temp_table)$p.value)
    G_test=formatSc(GTest(temp_table)$p.value)
    chisq_test=formatSc(chisq.test(temp_table)$p.value)
    
    logreg=tj_calculate_classifier(data_main = temp_df,ind_variables = "x" ,d_variable = "y")
    
    if (class(logreg$confusionMatrix)=="try-error"){
      logreg_test=""
    } else {
      logreg_test=paste0("Acc: ",round(logreg$confusionMatrix$overall[1],digits=2),",  ",
                         "AccPV:",formatSc(logreg$confusionMatrix$overall[6]),",  ",
                         "Bal.Acc.:",round(logreg$confusionMatrix$byClass[11],digits=2))
    }
    
  } else {
    fisher_test=NA
    logreg_test=NA
    Mantel_Haenszel_test=NA
    G_test=NA
    chisq_test=NA
  }
  
  temp_freq1=do.call(rbind,lapply(unique(temp_df$x),function(xx){
    x=temp_df[temp_df$x==xx,]
    data.frame(varBy=xx,table(x$y)/sum(table(x$y)))
  }))
  colnames(temp_freq1)<-c(nname,nnameGroup,"freq")
  
  temp_freq2=do.call(rbind,lapply(unique(temp_df$y),function(xx){
    x=temp_df[temp_df$y==xx,]
    data.frame(varBy=xx,table(x$x)/sum(table(x$x)))
  }))
  colnames(temp_freq2)<-c(nnameGroup,nname,"freq")
  
  
  textGG=textGrob(paste0(nname," vs ", nnameGroup))
  textGGE=textGrob("")
  textTest_2=textGrob(paste0("Fishers Test: ",fisher_test ))
  textTest_3=textGrob(paste0("Mantel-Haenszel Test: ",Mantel_Haenszel_test ))
  textTest_4=textGrob(paste0("G Test: ",G_test ))
  textTest_1=textGrob(paste0("Chi-Sq Test: ",chisq_test ))
  textTest_5=textGrob(paste0("LogReg Test: ",logreg_test ))
  plot1=ggplot(data=temp_df,aes(x=x,group=y,fill=y))+stat_count(position = "dodge")+theme_bw()+
    scale_x_discrete(nname)+scale_fill_manual(nnameGroup,values = cols1)
  plot2=ggplot(data=temp_df,aes(x=y,group=x,fill=x))+stat_count(position = "dodge")+theme_bw()+
    scale_fill_brewer(nname,palette="Dark2")+scale_x_discrete(nnameGroup)
  plot3=ggplot(data=temp_freq2,aes_string(y="freq",x=nname,group=nnameGroup,fill=nnameGroup))+
    geom_col(position = "dodge")+theme_bw()+
    scale_x_discrete(nname)+scale_fill_manual(nnameGroup,values =cols1)+ggtitle(paste0("Relative to ",nnameGroup))
  plot4=ggplot(data=temp_freq1,aes_string(y="freq",x=nnameGroup,group=nname,fill=nname))+
    geom_col(position = "dodge")+theme_bw()+
    scale_x_discrete(nnameGroup)+scale_fill_brewer(nname,palette="Dark2")+ggtitle(paste0("Relative to ",nname))
  
  textGG_t1=textGrob("Abs Counts")
  textGG_t3=textGrob(paste0("Relat. Counts within ",nname))
  textGG_t4=textGrob(paste0("Relat. Counts within ",nnameGroup))
  textGG_t2=textGrob(paste0("Relat. Counts"))
  
  tabGG1=tableGrob(temp_table)
  tabGG2=tableGrob(round2(temp_table/sum(temp_table)))
  
  
  
  temptab=dcast(temp_freq1,as.formula(paste0(nname,"~",nnameGroup)),value.var="freq")
  temptab[,-1]=round2(temptab[,-1])
  colnames(temptab)[1]="var"
  tabGG3=tableGrob(temptab)
  
  temptab=dcast(temp_freq2,as.formula(paste0(nname,"~",nnameGroup)),value.var="freq")
  temptab[,-1]=round2(temptab[,-1])
  colnames(temptab)[1]="var"
  tabGG4=tableGrob(temptab)
  
  
  plotAll=grid.arrange(textGG,textGGE,textGG_t1,textGG_t2,textGG_t3,textGG_t4,
                       plot1,plot2,plot3,plot4,
                       tabGG1,tabGG2,tabGG3,tabGG4,
                       textTest_1,textTest_2,textTest_3,textTest_4,textTest_5,
                       layout_matrix= matrix(c(1,7,7,7,7,9,9,9,9,3,11,11,11,11,11,15,17,
                                               1,7,7,7,7,9,9,9,9,3,11,11,11,11,11,15,17,
                                               1,7,7,7,7,9,9,9,9,4,12,12,12,12,12,16,18,
                                               1,7,7,7,7,9,9,9,9,4,12,12,12,12,12,16,18,
                                               2,8,8,8,8,10,10,10,10,5,13,13,13,13,13,19,2,
                                               2,8,8,8,8,10,10,10,10,5,13,13,13,13,13,19,2,
                                               2,8,8,8,8,10,10,10,10,6,14,14,14,14,14,19,2,
                                               2,8,8,8,8,10,10,10,10,6,14,14,14,14,14,19,2),17,8))
  ggsave(plotAll,filename = paste0(dir_output,"/mainPlot_",nname,".png"),width = 10,height=14)
  
  1
}



tj_summary_numericAux<-function(x){
    x=x[!is.na(x)]
    if (length(x)>3){
    data.frame(mean=round2Sc(mean(x)),sd=round2Sc(sd(x)),mad=round2Sc(mad(x)),
               min=round2Sc(min(x)),max=round2Sc(max(x)),
               q01=round2Sc(quantile(x,probs = 0.01)),
               q05=round2Sc(quantile(x,probs = 0.05)),
               q25=round2Sc(quantile(x,probs = 0.25)),
               q50=round2Sc(quantile(x,probs = 0.50)),
               q75=round2Sc(quantile(x,probs = 0.75)),
               q95=round2Sc(quantile(x,probs = 0.95)),
               q99=round2Sc(quantile(x,probs = 0.99)),
               nsample=length(x),stringsAsFactors = FALSE
               )
    } else {
      data.frame(mean=NA,sd=NA,mad=NA,
                 min=NA,max=NA,
                 q01=NA,
                 q05=NA,
                 q25=NA,
                 q50=NA,
                 q75=NA,
                 q95=NA,
                 q99=NA,
                 nsample=length(x),stringsAsFactors = FALSE
      )      
    }
}

tj_summary_numeric<-function(df){
  do.call(rbind,lapply(colnames(df),function(xn){
    x=df[[xn]]
    data.frame(varnum=xn,tj_summary_numericAux(x))
  }))
}

tj_summary_numericBY<-function(df,byvar){
  do.call(rbind,lapply(colnames(df),function(xn){
    outdf=data.frame(varnum=xn,do.call(rbind,by(df,byvar,function(y){
      x=y[[xn]]
      x=as.numeric(x)
      data.frame(tj_summary_numericAux(x),stringsAsFactors = FALSE)
    })),stringsAsFactors = FALSE)
    outdf$groupname=row.names(outdf)
    outdf
  }))
}




tj_explore_variableDependence<-function(df,df_var,dir_output="output/"){
  
  dir.create(dir_output,recursive = TRUE)
  df_classes=sapply(df,class)
  group_variable=df[[df_var]]
  numeric_columns_id = df_classes%in%c("numeric","integer")
  numeric_columns=colnames(df)[numeric_columns_id]
  categorical_columns_id = df_classes%in%c("character","factor")
  categorical_columns=colnames(df)[categorical_columns_id]
  df_numeric=df[,numeric_columns_id,with=FALSE]
  
  
  ## SUMMARY ##
  cat("Summaries..")
  sum_numeric=tj_summary_numeric(df_numeric)
  tj_write.table(sum_numeric,paste0(dir_output,"/summary_numeric.txt"))
  sum_numericBY=tj_summary_numericBY(df_numeric,byvar = group_variable)
  tj_write.table(sum_numericBY,paste0(dir_output,"/summary_numericBY.txt"))
  

  sink(paste0(dir_output,"/summary_category.txt"))
    print(lapply(df[,categorical_columns_id,with=FALSE],function(x){
      tabout=tj_sort_table(x)
      tabout[1:min(length(tabout),10)]
    }))
  sink()
  
  # Correlations and PCAs ##
  cat("Corrs and pcas..")
  dir.create(paste0(dir_output,"/corr_and_pca/"),recursive = TRUE)

  png(paste0(dir_output,"/corr_and_pca/corr_matrix_spearman.png"),width=800,height=800)
   corrplot.mixed(cor(df_numeric,method = 'spearman',use = "pairwise.complete.obs"),tl.pos = 'lt')
  dev.off()
  png(paste0(dir_output,"/corr_and_pca/corr_matrix_pearson.png"),width=800,height=800)
    corrplot.mixed(cor(df_numeric,method = 'pearson',use = "pairwise.complete.obs"),tl.pos = 'lt')
  dev.off()

  id_noNA=(!apply(df_numeric,1,function(x) any(is.na(x))))
  pca_data=prcomp(df_numeric[id_noNA,],center=TRUE,scale. = TRUE)
  tabSD=data.frame(type=c("sdev","sdev_frac","var_frac"),rbind(pca_data$sdev,
                                                             cumsum(pca_data$sdev)/sum(pca_data$sdev),
                                                             cumsum(pca_data$sdev^2)/sum(pca_data$sdev^2)))
  tj_write.table(tabSD,paste0(dir_output,"/corr_and_pca/pca_sd.txt"))

  df_temp=data.frame(pca_data$rotation)
  df_temp$variable=row.names(df_temp)
  tj_write.table(df_temp,paste0(dir_output,"/corr_and_pca/pca_rotations.txt"))

  plot_data=data.frame(pca_data$x,varby=group_variable[id_noNA],stringsAsFactors = FALSE)
  plot1=ggplot(data=plot_data,aes(x=PC1,y=PC2,colour=varby))+geom_point(alpha=0.2)+theme_bw()
  plot2=ggplot(data=plot_data,aes(x=PC2,y=PC3,colour=varby))+geom_point(alpha=0.2)+theme_bw()
  plot3=ggplot(data=plot_data,aes(x=PC1,y=PC3,colour=varby))+geom_point(alpha=0.2)+theme_bw()
  plot1b=ggplot(data=plot_data,aes(x=varby,y=PC1,colour=varby))+geom_boxplot(alpha=0.9)+theme_bw()
  plot2b=ggplot(data=plot_data,aes(x=varby,y=PC2,colour=varby))+geom_boxplot(alpha=0.9)+theme_bw()
  plot3b=ggplot(data=plot_data,aes(x=varby,y=PC3,colour=varby))+geom_boxplot(alpha=0.9)+theme_bw()
  ggsave(grid.arrange(plot1,plot2,plot3,plot1b,plot2,plot3b,ncol=3,nrow=2),
         filename=paste0(dir_output,"/corr_and_pca/pca_1_3.png"),width=15,height=20)
  # #

  ## Numericals comparisons ##
  cat("Numerical variables..")
  for (ivarnum in numeric_columns){
    x_temp=df[[ivarnum]]
    try(tj_explore_NumVsCategory(x=x_temp,y=group_variable,
                                 dir_output=paste0(dir_output,"/numVariables/"),
                                 nname=ivarnum,nnameGroup=df_var))
  }
  # # 
  
  table_lengths=sapply(df[,categorical_columns_id,with=FALSE],function(x) length(table(x)))
  cat("Categorical variables..")
  for (ivarcat in categorical_columns[table_lengths<12]){
    x_temp=df[[ivarcat]]
    try(tj_explore_CatVsCategory(x=x_temp,y=group_variable,
                                 dir_output=paste0(dir_output,"/CatVariables/"),
                                 nname=ivarcat,nnameGroup=df_var))
  }
  
  
  1
}

tj_explore_variableDependence(df=main_data_predicted,
                              df_var="PERC_predicted",
                              dir_output="plots/output_varExplore/")

tj_explore_variableDependence(df=main_data_predicted2,
                              df_var="PERC_predicted",
                              dir_output="plots/output_varExplore_withElison/")










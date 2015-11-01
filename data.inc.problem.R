#Propose a project to do while at The Data Incubator. We want to know about your ability to think at a high level. Try to think of projects that users or businesses will care about (as opposed to academic projects that only researchers will care about). The project does not have to be completely novel. Here are some useful links about data sources on our blog (Post 1 and Post 2).

gem.2014<-read.table("clipboard",header=T,row.names=1,sep="\t")
gsub(" ", "",rownames(gem.2014), fixed = TRUE)
gsub(" ", "_",colnames(gem.2014), fixed = TRUE)
par(mfrow=c(2,2))
plot(lm(R_and_D_Transfer~Post_school_entrepreneurial_education_and_training+Governmental_programs+Commercial_and_professional_infrastructure, data=gem.2014))
pheatmap(gem.2014,cluster_cols=F,show_colnames=F)
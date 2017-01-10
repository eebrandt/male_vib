### Codes for generating Figures 2 and 3 in Rosenthal et al. manuscript

library(igraph)
library(assortnet)
library(PCIT)
require(ggplot2);require(qgraph)

#load datasets
present<-read.csv("https://www.dropbox.com/s/hp5kr2ikvk47ruy/Present.csv?dl=1")  
absent<-read.csv("https://www.dropbox.com/s/jhy6w3efl0mzhmc/absent.csv?dl=1") 

#Separate into treatments
light.pres<-subset(present,Light.Dark=="L")
dark.pres<-subset(present,Light.Dark=="D")
light.abs<-subset(absent,Light.Dark=="L")
dark.abs<-subset(absent,Light.Dark=="D")

#make the datasets into a list for faster processing with lapply
dats=list(light.abs, dark.abs, light.pres, dark.pres)
names(dats)=c("light.abs", "dark.abs", "light.pres", "dark.pres")

#create correlation matrices
cor.mat=lapply(dats, function(x) {
  cm<-cor(x[,-c(1:3,17)], method="spearman", use="pairwise.complete.obs")
  diag(cm)=0
  return(cm)
})

#A function for constructing PCIT graph
run_pcit=function(x){
  meaningful=pcit(x)$idx
  unmeaningful<-1:length(x)
  unmeaningful<-unmeaningful[-meaningful]
  revised<-x
  revised[unmeaningful]<-0
  return(revised)
}

#use the above function to create matrix for PCIT graph
pcit.mats=lapply(cor.mat, run_pcit)
for (i in 1:length(pcit.mats)){
  rownames(pcit.mats[[i]])=colnames(pcit.mats[[i]])=
    c("Mass", "TDar", "BdyS", "#Thm", "#Tap", "BtPe", "BtDu", "GrPe", "GrDu", "P/Gr", 
      "#Bts", "#Grp", "Gr/B")
}
pcit.simple=lapply(pcit.mats, function(x){
  a=abs(x)
  a[a>0]=1
  return(a)})

#create network for each treatment
graphs=lapply(pcit.mats, function(x) graph_from_adjacency_matrix(abs(x), "undirected", weighted=T))

#now set up layout for plots
set.seed(3)
lay.list=lapply(graphs, layout_with_fr)
names(lay.list)=c('l.al', 'l.ad', 'l.pl', 'l.pd')

#add pc loadings for each network
pcload.al=c(-0.06, 0.04, -0.08, 0.91, 0.91, -0.83, -0.88, -0.58, 0.48, 0.32, 0.93, 0.46, -0.82)
pcload.ad=c(0.11, 0.03, 0.05, -0.05, 0.04, 0.20, -0.05, 0.81, 0.72, 0.73, 0.01, -0.65, -0.32)
pcload.pl=c(-0.12, -0.05, -0.02, 0.92, 0.81, -0.47, -0.03, -0.22, 0.17, -0.01, 0.88, 0.89, -0.02)
pcload.pd=c(-0.02, 0.08,-0.05, 0.59, 0.43, -0.62, -0.16, -0.75, 0.07, -0.05, 0.64, 0.90, 0.05)

#define shapes and sizes
shps<-c("square","triangle","square",rep("circle",10))
size=8

#plot networks with legends
par.default=par(no.readonly=T)
par(mfrow=c(2,2), mar=c(2,2,2,2))
qg.al=qgraph(abs(pcit.mats$light.abs), layout=lay.list$l.al, edge.color="black", shape=shps, fade=F, label.font=2, vsize=size, vsize2=size, color=rgb(1,1-abs(pcload.al),1-abs(pcload.al)))
mtext("Light &
      Absent",side=3,line=-.5,at=-1.05,cex=1.,font=4)
mtext("n=65",side=3,line=-1.5,at=-1.05,cex=.9,font=2)

qg.ad=qgraph(abs(pcit.mats$dark.abs), layout=lay.list$l.ad, edge.color="black", shape=shps, fade=F, label.font=2, vsize=size, vsize2=size, color=rgb(1,1-abs(pcload.ad),1-abs(pcload.ad)))
mtext("Dark &
      Absent",side=3,line=-.5,at=-1.05,cex=1.,font=4)
mtext("n=52",side=3,line=-1.5,at=-1.05,cex=.9,font=2)

qg.pl=qgraph(abs(pcit.mats$light.pres), layout=lay.list$l.pl, edge.color="black", shape=shps, fade=F, label.font=2, vsize=size, vsize2=size, color=rgb(1,1-abs(pcload.pl),1-abs(pcload.pl)))
mtext("Light &
      Present",side=3,line=-.5,at=-1.05,cex=1,font=4)
mtext("n=61",side=3,line=-1.5,at=-1.05,cex=.9,font=2)

qg.pd=qgraph(abs(pcit.mats$dark.pres), layout=lay.list$l.pd, edge.color="black", shape=shps, fade=F, label.font=2, vsize=size, vsize2=size, color=rgb(1,1-abs(pcload.pd),1-abs(pcload.pd)))
mtext("Dark &
      Present",side=3,line=-.5,at=-1.05,cex=1,font=4)
mtext("n=51",side=3,line=-1.5,at=-1.05,cex=.9,font=2)

par=par.default

#Apply the walktrap community detection algorithm to calculate modularity
coms=lapply(graphs, walktrap.community)
emp.mods=sapply(coms, modularity)

#Implement bootstrap (1,000x) to generate confidence intervals for modularity of each network. Here, we are running the bootstrap and saving the outputs in a 1,000 x 4 matrix of modularity measures. 

times=1000
mod=matrix(nrow=times, ncol=4)

for(i in 1:4){
  for (j in 1:times){
    s=sample(1:nrow(dats[[i]]), nrow(dats[[i]]), replace=T)
    newdat=dats[[i]][s,]
    cm=cor(newdat[,-c(1:3)], method="spearman", use="pairwise.complete.obs")
    diag(cm)=0
    pmat=run_pcit(cm)
    g=graph_from_adjacency_matrix(abs(pmat), "undirected", weighted=T)
    mod[j,i]=modularity(walktrap.community(g))
  }
}

#generate dataset of empirical modularity and bootstrap results, and plot results

library(reshape)
moddat=melt(mod)
names(moddat)=c("iteration", "treatment", "modularity")
moddat$treatment=factor(moddat$treatment)

moddat.split=split(moddat, moddat$treatment)
means=sapply(moddat.split, function(x) mean(x$modularity))
upper=sapply(moddat.split, function(x) quantile(x$modularity, probs=0.975))
lower=sapply(moddat.split, function(x) quantile(x$modularity, probs=0.025))
dp=data.frame(treatment=ordered(c("Abs/Light", "Abs/Dark", "Pres/Light", "Pres/Dark"), levels=c("Abs/Light", "Abs/Dark", "Pres/Light", "Pres/Dark")), emp.mods=emp.mods, means=means, upper=upper, lower=lower)
p=ggplot(dp, aes(x=treatment, y=means))
p+geom_point(data=dp, aes(x=treatment, y=emp.mods),size=5) +geom_errorbar(data=dp,aes(ymin=lower, ymax=upper), width=0, size=1)+theme_bw()+theme(text=element_text(size=20),axis.text=element_text(size=20), panel.grid.major=element_blank(), panel.grid.minor=element_blank())

dp
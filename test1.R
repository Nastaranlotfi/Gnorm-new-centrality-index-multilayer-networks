library(multinet)
library(igraph)
library(plyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(kableExtra)
library(akima)
library(plot3D)
library(CINNA)
library(corrgram)
library(png)
library(pheatmap)


# Set the working directory automatically to the source file location 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list= ls())

source("Aux_functions.R", encoding="utf-8")

data = read.csv("links_clean.csv", header=T, as.is=T)

# Check the data
head(data)
tail(data)

#Generating link file
Frui1 <- list()
Frui2 <- list()
Frui3 <- list()
Frui4 <- list()
Neg1 <- list()
Neg2 <- list()
Neg3 <- list()
Neg4 <- list()

leng<-dim(data)[1]
k2=1
k1=1
for (i in 1:leng) {
	if(data[[i,3]]=="Frugivory"){
		Frui1[k1]<-c(data[[i,1]])
		Frui2[k1]<-c(data[[i,2]])
		Frui3[k1]<-c(1)
		Frui4[k1]<-c(data[[i,3]])
		#cat('hi',"\n")
		k1=k1+1
		}#end if 1
	if(data[[i,3]]=="Nectarivory"){
		Neg1[k2]<-c(data[[i,1]])
		Neg2[k2]<-c(data[[i,2]])
		Neg3[k2]<-c(2)
		Neg4[k2]<-c(data[[i,3]])
		#cat('he',"\n")
		k2=k2+1
		}#end if 2
		
	}#end for


Frui<-list()
Neg<-list()
Frui<-cbind(Frui1,Frui2,Frui3,Frui4)
Neg<-cbind(Neg1,Neg2,Neg3,Neg4)

Tot<-rbind(Frui,Neg)
colnames(Tot) <- c("from","to", "layer_num", "layer")

# Check the link list
dim(Tot)
head(Tot)
tail(Tot)

###############################################

#Generating Name file
name1=unique(data$CurrentBatSpecies)
name1<- name1[order(name1) ]

Fa1=rep("Bats",length(name1))
Fa2=rep(1,length(name1))
Fa3=rep(1,length(name1))

name2=unique(data$CurrentPlantSpecies)
name2<- name2[order(name2) ]

Na1=rep("Plants",length(name2))
Na2=rep(2,length(name2))
Na3=rep(1,length(name2))


Fa<-cbind(name1,Fa1,Fa2,Fa3)
Na<-cbind(name2,Na1,Na2,Na3)

Names<-rbind(Fa,Na)
colnames(Names) <- c("name","taxon","taxon.label","species.size")

# Checke the node list
dim(Names)
head(Names)
tail(Names)

write.csv(Names,"nodes.csv", row.names = FALSE)
write.csv(Tot,"links.csv", row.names = FALSE)

cat('end_names_construction', "\n")

############################################################################################
############################################################################################
#####G_Analysis

nodes = read.csv("nodes.csv", header=T, as.is=T)
links = read.csv("links.csv", header=T, as.is=T)

##sorting nodes to be in order
nodes = nodes[order(nodes$name),] 

##Network construction
net_multinet = Convert_to_Multinet(nodes, links)

##Parameters regarding the partitioning and Omega and Gamma and number of iteration (for getting the mean)
partitions_of_omega = 10 # Number of partitions
seq_G = Create_seq_G_Merged(net_multinet, partitions_of_omega)
vec_W = Create_vec_W(partitions_of_omega)
gamma_min = 0.25
gamma_max = 4
gamma_spacing = 0.25
gammas = seq(from = gamma_min, to = gamma_max, by = gamma_spacing)
iterations = 100


##Saving lists definition
Seq_G_Mean_gamma_list = list() #different datasets of Seq_G_MeanG_norm_ordered_list = list() #guarda os diferentes nohs selecionados para plot
G_norm_list = list()


## G_analysis

cont_perc = 1 # Calculation of running progress

for (gamma_index in 1:length(gammas)) {
  	seq_G_list = list()
    	for (i in 1:iterations) {
    		seq_G_list[[i]] = Create_seq_G_Merged(net_multinet, partitions_of_omega, gamma = gammas[gamma_index])
    		cat(cont_perc*100/(iterations*length(gammas)), "%  ")###print the run progress
    		cont_perc = cont_perc + 1
  		}#end of iterations
  #----
  #Removing names
  	seq_G_list_no_names = list()
  	for (i in 1:length(seq_G_list)) {
		seq_G_list_temp = seq_G_list[[i]]
		seq_G_list_temp[,1] = 1
		seq_G_list_no_names[[i]] = seq_G_list_temp
  		}#end of seg_G_list
  
  #Summation of Gvalues during the iteration
  	seq_G_sum = seq_G_list_no_names[[1]]
	for (i in 2:length(seq_G_list)) {
		seq_G_sum = seq_G_sum + seq_G_list_no_names[[i]]
		}#end of sum for 100 iterations
		#seq_G_sum
  
  	#Finding the mean-G_value over iteration
	seq_G_mean = seq_G_sum / iterations
  
  	#Adding names
	seq_G_mean[,1] = seq_G_list[[1]]$actor
  
  	#STD-calculation
	seq_G_StdDev = StdDev_list_of_seq_G(seq_G_list)
  
  	#Sorting with G_norm
	nodes_G_norm = Sort_Nodes_by_Total_G(seq_G_mean, ordered = FALSE)
	nodes_G_norm_Ordered = Sort_Nodes_by_Total_G(seq_G_mean, ordered = TRUE)

  
  	#Saving G_values respect to gamma
	Seq_G_Mean_gamma_list[[gamma_index]] = cbind(seq_G_mean, gammas[gamma_index])
	G_norm_list[[gamma_index]] = nodes_G_norm
  
	}#end of gamma



##Finding mean over Gamma
G_norm_sum = G_norm_list[[1]]
for (i in 2:length(G_norm_list)) {
	G_norm_sum = G_norm_sum + G_norm_list[[i]]
	}
G_norm_mean = G_norm_sum / (length(G_norm_list))

##Sorting G_norm_mean
G_norm_mean_ordered =  sort(G_norm_mean, decreasing = TRUE)



save(gammas, vec_W, iterations, partitions_of_omega, links, nodes, Seq_G_Mean_gamma_list,G_norm_mean, G_norm_mean_ordered, file = "/home/nastaran/Downloads/marco/new_data/Bat_Net.RData")


cat('end_Gnorm', "\n")



#########################################################################
#########################################################################
####Plot sample of number of modules and modularity value for one run

partitions_of_omega1 = 4 
gamma_min1 = 0.5
gamma_max1 = 3.5
gamma_spacing1 = 0.5

plots=Plot_number_modularity(partitions_of_omega1,gamma_min1,gamma_max1,gamma_spacing1,net_multinet)


#########################################################################
#########################################################################
####Plot G_norm Frequency

G_plot<-G_norm_mean ##saving in new name for not making changes in main results
names(G_plot)<-NULL ##removing the names
df<-unlist(G_plot) ##changing the matrix into one vector
hist(df,breaks=5,col="darkmagenta",xlim=c(1,2),main="Distribution of Gnorm",,xlab='G_norm')##hist plot of frequency


#########################################################################
#########################################################################
####Plot Selected nodes with G-norm: two max, one near to mean, and one lower than mean of G-norm

seq_Gnorm_gamma_mean = Unite_list_of_dataframes(Seq_G_Mean_gamma_list)
selection = Select_Example_Nodes(G_norm_mean_ordered)##function that finds the nodes we are interested
for (i in 1:length(selection)) {
  	
  	chosen_node = names(selection[i])
  	jpg_name = paste(names(selection[i]), "_2d.jpg", sep = "")
  	jpeg(jpg_name, width = 700, height = 700)
  	plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	plot(plots)
  	jpg_name = paste(names(selection[i]), "_3d.jpg", sep = "")
  	jpeg(jpg_name, width = 700, height = 700)
  	Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	jpg_name = paste(names(selection[i]), "_heat.jpg", sep = "")
  	jpeg(jpg_name, width = 700, height = 700)
  	Plot_G_gamma_omega_heat_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
	}#end for


#########################################################################
#########################################################################
####Analysis of network parameters

##network construction with igraph
net_mono = graph_from_data_frame(d = links, vertices = nodes, directed = F)

clo = closeness(net_mono, normalized = FALSE)
btw = betweenness(net_mono, directed = FALSE, normalized = TRUE)
eig = eigen_centrality(net_mono)
eig_formated = eig$vector
deg = centr_degree(net_mono)
deg_formated = deg$res
names(deg_formated) = names(clo)


save(clo, btw, eig_formated, deg_formated,G_norm_mean, file = "/home/nastaran/Downloads/marco/new_data/bats_allCentr.RData")
cat('end_netparameter', "\n")

#########################################################################
#########################################################################
####Network seperation of bats and plants


n_bats = subset(nodes, taxon == "Bats")
n_plants = subset(nodes, taxon == "Plants")

eig = eig_formated
deg = deg_formated


Gnorm_bats = c()
Gnorm_plants = c()
names_bats = c()
names_plants = c()

clo_bats = c()
clo_plants = c()
names_bats = c()
names_plants = c()

btw_bats = c()
btw_plants = c()
names_bats = c()
names_plants = c()

eig_bats = c()
eig_plants = c()
names_bats = c()
names_plants = c()

deg_bats = c()
deg_plants = c()
names_bats = c()
names_plants = c()


k = 1
for (i in 1:length(clo)) {
  	for (j in 1:length(n_bats[,1])) {
    		if (names(clo[i]) == n_bats[j,1]) {
      			clo_bats[k] = clo[i]
     		 	names_bats[k] = names(clo[i]) 
      
      			Gnorm_bats[k] = G_norm_mean[i]
      
      			btw_bats[k] = btw[i]

      			eig_bats[k] = eig[i]
      
      			deg_bats[k] = deg[i]
           
     		 	k = k+1
    			}#end if
  		}#end for j
	}#end for i


k = 1
for (i in 1:length(clo)) {
	for (j in 1:length(n_plants[,1])) {
    		if (names(clo[i]) == n_plants[j,1]) {
      			clo_plants[k] = clo[i]
      			names_plants[k] = names(clo[i]) 
      
      			Gnorm_plants[k] = G_norm_mean[i]
      
      			btw_plants[k] = btw[i]     
      
      			eig_plants[k] = eig[i]
      
      			deg_plants[k] = deg[i]           
      			k = k+1
    			}#end if
  		}#end for j
	}#end for i


names(Gnorm_bats) = names_bats
names(Gnorm_plants) = names_plants

names(clo_bats) = names_bats
names(clo_plants) = names_plants

names(btw_bats) = names_bats
names(btw_plants) = names_plants

names(eig_bats) = names_bats
names(eig_plants) = names_plants

names(deg_bats) = names_bats
names(deg_plants) = names_plants


save(clo_bats, btw_bats, eig_bats, deg_bats, Gnorm_bats, file = "/home/nastaran/Downloads/marco/new_data/testbats_bats_allCentr.RData")
save(clo_plants, btw_plants, eig_plants, deg_plants, Gnorm_plants, file = "/home/nastaran/Downloads/marco/new_data/testbats_plants_allCentr.RData")


cat('end_netseparation', "\n")

###############################################################################
###############################################################################
####Plot correlgram



#Bats

sp_names = names(Gnorm_bats)

df = data.frame(clo_bats, btw_bats, eig_bats, deg_bats, Gnorm_bats)
names(df) = c("closeness", "betweeness", "eigen vector", "degreee", "Gnorm")
head(df)

#create a png with the correlogram Pearson
png(filename="C_correlogram_bats_bats_pearson.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "pearson", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlogram (Pearson) between centralities and Gnorm for the bats of the Bats-Plantas network", cex.main = 1.5)
dev.off()

#create a png with the correlogram Spearman
png(filename="C_correlogram_bats_bats_spearman.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "spearman", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlogram (Spearman) between centralities and Gnorm for the bats of the Bats-Plantas network", cex.main = 1.5)
dev.off()

#########################################################
##Plants

sp_names = names(Gnorm_plants)

df = data.frame(clo_plants, btw_plants, eig_plants, deg_plants, Gnorm_plants)
names(df) = c("closeness", "betweeness", "eigen vector", "degreee", "Gnorm")
head(df)

#create a png with the correlogram Pearson
png(filename="C_correlogram_bats_plants_pearson.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "pearson", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlogram (Pearson) between centralities and Gnorm for the plants of the Bats-Plantas network", cex.main = 1.5)
dev.off()

#create a png with the correlogram Spearman
png(filename="C_correlogram_bats_plants_spearman.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "spearman", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlogram (Spearman) between centralities and Gnorm for the plantes of the Bats-Plantas network", cex.main = 1.5)
dev.off()



####################################################################
#Total

sp_names = names(G_norm_mean)

df = data.frame(clo, btw, eig, deg, G_norm_mean)
names(df) = c("closeness", "betweeness", "eigen vector", "degreee", "Gnorm")
head(df)

#create a png with the correlogram Pearson
png(filename="C_correlogram_bats_all_pearson.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "pearson", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlogram (Pearson) between centralities and Gnorm for the entire Bats-Plantas network", cex.main = 1.5)
dev.off()

#create a png with the correlogram Spearman
png(filename="C_correlogram_bats_all_spearman.png", res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "spearman", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlogram (Spearman) between centralities and Gnorm for the entire Bats-Plantas network", cex.main = 1.5)
dev.off()

cat('end_plots', "\n")


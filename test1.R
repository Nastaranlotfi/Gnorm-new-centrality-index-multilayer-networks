library(akima)
library(CINNA)
library(corrgram)
library(dplyr)
library(ggplot2)
library(igraph)
library(kableExtra)
library(multinet)
library(pheatmap)
library(plot3D)
library(plyr)
library(png)
library(RColorBrewer)


################### SET UP AND DATA IMPORT #####################################


cat("\014")  

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list= ls())

if (!dir.exists(path = "data")){
  dir.create(path = "data")
} else {
  print("Dir already exists!")
}

if (!dir.exists(path = "input")){
  dir.create(path = "input")
} else {
  print("Dir already exists!")
}

if (!dir.exists(path = "figures")){
  dir.create(path = "figures")
} else {
  print("Dir already exists!")
}

if (!dir.exists(path = "results")){
  dir.create(path = "results")
} else {
  print("Dir already exists!")
}

currentTime_start <- Sys.time()

source("Aux_functions.R", encoding="utf-8")

data = read.csv("input/links_clean.csv", header=T, as.is=T)

head(data)
tail(data)

currentTime_prep <- Sys.time()


################### EDGE LIST ##################################################


Fruit1 <- list()
Fruit2 <- list()
Fruit3 <- list()
Fruit4 <- list()
Nectar1 <- list()
Nectar2 <- list()
Nectar3 <- list()
Nectar4 <- list()

leng<-dim(data)[1]
k2=1
k1=1
for (i in 1:leng) {
	if(data[[i,3]]=="Frugivory"){
		Fruit1[k1]<-c(data[[i,1]])
		Fruit2[k1]<-c(data[[i,2]])
		Fruit3[k1]<-c(1)
		Fruit4[k1]<-c(data[[i,3]])
		#cat('hi',"\n")
		k1=k1+1
		}#end if 1
	if(data[[i,3]]=="Nectarivory"){
		Nectar1[k2]<-c(data[[i,1]])
		Nectar2[k2]<-c(data[[i,2]])
		Nectar3[k2]<-c(2)
		Nectar4[k2]<-c(data[[i,3]])
		#cat('he',"\n")
		k2=k2+1
		}#end if 2
		
	}#end for


Fruit<-list()
Nectar<-list()
Fruit<-cbind(Fruit1,Fruit2,Fruit3,Fruit4)
Nectar<-cbind(Nectar1,Nectar2,Nectar3,Nectar4)

Links<-rbind(Fruit,Nectar)
colnames(Links) <- c("from","to", "layer_num", "layer")

dim(Links)
head(Links)
tail(Links)

currentTime_link <- Sys.time()
cat('end_link_construction', "\n")


################### NODE LIST ##################################################


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

Nodes<-rbind(Fa,Na)
colnames(Nodes) <- c("name","taxon","taxon.label","species.size")

dim(Nodes)
head(Nodes)
tail(Nodes)

write.csv(Nodes,"data/nodes1.csv", row.names = FALSE)
write.csv(Links,"data/links1.csv", row.names = FALSE)

currentTime_node <- Sys.time()
cat('end_node_construction', "\n")


################### FINDING THE GIANT COMPONENT ################################


# Identify max component, make new lists of nodes and links

nodes1 = read.csv("data/nodes1.csv", header=T, as.is=T)
links1 = read.csv("data/links1.csv", header=T, as.is=T)


net_mono1 = graph_from_data_frame(d = links1, vertices = nodes1, directed = F)

c=clusters(net_mono1, mode="weak") #finding the clusters
b=which.max(c$csize) #find the max
v=V(net_mono1)[c$membership!=b] #find the names of nodes in the max component

b1=split(names(v),v) #formating the v file into a list
b2=list()
for (i in 1:length(b1)){
	b2=append(b2,b1[[i]])}

b2=unlist(b2)

df1<-nodes1
df2<-links1

for (i in 1:length(b2)){#remove the nodes that don't belong to the max component
	df1<-df1 %>% filter(!name==b1[i])}

for (i in 1:length(b2)){#removing the links related to the removed nodes
	df2<-df2 %>% filter(!from==b1[i])
	df2<-df2 %>% filter(!to==b1[i])}
	
	
write.csv(df1,"data/nodes2.csv", row.names = FALSE)
write.csv(df2,"data/links2.csv", row.names = FALSE)

currentTime_compo <- Sys.time()
cat('end_names_filtering-by-max-component', "\n")


################### BUILDING THE MULTILAYER NETWORK ############################


# Package multinet

# Complete network #1
nodes1 = read.csv("data/nodes1.csv", header=T, as.is=T)
links1 = read.csv("data/links1.csv", header=T, as.is=T)

nodes1 = nodes1[order(nodes1$name),] 

net_multinet1 = Convert_to_Multinet(nodes1, links1)


# The giant component of the network #2
nodes2 = read.csv("data/nodes2.csv", header=T, as.is=T)
links2 = read.csv("data/links2.csv", header=T, as.is=T)

nodes2 = nodes2[order(nodes2$name),] 

net_multinet2 = Convert_to_Multinet(nodes2, links2)


# Compare the complete network to its giant component
net_multinet1
net_multinet2


currentTime_netcons <- Sys.time()
cat('end_network_construction', "\n")


################### PLOTTING THE MULTILAYER NETWORK ############################


# Package igraph

# Complete network #1
links_no_dupl1 = links1[-which(duplicated(links1[,c("from", "to")])==T),] 
net_layout1 = graph_from_data_frame(d = links_no_dupl1,
                                   vertices = nodes1, directed = F) 
layout1 = layout_nicely(net_layout1) 

png(filename="figures/network_visualization_complete.png", 
    res = 300, width = 4000, height = 3000)
Custom_plot2D(links1, nodes1, layout1, vertex_label_cex = NULL, vertex_size = 3)
dev.off()


# The giant component of the network #2
links_no_dupl2 = links2[-which(duplicated(links2[,c("from", "to")])==T),] 
net_layout2 = graph_from_data_frame(d = links_no_dupl2,
                                   vertices = nodes2, directed = F) 
layout2 = layout_nicely(net_layout2) 

png(filename="figures/network_visualization_component.png", 
    res = 300, width = 4000, height = 3000)
Custom_plot2D(links2, nodes2, layout2, vertex_label_cex = NULL, vertex_size = 3)
dev.off()


currentTime_netvis <- Sys.time()
cat('end_network_visualization', "\n")


################### GNORM CALCULATION ##########################################


# From here on, we analyze only the giant component of the network

# Partitioning, omega, gamma, and number of iterations (for getting the mean)
partitions_of_omega = 10 #Number of partitions
seq_G = Create_seq_G_Merged(net_multinet2, partitions_of_omega)
vec_W = Create_vec_W(partitions_of_omega)
gamma_min = 0.25
gamma_max = 4
gamma_spacing = 0.25
gammas = seq(from = gamma_min, to = gamma_max, by = gamma_spacing)
iterations = 100 #It takes a long time, but for stable results use at least 100

# Saving lists definition
Seq_G_Mean_gamma_list = list() 
G_norm_list = list()

# G_analysis
cont_perc = 1 # Calculation of running progress

for (gamma_index in 1:length(gammas)) {
	start_time <- round(as.numeric(Sys.time()))
  	seq_G_list = list()
    	for (i in 1:iterations) {
    		seq_G_list[[i]] = Create_seq_G_Merged(net_multinet2, 
    		                                      partitions_of_omega,
    		                                      gamma = gammas[gamma_index])
    		                                      
    		#####Run-time approximation
    		if (cont_perc==1 ){
    			end_time <- round(as.numeric(Sys.time()))
			time_taken <- round(end_time - start_time,2)
			print (time_taken)
		
			cat("Estimated time needed for run (secs): ", time_taken*(iterations*length(gammas)),"\n" )}
			#cat("\n")}
			#print (time_taken)
		
    		cat(cont_perc*100/(iterations*length(gammas)), "%  ")###print the run progress
    		cont_perc = cont_perc + 1
  		}#end of iterations
  
  
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

save(gammas, vec_W, iterations, partitions_of_omega, links2, nodes2,
     Seq_G_Mean_gamma_list,G_norm_mean, G_norm_mean_ordered,
     file = "results/Bat_Net.RData")

currentTime_Gnorm <- Sys.time()
cat('end_Gnorm', "\n")


################### MODULARITY FOR ONE RUN ######################################


partitions_of_omega1 = 4 
gamma_min1 = 0.5
gamma_max1 = 3.5
gamma_spacing1 = 0.5

plots = Plot_number_modularity(partitions_of_omega1,
                             gamma_min1,
                             gamma_max1,
                             gamma_spacing1,
                             net_multinet2)

currentTime_modularity <- Sys.time()
cat('end_modularity', "\n")


################### G-NORM FREQUENCY ###########################################


load("results/Bat_Net.RData")

G_plot<-G_norm_mean 
names(G_plot)<-NULL 
df<-unlist(G_plot) 

png(filename="figures/hist_Gnorm.png", 
    res = 300, width = 4000, height = 3000)
labs = colnames(df)

hist(df,breaks=5,col="darkmagenta", xlim=c(1,2),
     main="Distribution of Gnorm", xlab='G_norm')

dev.off()

currentTime_gnormfreq <- Sys.time()
cat('end_gnormfreq', "\n")


################### G-NORM OF SELECTED NODES ###################################


load("results/Bat_Net.RData")

seq_Gnorm_gamma_mean = Unite_list_of_dataframes(Seq_G_Mean_gamma_list)
selection = Select_Example_Nodes(G_norm_mean_ordered) #function that finds the nodes we are interested
for (i in 1:length(selection)) {
  	
  	chosen_node = names(selection[i])
  	png_name = paste("figures/",names(selection[i]), "_2d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean,
  	                                      chosen_node, vec_W, gammas)
  	plot(plots)
        dev.off()
  	png_name = paste("figures/",names(selection[i]), "_3d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, 
  	                          chosen_node, vec_W, gammas)
        dev.off()
  	png_name = paste("figures/",names(selection[i]), "_heat.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_heat_3D(seq_Gnorm_gamma_mean, 
  	                           chosen_node, vec_W, gammas)
  	dev.off()
	}#end for

currentTime_gnormnodes <- Sys.time()
cat('end_gnormnodes', "\n")


################### NETWORK PARAMETERS #########################################


nodes2 = read.csv("data/nodes2.csv", header=T, as.is=T)
links2 = read.csv("data/links2.csv", header=T, as.is=T)

net_mono = graph_from_data_frame(d = links2, vertices = nodes2, directed = F)

clo = closeness(net_mono, normalized = FALSE)
btw = betweenness(net_mono, directed = FALSE, normalized = TRUE)
eig = eigen_centrality(net_mono)
eig_formated = eig$vector
deg = centr_degree(net_mono)
deg_formated = deg$res
names(deg_formated) = names(clo)

clo[order(names(clo))]
btw[order(names(btw))]
eig[order(names(eig))]
deg[order(names(deg))]
G_norm_mean[order(names(G_norm_mean))]

save(clo, btw, eig_formated, deg_formated,
     G_norm_mean, file = "results/bats_allCentr.RData")

currentTime_netparameter <- Sys.time()
cat('end_netparameter', "\n")


################### SEPARATING NODE CLASSES ####################################


nodes2 = read.csv("data/nodes2.csv", header=T, as.is=T)


data=load("results/bats_allCentr.RData")
eig = eig_formated
deg = deg_formated

n_bats = subset(nodes2, taxon == "Bats")
n_plants = subset(nodes2, taxon == "Plants")
#Bats
clo_bats=Separation(n_bats,clo)
btw_bats = Separation(n_bats,btw)
eig_bats = Separation(n_bats,eig)
deg_bats = Separation(n_bats,deg)
Gnorm_bats=Separation(n_bats, G_norm_mean)
#Plants
clo_plants = Separation(n_plants,clo)
btw_plants = Separation(n_plants,btw)
eig_plants = Separation(n_plants,eig)
deg_plants = Separation(n_plants,deg)
Gnorm_plants = Separation(n_plants,G_norm_mean)

save(clo_bats, btw_bats, eig_bats,
     deg_bats, Gnorm_bats, file = "results/bats_bats_allCentr.RData")
save(clo_plants, btw_plants, eig_plants,
     deg_plants, Gnorm_plants, file = "results/bats_plants_allCentr.RData")

currentTime_netseparation <- Sys.time()
cat('end_netseparation', "\n")


################### CORRELOGRAMS ###############################################


# Bats
load("results/bats_bats_allCentr.RData")
sp_names = names(Gnorm_bats)

df = data.frame(clo_bats, btw_bats, eig_bats, deg_bats, Gnorm_bats)
names(df) = c("closeness", "betweeness", "eigen vector", "degreee", "Gnorm")
head(df)

png(filename="figures/C_correlogram_bats_bats_pearson.png",
    res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "pearson", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, 
         diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlogram (Pearson) between centralities and Gnorm for bats",
         cex.main = 1.5)
dev.off()

png(filename="figures/C_correlogram_bats_bats_spearman.png", 
    res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "spearman", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, 
         diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlogram (Spearman) between centralities and Gnorm for bats",
         cex.main = 1.5)
dev.off()


# Plants

load("results/bats_plants_allCentr.RData")
sp_names = names(Gnorm_plants)

df = data.frame(clo_plants, btw_plants, eig_plants, deg_plants, Gnorm_plants)
names(df) = c("closeness", "betweeness", "eigen vector", "degreee", "Gnorm")
head(df)

png(filename="figures/C_correlogram_bats_plants_pearson.png", 
    res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "pearson", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, 
         diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlogram (Pearson) between centralities and Gnorm for plants",
         cex.main = 1.5)
dev.off()

png(filename="figures/C_correlogram_bats_plants_spearman.png",
    res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "spearman", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, 
         diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlogram (Spearman) between centralities and Gnorm for plants",
         cex.main = 1.5)
dev.off()

currentTime_corrgrams <- Sys.time()
cat('end_corrgrams', "\n")


################### TOTAL ######################################################


load("results/bats_allCentr.RData")
sp_names = names(G_norm_mean)

df = data.frame(clo, btw, eig, deg, G_norm_mean)
names(df) = c("closeness", "betweeness", "eigen vector", "degreee", "Gnorm")
head(df)

#create a png with the correlogram Pearson
png(filename="figures/C_correlogram_bats_all_pearson.png",
    res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "pearson", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, 
         diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlogram (Pearson) between centralities and Gnorm for bats and plants",
         cex.main = 1.5)
dev.off()

#create a png with the correlogram Spearman
png(filename="figures/C_correlogram_bats_all_spearman.png",
    res = 300, width = 4000, height = 3000)
labs = colnames(df)
corrgram(df, cor.method = "spearman", order=FALSE, oma=c(12, 12, 7, 2), 
         lower.panel=panel.cor, upper.panel=panel.pts, 
         diag.panel=panel.density, text.panel=panel.txt,
         outer.labels=list(bottom=list(labels=labs,cex=2,srt=90),
                           left=list(labels=labs,cex=2,srt=0)),
         main="Correlogram (Spearman) between centralities and Gnorm for bats and plants",
         cex.main = 1.5)
dev.off()

currentTime_plots <- Sys.time()
cat('end_plots', "\n")


################### G-NORM PLOTS ###############################################


# Reading the names from a list, names taken from 2019 NatEcoEvo paper)
#Bats

seq_Gnorm_gamma_mean = Unite_list_of_dataframes(Seq_G_Mean_gamma_list)
selection =read.csv("input/Names_impo.csv",  as.is=1)
selection = selection[order(selection$name),]
for (i in 1:length(selection)) {
  	
  	chosen_node = selection[i]
  	png_name = paste("figures/important_",selection[i], "_2d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	plot(plots)
        dev.off()
  	png_name = paste("figures/important_",selection[i],"_3d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
        png_name = paste("figures/important_",selection[i],"_heat.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_heat_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
	}#end for

#Plants

seq_Gnorm_gamma_mean = Unite_list_of_dataframes(Seq_G_Mean_gamma_list)
selection =read.csv("input/Names_impo_plants.csv",  as.is=1)
selection = selection[order(selection$name),]
for (i in 1:length(selection)) {
  	
  	chosen_node = selection[i]
  	png_name = paste("figures/important_",selection[i], "_2d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	plot(plots)
        dev.off()
  	png_name = paste("figures/important_",selection[i],"_3d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
        png_name = paste("figures/important_",selection[i],"_heat.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_heat_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
	}#end for

currentTime_gnormplots <- Sys.time()
cat('end_gnormplots', "\n")


################### CENTRALITY #################################################


# Bats (for plants, just needed to replace clo_bats to clo_plants)

load("results/bats_bats_allCentr.RData")
clo1 = clo_bats
btw1 = btw_bats
eig1 = eig_bats
deg1 = deg_bats
Gnorm1 = Gnorm_bats
centr_list_bats = list(clo1, btw1, eig1, deg1, Gnorm1)

most_central_list = list()
ranking_cutoff = 10
for (i in 1:length(centr_list_bats)) {
  centr_temp = centr_list_bats[[i]]
  centr_temp = sort(centr_temp, decreasing = TRUE)
  centr_temp = centr_temp[1:ranking_cutoff]
  most_central_list[[i]] = centr_temp
}

# Compare how many nodes found in Gnorm are present in other methods
Gnorm_most_central = most_central_list[[5]]
similarity_bin = rep(0, length(most_central_list))
names(similarity_bin) = c("clo", "btw", "eig", "deg", "Gnorm")
similarity_string_list = list()
for (i in 1:(length(most_central_list))) {
  list_temp = list()
  for (j in 1:ranking_cutoff) {
    for (k in 1:ranking_cutoff) {
      if (names(Gnorm_most_central[j]) == names(most_central_list[[i]][k])) {
        similarity_bin[i] = similarity_bin[i] + 1
        list_temp = append(list_temp, names(Gnorm_most_central[j]))
      }
    }
  }
  similarity_string_list[[i]] = list_temp
}
similarity_bin = similarity_bin/ranking_cutoff

# Compare the distance between the rankings found in Gnorm with those present in the other methods
Gnorm_most_central = most_central_list[[5]]
similarity_dist = rep(0, length(most_central_list))
names(similarity_dist) = c("clo", "btw", "eig", "deg", "Gnorm")
for (i in 1:(length(most_central_list))) {
  list_temp = list()
  for (j in 1:ranking_cutoff) {
    for (k in 1:ranking_cutoff) {
      if (names(Gnorm_most_central[j]) == names(most_central_list[[i]][k])) {
        similarity_dist[i] = similarity_dist[i] + (1/(1+abs(j-k)))
      }
    }
  }
}
similarity_dist = similarity_dist/ranking_cutoff

# Saving both items of similarity in one RData
save(similarity_bin,similarity_dist, file = "results/similarity_Bat_Net.RData")


#Plants
load("results/bats_plants_allCentr.RData")
clo1 = clo_plants
btw1 = btw_plants
eig1 = eig_plants
deg1 = deg_plants
Gnorm1 = Gnorm_plants
centr_list_bats = list(clo1, btw1, eig1, deg1, Gnorm1)

most_central_list = list()
ranking_cutoff = 10
for (i in 1:length(centr_list_bats)) {
  centr_temp = centr_list_bats[[i]]
  centr_temp = sort(centr_temp, decreasing = TRUE)
  centr_temp = centr_temp[1:ranking_cutoff]
  most_central_list[[i]] = centr_temp
}

# Compare how many nodes found in Gnorm are present in other methods
Gnorm_most_central = most_central_list[[5]]
similarity_bin = rep(0, length(most_central_list))
names(similarity_bin) = c("clo", "btw", "eig", "deg", "Gnorm")
similarity_string_list = list()
for (i in 1:(length(most_central_list))) {
  list_temp = list()
  for (j in 1:ranking_cutoff) {
    for (k in 1:ranking_cutoff) {
      if (names(Gnorm_most_central[j]) == names(most_central_list[[i]][k])) {
        similarity_bin[i] = similarity_bin[i] + 1
        list_temp = append(list_temp, names(Gnorm_most_central[j]))
      }
    }
  }
  similarity_string_list[[i]] = list_temp
}
similarity_bin = similarity_bin/ranking_cutoff

# Compare the distance between the rankings found in Gnorm with those present in the other methods
Gnorm_most_central = most_central_list[[5]]
similarity_dist = rep(0, length(most_central_list))
names(similarity_dist) = c("clo", "btw", "eig", "deg", "Gnorm")
for (i in 1:(length(most_central_list))) {
  list_temp = list()
  for (j in 1:ranking_cutoff) {
    for (k in 1:ranking_cutoff) {
      if (names(Gnorm_most_central[j]) == names(most_central_list[[i]][k])) {
        similarity_dist[i] = similarity_dist[i] + (1/(1+abs(j-k)))
      }
    }
  }
}
similarity_dist = similarity_dist/ranking_cutoff

# Saving both items of similarity in one RData
save(similarity_bin,similarity_dist, file = "results/similarity_Plants_Net.RData")

currentTime_centrality <- Sys.time()
cat('end_centrality', "\n")


################### TOP 10 CENTRALITIES DETECTION ##############################


#Finding the top 10 in each centrality and plotting its relativ Gnorm

#Bats section

load("results/Bat_Net.RData")
seq_Gnorm_gamma_mean = Unite_list_of_dataframes(Seq_G_Mean_gamma_list)
load("results/bats_bats_allCentr.RData")

#Btas-> Clossness Centrality
clo1 = sort(clo_bats,decreasing=TRUE)
selection = names(clo1[1:10])
save(selection, file = "results/Bats_impo_Clo.RData")
for (i in 1:length(selection)) {
  	
  	chosen_node = selection[i]
  	png_name = paste("figures/Bats_Clo_",selection[i], "_2d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	plot(plots)
        dev.off()
  	png_name = paste("figures/Bats_Clo_",selection[i],"_3d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
        png_name = paste("figures/Bats_Clo_",selection[i],"_heat.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_heat_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
	}#end for


#Btas-> Betweenness Centrality
btw1 = sort( btw_bats,decreasing=TRUE)

selection = names(btw1[1:10])
save(selection, file = "results/Bats_impo_btw.RData")

for (i in 1:length(selection)) {
  	
  	chosen_node = selection[i]
  	png_name = paste("figures/Bats_Btw_",selection[i], "_2d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	plot(plots)
        dev.off()
  	png_name = paste("figures/Bats_Btw_",selection[i],"_3d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
        png_name = paste("figures/Bats_Btw_",selection[i],"_heat.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_heat_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
	}#end for

#Btas-> Eigenvector Centrality
eig1 = sort(eig_bats,decreasing=TRUE)
selection = names(eig1[1:10])
save(selection, file = "results/Bats_impo_eig.RData")
for (i in 1:length(selection)) {
  	
  	chosen_node = selection[i]
  	png_name = paste("figures/Bats_Eig_",selection[i], "_2d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	plot(plots)
        dev.off()
  	png_name = paste("figures/Bats_Eig_",selection[i],"_3d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
        png_name = paste("figures/Bats_Eig_",selection[i],"_heat.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_heat_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
	}#end for

#Btas-> Degree Centrality
deg1 = sort(deg_bats,decreasing=TRUE)

selection = names(deg1[1:10])
save(selection, file = "results/Bats_impo_deg.RData")
for (i in 1:length(selection)) {
  	
  	chosen_node = selection[i]
  	png_name = paste("figures/Bats_Deg_",selection[i], "_2d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	plot(plots)
        dev.off()
  	png_name = paste("figures/Bats_Deg_",selection[i],"_3d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
        png_name = paste("figures/Bats_Deg_",selection[i],"_heat.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_heat_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
	}#end for
	
#Btas-> Gnorm Centrality
Gnorm1 = sort(Gnorm_bats,decreasing=TRUE)

selection = names(Gnorm1[1:10])
save(selection, file = "results/Bats_impo_Gnorm.RData")
for (i in 1:length(selection)) {
  	
  	chosen_node = selection[i]
  	png_name = paste("figures/Bats_Gnorm_",selection[i], "_2d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	plot(plots)
        dev.off()
  	png_name = paste("figures/Bats_Gnorm_",selection[i],"_3d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
        png_name = paste("figures/Bats_Gnorm_",selection[i],"_heat.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_heat_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
	}#end for


#Plants section

load("results/Bat_Net.RData")
seq_Gnorm_gamma_mean = Unite_list_of_dataframes(Seq_G_Mean_gamma_list)
load("results/bats_plants_allCentr.RData")

#Plants-> Clossness Centrality
clo1 = sort(clo_plants,decreasing=TRUE)
selection = names(clo1[1:10])
save(selection, file = "results/Plants_impo_Clo.RData")
for (i in 1:length(selection)) {
  	
  	chosen_node = selection[i]
  	png_name = paste("figures/Plants_Clo_",selection[i], "_2d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	plot(plots)
        dev.off()
  	png_name = paste("figures/Plants_Clo_",selection[i],"_3d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
        png_name = paste("figures/Plants_Clo_",selection[i],"_heat.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_heat_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
	}#end for


#Plants-> Betweenness Centrality
btw1 = sort( btw_plants,decreasing=TRUE)

selection = names(btw1[1:10])
save(selection, file = "results/Plants_impo_btw.RData")
for (i in 1:length(selection)) {
  	
  	chosen_node = selection[i]
  	png_name = paste("figures/Plants_Btw_",selection[i], "_2d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	plot(plots)
        dev.off()
  	png_name = paste("figures/Plants_Btw_",selection[i],"_3d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
        png_name = paste("figures/Plants_Btw_",selection[i],"_heat.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_heat_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
	}#end for

#Plants-> Eigenvector Centrality
eig1 = sort(eig_plants,decreasing=TRUE)
selection = names(eig1[1:10])
save(selection, file = "results/Plants_impo_eig.RData")
for (i in 1:length(selection)) {
  	
  	chosen_node = selection[i]
  	png_name = paste("figures/Plants_Eig_",selection[i], "_2d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	plot(plots)
        dev.off()
  	png_name = paste("figures/Plants_Eig_",selection[i],"_3d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
        png_name = paste("figures/Plants_Eig_",selection[i],"_heat.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_heat_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
	}#end for

#Plants-> Degree Centrality
deg1 = sort(deg_plants,decreasing=TRUE)

selection = names(deg1[1:10])
save(selection, file = "results/Plants_impo_deg.RData")
for (i in 1:length(selection)) {
  	
  	chosen_node = selection[i]
  	png_name = paste("figures/Plants_Deg_",selection[i], "_2d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	plot(plots)
        dev.off()
  	png_name = paste("figures/Plants_Deg_",selection[i],"_3d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
        png_name = paste("figures/Plants_Deg_",selection[i],"_heat.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_heat_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
	}#end for
	
#Plants-> Gnorm Centrality
Gnorm1 = sort(Gnorm_plants,decreasing=TRUE)

selection = names(Gnorm1[1:10])
save(selection, file = "results/Plants_impo_Gnorm.RData")
for (i in 1:length(selection)) {
  	
  	chosen_node = selection[i]
  	png_name = paste("figures/Plants_Gnorm_",selection[i], "_2d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	plot(plots)
        dev.off()
  	png_name = paste("figures/Plants_Gnorm_",selection[i],"_3d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
        png_name = paste("figures/Plants_Gnorm_",selection[i],"_heat.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_heat_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
	}#end for


currentTime_topten <- Sys.time()
cat('end_top10_centralities', "\n")


################### PLOT TOP 10 AND BOTTOM 10 ##################################


#Btas-> Gnorm Centrality

load("results/bats_bats_allCentr.RData")
Gnorm1 = sort(Gnorm_bats,decreasing=TRUE)

selection = names(Gnorm1[1:10])
for (i in 1:length(selection)) {
  	
  	chosen_node = selection[i]
  	png_name = paste("figures/Bats_10top_Gnorm_",selection[i], "_2d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	plot(plots)
        dev.off()
  	png_name = paste("figures/Bats_10top_Gnorm_",selection[i],"_3d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
        png_name = paste("figures/Bats_10top_Gnorm_",selection[i],"_heat.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_heat_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
	}#end for


Gnorm1 = sort(Gnorm_bats,decreasing=FALSE)

selection = names(Gnorm1[1:10])
for (i in 1:length(selection)) {
  	
  	chosen_node = selection[i]
  	png_name = paste("figures/Bats_10last_Gnorm_",selection[i], "_2d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	plot(plots)
        dev.off()
  	png_name = paste("figures/Bats_10last_Gnorm_",selection[i],"_3d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
        png_name = paste("figures/Bats_10last_Gnorm_",selection[i],"_heat.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_heat_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
	}#end for


#Plants-> Gnorm Centrality

load("results/bats_plants_allCentr.RData")


Gnorm1 = sort(Gnorm_plants,decreasing=TRUE)

selection = names(Gnorm1[1:10])
for (i in 1:length(selection)) {
  	
  	chosen_node = selection[i]
  	png_name = paste("figures/Plants_10top_Gnorm_",selection[i], "_2d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	plot(plots)
        dev.off()
  	png_name = paste("figures/Plants_10top_Gnorm_",selection[i],"_3d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
        png_name = paste("figures/Plants_10top_Gnorm_",selection[i],"_heat.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_heat_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
	}#end for


Gnorm1 = sort(Gnorm_plants,decreasing=FALSE)

selection = names(Gnorm1[1:10])
for (i in 1:length(selection)) {
  	
  	chosen_node = selection[i]
  	png_name = paste("figures/Plants_10last_Gnorm_",selection[i], "_2d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	plots = G_curves_for_different_gammas(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	plot(plots)
        dev.off()
  	png_name = paste("figures/Plants_10last_Gnorm_",selection[i],"_3d.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_suf_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
        png_name = paste("figures/Plants_10last_Gnorm_",selection[i],"_heat.png", sep = "")
  	png(png_name, width = 700, height = 700)
  	Plot_G_gamma_omega_heat_3D(seq_Gnorm_gamma_mean, chosen_node, vec_W, gammas)
  	dev.off()
	}#end for
currentTime_toptenlastten <- Sys.time()
cat('end_top10_last10_Gnorm', "\n")


################### TIMERS #####################################################


sink(file = "results/timers.txt")

paste("Time spent running each section of the code")
paste("Lotfi et al., in prep.")
cat("\n")
paste("Start running the code:", currentTime_start)
cat("\n")
paste("Endtime for preparation:", currentTime_prep)
cat("\n")
paste("Endtime for link construction:", currentTime_link)
cat("\n")
paste("Endtime for node construction:", currentTime_node)
cat("\n")
paste("Endtime for identifying the giant component:", currentTime_compo)
cat("\n")
paste("Endtime for network visualization:", currentTime_netvis)
cat("\n")
paste("Endtime for Gnorm calculation:", currentTime_Gnorm)
cat("\n")
paste("Endtime for modularity calculation:", currentTime_modularity)
cat("\n")
paste("Endtime for Gnorm frequency calculation:", currentTime_gnormfreq)
cat("\n")
paste("Endtime for network parameters calculation:", currentTime_netparameter)
cat("\n")
paste("Endtime for separating network layers:", currentTime_netseparation)
cat("\n")
paste("Endtime for plotting separate correlograms for bats and plants:", currentTime_corrgrams)
cat("\n")
paste("Endtime for plotting joint correlograms:", currentTime_plots)
cat("\n")
paste("Endtime for plotting Gnorm:", currentTime_gnormplots)
cat("\n")
paste("Endtime for plotting centrality:", currentTime_centrality)
cat("\n")
paste("Endtime for plotting top 10 centrality:", currentTime_topten)
cat("\n")

paste("Endtime for plotting top 10 (last 10) Gnorm:", currentTime_toptenlastten)
cat("\n")
sink(file = NULL, )

################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### Authors: Nastaran Lotfi, Henrique S. Requejo, Francisco Rodrigues &
####          Marco A. R. Mello

#### See README for further info:
#### https://github.com/Nastaranlotfi/Test1-code#readme
################################################################################


################### SET UP AND DATA IMPORT #####################################


library(multinet)
library(igraph)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list= ls())

if (!dir.exists(path = "figures_random")){
  dir.create(path = "figures_random")
} else {
  print("Dir already exists!")
}

if (!dir.exists(path = "data_random")){
  dir.create(path = "data_random")
} else {
  print("Dir already exists!")
}

currentTime_start <- Sys.time()

source("Aux_functions_random.R", encoding="utf-8")



currentTime_prep <- Sys.time()







################### EDGE LIST ##################################################


Network_random=function(number_of_nodes, number_of_connections,temp_number_links,number_of_layers,permutation){
	
	links_df = data.frame(from = numeric(0), to = numeric(0), layer = numeric(0))
	links_dft = data.frame(from = numeric(0), to = numeric(0), layer = numeric(0))
	links_df1 = data.frame(from = numeric(0), to = numeric(0), layer = numeric(0))
	links_df2 = data.frame(from = numeric(0), to = numeric(0), layer = numeric(0))
	
  	#####Defining the main name of the nodes in each layer
  	#####Real nameing
  	
	node_name1<-seq(from = 1, to = number_of_nodes[1], by = 1)
	node_name2<-seq(from = number_of_nodes[1]+1, to = number_of_nodes[1]+number_of_nodes[2], by = 1)
	
	
  	###########################################################
	####making all nodes availble in both layers
	
	
	
	#####First, making the node names equal for both species
	
  	l1=as.integer(number_of_nodes[1])-as.integer(number_of_nodes[2])	
  	
  	tem1=Defined_nodes_layer(l1,node_name1,node_name2)
  	  	
  	node_epeci1=tem1[1]
	node_epeci2=tem1[2]
  	node_epeci1=unlist(node_epeci1[1])
	node_epeci2=unlist(node_epeci2[1])
  	
  	####Now, we select one layer, and make the random connection to 
  	####make all nodes availble
  	
  	
  	#############First model, number of links in each layer be the same as real network.
  	operat=1

  	k1=1
  	k2=1
  	while(operat<length(node_epeci1)+1){

  		lay=sample(2,1,replace=FALSE)
  		
  		if (lay==1 & dim(links_df1)[1]<temp_number_links[1]){
  		
 			links_df1[k1,1]=node_epeci1[operat]
			links_df1[k1,2]=node_epeci2[operat]
			links_df1[k1,3]=1
			k1=k1+1
			operat=operat+1
			
  			}#end if lay==1
  			
  		if (lay==2 & dim(links_df2)[1]<temp_number_links[2]){
  		
			links_df2[k2,1]=node_epeci1[operat]
			links_df2[k2,2]=node_epeci2[operat]
			links_df2[k2,3]=2
			k2=k2+1
  			operat=operat+1
  			}#end if lay==2
  		
  		
  		}#end while
  	
  	
  	number1=temp_number_links[1]+1
	links_df1=Add_edges_layer(links_df1,number1,node_name1,node_name2,1)
	
	number2=temp_number_links[2]+1
	links_df2=Add_edges_layer(links_df2,number2,node_name1,node_name2,2)

	links_dft=rbind(links_df1,links_df2)
	
	df<-links_dft[order(links_dft$from),]
  	
  	

	#file_name_links = paste("data_random/rand_same_", permutation, "_links.csv", sep ="")
  	#write.csv(df, file_name_links, row.names = FALSE, quote = FALSE)  	
  	
  	  	
  	##############Second model, links distributed randomely
  	
  	####Making all nodes availble
  	
  	for (i in 1:length(node_epeci1)){
  		links_df[i,1]=node_epeci1[i]
		links_df[i,2]=node_epeci2[i]
		links_df[i,3]=sample(2,1,replace=FALSE)
  		}#end for
  	
  	#####make all connection randomely
  	
  	links_df=Random_connection(links_df,number_of_connections,node_name1,node_name2)
  	
  	df1<-links_df[order(links_df$from),]
		
	

	file_name_links1 = paste("data_random/rand_total_", permutation, "_links.csv", sep ="")
	
  	write.csv(df1, file_name_links1, row.names = FALSE, quote = FALSE)
  	
	}#end network_random


###########################################################
####making all nodes availble in both layers

Defined_nodes_layer=function(l1,node_epeci1,node_epeci2){

	if (l1<0){

		for (i in 1:abs(l1)){
			node_epeci1=append(node_epeci1,sample (node_epeci1,1,replace=FALSE))
			}

		}

	if (l1>0){
		for (i in 1:abs(l1)){
			node_epeci2=append(node_epeci2,sample (node_epeci2,1,replace=FALSE))
			}

		}
	Layer=list(node_epeci1,node_epeci2)
	return (Layer)
	
	}#Defined_nodes_layer


##################################################################
#######Adding extra edge to the network to make same edge size
#We have two options: distribute totally random, 
#distribute related to the main network
#I add both functions

Add_edges_layer=function(links_df1,number,node_name1,node_name2,layer){
	length_layer1=dim(links_df1)[1]+1
	while (length_layer1<number){

		k=length_layer1
		links_df1[k,1]=sample (node_name1,1,replace=FALSE)
		links_df1[k,2]=sample (node_name2,1,replace=FALSE)
		links_df1[k,3]=layer

		links_df1=distinct(links_df1)
	
		length_layer1=dim(links_df1)[1]+1
		}

	return (links_df1)

	}#end Add_edges_layer


################################################################
######make random connection in two layers


Random_connection=function(links_dft,number_of_connections,node_name1,node_name2){

  	total_n=dim(links_dft)[1]+1
  	  	
  	while (total_n<number_of_connections+1){
  		lnum=sample (2,1,replace=FALSE)
  		k=dim(links_dft)[1]+1
  		
  		if (lnum==1){
  		
  			links_dft[k,1]=sample (node_name1,1,replace=FALSE)
  			links_dft[k,2]=sample (node_name2,1,replace=FALSE)
			links_dft[k,3]=1

			links_dft=distinct(links_dft)
  		
  			}
  	
  		if (lnum==2){
  		
  			links_dft[k,1]=sample (node_name1,1,replace=FALSE)
			links_dft[k,2]=sample (node_name2,1,replace=FALSE)
			links_dft[k,3]=2

			links_dft=distinct(links_dft)
  		
  		
  		
  			}
  		total_n=dim(links_dft)[1]+1
  		
  		
  		
  		}

	return (links_dft)
	
	}#end Random_connection
	

######################################################
####loading the main data for having information to construct the random networks

nodes1 = read.csv("data/nodes2.csv", header=T, as.is=T)
links1 = read.csv("data/links2.csv", header=T, as.is=T)

#Set the number of permutations to be used in all permutation analyses.

permutation <- 1



number_of_layers<-max(links1$layer_num)
number_of_connections<-as.integer(dim(links1)[1])
number_of_nodes<-as.integer (dim(nodes1)[1])


#####Defining the number of nodes in each group of bat and plants

temp_number_nodes=table(nodes1[3])

number_of_nodes_t1=temp_number_nodes[[1]]
number_of_nodes_t2=temp_number_nodes[[2]]


###################################################################
#####Finding number of nodes in each layer

temp_number_links=table(links1[3])#####first, we find the number of links in each layer

number_of_links_l1=temp_number_links[[1]]
number_of_links_l2=temp_number_links[[2]]


###################################################################
#####Name construction


nodes_ID1<- 1:number_of_nodes_t1
t1<-1+number_of_nodes_t1
t2<-number_of_nodes_t2+number_of_nodes_t1
nodes_ID2 <- t1: t2



Fa1=rep(1,length(number_of_nodes_t1))
Fa2=rep(2,length(number_of_nodes_t2))


Fa<-cbind(nodes_ID1,Fa1)
Na<-cbind(nodes_ID2,Fa2)

Names1<-rbind(Fa,Na)

colnames(Names1) <- c("name","taxon.label")


write.csv(Names1,"data_random/Names_random.csv", row.names = FALSE)


###############################################################
#####Random Network construction

for (i in 1:permutation){

	network<-Network_random(temp_number_nodes,number_of_connections,temp_number_links,number_of_layers,i)
	
	}

currentTime_link <- Sys.time()
######################################################################
## G_norm section

nodes = read.csv("data_random/Names_random.csv", header=T, as.is=T)
links = read.csv("data_random/rand_total_1_links.csv", header=T, as.is=T)

##sorting nodes to be in order
nodes = nodes[order(nodes$name),] 

##Network construction
net_multinet = Convert_to_Multinet(nodes, links)

##Parameters definition regarding the partitioning and Omega and Gamma and number of iteration (for getting the mean)

partitions_of_omega = 10 # Number of partitions
seq_G = Create_seq_G_Merged(net_multinet, partitions_of_omega)
vec_W = Create_vec_W(partitions_of_omega)
gamma_min = 0.25
gamma_max = 4
gamma_spacing = 0.25
gammas = seq(from = gamma_min, to = gamma_max, by = gamma_spacing)
iterations = 20


##Saving lists definition
Seq_G_Mean_gamma_list = list() #different datasets of Seq_G_MeanG_norm_ordered_list = list() #guarda os diferentes nohs selecionados para plot
G_norm_list = list()

######################################################################
## G_analysis

cont_perc = 1 # Calculation of running progress

for (gamma_index in 1:length(gammas)) {
	start_time <- round(as.numeric(Sys.time()))
  	seq_G_list = list()
    	for (i in 1:iterations) {
    		seq_G_list[[i]] = Create_seq_G_Merged(net_multinet, 
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



save(gammas, vec_W, iterations, partitions_of_omega, links, nodes, Seq_G_Mean_gamma_list,G_norm_mean, G_norm_mean_ordered, file = "data_random/rand_total.RData")
currentTime_Gnorm <- Sys.time()
#########Plot hist

load("data_random/rand_total.RData")

png(filename="figures_random/hist_Gnorm_random.png", 
    res = 300, width = 4000, height = 3000)

hist(G_norm_mean,col="darkmagenta")
dev.off()
currentTime_gnormfreq <- Sys.time()
cat('end_Gnorm', "\n")


sink(file = "data_random/timers.txt")

paste("Time spent running each section of the code")
paste("Lotfi et al., in prep.")
cat("\n")
paste("Start running the code:", currentTime_start)
cat("\n")
paste("Endtime for preparation:", currentTime_prep)
cat("\n")
paste("Endtime for random network construction:", currentTime_link)
cat("\n")
paste("Endtime for Gnorm calculation:", currentTime_Gnorm)
cat("\n")
paste("Endtime for Gnorm frequency calculation:", currentTime_gnormfreq)
cat("\n")
sink(file = NULL, )




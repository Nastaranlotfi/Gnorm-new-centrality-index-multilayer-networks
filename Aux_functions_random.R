#Functions---------------------------------------------------------------------------------------------

#Convert a csv file nodes and links into a multinet input file of type MULTIPLEX (VErsion 3.0)
Convert_csv_to_multinet_multiplex = function(nodes, links){
  	#create the index layer
	layer_index  = distinct(links, layer)
  	number_of_layers = nrow(layer_index)
  	layer_index = cbind(layer_index, layer_num = seq(1, number_of_layers, 1))
  
  	layers_text = c()
  	layers_text_temp = c()
  
  	#add Layer number
  	for (i in 1:number_of_layers) {
    		layers_text[i] = layer_index[i,]$layer
  		}#end for
  
  	# Add the nodes into the #ACTORS subset
  	actors_txt = nodes[,1]
  
  	##Defining the output file and its characteristics
  	# Name of output file
  	outfile <- "results_random/outfile.txt"
  
  	# first line of the file
  	cat("#TYPE","\n", file = outfile)
  
 	# subsequent lines appended to the output file
  	cat("multiplex", "\n\n", file = outfile, append = TRUE)
  	cat("#VERSION", "\n", file = outfile, append = TRUE)
  	cat("3.0", "\n\n", file = outfile, append = TRUE)
  	cat("#LAYERS", "\n", file = outfile, append = TRUE)
  	# Writing the name of Layers and specify if they are directed or undirected (deponding on your network, here is undirected)
  	for (i in 1:number_of_layers) {
    		cat(layers_text[i], file = outfile, append = TRUE)
    		cat(",UNDIRECTED","\n", file = outfile, append = TRUE)
  		}#end for
  	cat("\n", file = outfile, append = TRUE)
  	cat("#ACTOR ATTRIBUTES", "\n\n", file = outfile, append = TRUE)
  	cat("#VERTEX ATTRIBUTES", "\n\n", file = outfile, append = TRUE)
  	cat("#EDGE ATTRIBUTES", "\n\n", file = outfile, append = TRUE)
  	cat("#ACTORS", "\n", file = outfile, append = TRUE)
  	for (i in 1:nrow(nodes)) {
    		cat(nodes[i,1], "\n", file = outfile, append = TRUE)
  		}#end for
  	cat("\n", file = outfile, append = TRUE)
  	cat("#VERTICES","\n", file = outfile, append = TRUE)
  	# Writing nodes names with the layer they belong to
  	for (i in 1:number_of_layers) {
    		for (j in 1:length(actors_txt)) {
      			cat(actors_txt[j], file = outfile, append = TRUE)
      			cat(",", file = outfile, append = TRUE)
      			cat(layer_index[i,1], file = outfile, append = TRUE)
      			cat("\n", file = outfile, append = TRUE)
    			}#end for j
  		}#end for i
  	cat("\n", file = outfile, append = TRUE)
  	cat("#EDGES","\n", file = outfile, append = TRUE)
  	# Writing the links
  	#It is writen in a format of "from" -> "to" and "layer", and they are separated with ","
  	for (i in 1:nrow(links)) {
    		cat(links[i,"from"], file = outfile, append = TRUE)
    		cat(",", file = outfile, append = TRUE)
    		cat(links[i,"to"], file = outfile, append = TRUE)
    		cat(",", file = outfile, append = TRUE)
    		cat(links[i,"layer"], file = outfile, append = TRUE)
    		cat("\n", file = outfile, append = TRUE)
  		}#end for i
  	cat("\n", file = outfile, append = TRUE)
	}#end function


####################################################################################
#returns an index containing the layers and an associated number from the links

Find_layer_index = function(links){
  	layer_index  = distinct(links, layer)#separate the layers that are unique
  	number_of_layers = nrow(layer_index)
  	layer_index = cbind(layer_index, layer_num = seq(1, number_of_layers, 1))#associate a number to each layer
  	return(layer_index)
	}#end function


#####################################################################################
#separates the multiplex network into different igraph networks according to layers. The network input must be in igraph format

Separate_net_by_layers = function(layer_index, net){#try to embed the 'layer index' inside the function
  	net_by_layers = list()
  	number_of_layers = nrow(layer_index)
  	for (i in 1:number_of_layers) {
    		string_temp = layer_index$layer[i]
    		net_by_layers[[i]] = net - E(net)[E(net)$layer!=string_temp]
 		}#end for 
  	return(net_by_layers)
	}#end function


######################################################################################
#plot 2D multiplex networks using Igraph

Custom_plot2D = function(links, nodes, layout = NULL, colorCategory = 1, vertex_label_cex = 0, vertex_size = 3, plot_legend = FALSE){
  
  	net_igraph = graph_from_data_frame(d = links, vertices = nodes, directed = F)
  	V(net_igraph)$color = colorCategory
  
  	if (is.null(layout)) { #if the layout is null, calculate a layout (layout_nicely from igraph)
    		links_no_dupl = links[-which(duplicated(links[,c("from", "to")])==T),] # remove duplicate links not to influence the layout
    		net_layout = graph_from_data_frame(d = links_no_dupl, vertices = nodes, directed = F)
    		layout = layout_nicely(net_layout)
  		}#end if
  
  	vertex_label = nodes$id
  	if (is.null(vertex_label_cex)) {
    		vertex_label = NA
  		}#enfd if
  
  	line_curvature = 0
  	layer_index = Find_layer_index(links)
  	net_by_layers = Separate_net_by_layers(layer_index, net_igraph)
  	number_of_layers = nrow(layer_index)
  
  	# calculating how to lay out the layers in the image
  	par_x = floor(sqrt(number_of_layers)) # x dimention
  	par_y = ceiling(number_of_layers/par_x) #y dimention
  	if (plot_legend && (par_x*par_y) == number_of_layers) {
    		par_y = ceiling(number_of_layers/par_x) + 1
  		}#end if
  
  
  	par(mfrow=c(par_x, par_y), bg = "#f4f4f4", cex = 0.4) #change mfrow to optimize plot space
  	for (i in 1:number_of_layers) {
    		plot(net_by_layers[[i]],
         	vertex.color = V(net_by_layers[[i]])$color,
         	vertex.frame.color= "black",
         	vertex.shape = "circle",
         	vertex.size= vertex_size,
         	vertex.label=vertex_label,
         	vertex.label.color="black",
         	vertex.label.cex=vertex_label_cex,
         	edge.color = "gray",
         	edge.curved=line_curvature,
         	layout=layout,
         	bty = "c",
         	frame = TRUE) 
    		title(layer_index$layer[i],cex.main=3,col.main="#515357") #Print layer name as title
  		}#end for 
  
  
	}#end function


#################################################################################
#plot the G_norm distribution of the network

G_norm_dristrib_plot = function(G_norm_mean_ordered){
  	ggplot() + aes(G_norm_mean_ordered)+ geom_histogram(binwidth=0.1, colour="black", fill="#5195B8") +
    	ggtitle("Distribution of Normalized G") +
    	#coord_cartesian(ylim = c(0,50)) +
    	xlab("G") +
    	labs(x=expression(G["norm"]), y=("Frequency")) +
    	theme(axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18))
	}#end function



##################################################################################
#Returns a dataframe contanining: the actors and number of communities they belong to

Numb_of_com_per_actor = function(cluster){
  	actors = distinct(cluster, actor)
  	df = as.data.frame(actors)
  	unique_temp = cluster
  	for (i in 1:nrow(actors)) {
    		#find the actor records in the cluster
    		unique_temp = cluster[which(cluster$actor == actors[i,1]),]
    		unique_temp = distinct(unique_temp, cid)
    		number = nrow(unique_temp)
    		df[i,2] = number
  		}#end for
  	colnames(df)[2] = "numb_of_modules"
  	return(df)
	}#end function


###################################################################################
#Plotting the curve of a given node versus Omega for different Gamma's 

Plot_Decaimento <- function(node_index, vec_W, seq_G_Merged, seq_G_StdDev = NULL, gnorm_Value = NULL){
  
  	vec_G = as.numeric(seq_G_Merged[node_index, 2:(length(vec_W)+1)])
  
  	if (is.null(seq_G_StdDev)) {
    		vec_SD = rep(0, length(vec_W))
  		}#end if
  	else{
    		vec_SD = as.numeric(seq_G_StdDev[node_index, 2:(length(vec_W)+1)])
  		}#end else
  
  	txt = paste("Gnorm = ", gnorm_Value)
  
  	dataToPlot = as.data.frame(cbind(vec_W, vec_G, vec_SD))
  
  	plt = ggplot(dataToPlot, aes(x=vec_W, y=vec_G)) +  #'aes' defines the axes. 'geom_point' plots the points. 'geom_path' plots the lines
    	geom_point() + geom_path() + geom_errorbar(aes(ymin=vec_G-vec_SD, ymax=vec_G+vec_SD), width=.01, position=position_dodge(0.05)) + 
    	ggtitle(seq_G_Merged[node_index, 1]) + 
    	xlab(expression(paste("Coupling parameter(", omega,")"))) + 
    	ylab("Number of modules the node belongs to (G)") + 
    	theme(title = element_text(size = 16), axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14))
  
  	return(plt)  
	}#end function


####################################################################################
# Convert nodes.csv and links.csv into a multinet type network, which is used for multilayer analysis of the MULTINET package

Convert_to_Multinet <- function(nodes, links){
  	Convert_csv_to_multinet_multiplex(nodes, links) #convert the nodes and links files to the "outfile.txt" file that is importable on multinet
  	net_multinet = read_ml("results_random/outfile.txt", name = "unnamed", sep = ',', aligned = FALSE) #import "outifile.txt" to a multinet type network
  	return(net_multinet)
	}#end function


####################################################################################
# return a database of type 'seq_G_Merged' for a number of partitions of w (Omega)

Create_seq_G_Merged <- function(net_multinet, partitions, omega_initial = 0, gamma = 1){
  	partitions = partitions # number of partitions between zero and 1 for omega
  	seq_G = list() # initialize an array to store the number of groups of each node for different omega values
  	omega = omega_initial # initial value of omega
  	for (i in 1:(partitions+1)) {   
    		glouv = glouvain_ml(net_multinet, gamma=gamma, omega=omega) 
    		seq_G[[i]] = Numb_of_com_per_actor(glouv) #use the 'Numb_of_com_per_actor' function from this script
    		names(seq_G[[i]])[names(seq_G[[i]]) == "numb_of_modules"] <- sprintf("w%#.3f",omega) # rename the column to the omega value used. Important when merging
    		omega = omega + 1/partitions #update the value of omega
  		}#end for
  	seq_G_Merged = seq_G[1] #initializes the database that will join the "Gs" generated in for above
  	for (i in 2:(partitions+1)) {
    		seq_G_Merged = merge(seq_G_Merged, seq_G[i], by="actor") # merge using the 'actor' column as a key
  		}#end for
  	return(seq_G_Merged)
	}#end function


######################################################################################
#returns a numeric vector with omega values according to the input partition size
Create_vec_W <- function(partitions, omega_initial = 0){
  	omega = omega_initial # initial value of omega
  	vec_W = numeric(length = partitions)
  	for (i in 1:(partitions+1)) {
    		vec_W[i] = omega
    		omega = omega + 1/partitions #update the value of omega
 		}#end for
  	return(vec_W)
	}#end function


######################################################################################
#returns a df with the standard deviation of a test that resulted in a list of seq_G

StdDev_list_of_seq_G <- function(seq_G_list){
  	seq_G_StdDev = seq_G_list[[1]]
  	for (i in 1:nrow(seq_G_StdDev)) {
    		for (j in 2:ncol(seq_G_StdDev)) {
      			vector = c()
      			for (k in 1:length(seq_G_list)) {
        			vector[k] = as.numeric(seq_G_list[[k]][i, j])
      				}#end for k
      			seq_G_StdDev[i,j] = sd(vector)
    			}#end for j
  		}#end for i
  	return(seq_G_StdDev)
	}#end function


#######################################################################################
#Given an input df seq_G_Mean, returns a descending-ordered array of the highest indices of G

Sort_Nodes_by_Total_G <- function(seq_G, ordered = FALSE){
  	test = seq_G
  	rownames(test) = seq_G[,1] #transform the first column of a dataframe into the row name. Remember to change if you change the form of SEQ_G
  	test[1] = NULL
  
  	test_sum = rowSums(test)
  	test_sum_mean = mean(test_sum)
  	test_sum_norm = test_sum/test_sum_mean
  
  	if (ordered) {
    		test_ordered = sort(test_sum_norm, decreasing = TRUE)
    		return(test_ordered)
  		}#end if
  	else{
    		return(test_sum_norm)
  		}#end else
  
	}#end function
	
	
########################################################################################
#selection of best two decays, the one closest to the average and the one below the average

Select_Example_Nodes = function(nodes_G_norm_Ordered){
  	selection = nodes_G_norm_Ordered[1:2]
  	selection_mean = nodes_G_norm_Ordered[which.min(abs(nodes_G_norm_Ordered - 1))]
  	selection_bellow = nodes_G_norm_Ordered[runif(1, (as.integer(length(nodes_G_norm_Ordered)*3/4)), (length(nodes_G_norm_Ordered)-1))]
  	selection = append(selection, selection_mean, after = 2)
  	selection = append(selection, selection_bellow, after = 3)
  	return(selection)
	}#end function

#########################################################################################
# Merge a list of dataframes into a single dataframe

Unite_list_of_dataframes = function(df_list){
  	df = ldply(df_list, data.frame)
  	df = as.data.frame(df)
  	return(df)
	}#end function


#########################################################################################
# Extract the G curves for each gamma from a specific node. Important: The nodeName must be a string

G_curves_for_different_gammas = function(df, nodeName, vec_W, gammas){
	
  	df_new = df[which(df$actor==nodeName),]
  	#print (df_new)
  	#prepare the dataframe to be plotted
  	vec_G = as.numeric(df_new[1, 2:(length(vec_W)+1)])
  	vec_gamma = rep(gammas[1], length(vec_G))
  	dataToPlot = as.data.frame(cbind(vec_W, vec_G, vec_gamma))
  	for (i in 2:length(gammas)) {
    		vec_G = as.numeric(df_new[i, 2:(length(vec_W)+1)])
    		vec_gamma = rep(gammas[i], length(vec_G))
    		dataPrep = as.data.frame(cbind(vec_W, vec_G, vec_gamma))
    		dataToPlot = rbind(dataToPlot, dataPrep)
  		}#end for
  
  	#plot the curve families in 2D
  	plt2D = ggplot(dataToPlot, aes(x=vec_W, y=vec_G, colour=factor(vec_gamma))) + #'aes' defines the axes. 'geom_point' plots the points. 'geom_path' plots the lines
    	geom_point() + geom_line() +
    	ggtitle(df_new[1, 1]) + 
    	xlab(expression(paste("Coupling parameter(", omega,")"))) + 
    	ylab("Number of modules the node belongs to (G)") +
    	theme(title = element_text(size = 16), axis.title.x = element_text(size = 16),
        	axis.title.y = element_text(size = 16), axis.text.y = element_text(size = 14),
       	axis.text.x = element_text(size = 14))
  	plt2D = plt2D + scale_colour_discrete(name=expression(paste(gamma)))
  
  	return(plt2D)
	}#end function


########################################################################################
# 3D plot of G, gamma and omega

Plot_G_gamma_omega_suf_3D = function(df, nodeName, vec_W, gammas){
  	df_new = df[which(df$actor==nodeName),]
  
  	#prepare the dataframe to be plotted
  	vec_G = as.numeric(df_new[1, 2:(length(vec_W)+1)])
  	vec_gamma = rep(gammas[1], length(vec_G))
  	dataToPlot = as.data.frame(cbind(vec_W, vec_G, vec_gamma))
  	for (i in 2:length(gammas)) {
    		vec_G = as.numeric(df_new[i, 2:(length(vec_W)+1)])
    		vec_gamma = rep(gammas[i], length(vec_G))
    		dataPrep = as.data.frame(cbind(vec_W, vec_G, vec_gamma))
    		dataToPlot = rbind(dataToPlot, dataPrep)
  		}#end for 
  
  	# plot a 3D of G*W*GAMMA
  	x = dataToPlot$vec_W
  	y = dataToPlot$vec_gamma
  	z = dataToPlot$vec_G
  	s = interp(x,y,z)
  
  	p = persp3D(s$x, s$y, s$z, theta = 120, phi = 35, expand = 0.5,
              xlab =expression("Coupling parameter(omega)"), ylab = expression("Resolution(gamma)"), zlab = "Mean Gnorm",
              ticktype = "detailed", 
              clab = "Mean Gnorm",
              main = nodeName)
              #text(2,9, expression(varomega["1,2"]), cex=2)
  
  	return()
	}#end function


#######################################################################################
#Plot a 2D heatmap G, gamma and omega

Plot_G_gamma_omega_heat_3D = function(df, nodeName, vec_W, gammas){
  	df_new = df[which(df$actor==nodeName),]
  
  	#prepare the dataframe to be plotted
  	vec_G = as.numeric(df_new[1, 2:(length(vec_W)+1)])
  	vec_gamma = rep(gammas[1], length(vec_G))
  	dataToPlot = as.data.frame(cbind(vec_W, vec_G, vec_gamma))
  	for (i in 2:length(gammas)) {
    		vec_G = as.numeric(df_new[i, 2:(length(vec_W)+1)])
    		vec_gamma = rep(gammas[i], length(vec_G))
    		dataPrep = as.data.frame(cbind(vec_W, vec_G, vec_gamma))
    		dataToPlot = rbind(dataToPlot, dataPrep)
  		}#end for
  
  	# plot a surface G*W*GAMMA
  	x = dataToPlot$vec_W
  	y = dataToPlot$vec_gamma
  	z = dataToPlot$vec_G
  	s = interp(x,y,z)
  
  	p = image2D(z = s,
              xlab = (expression(paste("Coupling parameter(", omega,")"))), ylab = (expression(paste("Resolution(", gamma,")"))),
              ticktype = "detailed", 
              clab = "Mean Gnorm",
              main = nodeName)
  
  	return()
	}#end function


#########################################################################################
#Plot the network image highlighting nodes with Gnorm above a threshold. The palette definition is inside the function
# This function uses the Custom_plot2D function. 

Custom_plot2D_destaque_palette = function(nodes_G_norm, links, nodes, layout, size1, size2){
  	#separate and sort the nodes above the G_threshold
  	selected_nodes = sort(nodes_G_norm[which(nodes_G_norm > G_threshold)], decreasing = TRUE)
  
  	##as the minimum of colors that brewer.pal generates is 3, needs to be careful for when there are less than 3 nodes above the G_threshold
  
  	if (length(selected_nodes)<3) {
    		if (length(selected_nodes) == 2) {
      			palette_selected = c("#ff00ff", "#66ff66")
    			}#end if 
    		else{
      			palette_selected = "#ff00ff"
    			} #end else
  		}#end main if
  	else{
    		palette_selected = colorRampPalette(brewer.pal(n = length(selected_nodes), name = "Set1"))(length(selected_nodes))
  		}#end else
  	names(palette_selected) = names(selected_nodes)
  
  	color1 = "#F51d1d"
  	palette = rep(color1, times= length(nodes_G_norm))
  	names(palette) = names(nodes_G_norm)
  	palette[which(names(palette) %in% names(palette_selected))] = palette_selected
  
  	vertex_size = rep(size1, times= length(nodes_G_norm))
  	names(vertex_size) = names(nodes_G_norm)
  	vertex_size[which(names(vertex_size) %in% names(palette_selected))] = size2
  
  	Custom_plot2D(links, nodes, layout, palette, vertex_label_cex = NULL, vertex_size, plot_legend = TRUE)
  
  	plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  	legend("center", inset=.02, title=paste("Species with Gnorm \n greater than ", G_threshold),
         	names(palette_selected), cex=2, pch = 16, col = palette_selected, ncol = 2, bty = "n")
	}#end function


########################################################################################
#returns a dataframe with the network properties

Net_prop = function(net_multinet){
  	lnum = num_layers_ml(net_multinet) #number of network layers
  
  	# is for joining all layers into a single string. Good to use as text
  	lnames = layers_ml(net_multinet)
  	lnames2 = ""
  	for (i in 1:lnum) {
    		if (i==1) {
      			lnames2 = paste(lnames[i])
    			}#end if
    		else if (i==lnum) {
      			lnames2 = paste(lnames2, " e ", lnames[i])
    			}#end elseif
    		else{
      			lnames2 = paste(lnames2, ", ", lnames[i])
    			}#end else
  		}#end for	
  
  	nnum = num_actors_ml(net_multinet, layers = character(0))
  
  	cnum = num_edges_ml(net_multinet)
  
  	#Merge the column of values and property name to assemble the df network_properties
  	numbers = c(lnum, lnames2, nnum, cnum)
  
  	labels = c("Number of Layers", "Type of connections", "Number of nodes", "Number of links")
  
  	#Mount the network properties table
  	network_properties = data.frame(labels, numbers)
  	names(network_properties) = c("Property", "Value")
  
  	return(network_properties)
	}#end function

################################################################################
#Generates a list of each module with its actors. Used in the Change_cid_number function

sep_clusters_actors <- function(cluster){
	cluster_list = list()
  	for (i in 1:(max(cluster$cid)+1)) {
    		cluster_temp = subset(cluster, cluster$cid == i-1)
    		cluster_list[[i]] = cluster_temp
  		}#end for
  	return(cluster_list)
	}

###############################################################################
#Plot of number of modules and modularity value for one iteration

Plot_number_modularity <- function(partitions_of_omega1,gamma_min1,gamma_max1,gamma_spacing1,net_multinet){

	vec_W1 = Create_vec_W(partitions_of_omega1)

	gammas1 = seq(from = gamma_min1, to = gamma_max1, by = gamma_spacing1)

	#Initializes the number of modules array
	modules_quantity = matrix(0, nrow = length(vec_W1), ncol = length(gammas1))

	#Inicializa a matriz de valores de modularidade
	modularity = matrix(0, nrow = length(vec_W1), ncol = length(gammas1))

	for (j in 1:length(gammas1)) {
		cluster = list()
  		cluster_list = list()
  
  		for (i in 1:length(vec_W1) ) {
    			#Calculate communities
    			cluster[[i]] = glouvain_ml(net_multinet, gamma=gammas1[j], omega=vec_W1[i])
    			# Prepare the cluster to be standardized
    			cluster_list[[i]] = sep_clusters_actors(cluster[[i]])
    			#Counts how many modules exists
   			modules_quantity[i,j] = length(cluster_list[[i]])
    			#calculate the modularity
    			modularity[i,j] = modularity_ml(net_multinet, cluster[[i]])
  			}#end for i
  
		}#end for j	  

	#Adjust matrix column names to match gamma and omega values
	colnames(modularity) = gammas1
	rownames(modularity) = vec_W1
	colnames(modules_quantity) = gammas1
	rownames(modules_quantity) = vec_W1

	#######plot of Number of modules

	##trying to make differences between colors of plotted fig
	minm=min(modules_quantity)
	maxm=max(modules_quantity)

	divi=(maxm-minm)/80
	km=seq(minm,maxm,by=divi)

	#Generate the figure with the number of modules for different gamma and omega values
	png("figures/Number_of_modules.png", width = 1000, height = 800)
	pheatmap(modules_quantity, display_numbers = T, kmeans_k = NA, cluster_rows = FALSE,
         	cluster_cols = FALSE, show_rownames = T, show_colnames = T, fontsize = 20,
         	fontsize_number = 15, fontsize_row = 20, fontsize_col = 20, angle_col = 0,
         	legend = F, number_format = "%i", main = "Number of Modules",
         	border_color = "white", breaks = km,xlab = "specification variables", ylab =  "Car Models")

	dev.off()
	#print (modularity)



	########plot of Modularity
	minm=min(modularity)
	maxm=max(modularity)

	divi=(maxm-minm)/80
	km=seq(minm,maxm,by=divi)

	# Generate the figure with the modularity value for different gamma and omega values
	png("figures/Modularity.png", width = 1000, height = 800)
	pheatmap(modularity, display_numbers = T, kmeans_k = NA, cluster_rows = FALSE,
         	cluster_cols = FALSE, show_rownames = T, show_colnames = T, fontsize = 20,
         	fontsize_number = 15, fontsize_row = 20, fontsize_col = 20, angle_col = 0,
         	legend = F, number_format = "%0.3f",main = "Modularity",
         	border_color = "white", breaks = km)
	dev.off()
	return()
	}
	
#############################################################################

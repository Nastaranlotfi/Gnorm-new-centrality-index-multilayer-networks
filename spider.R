################################################################################
#### Ecological Synthesis Lab (SintECO): https://marcomellolab.wordpress.com

#### Authors: Nastaran Lotfi, Henrique S. Requejo, Francisco Rodrigues &
####          Marco A. R. Mello

#### See README for further info:
#### https://github.com/Nastaranlotfi/Test1-code#readme
################################################################################


################### SET UP #####################################################


library(ggplot2)
library(png)
library(fmsb)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


################### PLOTTING FUNCTIONS #########################################


create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "white", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}


Ploting_bats<-function(selection,main_data, v_bats,savename){

clo=clo_bats[selection]
btw=btw_bats[selection]
eig=eig_bats[selection]
deg=deg_bats[selection]
G=Gnorm_bats[selection]

Name=v_bats$code[match(selection,v_bats$name)]


Ploting_plants<-function(selection,main_data, v_plants,savename){
  
  clo=clo_plants[selection]
  btw=btw_plants[selection]
  eig=eig_plants[selection]
  deg=deg_plants[selection]
  G=Gnorm_plants[selection]
  
  Name=v_plants$code[match(selection,v_plants$name)]

	
### Data import
data <- data.frame( row.names = Name, Closeness=clo,
                    Betweenness=btw, Eigenvector=eig, Degree=deg, Gnorm=G)

max_min <- data.frame( Closeness=c(max(clo_bats),0),
                       Betweenness= c(max(btw_bats),0),
                       Eigenvector=c(max(eig_bats),0),
                       Degree=c(max(deg_bats),0),
                       Gnorm=c(max(Gnorm_bats),min(Gnorm_bats)) )
rownames(max_min) <- c("Max", "Min")

df <- rbind(max_min, data)

### Plotting setup
png_name = paste("figures/",savename,".png", sep = "")
png(png_name, width = 700, height = 700)

op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(data = df,
                            color = c("#FFC0CB","#87CEFA", "#FFDAB9",
                                      "#90EE90", "#EEE8AA"))
# Add an horizontal legend
legend(
  x = "left", legend = rownames(df[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col =c("#FFC0CB","#87CEFA", "#FFDAB9",
                               "#90EE90", "#EEE8AA"),
 	text.col = "black", cex = 1, pt.cex = 1.5
  	)
	par(op)
	dev.off()
}

### Prepare the data for plotting

data <- data.frame( row.names = Name, Closeness=clo,
	                    Betweenness=btw, Eigenvector=eig, Degree=deg, Gnorm=G)



max_min <- data.frame( Closeness=c(max(clo_plants),0),
	                       Betweenness= c(max(btw_plants),0),
	                       Eigenvector=c(max(eig_plants),0),
	                       Degree=c(max(deg_plants),0),
	                       Gnorm=c(max(Gnorm_plants),min(Gnorm_plants)) )
rownames(max_min) <- c("Max", "Min")

df <- rbind(max_min, data)

### Plot the data
png_name = paste("figures/",savename,".png", sep = "")
png(png_name, width = 700, height = 700)

op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(data = df,
	                            color = c("#FFC0CB","#87CEFA",
	                                      "#FFDAB9", "#90EE90", "#EEE8AA"))
# Add an horizontal legend
legend(
  x = "left", legend = rownames(df[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col =c("#FFC0CB","#87CEFA", "#FFDAB9",
 	                              "#90EE90", "#EEE8AA"),
  text.col = "black", cex = 1, pt.cex = 1.5
  )
par(op)
dev.off()

}


################### DATA IMPORT AND PLOTTING ###################################


######## Bats

main_data=load("results/bats_bats_allCentr.RData")
v_bats=read.csv('input/bats_code.csv',header=T, as.is=T)


G1 = sort(Gnorm_bats,decreasing=TRUE)

### Find the first 5
selection = names(G1[1:5])

Ploting_bats(selection,main_data, v_bats,"1-5_first_bat")
cat("'end first 1-5 bats\n")

### Find the 5 second

selection = names(G1[6:10])

Ploting_bats(selection,main_data, v_bats,"6-10_first_bat")

cat("'end first 6-10 bats\n")

### Reverse
G1 = sort(Gnorm_bats,decreasing=FALSE)

### Find the last 5
selection = names(G1[1:5])

Ploting_bats(selection,main_data, v_bats,"1-5_end_bat")
cat("'end last 1-5 bats\n")

### Find the 5 second

selection = names(G1[6:10])

Ploting_bats(selection,main_data, v_bats,"6-10_end_bat")

cat("'end last 6-10 bats\n")


######## Plants

main_data=load("results/bats_plants_allCentr.RData")
v_plants=read.csv('input/plants_code.csv',header=T, as.is=T)


G1 = sort(Gnorm_plants,decreasing=TRUE)

### Find the first 5
selection = names(G1[1:5])

Ploting_plants(selection,main_data, v_plants,"1-5_first_plant")
cat("'end first 1-5 plants\n")

### Find the 5 second

selection = names(G1[6:10])

Ploting_plants(selection,main_data, v_plants,"6-10_first_plant")
cat("'end first 6-10 plants\n")

### Reverse
G1 = sort(Gnorm_plants,decreasing=FALSE)

### Find the last 5
selection = names(G1[1:5])

Ploting_plants(selection,main_data, v_plants,"1-5_end_plant")
cat("'end last 1-5 plants\n")

### Find the 5 second

selection = names(G1[6:10])

Ploting_plants(selection,main_data, v_plants,"6-10_end_plant")
cat("'end last 6-10 plants\n")


#Funcoes auxiliares---------------------------------------------------------------------------------------------

# converte um csv nodes e links em um arquivo de entrada multinet do tipo MULTIPLEX (VErsion 3.0)
Convert_csv_to_multinet_multiplex = function(nodes, links){
  #cria o layer index
  layer_index  = distinct(links, layer)
  number_of_layers = nrow(layer_index)
  layer_index = cbind(layer_index, layer_num = seq(1, number_of_layers, 1))
  
  layers_text = c()
  layers_text_temp = c()
  
  #adiciona o nome das camadas
  for (i in 1:number_of_layers) {
    layers_text[i] = layer_index[i,]$layer
  }
  
  # faz o subset dos nodes para a categoria #ACTORS
  actors_txt = nodes[,1]
  
  # name of output file
  outfile <- "outfile.txt"
  
  # first line of the file
  cat("#TYPE","\n", file = outfile)
  
  # subsequent lines appended to the output file
  cat("multiplex", "\n\n", file = outfile, append = TRUE)
  cat("#VERSION", "\n", file = outfile, append = TRUE)
  cat("3.0", "\n\n", file = outfile, append = TRUE)
  cat("#LAYERS", "\n", file = outfile, append = TRUE)
  # escreve as permutaoes das layers
  for (i in 1:number_of_layers) {
    cat(layers_text[i], file = outfile, append = TRUE)
    cat(",UNDIRECTED","\n", file = outfile, append = TRUE)
  }
  cat("\n", file = outfile, append = TRUE)
  # vou manter esse vazio por enquanto
  cat("#ACTOR ATTRIBUTES", "\n\n", file = outfile, append = TRUE)
  cat("#VERTEX ATTRIBUTES", "\n\n", file = outfile, append = TRUE)
  cat("#EDGE ATTRIBUTES", "\n\n", file = outfile, append = TRUE)
  cat("#ACTORS", "\n", file = outfile, append = TRUE)
  for (i in 1:nrow(nodes)) {
    cat(nodes[i,1], "\n", file = outfile, append = TRUE)
  }
  cat("\n", file = outfile, append = TRUE)
  cat("#VERTICES","\n", file = outfile, append = TRUE)
  # escreve todos os atores em todas as camadas
  for (i in 1:number_of_layers) {
    for (j in 1:length(actors_txt)) {
      cat(actors_txt[j], file = outfile, append = TRUE)
      cat(",", file = outfile, append = TRUE)
      cat(layer_index[i,1], file = outfile, append = TRUE)
      cat("\n", file = outfile, append = TRUE)
    }
  }
  cat("\n", file = outfile, append = TRUE)
  cat("#EDGES","\n", file = outfile, append = TRUE)
  # escreve os links
  # IMPORTANTE: os links devem ter seus headers nomeados como "from" "to" e "layer"
  for (i in 1:nrow(links)) {
    cat(links[i,"from"], file = outfile, append = TRUE)
    cat(",", file = outfile, append = TRUE)
    cat(links[i,"to"], file = outfile, append = TRUE)
    cat(",", file = outfile, append = TRUE)
    cat(links[i,"layer"], file = outfile, append = TRUE)
    cat("\n", file = outfile, append = TRUE)
  }
  cat("\n", file = outfile, append = TRUE)
}

#retorna um index contendo as camadas e um numero associado a partir dos links
Find_layer_index = function(links){
  layer_index  = distinct(links, layer)#separa as layers que sao unicas
  number_of_layers = nrow(layer_index)
  layer_index = cbind(layer_index, layer_num = seq(1, number_of_layers, 1))#associa um numero para cada layer
  return(layer_index)
}

#separa a rede multiplex em diferentes redes igraph de acordo com as camadas. A entrada de rede deve ser no formato igraph
Separate_net_by_layers = function(layer_index, net){#TODO: tentar embutir o 'layer_index' dentro da funcao
  net_by_layers = list()
  number_of_layers = nrow(layer_index)
  for (i in 1:number_of_layers) {
    string_temp = layer_index$layer[i]
    net_by_layers[[i]] = net - E(net)[E(net)$layer!=string_temp]
  } 
  return(net_by_layers)
}

#plota redes multiplex em 2D usando o Igraph
Custom_plot2D = function(links, nodes, layout = NULL, colorCategory = 1, vertex_label_cex = 0, vertex_size = 3, plot_legend = FALSE){
  
  net_igraph = graph_from_data_frame(d = links, vertices = nodes, directed = F)
  V(net_igraph)$color = colorCategory
  
  if (is.null(layout)) { #caso o layout seja null, calcula um layout (layout_nicely do igraph)
    links_no_dupl = links[-which(duplicated(links[,c("from", "to")])==T),] # retira os duplicados para nao influenciar no layout
    net_layout = graph_from_data_frame(d = links_no_dupl, vertices = nodes, directed = F)
    layout = layout_nicely(net_layout)
  }
  
  vertex_label = nodes$id
  if (is.null(vertex_label_cex)) {
    vertex_label = NA
  }
  
  line_curvature = 0
  layer_index = Find_layer_index(links)
  net_by_layers = Separate_net_by_layers(layer_index, net_igraph)
  number_of_layers = nrow(layer_index)
  
  # calcula como dispor as camadas na imagem
  par_x = floor(sqrt(number_of_layers))
  par_y = ceiling(number_of_layers/par_x)
  if (plot_legend && (par_x*par_y) == number_of_layers) {
    par_y = ceiling(number_of_layers/par_x) + 1
  }
  
  
  par(mfrow=c(par_x, par_y), bg = "#f4f4f4", cex = 0.4) # TODO: mudar o mfrow para otimizar o espaco do plot
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
         frame = TRUE) #TODO: ver o que eh esse parametro
    title(layer_index$layer[i],cex.main=3,col.main="#515357") #mostra o nome da camada como titulo
  }
  
  
}

#plota a distribuição de G_norm da rede
G_norm_dristrib_plot = function(G_norm_mean_ordered){
  ggplot() + aes(G_norm_mean_ordered)+ geom_histogram(binwidth=0.1, colour="black", fill="#5195B8") +
    ggtitle("Distribuicao de G normalizado") +
    #coord_cartesian(ylim = c(0,50)) +
    xlab("G") +
    labs(x=expression(G["norm"]), y=("Frequência")) +
    theme(axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18))
}

#Retorna um dataframe com os atores e quantas comunidaes eles fazem parte
Numb_of_com_per_actor = function(cluster){
  actors = distinct(cluster, actor)
  df = as.data.frame(actors)
  unique_temp = cluster
  for (i in 1:nrow(actors)) {
    #encontrar os registros do ator no cluster
    unique_temp = cluster[which(cluster$actor == actors[i,1]),]
    unique_temp = distinct(unique_temp, cid)
    number = nrow(unique_temp)
    df[i,2] = number
  }
  colnames(df)[2] = "numb_of_modules"
  return(df)
}

# plota a curva de decaimento de um noh dado um dataframe tipo 'seq_G_merged' , o numero do noh e um vetor de valores de w
#TODO: Acho que da pra melhorar pedindo de entrada apenas um dataframe  com  os valores de w embutidos
Plot_Decaimento <- function(node_index, vec_W, seq_G_Merged, seq_G_StdDev = NULL, gnorm_Value = NULL){
  
  vec_G = as.numeric(seq_G_Merged[node_index, 2:(length(vec_W)+1)])
  
  if (is.null(seq_G_StdDev)) {
    vec_SD = rep(0, length(vec_W))
  }else{
    vec_SD = as.numeric(seq_G_StdDev[node_index, 2:(length(vec_W)+1)])
  }
  
  txt = paste("Gnorm = ", gnorm_Value)
  
  dataToPlot = as.data.frame(cbind(vec_W, vec_G, vec_SD))
  
  plt = ggplot(dataToPlot, aes(x=vec_W, y=vec_G)) + #'aes' define os eixos. 'geom_point' plota os pontos. 'geom_path' plota as linhas
    geom_point() + geom_path() + geom_errorbar(aes(ymin=vec_G-vec_SD, ymax=vec_G+vec_SD), width=.01, position=position_dodge(0.05)) + 
    ggtitle(seq_G_Merged[node_index, 1]) + 
    xlab(expression(paste("Parâmetro de acoplamento(", omega,")"))) + 
    ylab("Número de módulos que o nó pertence (G)") +
    #annotate("text", x = 0.9, y=2, size = 7, label = txt) + 
    theme(title = element_text(size = 16), axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16), axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14))
  
  return(plt)  
}

# Converte nodes.csv e links.csv em uma rede tipo  multinet, que eh usada para analise multicamada do pacota MULTINET
Convert_to_Multinet <- function(nodes, links){
  Convert_csv_to_multinet_multiplex(nodes, links) #converte os arquivos nodes e links para o arquivo "outfile.txt" que eh importavel no multinet
  net_multinet = read_ml("outfile.txt", name = "unnamed", sep = ',', aligned = FALSE) #importa o "outifile.txt" para uma rede do tipo multinet
  return(net_multinet)
}

# retorna um banco de dados do tipo 'seq_G_Merged' para um numero de particoes de w
Create_seq_G_Merged <- function(net_multinet, partitions, omega_inicial = 0, gamma = 1){
  partitions = partitions # numero de particoes entre zero e 1 para omega
  seq_G = list() # inicializa um vetor para armazenar o numero de grupos de cada noh para diferentes valores de omega
  omega = omega_inicial # valor inicial de omega
  for (i in 1:(partitions+1)) {
    #glouv = glouvain_ml(net_multinet, gamma=gamma, omega=omega, limit=0) #calcula os modulos usado o Louvain (pacote multinet)
    glouv = glouvain_ml(net_multinet, gamma=gamma, omega=omega) 
    seq_G[[i]] = Numb_of_com_per_actor(glouv) #usa a funcao 'Numb_of_com_per_actor' deste script
    names(seq_G[[i]])[names(seq_G[[i]]) == "numb_of_modules"] <- sprintf("w%#.3f",omega) # muda o nome da coluna para o valor de omega usado. Importante na hora do merge
    omega = omega + 1/partitions #atualiza o valor de omega
  }
  seq_G_Merged = seq_G[1] #inicializa o banco de dados que vai unir os "Gs" gerados no for acima
  for (i in 2:(partitions+1)) {
    seq_G_Merged = merge(seq_G_Merged, seq_G[i], by="actor") # faz um merge usando como key a coluna 'actor'
  }
  return(seq_G_Merged)
}

#retorna um vetor numerico com os valores de omega de acordo com o tamanho da particao de entrada
Create_vec_W <- function(partitions, omega_inicial = 0){
  omega = omega_inicial # valor inicial de omega
  vec_W = numeric(length = partitions)
  for (i in 1:(partitions+1)) {
    vec_W[i] = omega
    omega = omega + 1/partitions #atualiza o valor de omega
  }
  return(vec_W)
}

#retorna um df com o desvio padr?o de um ensaio que obteve como resultado uma lista de seq_G
StdDev_list_of_seq_G <- function(seq_G_list){
  seq_G_StdDev = seq_G_list[[1]]
  for (i in 1:nrow(seq_G_StdDev)) {
    for (j in 2:ncol(seq_G_StdDev)) {
      vector = c()
      for (k in 1:length(seq_G_list)) {
        vector[k] = as.numeric(seq_G_list[[k]][i, j])
      }
      seq_G_StdDev[i,j] = sd(vector)
    }
  }
  return(seq_G_StdDev)
}

#Dado um df de entrada seq_G_Mean, retorna um array ordenado de forma decrescente dos maiores indices de G
Sort_Nodes_by_Total_G <- function(seq_G, ordered = FALSE){
  teste = seq_G
  rownames(teste) = seq_G[,1] #transforma a primeira coluna de um dataframe no nome da linha. Lembrar de mudar caso mude a forma de SEQ_G
  teste[1] = NULL
  
  teste_sum = rowSums(teste)
  teste_sum_mean = mean(teste_sum)
  teste_sum_norm = teste_sum/teste_sum_mean
  
  if (ordered) {
    teste_ordered = sort(teste_sum_norm, decreasing = TRUE)
    return(teste_ordered)
  }else{
    return(teste_sum_norm)
  }
  
}

#seleciona os dois melhores decaimentos, o mais próximo da média e um abaixo da média
Select_Example_Nodes = function(nodes_G_norm_Ordered){
  selection = nodes_G_norm_Ordered[1:2]
  selection_mean = nodes_G_norm_Ordered[which.min(abs(nodes_G_norm_Ordered - 1))]
  selection_bellow = nodes_G_norm_Ordered[runif(1, (as.integer(length(nodes_G_norm_Ordered)*3/4)), (length(nodes_G_norm_Ordered)-1))]
  selection = append(selection, selection_mean, after = 2)
  selection = append(selection, selection_bellow, after = 3)
  return(selection)
}

#Une uma lista de dataframes em um unico dataframe
Unite_list_of_dataframes = function(df_list){
  df = ldply(df_list, data.frame)
  df = as.data.frame(df)
  return(df)
}

#Extrai as curvas G para cada gamma de um noh especifico. O nodeName deve ser uma string
G_curves_for_different_gammas = function(df, nodeName, vec_W, gammas){
  df_new = df[which(df$actor==nodeName),]
  
  #prepara o dataframe que sera plotado
  vec_G = as.numeric(df_new[1, 2:(length(vec_W)+1)])
  vec_gamma = rep(gammas[1], length(vec_G))
  dataToPlot = as.data.frame(cbind(vec_W, vec_G, vec_gamma))
  for (i in 2:length(gammas)) {
    vec_G = as.numeric(df_new[i, 2:(length(vec_W)+1)])
    vec_gamma = rep(gammas[i], length(vec_G))
    dataPrep = as.data.frame(cbind(vec_W, vec_G, vec_gamma))
    dataToPlot = rbind(dataToPlot, dataPrep)
  }
  
  #plota as familas de curvas G em 2D
  plt2D = ggplot(dataToPlot, aes(x=vec_W, y=vec_G, colour=factor(vec_gamma))) + #'aes' define os eixos. 'geom_point' plota os pontos. 'geom_path' plota as linhas
    geom_point() + geom_line() +
    ggtitle(df_new[1, 1]) + 
    xlab(expression(paste("Parâmetro de acoplamento(", omega,")"))) + 
    ylab("Numero de módulos que o nó pertence (G)") +
    #annotate("text", x = 0.9, y=2, size = 7, label = txt) + 
    theme(title = element_text(size = 16), axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16), axis.text.y = element_text(size = 14),
          axis.text.x = element_text(size = 14))
  plt2D = plt2D + scale_colour_discrete(name=expression(paste(gamma)))
  
  return(plt2D)
}

#Plota a superficie 3D G, gamma e omega
Plot_G_gamma_omega_suf_3D = function(df, nodeName, vec_W, gammas){
  df_new = df[which(df$actor==nodeName),]
  
  #prepara o dataframe que sera plotado
  vec_G = as.numeric(df_new[1, 2:(length(vec_W)+1)])
  vec_gamma = rep(gammas[1], length(vec_G))
  dataToPlot = as.data.frame(cbind(vec_W, vec_G, vec_gamma))
  for (i in 2:length(gammas)) {
    vec_G = as.numeric(df_new[i, 2:(length(vec_W)+1)])
    vec_gamma = rep(gammas[i], length(vec_G))
    dataPrep = as.data.frame(cbind(vec_W, vec_G, vec_gamma))
    dataToPlot = rbind(dataToPlot, dataPrep)
  }
  
  # plota a superficie GxWxGAMMA
  x = dataToPlot$vec_W
  y = dataToPlot$vec_gamma
  z = dataToPlot$vec_G
  s = interp(x,y,z)
  
  p = persp3D(s$x, s$y, s$z, theta = 120, phi = 35, expand = 0.5,
              xlab = "Acoplamento", ylab = "Resolução", zlab = "Gnorm médio",
              ticktype = "detailed", 
              clab = "Gnorm médio",
              main = nodeName)
  
  return()
}

#Plota um heatmap 2D G, gamma e omega
Plot_G_gamma_omega_heat_3D = function(df, nodeName, vec_W, gammas){
  df_new = df[which(df$actor==nodeName),]
  
  #prepara o dataframe que sera plotado
  vec_G = as.numeric(df_new[1, 2:(length(vec_W)+1)])
  vec_gamma = rep(gammas[1], length(vec_G))
  dataToPlot = as.data.frame(cbind(vec_W, vec_G, vec_gamma))
  for (i in 2:length(gammas)) {
    vec_G = as.numeric(df_new[i, 2:(length(vec_W)+1)])
    vec_gamma = rep(gammas[i], length(vec_G))
    dataPrep = as.data.frame(cbind(vec_W, vec_G, vec_gamma))
    dataToPlot = rbind(dataToPlot, dataPrep)
  }
  
  # plota a superficie GxWxGAMMA
  x = dataToPlot$vec_W
  y = dataToPlot$vec_gamma
  z = dataToPlot$vec_G
  s = interp(x,y,z)
  
  p = image2D(z = s,
              xlab = "Acoplamento", ylab = "Resolução",
              ticktype = "detailed", 
              clab = "Gnorm médio",
              main = nodeName)
  
  return()
}

#Plota a imagem da rede destacando os nos com Gnorm acima de um treshold. A definicao da paleta esta dentro da funcao
# Esta funcao usa a funcao Custom_plot2D. TODO: reduzir para deixar a funçao plot_2D externa
Custom_plot2D_destaque_palette = function(nodes_G_norm, links, nodes, layout, size1, size2){
  #separa e ordena os nos acima do G_threshold
  selected_nodes = sort(nodes_G_norm[which(nodes_G_norm > G_threshold)], decreasing = TRUE)
  
  #como o minimo de cores que o brewer.pal gera são 3, tive que fazer essa checagem para quando existirem menos de 3 nos acima do G_threshold
  # TODO: Otimizar e transformar em funcao
  if (length(selected_nodes)<3) {
    if (length(selected_nodes) == 2) {
      palette_selected = c("#ff00ff", "#66ff66")
    }else{
      palette_selected = "#ff00ff"
    }  
  }else{
    palette_selected = colorRampPalette(brewer.pal(n = length(selected_nodes), name = "Set1"))(length(selected_nodes))
  }
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
  legend("center", inset=.02, title=paste("Especies com Gnorm \n maiores que ", G_threshold),
         names(palette_selected), cex=2, pch = 16, col = palette_selected, ncol = 2, bty = "n")
}

#retorna um dataframe com as propriedades da rede
Net_prop = function(net_multinet){
  lnum = num_layers_ml(net_multinet) #numero de camadas da rede
  
  # serve para juntar toas as camadas em uma string soh. Bom para usar como texto
  lnames = layers_ml(net_multinet)
  lnames2 = ""
  for (i in 1:lnum) {
    if (i==1) {
      lnames2 = paste(lnames[i])
    }
    else if (i==lnum) {
      lnames2 = paste(lnames2, " e ", lnames[i])
    }
    else{
      lnames2 = paste(lnames2, ", ", lnames[i])
    }
  }
  
  nnum = num_actors_ml(net_multinet, layers = character(0))
  
  cnum = num_edges_ml(net_multinet)
  
  #Monta a coluna de valores e nome da propriedade para montar o df propriedades_da_rede
  numbers = c(lnum, lnames2, nnum, cnum)
  
  labels = c("N?mero de Camadas", "Tipo de conex?es", "N?mero de n?s", "N?mero de conex?es")
  
  #Monta a tabela de prorpiedades da rede
  propriedades_rede = data.frame(labels, numbers)
  names(propriedades_rede) = c("Propriedade", "Valor")
  
  return(propriedades_rede)
}

#-------------------------------------------------------------------

# Test1-code

## A new centrality index for multilayer networks

Authors: Nastaran Lotfi, Henrique Requejo, Francisco Rodrigues & Marco Mello

Follow-up of Henrique Requejo's BSc monograph:

Requejo HS. 2021. Um teste do algoritmo de modularidade Louvain como uma ferramenta para detectar espécies-chave em redes de interações multicamada. Honors Degree Monograph, Computational and Applied Mathematics, Universidade de São Paulo, São Paulo, Brazil. Advisor: Mello MAR.

Original source: <https://github.com/marmello77/TF>

[Ecological Synthesis Lab](https://marcomellolab.wordpress.com) (SintECO), University of São Paulo.

E-mails: nas.naslot@gmail.com OR marmello@usp.br

First published on June 15th, 2022 (English version).

Run in R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics".

Disclaimer: You may freely use the software and data provided here for any purposes at your own risk. We assume no responsibility or liability for the use of this material, convey no license or title under any patent, copyright, or mask work right to the product. We reserve the right to make changes in the material without notification. We also make no representation or warranty that such application will be suitable for the specified use without further testing or modification. If this material helps you produce any academic work (paper, book, chapter, monograph, dissertation, report, talk, lecture or similar), please acknowledge the authors and cite the original data paper and this repository.

## Functionality and origin

The data and code provided here aim at making reproducible the graphical and numerical analyses presented in our data paper.


## List of folders and files

1.  Input (folder)

    a.  links_clean.csv -\> Main input, contains connections and the layer they belong to.
    
    b.  Names_impo.csv -\> contains the names of important Bat species which were obtained from other research.
    
    c.  Names_impo_plants.csv -\> contains the name of important Plants species which were obtained from other research.
    
    d.  bats_code.csv -\> Name of the Bat species with their abbreviation format
    
    d.  plants_code.csv -\> Name of the plant species with their abbreviation format
       
    
2.  Data (folder)

    a.  links1.csv -\> Produced with test1.R, links of the whole network

    b.  links2.csv -\> Produced with test1.R, links of the maximum component of the network

    c.  nodes1.csv -\> Produced with test1.R, name of the nodes of the whole network
    
    d.  nodes2.csv -\> Produced with test1.R, name of the nodes of the maximum component of the network
    

    
3.  Figures (folder)

    a.  ..._end_bat/plant.png

    b.  ..._firts_bat/plant.png

    c.  Bats/Plants_10last/10top_Gnorm_name of species_2d/3d/heat.png

    d.  Bats/Plants_btw/Clo/Deg/Eig_name of species_2d/3d/heat.png

    e.  Btas_Gnorm_name of species_2d/3d/heat.png
    
    f.  Modularity.png
    
    g.  Number_of_modules.png
    
    h.  hist_Gnorm.png
    
    i. important__name of species_2d/3d/heat.png
    
    j.  Network_visualization_complete.png
    
    k.  Network_visualization_component.png
    
    l. C_correlogram_bats_bats/plants/all_pearson/spearman.png
    
    
4.  data_random

    a.  rand_total_..._links.csv  -\> random network produced with the same size of real network or maximum component
    
    b.  rand_total.RData -\> results obtained for Gnorm analysis of random network    
    
5.  figures_random (folder)

    a.  hist_Gnorm_random.png

6.  Code (main page)

    a.  Aux_function.R -\> script containing the functions used in other scripts

    b.  Aux_function_random.R -\> script containing the functions used in random network scripts

    c.  test1.R -\> script for making a network, analyzing Gnorm, calculation of closeness, degree, eigenvector and betweenness centralities, plotting the related figures.

    d.  random_final.R -\> Generate the random network, calculates the Gnorm, and plots the hist Gnorm of random network

    e.  spider.R -\> Generates the spider plot
            
        
        
7.   results

    a. Bat/Plant_impo_btw/eig/deg/clo/Gnorm.RData
    
    b. Bat_Net_RData
    
    c. bats_allCentr.RData
    
    d. bats_bats/plants_allCentr.RData
    
    e. outfile.txt
    
    f. similarity_bat/plant_Net.RData
    
    g. timers.txt


## Instructions

1.  Run the respective script to reproduce the chosen figure or table;

    Order of running programs:
        aa.   test1.R  (main code)
        bb.   spider.R  (producing spider plot)
        cc.   random_final.R (main code for random network)
        
        
2.  Follow the instructions provided in the script;

3.  Check the file in the Figures folder.


## Feedback

If you have any questions, corrections, or suggestions, please feel free to open an [issue](https://github.com/Nastaranlotfi/Test1-code/issues) or make a [pull request](https://github.com/Nastaranlotfi/Test1-code/pulls).

## Acknowledgments
N. Lotfi is thankful to the FAPESP (grant with number 2020/08359-1) for the support given to this research



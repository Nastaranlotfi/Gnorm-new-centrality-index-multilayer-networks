# Test1-code

## Supplement to the paper: Lofti N, Requejo HS, Rodrigues F, Mello MAR. A new centrality index for multilayer networks. *In prep*.

Authors: Nastaran Lotfi, Henrique S. Requejo, Francisco A. Rodrigues & Marco A. R. Mello

Follow-up of Henrique Requejo's BSc monograph:

Requejo HS. 2021. Um teste do algoritmo de modularidade Louvain como uma ferramenta para detectar espécies-chave em redes de interações multicamada. Honors Degree Monograph, Graduação em Matemática Aplicada e Computacional, Instituto de Matemática e Estatística, Universidade de São Paulo, São Paulo, Brazil. Advisor: Mello MAR.

[Ecological Synthesis Lab](https://marcomellolab.wordpress.com) (SintECO), University of São Paulo.

E-mails: [nas.naslot\@gmail.com](mailto:nas.naslot@gmail.com){.email} OR [marmello\@usp.br](mailto:marmello@usp.br){.email}

First published on June 15th, 2022 (English version).

Run in R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics".

Disclaimer: You may freely use the software and data provided here for any purposes at your own risk. We assume no responsibility or liability for the use of this material, convey no license or title under any patent, copyright, or mask work right to the product. We reserve the right to make changes in the material without notification. We also make no representation or warranty that such application will be suitable for the specified use without further testing or modification. If this material helps you produce any academic work (paper, book, chapter, monograph, dissertation, report, talk, lecture or similar), please acknowledge the authors and cite the original data paper and this repository.

## Functionality and origin

The data and code provided here aim at making reproducible the graphical and numerical analyses presented in our paper.

## List of folders and files

1.  Input (folder)

    a.  links_clean.csv -\> Main input, contains connections and the layer they belong to.

    b.  Names_impo.csv -\> contains the names of important Bat species which were obtained from other research.

    c.  Names_impo_plants.csv -\> contains the name of important Plants species which were obtained from other research.

    d.  bats_code.csv -\> Name of the Bat species with their abbreviation format

    e.  plants_code.csv -\> Name of the plant species with their abbreviation format

2.  Data (folder)

    a.  links1.csv -\> Produced with test1.R, links of the whole network

    b.  links2.csv -\> Produced with test1.R, links of the maximum component of the network

    c.  nodes1.csv -\> Produced with test1.R, name of the nodes of the whole network

    d.  nodes2.csv -\> Produced with test1.R, name of the nodes of the maximum component of the network

3.  Figures (folder)

    a.  ...\_end_bat/plant.png

    b.  ...\_firts_bat/plant.png

    c.  Bats/Plants_10last/10top_Gnorm_name of species_2d/3d/heat.png

    d.  Bats/Plants_btw/Clo/Deg/Eig_name of species_2d/3d/heat.png

    e.  Btas_Gnorm_name of species_2d/3d/heat.png

    f.  Modularity.png

    g.  Number_of_modules.png

    h.  hist_Gnorm.png

    i.  important\_\_name of species_2d/3d/heat.png

    j.  Network_visualization_complete.png

    k.  Network_visualization_component.png

    l.  C_correlogram_bats_bats/plants/all_pearson/spearman.png

4.  data_random

    a.  rand_total\_...\_links.csv -\> random network produced with the same size of real network or maximum component

    b.  rand_total.RData -\> results obtained for Gnorm analysis of random network

5.  figures_random (folder)

    a.  hist_Gnorm_random.png

6.  Code (main page)

    a.  Aux_function.R -\> script containing the functions used in other scripts

    b.  Aux_function_random.R -\> script containing the functions used in random network scripts

    c.  test1.R -\> script for making a network, analyzing Gnorm, calculation of closeness, degree, eigenvector and betweenness centralities, plotting the related figures.

    d.  random_final.R -\> Generate the random network, calculates the Gnorm, and plots the hist Gnorm of random network

    e.  spider.R -\> Generates the spider plot

7.  results

    a. Bat/Plant_impo_btw/eig/deg/clo/Gnorm.RData

    b. Bat_Net_RData

    c. bats_allCentr.RData

    d. bats_bats/plants_allCentr.RData

    e. outfile.txt

    f. similarity_bat/plant_Net.RData

    g. timers.txt

## Instructions

1.  Run the respective script to reproduce the chosen figure or table;

    Order of running programs: aa. test1.R (main code) bb. spider.R (producing spider plot) cc. random_final.R (main code for random network)

2.  Follow the instructions provided in the script;

3.  Check the file in the Figures folder.

## Feedback

If you have any questions, corrections, or suggestions, please feel free to open an [issue](https://github.com/Nastaranlotfi/Test1-code/issues) or make a [pull request](https://github.com/Nastaranlotfi/Test1-code/pulls).

## Acknowledgments

We are grateful to our lab mates and institutions, who helped us at different stages of this project. This study is derived from the B.Sc. monograph of H.S. Requejo. C. Emer participated in the defense committee and contributed with insightful suggestions. N. Lotfi is thankful to the FAPESP (grant with number 2020/08359-1) for the support given to this research. MARM was funded by the Alexander von Humboldt Foundation (AvH, 3.4-8151/15037 and 3.2-BRA/1134644), National Council for Scientific and Technological Development (CNPq, 304498/2019-0), São Paulo Research Foundation (FAPESP, 2018/20695-7), and Dean of Research of the University of São Paulo (PRP-USP, 18.1.660.41.7). We also thank the [Stack Overflow](https://stackoverflow.com) community, where we solve most of our coding dilemmas.

## Reference

-   Lofti N, Requejo HS, Rodrigues FA, Mello MAR. A new centrality index for multilayer networks. *In prep*.

## Source repos

[TF](https://github.com/marmello77/TF)

## Source studies

-   Bianconi, G. (2018). Multilayer networks: Structure and function. Oxford University Press. <http://dx.doi.org/10.1093/oso/9780198753919.001.0001>

-   Blondel, V. D., Guillaume, J.-L., Lambiotte, R., & Lefebvre, E. (2008). Fast unfolding of communities in large networks. Journal of Statistical Mechanics: Theory and Experiment, 2008(10), P10008. <https://doi.org/10.1088/1742-5468/2008/10/p10008>

-   Mello, M., Rodrigues, F., Costa, L., Kissling, W., Şekercioğlu, Ç., Marquitti, F., & Kalko, E. (2014). Keystone species in seed dispersal networks are mainly determined by dietary specialization. Oikos, 124(8), 1031--1039. <https://doi.org/10.1111/oik.01613>

-   Mello, M. A. R., Felix, G. M., Pinheiro, R. B. P., Muylaert, R. L., Geiselman, C., Santana, S. E., Tschapka, M., Lotfi, N., Rodrigues, F. A., & Stevens, R. D. (2019). Insights into the assembly rules of a continent-wide multilayer network. Nature Ecology & Evolution, 3(11), 1525--1532. <https://doi.org/10.1038/s41559-019-1002-3>

-   Mucha, P. J., Richardson, T., Macon, K., Porter, M. A., & Onnela, J.-P. (2010). Com- munity structure in time-dependent, multiscale, and multiplex networks. Science, 328(5980), 876--878. <https://doi.org/10.1126/science.1184819>

-   Pilosof, S., Porter, M. A., Pascual, M., & Kéfi, S. (2017). The multilayer nature of ecological networks. Nature Ecology & Evolution, 1(4). <https://doi.org/10.1038/s41559-017-0101>

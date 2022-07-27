# Test1-code

## Supplement to the paper: Lotfi N, Requejo HS, Rodrigues F, Mello MAR. A new centrality index for multilayer networks. *In prep*.

Authors: Nastaran Lotfi, Henrique S. Requejo, Francisco A. Rodrigues & Marco A. R. Mello

Follow-up of Henrique Requejo's BSc monograph:

Requejo HS. 2021. *Um teste do algoritmo de modularidade Louvain como uma ferramenta para detectar espécies-chave em redes de interações multicamada*. Honors Degree Monograph, Graduação em Matemática Aplicada e Computacional, Instituto de Matemática e Estatística, Universidade de São Paulo, São Paulo, Brazil. Advisor: Mello MAR.

[Ecological Synthesis Lab](https://marcomellolab.wordpress.com) (SintECO), University of São Paulo.

E-mails: [nas.naslot\@gmail.com](mailto:nas.naslot@gmail.com){.email} OR [marmello\@usp.br](mailto:marmello@usp.br){.email}

First published on June 15th, 2022 (English version).

Run in R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics".

Disclaimer: You may freely use the software and data provided here for any purposes at your own risk. We assume no responsibility or liability for the use of this material, convey no license or title under any patent, copyright, or mask work right to the product. We reserve the right to make changes in the material without notification. We also make no representation or warranty that such application will be suitable for the specified use without further testing or modification. If this material helps you produce any academic work (paper, book, chapter, monograph, dissertation, report, talk, lecture or similar), please acknowledge the authors and cite the original data paper and this repository.

## Functionality and origin

The data and code provided here aim at making reproducible the graphical and numerical analyses presented in our paper.

## List of folders and files

1.  Code (main folder)

    a.  Aux_function.R -\> Script containing the functions used in other scripts.

    b.  Aux_function_random.R -\> Script containing the functions used in random network scripts.

    c.  test1.R -\> Script for making a network, analyzing Gnorm, calculating closeness, degree, eigenvector, and betweenness centralities, and plotting the related figures.

    d.  random_final.R -\> Generates the random network, calculates Gnorm, and plots the histogram of Gnorm of the random network.

    e.  spider.R -\> Plots the spidercharts.

2.  Input (folder)

    a.  links_clean.csv -\> Main input, contains the links and the layers they belong to.

    b.  Names_impo.csv -\> Contains the names of important bat species identified by Mello et al. (2019).

    c.  Names_impo_plants.csv -\> Contains the name of important plant species identified by Mello et al. (2019).

    d.  bats_code.csv -\> Full scientific names of the bat species with their abbreviation codes.

    e.  plants_code.csv -\> Full scientific names of the plant species with their abbreviation codes.

3.  Data (folder)

    a.  links1.csv -\> Produced with test1.R, links of the complete network.

    b.  links2.csv -\> Produced with test1.R, links of the maximum component of the network.

    c.  nodes1.csv -\> Produced with test1.R, nodes of the complete network.

    d.  nodes2.csv -\> Produced with test1.R, nodes of the maximum component of the network

4.  Figures (folder)

    a.  Bats/Plants_10last/10top_Gnorm_name of species_2d/3d/heat.png -\> The top 10 and bottom 10 species in the ranking of Gnorm are plotted. File naming goes like: 1. bats or plants; 2. 10 top or 10 last; 3. Gnorm; 4. name of species; 5. type of plot (2d, 3d or heat).

    b.  Bats/Plants_btw/Clo/Deg/Eig_name of species_2d/3d/heat.png -\> The top 10 species in each ranking of centrality, which are plotted with their relative Gnorm. File naming goes like: 1. bats or plants; 2. type of centrality could be btw(betweenness), clo(closeness), eig(eigenvector), deg (degree) or Gnorm; 3. name of species 4. type of plot (2d, 3d or heat).

    c.  important\_\_name of species_2d/3d/heat.png -\> Plotting the Gnorm of species from a list (important names), names taken from Mello et al. (2019). File naming goes like: 1. important; 2. species name; 3. type of plot (2d, 3d or heat).

    d.  ...\_end_bat/plant.png -\> Spidercharts produced with `spider.R`.

    e.  ...\_firts_bat/plant.png -\> Spidercharts produced with `spider.R`.

    f.  Modularity.png

    g.  Number_of_modules.png

    h.  hist_Gnorm.png

    i.  Network_visualization_complete.png -\> Graph of the complete netwotk.

    j.  Network_visualization_component.png -\> Graph of the giant component of the network.

    k.  C_correlogram_bats_bats/plants/all_pearson/spearman.png -\> Centrality correlogram.

5.  data_random

    a.  rand_total\_...\_links.csv -\> Random network produced with the same size of real network or its maximum component.

    b.  rand_total.RData -\> Results obtained for the Gnorm analysis of the random network.

6.  figures_random (folder)

    a.  hist_Gnorm_random.png

7.  Results (folder)

    a.  Bat/Plant_impo_btw/eig/deg/clo/Gnorm.RData

    b.  Bat_Net_RData -\> Contains the main results obtianed in the Gnorm section (bats and plants).

    c.  bats_allCentr.RData -\> Contains the results of all centralities (bats and plants).

    d.  bats_bats/plants_allCentr.RData -\> Contains separate results of centralities for bats and plants.

    e.  outfile.txt -\> General information about the multilayer network created with the package *multinet*.

    f.  similarity_bat/plant_Net.RData -\> Values of similarity between Gnorm and other centralities.

    g.  timers.txt -\> Time spent running each section of `test1.R`.

## Instructions

1.  Run the respective script to reproduce the chosen analysis, figure or table. You should run the scripts in the following order:

i. `test1.R:` the main code, which produces all information needed for running the other scripts. It is necessary to run this first, and then use the results for the next codes. It contains 95% of all analyses;

ii. `spider.R:` produces the spidercharts, using data produced by `test1.R`;

iii. `random_final.R:` the main code for producing the random network. It uses information about the number of links and number of layers, then it builds the random network and calculates Gnorm for it.

2.  Follow the instructions provided in each script.

3.  Check the files in the `Figures` folder.

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

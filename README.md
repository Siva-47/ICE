# Iterative Connecting Probability Estimation for Networks

This repository is the official implementation of the paper "Iterative Connecting Probability Estimation for Networks".

<img width="150" height="150" src="https://github.com/sivayu47/ICE/blob/main/ICE.png"/>

## Requirements

The code are mainly written in R (version 4.0.3). The following R packages are required and can be installed directly in R:

- load and process data
  - data.table
  - igraph
- speed the computation
  - Rfast 
  - Matrix
- visualize the results
  - ggplot2 
  - pheatmap
  - viridisLite

For example, to install 'data.table', run

```setup
install.packages('data.table')
```

## Methods

We list all the methods we discuss in folder **Methods**, including our proposed method (ICE), and
[neighborhood averaging method (NS)](http://dept.stat.lsa.umich.edu/~jizhu/pubs/Zhang-Biometrika17.pdf) proposed by
Zhang et al. (2017),
[universal singular value thresholding algorithm (USVT)](https://arxiv.org/pdf/1212.1247.pdf) (Chatterjee et al., 2015)
and neighborhood averaging using true neighbors (Oracle).
To reproduce the results in the paper, you should run the code in all the files
in this folder first.

The other two methods, [stochastic blockmodel approximation algorithm (SBA)](https://papers.nips.cc/paper/2013/file/b7b16ecf8ca53723593894116071700c-Paper.pdf) (Airoldi et al., 2013), [sorting and smoothing method (SAS)](https://scholar.harvard.edu/files/stanleychan/files/chan_airoldi_2014_0.pdf) (Chan and Airoldi, 2014)
are provided by Chan and Airoldi (2014) and written in MATLAB.
We place the codes in folder **simulation/SAS_SBA**.

## Application

File **application.R** provides a brief example about how to use our proposed method (ICE) with a given network, including some necessary details, like 
how to stop the iterations and select appropriate tuning parameters.

Mark: To generate the simulated network, the code in file **simulation/network_generate.R** should be runned first.

## Simulation

To evaluate our method on simulated networks and compare it with NS, USVT and Oracle, run **simulation/network_generate.R** and **simulation/simulation.R**.

Folder **simulation/data** includes the network data generated, which can be used as the inputs for methods written with Matlab (SAS and SBA). 

Folder **simulation/result** includes the results. We have store the data and results on Graphon 1 with 3 repetitions ***(saving your time!)*** as an example.

To get the performance of SAS and SBA on the same networks, run **simulation/SAS_SBA/simulation_SAS_SBA.m**.

## Real Data Analysis

To evaluate our method on the real network provided by [Nuerodata](http://mrneurodata.s3-website-us-east-1.amazonaws.com/BNU3/ndmg_0-0-48/graphs/DS00350/sub-0027055_ses-1_dwi_DS00350.gpickle):

- run **real_data/estimate.R** first to get the estimates obtained by the above methods
- run **real_data/link_prediction.R** to evaluate their performances via the task of link prediction
- run **real_data/host_game.R** to evaluate their performances via the "host game" we propose in the paper

Folder **real_data/data** includes the original network data and the adjacency matrix. 

Folder **real_data/result** includes the results of the evaluation procedure with 3 repetitions ***(saving your time!)***  as an example.

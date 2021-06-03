# Iterative Connecting Probability Estimation for Networks

This repository is the official implementation of the paper "Iterative Connecting Probability Estimation for Networks".

<img width="150" height="150" src="https://github.com/sivayu47/ICE/blob/main/ICE.png"/>

## Requirements

The code are mainly written with R (version 4.0.3). The following R packages are required and can be installed directly in R:

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

```setup
install.packages('data.table')
```

## Methods

We list all the methods we discuss in the dictionary **Methods**, including our proposed method (ICE), and
neighborhood averaging method (NS) proposed by 

## Application

The file **application.R** shows how to use our proposed method with a given network, including some necessary details, like 
how to stop the iterations and select tuning parameters.

>📋  Describe how to train the models, with example commands on how to train the models in your paper, including the full training procedure and appropriate hyperparameters.

## Evaluation

To evaluate my model on ImageNet, run:

```eval
python eval.py --model-file mymodel.pth --benchmark imagenet
```

>📋  Describe how to evaluate the trained models on benchmarks reported in the paper, give commands that produce the results (section below).

## Pre-trained Models

You can download pretrained models here:

- [My awesome model](https://drive.google.com/mymodel.pth) trained on ImageNet using parameters x,y,z. 

>📋  Give a link to where/how the pretrained models can be downloaded and how they were trained (if applicable).  Alternatively you can have an additional column in your results table with a link to the models.

## Results

Our model achieves the following performance on :

### [Image Classification on ImageNet](https://paperswithcode.com/sota/image-classification-on-imagenet)

| Model name         | Top 1 Accuracy  | Top 5 Accuracy |
| ------------------ |---------------- | -------------- |
| My awesome model   |     85%         |      95%       |

>📋  Include a table of results from your paper, and link back to the leaderboard for clarity and context. If your main result is a figure, include that figure and link to the command or notebook to reproduce it. 


## Contributing

>📋  Pick a licence and describe how to contribute to your code repository. 

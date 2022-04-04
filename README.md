Voting Participation of Canadian Citizens 

Author: Mahak Jain
Date: April 3, 2022
E-mail: mahak.jain@mail.utoronto.ca
Overview of the paper
This repository explores the Canadian GSS 2013 data to better understand factors that may affect voter participation.

Obtaining data
The data is taken from General Social Survey (GSS), Cycle 27, 2013 (version 2): Social Identity, through CHASS website, accessible over 25 different universities.  

It is also included in this repository, in the directory "inputs/data/AAEV5tkb.txt."

Preprocess data
After obtaining the STATA data on GSS, the script "01-data_cleaning.R", located in "scripts/01-data_cleaning.R", can be used to preprocess the data and save the file as a csv file in the directory "outputs/data/raw_data.csv" (will be done later).


Building the Report
There is a RMarkDown document located in "outputs/paper/paper.Rmd". This file is used to produce the report "placeholdername". It contains the R code to produce the graphs and the report format code. The reference used are also located in "outputs/paper/references.bib".

File Structure
Inputs
In this folder, you will find GSS 2013 raw data, cleaned datasets.
Outputs
In this folder you will find a reference file, RMarkdown file, and a pdf document of the paper. (to be uploaded)
Scripts
This folder contains R-Scripts to retrieve, clean, and do analysis (visulization) with the dataset. (to be done)


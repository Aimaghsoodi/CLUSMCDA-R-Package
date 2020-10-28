# CLUSMCDA-R-Package
##### Corresponding Author/ Algorithm Developer: Abtin Ijadi Maghsoodi
abtin.ijadi-maghsoodi@auckland.ac.nz - aimaghsoodi@outlook.com
##### Software/Package Developer: Dara Riahi
darariahi@ut.ac.ir - riahi.dara@gmail.com
##### Support Team for the CLUS-MCDA Project: CLUS-MCDA@outlook.com 

This package is a part of the CLUS-MCDA project developed by Abteen Ijadi Maghsoodi. This package is a runnable package with no need to define any dependencies and it will ask the user the required information/input-data as you load the library and start the algorithm, this package has been coded by Dara Riahi and developed by Abteen Ijadi Maghsoodi.  CLUS-MCDA (Cluster analysis for improving Multiple Criteria Decision Analysis) algorithm is an iterative methodology based on the K-means clustering technique combined with the Multi-Objective Optimization on the basis of Ratio Analysis plus the full MULTIplicative form (MULTIMOORA) developed by Ijadi Maghsoodi et al. [1]. 

Update for [2]: In this update the CLUS-MCDA approach has been utilized with Parallel Decision Making (PDM), Multi-Scenario Structure and the use of attributes weights, in case a user needed any of these calculating options.  

## A NEW UPDATE IS ON THE WAY! 

The related references of this paper can be found in:  

[1] Ijadi Maghsoodi, A., Kavian, A., Khalilzadeh, M., &amp; Brauers, W. K. M. (2018). CLUS-MCDA: A Novel Framework based on Cluster Analysis and Multiple Criteria Decision Theory in a Supplier Selection Problem. Computers &amp; Industrial Engineering, 118 (August 2017), 409–422. Elsevier. Retrieved from http://linkinghub.elsevier.com/retrieve/pii/S0360835218300962 

[2] Ijadi Maghsoodi, A., Riahi, D., E. Herrera-Viedma, E. K. Zavadskas, An Integrated Parallel Big Data Decision Support Tool Using the W-CLUS-MCDA: A Multi-Scenario Personnel Assessment. [Under-Review]  

If you have any questions about the CLUS-MCDA runnable package/software please do not hesitate to contact our support email at abtin.ijadi-maghsoodi@auckland.ac.nz

## Installation

You can install the released version of CLUSMCDA from this repository with devtools package: 

``` r
install.packages("devtools")
```

Load the devtools package: 

``` r
library(devtools)
```

then use to instal the CLUS-MCDA from this repository: 

``` r
install_github("Aimaghsoodi/CLUSMCDA-R-Package").
```

Or 

You can download the .tar.gz file from: 
https://github.com/Aimaghsoodi/CLUSMCDA-R-Package/blob/master/CLUSMCDA%20Package-V0.2.tar.gz

and then install the package manualy from your drive or this command: 

``` r
install.packages(A*)
```
A* = The CLUS-MCDA saved destination for example C:\Downloads\

#### You can also install the CLUS-MCDA package with R or RStudio installation commands. 

Then load the CLUS-MCDA package: 

``` r
library(CLUSMCDA)
```


## User Manual
### How to use the package?

After loading the CLUS-MCDA package: 

``` r
library(CLUSMCDA)
```

Then, all you have to do is just type CLUSMCDA(strt) to run the CLUS-MCDA runnable program/package. 

``` r
CLUSMCDA(strt)
```

This package has been assembled in a way that there is no need for any dependecies to load the package. 
The required information to run the package will be asked based on the user's needs. 
These are the questions that the user need to answer in order to obtain the final results and outcomes.

**** For example data sets refer to DOI: 10.17632/wgszp47nkd.2 and DOI: 10.17632/y3wv3gf28n.3

Follow these steps to obtain the final answers of the CLUS-MCDA approach: 
1. Load CLUS-MCDA package
2. 
``` r
CLUSMCDA(strt)
```
3. Answer these questions on the interface in front of the questions: 
  A. How many CSV files (as input data) do you want to load/calculate? ****Answer

  B. Column/Criteria start point:"Use capital letters to load columns/criteria/attributes"? ****Answer

  C. In Which column is your clustering attribute located? ****Answer

  D. In Which column is your main alternatives are located? ****Answer

  E. In Which column is your categorizing/classification attribute is located? ****Answer
  
  F. Input CSV file Information 1: **** You have to load your input data in form of a .csv file in the package
  
  G. How many cost/non-beneficial columns/crietria do you have?****Answer
  
  H. Then, Use capital letters to load cost column(s)/crietria:****Answer [as many as you mentioned]
  
  I. How many benefit/beneficial columns/crietria do you have? ****Answer
  
  J. Then, Use capital letters to load benefit column(s)/crietria:****Answer [as many as you mentioned]
  
  K. [If Available] Enter weight for each criterion from left to right according to your desired or input original data csv file(s) ****Answer [as many as you mentioned]
  
Note that, two answers may be pop up, 1) The summation of weights are 1; therefore the expert bias is not available and you may proceed or 2) The summation of weights are not 1; which means that an expert bias error is available but you may proceed [Ignore this message if you wish to proceed or press ESC button in order to quit the program]. In which, the second warning means that you may have a summation of weight less or higher than 1 which is a result of a BIAS or weighting error. YOU MAY PROCEED IF YOU WANT, ****This is just a warning.
 
   L. If you wish to have a CSV output file write Y, otherwise write N : ****Answer [Select a CSV file to save the final outcomes/results if the answer is Y], Otherwise, the final results/outcomes will be shown in ### R Console
   

For more information about the scientific reports on CLUS-MCDA please refere to: 

[1] Ijadi Maghsoodi, A., Kavian, A., Khalilzadeh, M., Brauers, W. K. M. (2018). CLUS-MCDA: A Novel Framework based on Cluster Analysis and Multiple Criteria Decision Theory in a Supplier Selection Problem. Computers Industrial Engineering, 118 (August 2017), 409–422. Elsevier. Retrieved from http://linkinghub.elsevier.com/retrieve/pii/S0360835218300962 

[2] Ijadi Maghsoodi, A., Riahi, D., E. Herrera-Viedma, E. K. Zavadskas, An integrated parallel big data decision support tool using the W-CLUS-MCDA: A multi-scenario personnel assessment. Knowledge-Based Systems, Volume 195, 11 May 2020, 105749. Elsevier. Retrieved from https://doi.org/10.1016/j.knosys.2020.105749 

  
  

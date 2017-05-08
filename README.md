# Predicting Attack Type in Global Terrorist Events

## Overview/Synopsis
Analyses of terrorist groups and incidents have begun to recognize the evolutionary nature of terrorism, as well as its ability to adapt to counter-terrorism tactics. This project is intended to predict the clusters of terrorist attack using attack type, weapon type, target type, suicide dummy, month. The analysis is performed on 4 countries that have the most frequent attacks: Iraq, Afghanistan, Pakistan, India.  

## Use  
The product can improve the decision of the global counter terrorism operation. 
We used several clustering algorithms, including k-means, hierarchical clustering to identify the key features of the terrorism, and also created visualization of the result.


## Data Used
Our data is the Global Terrorism Database compiled by the University of Maryland. It collects information about each terrorist attack reported by governments around the world since 1970. The latest update has over 15,0000 records. Key variables of this dataset include:
1.	Types of attack: bombing, kidnapping, etc
2.	Weapon used in the attack: Handgun, Automatic Weapon, TNT, etc.
3.	Time: year, month, date
4.	Location: country/region, state/province, city; geographical coordinates
5.	Casualties/death toll
6.	Identification of attackers: recognized terrorist group, individuals
7.	Types of weapon used in the attack
8.	Types of targets: civilian, government agencies, military bases, etc
9.	Responsibility Claim Mode: NA, website, phone call, etc 

## Progress Log
* 2017-03-29 Project data downloaded
* 2017-03-29 Project proposal submitted
* 2017-04-16 Project data cleaning
* 2017-04-16 Kmeans applied to attack type
* 2017-05-01 Hierarchical clustering applied to input features
* 2017-05-03 Hierarchical clustering applied to 4 countries
* 2017-05-06 Create functions to implement HC and wrapped it into loop
* 2017-05-07 Style Map

## Credits
Credit to Lipei Tao, Yuan Xiang, Jingyu Gao

# Predicting Attack Type in Global Terrorist Events

## Overview/Synopsis
Analyses of terrorist groups and incidents have begun to recognize the evolutionary nature of terrorism, as well as its ability to adapt to counter-terrorism tactics. This project is intended to predict the clusters of terrorist attack using attack type, weapon type, target type, terrorist group type.    

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

## Credits
Credit to Jingyu Gao, Lipei Tao, Yuan Xiang

## License

MIT License

Copyright (c) [2017] [Jingyu Gao, Lipei Tao, Yuan Xiang]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
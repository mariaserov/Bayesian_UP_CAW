# Descriptive Analysis of Dowry Deaths

library(INLA)
library(ggplot2)
library(knitr)
library(kableExtra)
library(sp)
library(sf)
library(RColorBrewer)
library(spdep)

load("../DS6_CrimeUttarPradesh/CrimeUttarPradesh.RData")

str(data)
# 'data.frame':   980 obs. of  13 variables:
# $ dist        : Factor w/ 586 levels "Adilabad","Agra",..: 2 9 10 14 25 27 31 32 36 37 ...
# $ state       : Factor w/ 35 levels "Andaman & Nicobar Island",..: 33 33 33 33 33 33 33 33 33 33 ...
# $ year        : int  2001 2001 2001 2001 2001 2001 2001 2001 2001 2001 ...
# $ rape        : int  66 50 42 19 11 11 15 14 18 8 ...
# $ dowry       : int  63 44 56 14 21 32 20 19 19 12 ...
# $ pop         : num  786257 628800 1087233 459174 250801 ...
# $ e_rape      : num  33 26.4 45.6 19.3 10.5 ...
# $ e_dowry     : num  37 29.6 51.1 21.6 11.8 ...
# $ smr_rape    : num  2.002 1.896 0.921 0.987 1.046 ...
# $ smr_dowry   : num  1.704 1.488 1.096 0.649 1.781 ...
# $ ID_area     : int  1 2 3 4 5 6 7 8 9 10 ...
# $ ID_year     : int  1 1 1 1 1 1 1 1 1 1 ...
# $ ID_area_year: int  1 2 3 4 5 6 7 8 9 10 ...

head(data)
#             dist         state year rape dowry     pop   e_rape
# 1           Agra Uttar Pradesh 2001   66    63  786257 32.97396
# 2        Aligarh Uttar Pradesh 2001   50    44  628800 26.37055
# 3      Allahabad Uttar Pradesh 2001   42    56 1087233 45.59626
# 4 Ambedkar Nagar Uttar Pradesh 2001   19    14  459174 19.25679
# 5        Auraiya Uttar Pradesh 2001   11    21  250801 10.51806
# 6       Azamgarh Uttar Pradesh 2001   11    32  896694 37.60546
#    e_dowry  smr_rape smr_dowry ID_area ID_year ID_area_year
# 1 36.96372 2.0015794 1.7043741       1       1            1
# 2 29.56131 1.8960548 1.4884322       2       1            2
# 3 51.11328 0.9211282 1.0956058       3       1            3
# 4 21.58681 0.9866650 0.6485443       4       1            4
# 5 11.79072 1.0458197 1.7810616       5       1            5
# 6 42.15561 0.2925108 0.7590924       6       1            6

summary(data)
#              dist                          state          year     
#  Agra          : 14   Uttar Pradesh           :980   Min.   :2001  
#  Aligarh       : 14   Andaman & Nicobar Island:  0   1st Qu.:2004  
#  Allahabad     : 14   Andhra Pradesh          :  0   Median :2008  
#  Ambedkar Nagar: 14   Arunanchal Pradesh      :  0   Mean   :2008  
#  Auraiya       : 14   Assam                   :  0   3rd Qu.:2011  
#  Azamgarh      : 14   Bihar                   :  0   Max.   :2014  
#  (Other)       :896   (Other)                 :  0                 
#       rape            dowry            pop              e_rape      
#  Min.   :  0.00   Min.   : 1.00   Min.   : 150193   Min.   : 6.299  
#  1st Qu.: 11.00   1st Qu.:16.00   1st Qu.: 391947   1st Qu.:16.437  
#  Median : 20.00   Median :26.00   Median : 586967   Median :24.616  
#  Mean   : 26.03   Mean   :29.18   Mean   : 620767   Mean   :26.034  
#  3rd Qu.: 35.00   3rd Qu.:38.00   3rd Qu.: 815100   3rd Qu.:34.184  
#  Max.   :164.00   Max.   :98.00   Max.   :1543763   Max.   :64.742  
#                                                                    
#     e_dowry          smr_rape        smr_dowry          ID_area    
#  Min.   : 7.061   Min.   :0.0000   Min.   :0.08895   Min.   : 1.0  
#  1st Qu.:18.426   1st Qu.:0.5244   1st Qu.:0.64509   1st Qu.:18.0  
#  Median :27.595   Median :0.8931   Median :0.93890   Median :35.5  
#  Mean   :29.184   Mean   :1.0093   Mean   :1.04229   Mean   :35.5  
#  3rd Qu.:38.320   3rd Qu.:1.3308   3rd Qu.:1.35765   3rd Qu.:53.0  
#  Max.   :72.576   Max.   :4.6112   Max.   :3.11920   Max.   :70.0  
#                                                                    
#     ID_year      ID_area_year  
#  Min.   : 1.0   Min.   :  1.0  
#  1st Qu.: 4.0   1st Qu.:245.8  
#  Median : 7.5   Median :490.5  
#  Mean   : 7.5   Mean   :490.5  
#  3rd Qu.:11.0   3rd Qu.:735.2  
#  Max.   :14.0   Max.   :980.0  

# Missingness check
sum(is.na(data))
# [1] 0

class(carto_up)
# [1] "SpatialPolygonsDataFrame"
# attr(,"package")
# [1] "sp"

names(carto_up)
# [1] "st.dist" "state"   "dist"    "ID_area"

head(carto_up@data)
#                        st.dist         state           dist ID_area
# 1           Uttar Pradesh_Agra Uttar Pradesh           Agra       1
# 2        Uttar Pradesh_Aligarh Uttar Pradesh        Aligarh       2
# 3      Uttar Pradesh_Allahabad Uttar Pradesh      Allahabad       3
# 4 Uttar Pradesh_Ambedkar Nagar Uttar Pradesh Ambedkar Nagar       4
# 5        Uttar Pradesh_Auraiya Uttar Pradesh        Auraiya       5
# 6       Uttar Pradesh_Azamgarh Uttar Pradesh       Azamgarh       6

proj4string(carto_up)
# [1] NA (is this WGS84? no info)

plot(carto_up)
# looks alright

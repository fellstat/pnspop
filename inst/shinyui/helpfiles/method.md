# Method

Every individual in the sample can be traced back from recruiter to recruiter to 
an originating seed, so each seed can be thought of as the base of a tree. The 
rate at which the identifiers of individuals in one tree are seen among the 
nominated “friends” of other trees is related to the population size. If there 
are few overlaps, the population is likely large. If there are many, it is 
likely small. the **Cross-Sample** estimator uses this overlap to estimate population size.

Similarly, the rate at which the nominated “friends” from one tree are seen among 
the nominated “friends” of other trees is also related to the population size. 
The **Cross-Alter** estimator uses this for population size estimation.

The **Cross-Network** estimator combines the evidence from both of these 
overlaps to construct its population size estimate.

------------

The Cross-Network estimator is recommended as a default because it uses the most
information. However, the Cross-Sample estimator may be preferable if it is
suspected that nominations are preferentially hitting a subset of the population
and hence creating alter and network estimates that are implausibly large

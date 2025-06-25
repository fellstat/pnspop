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

The **Within Tree (n2)** estimator is similar to the **Cross-Sample** estimator except that it also includes matches within each tree. This can potentially bias the result when there is clustering in the underlying social network, which is true of most social networks. It is only advisable to use this estimator if most participants belong to a single tree. It may also be used in cases where each seed is started from a different physical site and there is little to no mixing between sites. Confidence intervals are not available for this estimator.

------------

## Recommendations

The **Cross-Network** estimator is recommended as a default because it uses the most
information. However, the **Cross-Sample** estimator may be preferable if it is
suspected that nominations are preferentially hitting a subset of the population
and hence creating alter and network estimates that are implausibly large.

------------

Fellows, I. E. (2022). Estimating population size from a privatized network sample. Journal of Survey Statistics and Methodology, 10(5), 1346-1369.

Khan, B., Lee, H. W., Fellows, I., & Dombrowski, K. (2018). One-step estimation of networked population size: Respondent-driven capture-recapture with anonymity. PloS one, 13(4), e0195959.







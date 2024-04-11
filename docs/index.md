# Privatized Network Sampling (PNS): User Interface Documentation


While numerous methodologies exist to estimate proportions and means from an respondent driven sample (RDS), estimating population size is more challenging. A privatized network sample (PNS) is similar to an RDS, but also collects identifiers for each individual's network connections protecting privacy through the use of a hash function.

This package implements methods for estimating population size in PNS studies.

# Data

An RDS sampling process begins with the selection of a set of seed individuals from the population. Ideally these seeds would be randomly selected from the population, but this is not required. Information about these individuals is collected, including the number of connections they have to other members of the population (known as their degree or network size). They are then given a set of coupons to give to these connections in order to recruit them into the study. Their recruits are then enrolled in the study and are themselves asked to recruit other members of the population. This process proceeds until the sample size is reached.

Privatized network sampling (PNS) proceeds similarly to an RDS sample, except that individuals are asked the identities of their network connections (i.e. alters), which are then protected through the use of a hashing function. The number of network neighbors collected may be capped at a certain number (e.g. 10) to avoid overburdening the respondent.

# Choosing An Underlying Identifier



# Privatizing Identifiers using Hash Functions

The identifiers may be protected through the use of a hash function. A hashing function is a one way function that transforms an identifier into a (semi-)unique string of characters. There are many different hash functions that can be used, including cryptographic hash functions like SHA-256. Using SHA-256 on the identifier 'Ian Fellows (555) 212-5364' results in the value '276a8cf8f0ea69943c737ff47565233b5314169eb6a4996da43b3a1acb293230'.

It is up to the study to determine how to integrate hashing into the data collection process, however, we have developed tools to make this as easy and seamless as possible.

# Via Webpage







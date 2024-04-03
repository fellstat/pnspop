# Rho

This is the probability that two random people in the population share the same 
hashed identifier.

Rho can be estimated from the data by seeing how many individuals in the sample 
have the same hashed ID. However, care should be taken when doing this as even a
single duplicate enrollment (i.e. the same person enrolling twice) will cause a
large error in the estimation.

Rho can also be calculated based on how the hash was constructed. For example, if
the last 5 digits of the phone number is used as the underlying identifier passed 
to the hash function, the probability that the first digits are the the same between 
two random people is 1/10. The same is true for the remaining 4 digits, so the 
probability that they share all five is:

```
(1/10) * (1/10) * (1/10) * (1/10) * (1/10) = 10^(-5) = .00001
```

Similarly if letters are used, the probability that individuals share a letter
can be (roughly) approximated by one over the number of letters (1/26). Thus,
an identifier based on three digits of a phone number and one initial would have an
approximate rho of:

```
(1/26) * (1/10) * (1/10) * (1/10) = .00003846
```


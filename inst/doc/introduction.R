## -----------------------------------------------------------------------------
library(pnspop)
data(faux_pns)
head(faux_pns[-3],10)

## -----------------------------------------------------------------------------
cross_tree_pse(
  subject = faux_pns$subject,
  recruiter = faux_pns$recruiter,
  subject_hash = faux_pns$subject_hash, 
  degree = faux_pns$degree,
  nbrs = faux_pns[paste0("friend_hash",1:11)], 
  method = "network",
  rho = .001,
  small_sample_fraction = FALSE)

cross_tree_pse(
  subject = faux_pns$subject,
  recruiter = faux_pns$recruiter,
  subject_hash = faux_pns$subject_hash, 
  degree = faux_pns$degree,
  nbrs = faux_pns[paste0("friend_hash",1:11)], 
  method = "alter",
  rho = .001,
  small_sample_fraction = FALSE)

cross_tree_pse(
  subject = faux_pns$subject,
  recruiter = faux_pns$recruiter,
  subject_hash = faux_pns$subject_hash, 
  degree = faux_pns$degree,
  nbrs = faux_pns[paste0("friend_hash",1:11)], 
  method = "sample",
  rho = .001,
  small_sample_fraction = FALSE)

## -----------------------------------------------------------------------------
bootstrap_pse(
  subject = faux_pns$subject,
  recruiter = faux_pns$recruiter,
  subject_hash = faux_pns$subject_hash, 
  degree = faux_pns$degree,
  nbrs = faux_pns[paste0("friend_hash",1:11)], 
  method = "network",
  rho = .001,
  small_sample_fraction = FALSE,
  n_bootstrap = 10,
  progress = FALSE)


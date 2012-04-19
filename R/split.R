split_tot <- function(t, x0, q0, d1, d2, q1, q2, ad1, aq1, ad2, aq2, br=20)
{
  qbr <- prolif_q(br, x0, q0, d1, q1, ad1, aq1)
  dbr <- prolif_d(br, x0, q0, d1, q1, ad1, aq1)
  append(
    prolif_tot(t[t<=br],    x0,  q0, d1, q1, ad1, aq1),
    prolif_tot(t[t>br]-br, dbr, qbr, d2, q2, ad2, aq2) )
}

split_q <- function(t, x0, q0, d1, d2, q1, q2, ad1, aq1, ad2, aq2, br=20)
{
  qbr <- prolif_q(br, x0, q0, d1, q1, ad1, aq1)
  dbr <- prolif_d(br, x0, q0, d1, q1, ad1, aq1)
  append(
    prolif_q(t[t<=br],    x0,  q0, d1, q1, ad1, aq1),
    prolif_q(t[t>br]-br, dbr, qbr, d2, q2, ad2, aq2) )
}

split_d <- function(t, x0, q0, d1, d2, q1, q2, ad1, aq1, ad2, aq2, br=20)
{
  qbr <- prolif_q(br, x0, q0, d1, q1, ad1, aq1)
  dbr <- prolif_d(br, x0, q0, d1, q1, ad1, aq1)
  append(
    prolif_d(t[t<=br],    x0,  q0, d1, q1, ad1, aq1),
    prolif_d(t[t>br]-br, dbr, qbr, d2, q2, ad2, aq2) )
}

split_fq <- function(t, x0, q0, d1, d2, q1, q2, ad1, aq1, ad2, aq2, br=20)
{
  split_q(t, x0, q0, d1, d2, q1, q2, ad1, aq1, ad2, aq2, br) /
  split_tot(t, x0, q0, d1, d2, q1, q2, ad1, aq1, ad2, aq2, br)
}

split_fd <- function(t, x0, q0, d1, d2, q1, q2, ad1, aq1, ad2, aq2, br=20)
{
  split_d(t, x0, q0, d1, d2, q1, q2, ad1, aq1, ad2, aq2, br) /
  split_tot(t, x0, q0, d1, d2, q1, q2, ad1, aq1, ad2, aq2, br)
}
twTStats <- function (twT) 
{
    twT = as.double(twT)
    m1 = twT[1] + twT[2]
    m2 = twT[1] + twT[3]
    M = sum(twT)
    ##check to see if either citation count is zero
    if( (m1 == 0) || (m2 == 0 ) )
      return(c(Concordance = 0, Jaccard  = 0, Hubert = 0)) 
    else
     return(c(Concordance = twT[1], Jaccard = twT[1]/(sum(twT[1:3])), 
        Hubert = (M * twT[1] - m1 * m2)/sqrt(m1 * m2 * (M - m1) * 
            (M - m2))))
}


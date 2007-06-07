twTStats <- function (twT) 
{
    if(length(twT) != 4 )
       stop("only 2 by 2 tables can be handled")
    #if it is a matrix then it is in the wrong order
    #and we rearrange it
    if( is.matrix(twT) )
      twT = c(twT[1,], twT[2,])

    twT = as.double(twT)
    m1 = twT[1] + twT[2]
    m2 = twT[1] + twT[3]
    M = sum(twT)
    ##check to see if either citation count is zero
    if( (m1 == 0) || (m2 == 0 ) )
      return(c(OddsRatio = 1, Concordance = 0, Jaccard  = 0, Hubert = 0)) 
    else
     return(c(OddsRatio = (twT[1]*twT[4])/(twT[2]*twT[3]), 
              Concordance = twT[1], 
              Jaccard = twT[1]/(sum(twT[1:3])), 
              Hubert = (M * twT[1] - m1 * m2)/sqrt(m1 * m2 * (M - m1) * 
                           (M - m2))))
}


"actorAdjTable" <-
function (twT, eps = 1e-08) 
{
    twT = as.double(twT)
    m1 = twT[1] * twT[4]
    m2 = twT[2] * twT[3]
    D = m1 - m2
    if (abs(D) < eps) 
        return(c(u11 = 0.5, u12 = 0.5, u21 = 0.5, u22 = 0.5))
    v = (m1 - sqrt(m1 * m2))/D
    return(c(u11 = v, u12 = 1 - v, u21 = 1 - v, u22 = v))
}


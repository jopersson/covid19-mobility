

# Create spatial weighting matrix

# init matrix
canton_neighbors = matrix(0, nrow = nlevels(dta$canton_codes), 
                             ncol = nlevels(dta$canton_codes))

rownames(canton_neighbors) <- colnames(canton_neighbors) <- levels(dta$canton_codes)

# add 1's for matrix elements that are neighbors 
canton_neighbors["AG","BL"] <- 1
canton_neighbors["AG","SO"] <- 1
canton_neighbors["AG","ZH"] <- 1
canton_neighbors["AG","ZG"] <- 1
canton_neighbors["AG","LU"] <- 1
canton_neighbors["AG","BE"] <- 1

canton_neighbors["BS","BL"] <- 1

canton_neighbors["BL","BS"] <- 1
canton_neighbors["BL","SO"] <- 1
canton_neighbors["BL","JU"] <- 1
canton_neighbors["BL","BE"] <- 1
canton_neighbors["BL","AG"] <- 1

canton_neighbors["SO","BL"] <- 1
canton_neighbors["SO","AG"] <- 1
canton_neighbors["SO","JU"] <- 1
canton_neighbors["SO","BE"] <- 1

canton_neighbors["JU","BL"] <- 1
canton_neighbors["JU","SO"] <- 1
canton_neighbors["JU","BE"] <- 1
canton_neighbors["JU","NE"] <- 1

canton_neighbors["NE","JU"] <- 1
canton_neighbors["NE","BE"] <- 1
canton_neighbors["NE","VD"] <- 1
canton_neighbors["NE","FR"] <- 1

canton_neighbors["VD","FR"] <- 1
canton_neighbors["VD","NE"] <- 1
canton_neighbors["VD","BE"] <- 1
canton_neighbors["VD","GE"] <- 1
canton_neighbors["VD","VS"] <- 1

canton_neighbors["GE","VD"] <- 1

canton_neighbors["FR","BE"] <- 1
canton_neighbors["FR","NE"] <- 1
canton_neighbors["FR","VD"] <- 1

canton_neighbors["VS","BE"] <- 1
canton_neighbors["VS","TI"] <- 1
canton_neighbors["VS","VD"] <- 1
canton_neighbors["VS","UR"] <- 1

canton_neighbors["BE","AG"] <- 1
canton_neighbors["BE","SO"] <- 1
canton_neighbors["BE","JU"] <- 1
canton_neighbors["BE","NE"] <- 1
canton_neighbors["BE","FR"] <- 1
canton_neighbors["BE","VD"] <- 1
canton_neighbors["BE","VS"] <- 1
canton_neighbors["BE","UR"] <- 1
canton_neighbors["BE","OW"] <- 1
canton_neighbors["BE","LU"] <- 1
canton_neighbors["BE","NW"] <- 1


canton_neighbors["OW","LU"] <- 1
canton_neighbors["OW","BE"] <- 1
canton_neighbors["OW","UR"] <- 1
canton_neighbors["OW","NW"] <- 1

canton_neighbors["LU","AG"] <- 1
canton_neighbors["LU","BE"] <- 1
canton_neighbors["LU","OW"] <- 1
canton_neighbors["LU","NW"] <- 1
canton_neighbors["LU","SZ"] <- 1
canton_neighbors["LU","ZG"] <- 1

canton_neighbors["NW","UR"] <- 1
canton_neighbors["NW","SZ"] <- 1
canton_neighbors["NW","LU"] <- 1
canton_neighbors["NW","OW"] <- 1
canton_neighbors["NW","BE"] <- 1

canton_neighbors["TI","VS"] <- 1
canton_neighbors["TI","UR"] <- 1
canton_neighbors["TI","GR"] <- 1

canton_neighbors["UR","GR"] <- 1
canton_neighbors["UR","GL"] <- 1
canton_neighbors["UR","SZ"] <- 1
canton_neighbors["UR","NW"] <- 1
canton_neighbors["UR","OW"] <- 1
canton_neighbors["UR","BE"] <- 1
canton_neighbors["UR","VS"] <- 1
canton_neighbors["UR","TI"] <- 1

canton_neighbors["GR","GL"] <- 1
canton_neighbors["GR","SG"] <- 1
canton_neighbors["GR","TI"] <- 1
canton_neighbors["GR","UR"] <- 1

canton_neighbors["GL","SZ"] <- 1
canton_neighbors["GL","SG"] <- 1
canton_neighbors["GL","GR"] <- 1
canton_neighbors["GL","UR"] <- 1

canton_neighbors["SZ","ZG"] <- 1
canton_neighbors["SZ","ZH"] <- 1
canton_neighbors["SZ","SG"] <- 1
canton_neighbors["SZ","GL"] <- 1
canton_neighbors["SZ","UR"] <- 1
canton_neighbors["SZ","NW"] <- 1
canton_neighbors["SZ","LU"] <- 1

canton_neighbors["SG","TG"] <- 1
canton_neighbors["SG","AR"] <- 1
canton_neighbors["SG","AI"] <- 1
canton_neighbors["SG","GR"] <- 1
canton_neighbors["SG","GL"] <- 1
canton_neighbors["SG","SZ"] <- 1
canton_neighbors["SG","ZH"] <- 1

canton_neighbors["AI","SG"] <- 1
canton_neighbors["AI","AR"] <- 1

canton_neighbors["AR","AI"] <- 1
canton_neighbors["AR","SG"] <- 1

canton_neighbors["ZG","ZH"] <- 1
canton_neighbors["ZG","SZ"] <- 1
canton_neighbors["ZG","LU"] <- 1
canton_neighbors["ZG","AG"] <- 1

canton_neighbors["ZH","SH"] <- 1
canton_neighbors["ZH","TG"] <- 1
canton_neighbors["ZH","SG"] <- 1
canton_neighbors["ZH","SZ"] <- 1
canton_neighbors["ZH","ZG"] <- 1
canton_neighbors["ZH","AG"] <- 1

canton_neighbors["SH","TG"] <- 1
canton_neighbors["SH","ZH"] <- 1

canton_neighbors["TG","SH"] <- 1
canton_neighbors["TG","ZH"] <- 1
canton_neighbors["TG","SG"] <- 1



canton_neighbors["BL","AG"] <- 1
canton_neighbors["SO","AG"] <- 1
canton_neighbors["ZH","AG"] <- 1
canton_neighbors["ZG","AG"] <- 1
canton_neighbors["LU","AG"] <- 1
canton_neighbors["BE","AG"] <- 1

canton_neighbors["BL","BS"] <- 1

canton_neighbors["BS","BL"] <- 1
canton_neighbors["SO","BL"] <- 1
canton_neighbors["JU","BL"] <- 1
canton_neighbors["BE","BL"] <- 1
canton_neighbors["AG","BL"] <- 1

canton_neighbors["BL","SO"] <- 1
canton_neighbors["AG","SO"] <- 1
canton_neighbors["JU","SO"] <- 1
canton_neighbors["BE","SO"] <- 1

canton_neighbors["BL","JU"] <- 1
canton_neighbors["SO","JU"] <- 1
canton_neighbors["BE","JU"] <- 1
canton_neighbors["NE","JU"] <- 1

canton_neighbors["JU","NE"] <- 1
canton_neighbors["BE","NE"] <- 1
canton_neighbors["VD","NE"] <- 1
canton_neighbors["FR","NE"] <- 1

canton_neighbors["FR","VD"] <- 1
canton_neighbors["NE","VD"] <- 1
canton_neighbors["BE","VD"] <- 1
canton_neighbors["GE","VD"] <- 1
canton_neighbors["VS","VD"] <- 1

canton_neighbors["VD","GE"] <- 1

canton_neighbors["BE","FR"] <- 1
canton_neighbors["NE","FR"] <- 1
canton_neighbors["VD","FR"] <- 1

canton_neighbors["BE","VS"] <- 1
canton_neighbors["TI","VS"] <- 1
canton_neighbors["VD","VS"] <- 1
canton_neighbors["UR","VS"] <- 1

canton_neighbors["AG","BE"] <- 1
canton_neighbors["SO","BE"] <- 1
canton_neighbors["JU","BE"] <- 1
canton_neighbors["NE","BE"] <- 1
canton_neighbors["FR","BE"] <- 1
canton_neighbors["VD","BE"] <- 1
canton_neighbors["VS","BE"] <- 1
canton_neighbors["UR","BE"] <- 1
canton_neighbors["OW","BE"] <- 1
canton_neighbors["LU","BE"] <- 1
canton_neighbors["NW","BE"] <- 1


canton_neighbors["LU","OW"] <- 1
canton_neighbors["BE","OW"] <- 1
canton_neighbors["UR","OW"] <- 1
canton_neighbors["NW","OW"] <- 1

canton_neighbors["AG","LU"] <- 1
canton_neighbors["BE","LU"] <- 1
canton_neighbors["OW","LU"] <- 1
canton_neighbors["NW","LU"] <- 1
canton_neighbors["SZ","LU"] <- 1
canton_neighbors["ZG","LU"] <- 1

canton_neighbors["UR","NW"] <- 1
canton_neighbors["SZ","NW"] <- 1
canton_neighbors["LU","NW"] <- 1
canton_neighbors["OW","NW"] <- 1
canton_neighbors["BE","NW"] <- 1

canton_neighbors["VS","TI"] <- 1
canton_neighbors["UR","TI"] <- 1
canton_neighbors["GR","TI"] <- 1

canton_neighbors["GR","UR"] <- 1
canton_neighbors["GL","UR"] <- 1
canton_neighbors["SZ","UR"] <- 1
canton_neighbors["NW","UR"] <- 1
canton_neighbors["OW","UR"] <- 1
canton_neighbors["BE","UR"] <- 1
canton_neighbors["VS","UR"] <- 1
canton_neighbors["TI","UR"] <- 1

canton_neighbors["GL","GR"] <- 1
canton_neighbors["SG","GR"] <- 1
canton_neighbors["TI","GR"] <- 1
canton_neighbors["UR","GR"] <- 1

canton_neighbors["SZ","GL"] <- 1
canton_neighbors["SG","GL"] <- 1
canton_neighbors["GR","GL"] <- 1
canton_neighbors["UR","GL"] <- 1

canton_neighbors["ZG","SZ"] <- 1
canton_neighbors["ZH","SZ"] <- 1
canton_neighbors["SG","SZ"] <- 1
canton_neighbors["GL","SZ"] <- 1
canton_neighbors["UR","SZ"] <- 1
canton_neighbors["NW","SZ"] <- 1
canton_neighbors["LU","SZ"] <- 1

canton_neighbors["TG","SG"] <- 1
canton_neighbors["AR","SG"] <- 1
canton_neighbors["AI","SG"] <- 1
canton_neighbors["GR","SG"] <- 1
canton_neighbors["GL","SG"] <- 1
canton_neighbors["SZ","SG"] <- 1
canton_neighbors["ZH","SG"] <- 1

canton_neighbors["SG","AI"] <- 1
canton_neighbors["AR","AI"] <- 1

canton_neighbors["AI","AR"] <- 1
canton_neighbors["SG","AR"] <- 1

canton_neighbors["ZH","ZG"] <- 1
canton_neighbors["SZ","ZG"] <- 1
canton_neighbors["LU","ZG"] <- 1
canton_neighbors["AG","ZG"] <- 1

canton_neighbors["SH","ZH"] <- 1
canton_neighbors["TG","ZH"] <- 1
canton_neighbors["SG","ZH"] <- 1
canton_neighbors["SZ","ZH"] <- 1
canton_neighbors["ZG","ZH"] <- 1
canton_neighbors["AG","ZH"] <- 1

canton_neighbors["TG","SH"] <- 1
canton_neighbors["ZH","SH"] <- 1

canton_neighbors["SH","TG"] <- 1
canton_neighbors["ZH","TG"] <- 1
canton_neighbors["SG","TG"] <- 1

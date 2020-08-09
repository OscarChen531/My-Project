attach(pp2)

# 1985
Pitching1985 <- pp2[yearID=='1985',]
ERA_Plus_1985 <- mean(Pitching1985$ERA, na.rm = TRUE)/Pitching1985$ERA
APR_1985 <- (Pitching1985$IPouts/3) / (9*mean(Pitching1985$ERA, na.rm = TRUE)- Pitching1985$ERA)
ERA_Plus_1985 <- round(ERA_Plus_1985,2)
APR_1985 <- round(APR_1985, 2)
Advanced1985 <- data.frame(x=ERA_Plus_1985,
                           y=APR_1985)
colnames(Advanced1985) <- c('ERA+', 'APR')
Pitching1985 <- cbind(Pitching1985, Advanced1985)

# 1986
Pitching1986 <- pp2[yearID=='1986',]
ERA_Plus_1986 <- mean(Pitching1986$ERA, na.rm = TRUE)/Pitching1986$ERA
APR_1986 <- (Pitching1986$IPouts/3) / (9*mean(Pitching1986$ERA, na.rm = TRUE)- Pitching1986$ERA)
ERA_Plus_1986 <- round(ERA_Plus_1986,2)
APR_1986 <- round(APR_1986, 2)
Advanced1986 <- data.frame(x=ERA_Plus_1986,
                           y=APR_1986)
colnames(Advanced1986) <- c('ERA+', 'APR')
Pitching1986 <- cbind(Pitching1986, Advanced1986)

# 1987
Pitching1987 <- pp2[yearID=='1987',]
ERA_Plus_1987 <- mean(Pitching1987$ERA, na.rm = TRUE)/Pitching1987$ERA
APR_1987 <- (Pitching1987$IPouts/3) / (9*mean(Pitching1987$ERA, na.rm = TRUE)- Pitching1987$ERA)
ERA_Plus_1987 <- round(ERA_Plus_1987,2)
APR_1987 <- round(APR_1987, 2)
Advanced1987 <- data.frame(x=ERA_Plus_1987,
                           y=APR_1987)
colnames(Advanced1987) <- c('ERA+', 'APR')
Pitching1987 <- cbind(Pitching1987, Advanced1987)

# 1988
Pitching1988 <- pp2[yearID=='1988',]
ERA_Plus_1988 <- mean(Pitching1988$ERA, na.rm = TRUE)/Pitching1988$ERA
APR_1988 <- (Pitching1988$IPouts/3) / (9*mean(Pitching1988$ERA, na.rm = TRUE)- Pitching1988$ERA)
ERA_Plus_1988 <- round(ERA_Plus_1988,2)
APR_1988 <- round(APR_1988, 2)
Advanced1988 <- data.frame(x=ERA_Plus_1988,
                           y=APR_1988)
colnames(Advanced1988) <- c('ERA+', 'APR')
Pitching1988 <- cbind(Pitching1988, Advanced1988)

# 1989
Pitching1989 <- pp2[yearID=='1989',]
ERA_Plus_1989 <- mean(Pitching1989$ERA, na.rm = TRUE)/Pitching1989$ERA
APR_1989 <- (Pitching1989$IPouts/3) / (9*mean(Pitching1989$ERA, na.rm = TRUE)- Pitching1989$ERA)
ERA_Plus_1989 <- round(ERA_Plus_1989,2)
APR_1989 <- round(APR_1989, 2)
Advanced1989 <- data.frame(x=ERA_Plus_1989,
                           y=APR_1989)
colnames(Advanced1989) <- c('ERA+', 'APR')
Pitching1989 <- cbind(Pitching1989, Advanced1989)

# 1990
Pitching1990 <- pp2[yearID=='1990',]
ERA_Plus_1990 <- mean(Pitching1990$ERA, na.rm = TRUE)/Pitching1990$ERA
APR_1990 <- (Pitching1990$IPouts/3) / (9*mean(Pitching1990$ERA, na.rm = TRUE)- Pitching1990$ERA)
ERA_Plus_1990 <- round(ERA_Plus_1990,2)
APR_1990 <- round(APR_1990, 2)
Advanced1990 <- data.frame(x=ERA_Plus_1990,
                           y=APR_1990)
colnames(Advanced1990) <- c('ERA+', 'APR')
Pitching1990 <- cbind(Pitching1990, Advanced1990)

# 1991
Pitching1991 <- pp2[yearID=='1991',]
ERA_Plus_1991 <- mean(Pitching1991$ERA, na.rm = TRUE)/Pitching1991$ERA
APR_1991 <- (Pitching1991$IPouts/3) / (9*mean(Pitching1991$ERA, na.rm = TRUE)- Pitching1991$ERA)
ERA_Plus_1991 <- round(ERA_Plus_1991,2)
APR_1991 <- round(APR_1991, 2)
Advanced1991 <- data.frame(x=ERA_Plus_1991,
                           y=APR_1991)
colnames(Advanced1991) <- c('ERA+', 'APR')
Pitching1991 <- cbind(Pitching1991, Advanced1991)

# 1992
Pitching1992 <- pp2[yearID=='1992',]
ERA_Plus_1992 <- mean(Pitching1992$ERA, na.rm = TRUE)/Pitching1992$ERA
APR_1992 <- (Pitching1992$IPouts/3) / (9*mean(Pitching1992$ERA, na.rm = TRUE)- Pitching1992$ERA)
ERA_Plus_1992 <- round(ERA_Plus_1992,2)
APR_1992 <- round(APR_1992, 2)
Advanced1992 <- data.frame(x=ERA_Plus_1992,
                           y=APR_1992)
colnames(Advanced1992) <- c('ERA+', 'APR')
Pitching1992 <- cbind(Pitching1992, Advanced1992)

# 1993
Pitching1993 <- pp2[yearID=='1993',]
ERA_Plus_1993 <- mean(Pitching1993$ERA, na.rm = TRUE)/Pitching1993$ERA
APR_1993 <- (Pitching1993$IPouts/3) / (9*mean(Pitching1993$ERA, na.rm = TRUE)- Pitching1993$ERA)
ERA_Plus_1993 <- round(ERA_Plus_1993,2)
APR_1993 <- round(APR_1993, 2)
Advanced1993 <- data.frame(x=ERA_Plus_1993,
                           y=APR_1993)
colnames(Advanced1993) <- c('ERA+', 'APR')
Pitching1993 <- cbind(Pitching1993, Advanced1993)

# 1994
Pitching1994 <- pp2[yearID=='1994',]
ERA_Plus_1994 <- mean(Pitching1994$ERA, na.rm = TRUE)/Pitching1994$ERA
APR_1994 <- (Pitching1994$IPouts/3) / (9*mean(Pitching1994$ERA, na.rm = TRUE)- Pitching1994$ERA)
ERA_Plus_1994 <- round(ERA_Plus_1994,2)
APR_1994 <- round(APR_1994, 2)
Advanced1994 <- data.frame(x=ERA_Plus_1994,
                           y=APR_1994)
colnames(Advanced1994) <- c('ERA+', 'APR')
Pitching1994 <- cbind(Pitching1994, Advanced1994)

# 1995
Pitching1995 <- pp2[yearID=='1995',]
ERA_Plus_1995 <- mean(Pitching1995$ERA, na.rm = TRUE)/Pitching1995$ERA
APR_1995 <- (Pitching1995$IPouts/3) / (9*mean(Pitching1995$ERA, na.rm = TRUE)- Pitching1995$ERA)
ERA_Plus_1995 <- round(ERA_Plus_1995,2)
APR_1995 <- round(APR_1995, 2)
Advanced1995 <- data.frame(x=ERA_Plus_1995,
                           y=APR_1995)
colnames(Advanced1995) <- c('ERA+', 'APR')
Pitching1995 <- cbind(Pitching1995, Advanced1995)

# 1996
Pitching1996 <- pp2[yearID=='1996',]
ERA_Plus_1996 <- mean(Pitching1996$ERA, na.rm = TRUE)/Pitching1996$ERA
APR_1996 <- (Pitching1996$IPouts/3) / (9*mean(Pitching1996$ERA, na.rm = TRUE)- Pitching1996$ERA)
ERA_Plus_1996 <- round(ERA_Plus_1996,2)
APR_1996 <- round(APR_1996, 2)
Advanced1996 <- data.frame(x=ERA_Plus_1996,
                           y=APR_1996)
colnames(Advanced1996) <- c('ERA+', 'APR')
Pitching1996 <- cbind(Pitching1996, Advanced1996)

# 1997
Pitching1997 <- pp2[yearID=='1997',]
ERA_Plus_1997 <- mean(Pitching1997$ERA, na.rm = TRUE)/Pitching1997$ERA
APR_1997 <- (Pitching1997$IPouts/3) / (9*mean(Pitching1997$ERA, na.rm = TRUE)- Pitching1997$ERA)
ERA_Plus_1997 <- round(ERA_Plus_1997,2)
APR_1997 <- round(APR_1997, 2)
Advanced1997 <- data.frame(x=ERA_Plus_1997,
                           y=APR_1997)
colnames(Advanced1997) <- c('ERA+', 'APR')
Pitching1997 <- cbind(Pitching1997, Advanced1997)

# 1998
Pitching1998 <- pp2[yearID=='1998',]
ERA_Plus_1998 <- mean(Pitching1998$ERA, na.rm = TRUE)/Pitching1998$ERA
APR_1998 <- (Pitching1998$IPouts/3) / (9*mean(Pitching1998$ERA, na.rm = TRUE)- Pitching1998$ERA)
ERA_Plus_1998 <- round(ERA_Plus_1998,2)
APR_1998 <- round(APR_1998, 2)
Advanced1998 <- data.frame(x=ERA_Plus_1998,
                           y=APR_1998)
colnames(Advanced1998) <- c('ERA+', 'APR')
Pitching1998 <- cbind(Pitching1998, Advanced1998)

# 1999
Pitching1999 <- pp2[yearID=='1999',]
ERA_Plus_1999 <- mean(Pitching1999$ERA, na.rm = TRUE)/Pitching1999$ERA
APR_1999 <- (Pitching1999$IPouts/3) / (9*mean(Pitching1999$ERA, na.rm = TRUE)- Pitching1999$ERA)
ERA_Plus_1999 <- round(ERA_Plus_1999,2)
APR_1999 <- round(APR_1999, 2)
Advanced1999 <- data.frame(x=ERA_Plus_1999,
                           y=APR_1999)
colnames(Advanced1999) <- c('ERA+', 'APR')
Pitching1999 <- cbind(Pitching1999, Advanced1999)

# 2000
Pitching2000 <- pp2[yearID=='2000',]
ERA_Plus_2000 <- mean(Pitching2000$ERA, na.rm = TRUE)/Pitching2000$ERA
APR_2000 <- (Pitching2000$IPouts/3) / (9*mean(Pitching2000$ERA, na.rm = TRUE)- Pitching2000$ERA)
ERA_Plus_2000 <- round(ERA_Plus_2000,2)
APR_2000 <- round(APR_2000, 2)
Advanced2000 <- data.frame(x=ERA_Plus_2000,
                           y=APR_2000)
colnames(Advanced2000) <- c('ERA+', 'APR')
Pitching2000 <- cbind(Pitching2000, Advanced2000)

# 2001
Pitching2001 <- pp2[yearID=='2001',]
ERA_Plus_2001 <- mean(Pitching2001$ERA, na.rm = TRUE)/Pitching2001$ERA
APR_2001 <- (Pitching2001$IPouts/3) / (9*mean(Pitching2001$ERA, na.rm = TRUE)- Pitching2001$ERA)
ERA_Plus_2001 <- round(ERA_Plus_2001,2)
APR_2001 <- round(APR_2001, 2)
Advanced2001 <- data.frame(x=ERA_Plus_2001,
                           y=APR_2001)
colnames(Advanced2001) <- c('ERA+', 'APR')
Pitching2001 <- cbind(Pitching2001, Advanced2001)

# 2002
Pitching2002 <- pp2[yearID=='2002',]
ERA_Plus_2002 <- mean(Pitching2002$ERA, na.rm = TRUE)/Pitching2002$ERA
APR_2002 <- (Pitching2002$IPouts/3) / (9*mean(Pitching2002$ERA, na.rm = TRUE)- Pitching2002$ERA)
ERA_Plus_2002 <- round(ERA_Plus_2002,2)
APR_2002 <- round(APR_2002, 2)
Advanced2002 <- data.frame(x=ERA_Plus_2002,
                           y=APR_2002)
colnames(Advanced2002) <- c('ERA+', 'APR')
Pitching2002 <- cbind(Pitching2002, Advanced2002)

# 2003
Pitching2003 <- pp2[yearID=='2003',]
ERA_Plus_2003 <- mean(Pitching2003$ERA, na.rm = TRUE)/Pitching2003$ERA
APR_2003 <- (Pitching2003$IPouts/3) / (9*mean(Pitching2003$ERA, na.rm = TRUE)- Pitching2003$ERA)
ERA_Plus_2003 <- round(ERA_Plus_2003,2)
APR_2003 <- round(APR_2003, 2)
Advanced2003 <- data.frame(x=ERA_Plus_2003,
                           y=APR_2003)
colnames(Advanced2003) <- c('ERA+', 'APR')
Pitching2003 <- cbind(Pitching2003, Advanced2003)

# 2004
Pitching2004 <- pp2[yearID=='2004',]
ERA_Plus_2004 <- mean(Pitching2004$ERA, na.rm = TRUE)/Pitching2004$ERA
APR_2004 <- (Pitching2004$IPouts/3) / (9*mean(Pitching2004$ERA, na.rm = TRUE)- Pitching2004$ERA)
ERA_Plus_2004 <- round(ERA_Plus_2004,2)
APR_2004 <- round(APR_2004, 2)
Advanced2004 <- data.frame(x=ERA_Plus_2004,
                           y=APR_2004)
colnames(Advanced2004) <- c('ERA+', 'APR')
Pitching2004 <- cbind(Pitching2004, Advanced2004)

# 2005
Pitching2005 <- pp2[yearID=='2005',]
ERA_Plus_2005 <- mean(Pitching2005$ERA, na.rm = TRUE)/Pitching2005$ERA
APR_2005 <- (Pitching2005$IPouts/3) / (9*mean(Pitching2005$ERA, na.rm = TRUE)- Pitching2005$ERA)
ERA_Plus_2005 <- round(ERA_Plus_2005,2)
APR_2005 <- round(APR_2005, 2)
Advanced2005 <- data.frame(x=ERA_Plus_2005,
                           y=APR_2005)
colnames(Advanced2005) <- c('ERA+', 'APR')
Pitching2005 <- cbind(Pitching2005, Advanced2005)

# 2006
Pitching2006 <- pp2[yearID=='2006',]
ERA_Plus_2006 <- mean(Pitching2006$ERA, na.rm = TRUE)/Pitching2006$ERA
APR_2006 <- (Pitching2006$IPouts/3) / (9*mean(Pitching2006$ERA, na.rm = TRUE)- Pitching2006$ERA)
ERA_Plus_2006 <- round(ERA_Plus_2006,2)
APR_2006 <- round(APR_2006, 2)
Advanced2006 <- data.frame(x=ERA_Plus_2006,
                           y=APR_2006)
colnames(Advanced2006) <- c('ERA+', 'APR')
Pitching2006 <- cbind(Pitching2006, Advanced2006)

# 2007
Pitching2007 <- pp2[yearID=='2007',]
ERA_Plus_2007 <- mean(Pitching2007$ERA, na.rm = TRUE)/Pitching2007$ERA
APR_2007 <- (Pitching2007$IPouts/3) / (9*mean(Pitching2007$ERA, na.rm = TRUE)- Pitching2007$ERA)
ERA_Plus_2007 <- round(ERA_Plus_2007,2)
APR_2007 <- round(APR_2007, 2)
Advanced2007 <- data.frame(x=ERA_Plus_2007,
                           y=APR_2007)
colnames(Advanced2007) <- c('ERA+', 'APR')
Pitching2007 <- cbind(Pitching2007, Advanced2007)

# 2008
Pitching2008 <- pp2[yearID=='2008',]
ERA_Plus_2008 <- mean(Pitching2008$ERA, na.rm = TRUE)/Pitching2008$ERA
APR_2008 <- (Pitching2008$IPouts/3) / (9*mean(Pitching2008$ERA, na.rm = TRUE)- Pitching2008$ERA)
ERA_Plus_2008 <- round(ERA_Plus_2008,2)
APR_2008 <- round(APR_2008, 2)
Advanced2008 <- data.frame(x=ERA_Plus_2008,
                           y=APR_2008)
colnames(Advanced2008) <- c('ERA+', 'APR')
Pitching2008 <- cbind(Pitching2008, Advanced2008)

# 2009
Pitching2009 <- pp2[yearID=='2009',]
ERA_Plus_2009 <- mean(Pitching2009$ERA, na.rm = TRUE)/Pitching2009$ERA
APR_2009 <- (Pitching2009$IPouts/3) / (9*mean(Pitching2009$ERA, na.rm = TRUE)- Pitching2009$ERA)
ERA_Plus_2009 <- round(ERA_Plus_2009,2)
APR_2009 <- round(APR_2009, 2)
Advanced2009 <- data.frame(x=ERA_Plus_2009,
                           y=APR_2009)
colnames(Advanced2009) <- c('ERA+', 'APR')
Pitching2009 <- cbind(Pitching2009, Advanced2009)

# 2010
Pitching2010 <- pp2[yearID=='2010',]
ERA_Plus_2010 <- mean(Pitching2010$ERA, na.rm = TRUE)/Pitching2010$ERA
APR_2010 <- (Pitching2010$IPouts/3) / (9*mean(Pitching2010$ERA, na.rm = TRUE)- Pitching2010$ERA)
ERA_Plus_2010 <- round(ERA_Plus_2010,2)
APR_2010 <- round(APR_2010, 2)
Advanced2010 <- data.frame(x=ERA_Plus_2010,
                           y=APR_2010)
colnames(Advanced2010) <- c('ERA+', 'APR')
Pitching2010 <- cbind(Pitching2010, Advanced2010)

# 2011
Pitching2011 <- pp2[yearID=='2011',]
ERA_Plus_2011 <- mean(Pitching2011$ERA, na.rm = TRUE)/Pitching2011$ERA
APR_2011 <- (Pitching2011$IPouts/3) / (9*mean(Pitching2011$ERA, na.rm = TRUE)- Pitching2011$ERA)
ERA_Plus_2011 <- round(ERA_Plus_2011,2)
APR_2011 <- round(APR_2011, 2)
Advanced2011 <- data.frame(x=ERA_Plus_2011,
                           y=APR_2011)
colnames(Advanced2011) <- c('ERA+', 'APR')
Pitching2011 <- cbind(Pitching2011, Advanced2011)

# 2012
Pitching2012 <- pp2[yearID=='2012',]
ERA_Plus_2012 <- mean(Pitching2012$ERA, na.rm = TRUE)/Pitching2012$ERA
APR_2012 <- (Pitching2012$IPouts/3) / (9*mean(Pitching2012$ERA, na.rm = TRUE)- Pitching2012$ERA)
ERA_Plus_2012 <- round(ERA_Plus_2012,2)
APR_2012 <- round(APR_2012, 2)
Advanced2012 <- data.frame(x=ERA_Plus_2012,
                           y=APR_2012)
colnames(Advanced2012) <- c('ERA+', 'APR')
Pitching2012 <- cbind(Pitching2012, Advanced2012)

# 2013
Pitching2013 <- pp2[yearID=='2013',]
ERA_Plus_2013 <- mean(Pitching2013$ERA, na.rm = TRUE)/Pitching2013$ERA
APR_2013 <- (Pitching2013$IPouts/3) / (9*mean(Pitching2013$ERA, na.rm = TRUE)- Pitching2013$ERA)
ERA_Plus_2013 <- round(ERA_Plus_2013,2)
APR_2013 <- round(APR_2013, 2)
Advanced2013 <- data.frame(x=ERA_Plus_2013,
                           y=APR_2013)
colnames(Advanced2013) <- c('ERA+', 'APR')
Pitching2013 <- cbind(Pitching2013, Advanced2013)

# 2014
Pitching2014 <- pp2[yearID=='2014',]
ERA_Plus_2014 <- mean(Pitching2014$ERA, na.rm = TRUE)/Pitching2014$ERA
APR_2014 <- (Pitching2014$IPouts/3) / (9*mean(Pitching2014$ERA, na.rm = TRUE)- Pitching2014$ERA)
ERA_Plus_2014 <- round(ERA_Plus_2014,2)
APR_2014 <- round(APR_2014, 2)
Advanced2014 <- data.frame(x=ERA_Plus_2014,
                           y=APR_2014)
colnames(Advanced2014) <- c('ERA+', 'APR')
Pitching2014 <- cbind(Pitching2014, Advanced2014)

# 2015
Pitching2015 <- pp2[yearID=='2015',]
ERA_Plus_2015 <- mean(Pitching2015$ERA, na.rm = TRUE)/Pitching2015$ERA
APR_2015 <- (Pitching2015$IPouts/3) / (9*mean(Pitching2015$ERA, na.rm = TRUE)-Pitching2015$ERA)
ERA_Plus_2015 <- round(ERA_Plus_2015,2)
APR_2015 <- round(APR_2015, 2)
Advanced2015 <- data.frame(x=ERA_Plus_2015,
                           y=APR_2015)
colnames(Advanced2015) <- c('ERA+', 'APR')
Pitching2015 <- cbind(Pitching2015, Advanced2015)

# 2016
Pitching2016 <- pp2[yearID=='2016',]
ERA_Plus_2016 <- mean(Pitching2016$ERA, na.rm = TRUE)/Pitching2016$ERA
APR_2016 <- (Pitching2016$IPouts/3) / (9*mean(Pitching2016$ERA, na.rm = TRUE)-Pitching2016$ERA)
ERA_Plus_2016 <- round(ERA_Plus_2016,2)
APR_2016 <- round(APR_2016, 2)
Advanced2016 <- data.frame(x=ERA_Plus_2016,
                           y=APR_2016)
colnames(Advanced2016) <- c('ERA+', 'APR')
Pitching2016 <- cbind(Pitching2016, Advanced2016)

# 1985-2016
Pitching1985_2016 <- rbind(Pitching1985, Pitching1986, Pitching1987, Pitching1988, Pitching1989, Pitching1990,
                           Pitching1991, Pitching1992, Pitching1993, Pitching1994, Pitching1995, Pitching1996,
                           Pitching1997, Pitching1998, Pitching1999, Pitching2000, Pitching2001, Pitching2002,
                           Pitching2003, Pitching2004, Pitching2005, Pitching2006, Pitching2007, Pitching2008,
                           Pitching2009, Pitching2010, Pitching2011,Pitching2012, Pitching2013, Pitching2014, Pitching2015, Pitching2016)
Pitching1985_2016 <- Pitching1985_2016[,-20]
Pitching1985_2016$ERA =with(Pitching1985_2016, ER*9/(IPouts/3))
Pitching1985_2016$ERA <- round(Pitching1985_2016$ERA, 2)
Pitching1985_2016$`ERA+` <- Pitching1985_2016$`ERA+`*100
detach(pp2)


# performance vs salary
pitching_salary <- merge(Pitching1985_2016, Salaries,by=c("playerID", "yearID", "teamID", "lgID"))
colnames(pitching_salary) <- c("playerID", "yearID", "teamID", "lgID", "stint",  "W",  "L",  "G", "GS", "CG", "SHO", "SV", "IPouts", "H", "ER", "HR", "BB", "SO", "BAOpp", "IBB", "WP", "HBP", "BK",  "BFP", "GF", "R", "SH", "SF", "GIDP", "ERA_plus", "APR", "ERA","salary")



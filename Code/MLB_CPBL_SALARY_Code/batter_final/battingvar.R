attach(ds2)

# 2016
Batting2016 <- ds2[yearID=='2016',]
AVG2016 <- (Batting2016$H)/(Batting2016$AB)
OBP2016 <- (Batting2016$H + Batting2016$BB + Batting2016$HBP)/(Batting2016$AB + Batting2016$BB + Batting2016$HBP + Batting2016$SF)
X1B <- Batting2016$H - (Batting2016$X2B + Batting2016$X3B + Batting2016$HR)
TB2016 <- 1*X1B + 2*Batting2016$X2B + 3*Batting2016$X3B + 4*Batting2016$HR
SLG2016 <- TB2016/Batting2016$AB
OPS2016 <- OBP2016 + SLG2016
IsoP2016 <- SLG2016 - AVG2016
IsoD2016 <- OBP2016 - AVG2016
SecA2016 <- (Batting2016$BB + (TB2016 - Batting2016$H) + (Batting2016$SB - Batting2016$CS)) / Batting2016$AB
EqA2016 <- (Batting2016$H + TB2016 + 1.5*(Batting2016$BB + Batting2016$HBP) + Batting2016$SB) / (Batting2016$AB + Batting2016$BB + Batting2016$HBP + Batting2016$CS + (Batting2016$SB/3))
OPS_Plus_2016 <- ((OBP2016/(sum(OBP2016, na.rm = T)/length(OBP2016))) + (SLG2016/(sum(SLG2016, na.rm = T)/length(SLG2016))) -1)
A_2016 <- Batting2016$H + Batting2016$BB + Batting2016$HBP - Batting2016$CS - Batting2016$GIDP
B_2016 <- TB2016 + 0.24*(Batting2016$BB - Batting2016$IBB + Batting2016$HBP) + 0.62*Batting2016$SB + 0.5*(Batting2016$SH + Batting2016$SF) - 0.3*(Batting2016$SO)
C_2016 <- Batting2016$AB + Batting2016$BB + Batting2016$HBP + Batting2016$SH + Batting2016$SF
RC2016 <- ((2.4*C_2016 + A_2016)*(3*C_2016 + B_2016) / (9*C_2016)) - 0.9*C_2016

Advanced2016 <- data.frame(AVG2016, OBP2016, SLG2016, OPS2016, IsoP2016, IsoD2016,
                           SecA2016, EqA2016, OPS_Plus_2016, RC2016)
colnames(Advanced2016) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced2016 <- round(Advanced2016, 3)
Batting2016 <- cbind(Batting2016, Advanced2016)

# 2015
Batting2015 <- ds2[yearID=='2015',]
AVG2015 <- (Batting2015$H)/(Batting2015$AB)
OBP2015 <- (Batting2015$H + Batting2015$BB + Batting2015$HBP)/(Batting2015$AB + Batting2015$BB + Batting2015$HBP + Batting2015$SF)
X1B <- Batting2015$H - (Batting2015$X2B + Batting2015$X3B + Batting2015$HR)
TB2015 <- 1*X1B + 2*Batting2015$X2B + 3*Batting2015$X3B + 4*Batting2015$HR
SLG2015 <- TB2015/Batting2015$AB
OPS2015 <- OBP2015 + SLG2015
IsoP2015 <- SLG2015 - AVG2015
IsoD2015 <- OBP2015 - AVG2015
SecA2015 <- (Batting2015$BB + (TB2015 - Batting2015$H) + (Batting2015$SB - Batting2015$CS)) / Batting2015$AB
EqA2015 <- (Batting2015$H + TB2015 + 1.5*(Batting2015$BB + Batting2015$HBP) + Batting2015$SB) / (Batting2015$AB + Batting2015$BB + Batting2015$HBP + Batting2015$CS + (Batting2015$SB/3))
OPS_Plus_2015 <- ((OBP2015/(sum(OBP2015, na.rm = T)/length(OBP2015))) + (SLG2015/(sum(SLG2015, na.rm = T)/length(SLG2015))) -1)
A_2015 <- Batting2015$H + Batting2015$BB + Batting2015$HBP - Batting2015$CS - Batting2015$GIDP
B_2015 <- TB2015 + 0.24*(Batting2015$BB - Batting2015$IBB + Batting2015$HBP) + 0.62*Batting2015$SB + 0.5*(Batting2015$SH + Batting2015$SF) - 0.3*(Batting2015$SO)
C_2015 <- Batting2015$AB + Batting2015$BB + Batting2015$HBP + Batting2015$SH + Batting2015$SF
RC2015 <- ((2.4*C_2015 + A_2015)*(3*C_2015 + B_2015) / (9*C_2015)) - 0.9*C_2015

Advanced2015 <- data.frame(AVG2015, OBP2015, SLG2015, OPS2015, IsoP2015, IsoD2015,
                           SecA2015, EqA2015, OPS_Plus_2015, RC2015)
colnames(Advanced2015) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced2015 <- round(Advanced2015, 3)
Batting2015 <- cbind(Batting2015, Advanced2015)

# 2014
Batting2014 <- ds2[yearID=='2014',]
AVG2014 <- (Batting2014$H)/(Batting2014$AB)
OBP2014 <- (Batting2014$H + Batting2014$BB + Batting2014$HBP)/(Batting2014$AB + Batting2014$BB + Batting2014$HBP + Batting2014$SF)
X1B <- Batting2014$H - (Batting2014$X2B + Batting2014$X3B + Batting2014$HR)
TB2014 <- 1*X1B + 2*Batting2014$X2B + 3*Batting2014$X3B + 4*Batting2014$HR
SLG2014 <- TB2014/Batting2014$AB
OPS2014 <- OBP2014 + SLG2014
IsoP2014 <- SLG2014 - AVG2014
IsoD2014 <- OBP2014 - AVG2014
SecA2014 <- (Batting2014$BB + (TB2014 - Batting2014$H) + (Batting2014$SB - Batting2014$CS)) / Batting2014$AB
EqA2014 <- (Batting2014$H + TB2014 + 1.5*(Batting2014$BB + Batting2014$HBP) + Batting2014$SB) / (Batting2014$AB + Batting2014$BB + Batting2014$HBP + Batting2014$CS + (Batting2014$SB/3))
OPS_Plus_2014 <- ((OBP2014/(sum(OBP2014, na.rm = T)/length(OBP2014))) + (SLG2014/(sum(SLG2014, na.rm = T)/length(SLG2014))) -1)
A_2014 <- Batting2014$H + Batting2014$BB + Batting2014$HBP - Batting2014$CS - Batting2014$GIDP
B_2014 <- TB2014 + 0.24*(Batting2014$BB - Batting2014$IBB + Batting2014$HBP) + 0.62*Batting2014$SB + 0.5*(Batting2014$SH + Batting2014$SF) - 0.3*(Batting2014$SO)
C_2014 <- Batting2014$AB + Batting2014$BB + Batting2014$HBP + Batting2014$SH + Batting2014$SF
RC2014 <- ((2.4*C_2014 + A_2014)*(3*C_2014 + B_2014) / (9*C_2014)) - 0.9*C_2014

Advanced2014 <- data.frame(AVG2014, OBP2014, SLG2014, OPS2014, IsoP2014, IsoD2014,
                           SecA2014, EqA2014, OPS_Plus_2014, RC2014)
colnames(Advanced2014) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced2014 <- round(Advanced2014, 3)
Batting2014 <- cbind(Batting2014, Advanced2014)

# 2013
Batting2013 <- ds2[yearID=='2013',]
AVG2013 <- (Batting2013$H)/(Batting2013$AB)
OBP2013 <- (Batting2013$H + Batting2013$BB + Batting2013$HBP)/(Batting2013$AB + Batting2013$BB + Batting2013$HBP + Batting2013$SF)
X1B <- Batting2013$H - (Batting2013$X2B + Batting2013$X3B + Batting2013$HR)
TB2013 <- 1*X1B + 2*Batting2013$X2B + 3*Batting2013$X3B + 4*Batting2013$HR
SLG2013 <- TB2013/Batting2013$AB
OPS2013<- OBP2013 + SLG2013
IsoP2013<- SLG2013 - AVG2013
IsoD2013<- OBP2013 - AVG2013
SecA2013<- (Batting2013$BB + (TB2013 - Batting2013$H) + (Batting2013$SB - Batting2013$CS)) / Batting2013$AB
EqA2013<- (Batting2013$H + TB2013 + 1.5*(Batting2013$BB + Batting2013$HBP) + Batting2013$SB) / (Batting2013$AB + Batting2013$BB + Batting2013$HBP + Batting2013$CS + (Batting2013$SB/3))
OPS_Plus_2013<- ((OBP2013/(sum(OBP2013, na.rm = T)/length(OBP2013))) + (SLG2013/(sum(SLG2013, na.rm = T)/length(SLG2013))) -1)
A_2013<- Batting2013$H + Batting2013$BB + Batting2013$HBP - Batting2013$CS - Batting2013$GIDP
B_2013<- TB2013 + 0.24*(Batting2013$BB - Batting2013$IBB + Batting2013$HBP) + 0.62*Batting2013$SB + 0.5*(Batting2013$SH + Batting2013$SF) - 0.3*(Batting2013$SO)
C_2013<- Batting2013$AB + Batting2013$BB + Batting2013$HBP + Batting2013$SH + Batting2013$SF
RC2013 <- ((2.4*C_2013 + A_2013)*(3*C_2013 + B_2013) / (9*C_2013)) - 0.9*C_2013

Advanced2013 <- data.frame(AVG2013, OBP2013, SLG2013, OPS2013, IsoP2013, IsoD2013,
                           SecA2013, EqA2013, OPS_Plus_2013, RC2013)
colnames(Advanced2013) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced2013 <- round(Advanced2013, 3)
Batting2013 <- cbind(Batting2013, Advanced2013)

# 2012
Batting2012<- ds2[yearID=='2012',]
AVG2012 <- (Batting2012$H)/(Batting2012$AB)
OBP2012 <- (Batting2012$H + Batting2012$BB + Batting2012$HBP)/(Batting2012$AB + Batting2012$BB + Batting2012$HBP + Batting2012$SF)
X1B <- Batting2012$H - (Batting2012$X2B + Batting2012$X3B + Batting2012$HR)
TB2012 <- 1*X1B + 2*Batting2012$X2B + 3*Batting2012$X3B + 4*Batting2012$HR
SLG2012 <- TB2012/Batting2012$AB
OPS2012 <- OBP2012 + SLG2012
IsoP2012 <- SLG2012 - AVG2012
IsoD2012 <- OBP2012 - AVG2012
SecA2012 <- (Batting2012$BB + (TB2012 - Batting2012$H) + (Batting2012$SB - Batting2012$CS)) / Batting2012$AB
EqA2012 <- (Batting2012$H + TB2012 + 1.5*(Batting2012$BB + Batting2012$HBP) + Batting2012$SB) / (Batting2012$AB + Batting2012$BB + Batting2012$HBP + Batting2012$CS + (Batting2012$SB/3))
OPS_Plus_2012 <- (((sum(OBP2012, na.rm = T)/length(OBP2012))) + (SLG2012/(sum(SLG2012, na.rm = T)/length(SLG2012))) -1)
A_2012 <- Batting2012$H + Batting2012$BB + Batting2012$HBP - Batting2012$CS - Batting2012$GIDP
B_2012 <- TB2012 + 0.24*(Batting2012$BB - Batting2012$IBB + Batting2012$HBP) + 0.62*Batting2012$SB + 0.5*(Batting2012$SH + Batting2012$SF) - 0.3*(Batting2012$SO)
C_2012 <- Batting2012$AB + Batting2012$BB + Batting2012$HBP + Batting2012$SH + Batting2012$SF
RC2012 <- ((2.4*C_2012 + A_2012)*(3*C_2012 + B_2012) / (9*C_2012)) - 0.9*C_2012

Advanced2012 <- data.frame(AVG2012, OBP2012, SLG2012, OPS2012, IsoP2012, IsoD2012,
                           SecA2012, EqA2012, OPS_Plus_2012, RC2012)
colnames(Advanced2012) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced2012 <- round(Advanced2012, 3)
Batting2012 <- cbind(Batting2012, Advanced2012)

# 2011
Batting2011 <- ds2[yearID=='2011',]
AVG2011 <- (Batting2011$H)/(Batting2011$AB)
OBP2011 <- (Batting2011$H + Batting2011$BB + Batting2011$HBP)/(Batting2011$AB + Batting2011$BB + Batting2011$HBP + Batting2011$SF)
X1B <- Batting2011$H - (Batting2011$X2B + Batting2011$X3B + Batting2011$HR)
TB2011 <- 1*X1B + 2*Batting2011$X2B + 3*Batting2011$X3B + 4*Batting2011$HR
SLG2011 <- TB2011/Batting2011$AB
OPS2011 <- OBP2011 + SLG2011
IsoP2011 <- SLG2011 - AVG2011
IsoD2011 <- OBP2011 - AVG2011
SecA2011 <- (Batting2011$BB + (TB2011 - Batting2011$H) + (Batting2011$SB - Batting2011$CS)) / Batting2011$AB
EqA2011 <- (Batting2011$H + TB2011 + 1.5*(Batting2011$BB + Batting2011$HBP) + Batting2011$SB) / (Batting2011$AB + Batting2011$BB + Batting2011$HBP + Batting2011$CS + (Batting2011$SB/3))
OPS_Plus_2011 <- ((OBP2011/(sum(OBP2011, na.rm = T)/length(OBP2011))) + (SLG2011/(sum(SLG2011, na.rm = T)/length(SLG2011))) -1)
A_2011 <- Batting2011$H + Batting2011$BB + Batting2011$HBP - Batting2011$CS - Batting2011$GIDP
B_2011 <- TB2011 + 0.24*(Batting2011$BB - Batting2011$IBB + Batting2011$HBP) + 0.62*Batting2011$SB + 0.5*(Batting2011$SH + Batting2011$SF) - 0.3*(Batting2011$SO)
C_2011 <- Batting2011$AB + Batting2011$BB + Batting2011$HBP + Batting2011$SH + Batting2011$SF
RC2011 <- ((2.4*C_2011 + A_2011)*(3*C_2011 + B_2011) / (9*C_2011)) - 0.9*C_2011

Advanced2011 <- data.frame(AVG2011, OBP2011, SLG2011, OPS2011, IsoP2011, IsoD2011,
                           SecA2011, EqA2011, OPS_Plus_2011, RC2011)
colnames(Advanced2011) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced2011 <- round(Advanced2011, 3)
Batting2011 <- cbind(Batting2011, Advanced2011)

# 2010
Batting2010 <- ds2[yearID=='2010',]
AVG2010 <- (Batting2010$H)/(Batting2010$AB)
OBP2010 <- (Batting2010$H + Batting2010$BB + Batting2010$HBP)/(Batting2010$AB + Batting2010$BB + Batting2010$HBP + Batting2010$SF)
X1B <- Batting2010$H - (Batting2010$X2B + Batting2010$X3B + Batting2010$HR)
TB2010 <- 1*X1B + 2*Batting2010$X2B + 3*Batting2010$X3B + 4*Batting2010$HR
SLG2010 <- TB2010/Batting2010$AB
OPS2010<- OBP2010 + SLG2010
IsoP2010 <- SLG2010 - AVG2010
IsoD2010 <- OBP2010 - AVG2010
SecA2010 <- (Batting2010$BB + (TB2010 - Batting2010$H) + (Batting2010$SB - Batting2010$CS)) / Batting2010$AB
EqA2010 <- (Batting2010$H + TB2010 + 1.5*(Batting2010$BB + Batting2010$HBP) + Batting2010$SB) / (Batting2010$AB + Batting2010$BB + Batting2010$HBP + Batting2010$CS + (Batting2010$SB/3))
OPS_Plus_2010 <- ((OBP2010/(sum(OBP2010, na.rm = T)/length(OBP2010))) + (SLG2010/(sum(SLG2010, na.rm = T)/length(SLG2010))) -1)
A_2010 <- Batting2010$H + Batting2010$BB + Batting2010$HBP - Batting2010$CS - Batting2010$GIDP
B_2010 <- TB2010 + 0.24*(Batting2010$BB - Batting2010$IBB + Batting2010$HBP) + 0.62*Batting2010$SB + 0.5*(Batting2010$SH + Batting2010$SF) - 0.3*(Batting2010$SO)
C_2010 <- Batting2010$AB + Batting2010$BB + Batting2010$HBP + Batting2010$SH + Batting2010$SF
RC2010 <- ((2.4*C_2010 + A_2010)*(3*C_2010 + B_2010) / (9*C_2010)) - 0.9*C_2010

Advanced2010 <- data.frame(AVG2010, OBP2010, SLG2010, OPS2010, IsoP2010, IsoD2010,
                           SecA2010, EqA2010, OPS_Plus_2010, RC2010)
colnames(Advanced2010) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced2010 <- round(Advanced2010, 3)
Batting2010 <- cbind(Batting2010, Advanced2010)

# 2009
Batting2009 <- ds2[yearID=='2009',]
AVG2009 <- (Batting2009$H)/(Batting2009$AB)
OBP2009<- (Batting2009$H + Batting2009$BB + Batting2009$HBP)/(Batting2009$AB + Batting2009$BB + Batting2009$HBP + Batting2009$SF)
X1B <- Batting2009$H - (Batting2009$X2B + Batting2009$X3B + Batting2009$HR)
TB2009<- 1*X1B + 2*Batting2009$X2B + 3*Batting2009$X3B + 4*Batting2009$HR
SLG2009<- TB2009/Batting2009$AB
OPS2009 <- OBP2009 + SLG2009
IsoP2009 <- SLG2009 - AVG2009
IsoD2009 <- OBP2009 - AVG2009
SecA2009 <- (Batting2009$BB + (TB2009 - Batting2009$H) + (Batting2009$SB - Batting2009$CS)) / Batting2009$AB
EqA2009 <- (Batting2009$H + TB2009+ 1.5*(Batting2009$BB + Batting2009$HBP) + Batting2009$SB) / (Batting2009$AB + Batting2009$BB + Batting2009$HBP + Batting2009$CS + (Batting2009$SB/3))
OPS_Plus_2009 <- ((OBP2009/(sum(OBP2009, na.rm = T)/length(OBP2009))) + (SLG2009/(sum(SLG2009, na.rm = T)/length(SLG2009))) -1)
A_2009 <- Batting2009$H + Batting2009$BB + Batting2009$HBP - Batting2009$CS - Batting2009$GIDP
B_2009 <- TB2009 + 0.24*(Batting2009$BB - Batting2009$IBB + Batting2009$HBP) + 0.62*Batting2009$SB + 0.5*(Batting2009$SH + Batting2009$SF) - 0.3*(Batting2009$SO)
C_2009 <- Batting2009$AB + Batting2009$BB + Batting2009$HBP + Batting2009$SH + Batting2009$SF
RC2009 <- ((2.4*C_2009 + A_2009)*(3*C_2009 + B_2009) / (9*C_2009)) - 0.9*C_2009

Advanced2009 <- data.frame(AVG2009, OBP2009, SLG2009, OPS2009, IsoP2009, IsoD2009,
                           SecA2009, EqA2009, OPS_Plus_2009, RC2009)
colnames(Advanced2009) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced2009 <- round(Advanced2009, 3)
Batting2009 <- cbind(Batting2009, Advanced2009)

# 2008
Batting2008 <- ds2[yearID=='2008',]
AVG2008 <- (Batting2008$H)/(Batting2008$AB)
OBP2008 <- (Batting2008$H + Batting2008$BB + Batting2008$HBP)/(Batting2008$AB + Batting2008$BB + Batting2008$HBP + Batting2008$SF)
X1B <- Batting2008$H - (Batting2008$X2B + Batting2008$X3B + Batting2008$HR)
TB2008 <- 1*X1B + 2*Batting2008$X2B + 3*Batting2008$X3B + 4*Batting2008$HR
SLG2008 <- TB2008/Batting2008$AB
OPS2008 <- OBP2008 + SLG2008
IsoP2008 <- SLG2008 - AVG2008
IsoD2008 <- OBP2008 - AVG2008
SecA2008 <- (Batting2008$BB + (TB2008 - Batting2008$H) + (Batting2008$SB - Batting2008$CS)) / Batting2008$AB
EqA2008 <- (Batting2008$H + TB2008 + 1.5*(Batting2008$BB + Batting2008$HBP) + Batting2008$SB) / (Batting2008$AB + Batting2008$BB + Batting2008$HBP + Batting2008$CS + (Batting2008$SB/3))
OPS_Plus_2008 <- ((OBP2008/(sum(OBP2008, na.rm = T)/length(OBP2008))) + (SLG2008/(sum(SLG2008, na.rm = T)/length(SLG2008))) -1)
A_2008 <- Batting2008$H + Batting2008$BB + Batting2008$HBP - Batting2008$CS - Batting2008$GIDP
B_2008 <- TB2008 + 0.24*(Batting2008$BB - Batting2008$IBB + Batting2008$HBP) + 0.62*Batting2008$SB + 0.5*(Batting2008$SH + Batting2008$SF) - 0.3*(Batting2008$SO)
C_2008 <- Batting2008$AB + Batting2008$BB + Batting2008$HBP + Batting2008$SH + Batting2008$SF
RC2008 <- ((2.4*C_2008 + A_2008)*(3*C_2008 + B_2008) / (9*C_2008)) - 0.9*C_2008

Advanced2008 <- data.frame(AVG2008, OBP2008, SLG2008, OPS2008, IsoP2008, IsoD2008,
                           SecA2008, EqA2008, OPS_Plus_2008, RC2008)
colnames(Advanced2008) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced2008 <- round(Advanced2008, 3)
Batting2008 <- cbind(Batting2008, Advanced2008)

# 2007
Batting2007 <- ds2[yearID=='2007',]
AVG2007 <- (Batting2007$H)/(Batting2007$AB)
OBP2007 <- (Batting2007$H + Batting2007$BB + Batting2007$HBP)/(Batting2007$AB + Batting2007$BB + Batting2007$HBP + Batting2007$SF)
X1B <- Batting2007$H - (Batting2007$X2B + Batting2007$X3B + Batting2007$HR)
TB2007 <- 1*X1B + 2*Batting2007$X2B + 3*Batting2007$X3B + 4*Batting2007$HR
SLG2007 <- TB2007/Batting2007$AB
OPS2007 <- OBP2007 + SLG2007
IsoP2007 <- SLG2007 - AVG2007
IsoD2007 <- OBP2007 - AVG2007
SecA2007 <- (Batting2007$BB + (TB2007 - Batting2007$H) + (Batting2007$SB - Batting2007$CS)) / Batting2007$AB
EqA2007 <- (Batting2007$H + TB2007 + 1.5*(Batting2007$BB + Batting2007$HBP) + Batting2007$SB) / (Batting2007$AB + Batting2007$BB + Batting2007$HBP + Batting2007$CS + (Batting2007$SB/3))
OPS_Plus_2007 <- ((OBP2007/(sum(OBP2007, na.rm = T)/length(OBP2007))) + (SLG2007/(sum(SLG2007, na.rm = T)/length(SLG2007))) -1)
A_2007 <- Batting2007$H + Batting2007$BB + Batting2007$HBP - Batting2007$CS - Batting2007$GIDP
B_2007 <- TB2007 + 0.24*(Batting2007$BB - Batting2007$IBB + Batting2007$HBP) + 0.62*Batting2007$SB + 0.5*(Batting2007$SH + Batting2007$SF) - 0.3*(Batting2007$SO)
C_2007 <- Batting2007$AB + Batting2007$BB + Batting2007$HBP + Batting2007$SH + Batting2007$SF
RC2007 <- ((2.4*C_2007 + A_2007)*(3*C_2007 + B_2007) / (9*C_2007)) - 0.9*C_2007

Advanced2007 <- data.frame(AVG2007, OBP2007, SLG2007, OPS2007, IsoP2007, IsoD2007,
                           SecA2007, EqA2007, OPS_Plus_2007, RC2007)
colnames(Advanced2007) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced2007 <- round(Advanced2007, 3)
Batting2007 <- cbind(Batting2007, Advanced2007)

# 2006
Batting2006 <- ds2[yearID=='2006',]
AVG2006 <- (Batting2006$H)/(Batting2006$AB)
OBP2006 <- (Batting2006$H + Batting2006$BB + Batting2006$HBP)/(Batting2006$AB + Batting2006$BB + Batting2006$HBP + Batting2006$SF)
X1B <- Batting2006$H - (Batting2006$X2B + Batting2006$X3B + Batting2006$HR)
TB2006 <- 1*X1B + 2*Batting2006$X2B + 3*Batting2006$X3B + 4*Batting2006$HR
SLG2006 <- TB2006/Batting2006$AB
OPS2006 <- OBP2006 + SLG2006
IsoP2006 <- SLG2006 - AVG2006
IsoD2006 <- OBP2006 - AVG2006
SecA2006 <- (Batting2006$BB + (TB2006 - Batting2006$H) + (Batting2006$SB - Batting2006$CS)) / Batting2006$AB
EqA2006 <- (Batting2006$H + TB2006 + 1.5*(Batting2006$BB + Batting2006$HBP) + Batting2006$SB) / (Batting2006$AB + Batting2006$BB + Batting2006$HBP + Batting2006$CS + (Batting2006$SB/3))
OPS_Plus_2006 <- ((OBP2006/(sum(OBP2006, na.rm = T)/length(OBP2006))) + (SLG2006/(sum(SLG2006, na.rm = T)/length(SLG2006))) -1)
A_2006 <- Batting2006$H + Batting2006$BB + Batting2006$HBP - Batting2006$CS - Batting2006$GIDP
B_2006 <- TB2006 + 0.24*(Batting2006$BB - Batting2006$IBB + Batting2006$HBP) + 0.62*Batting2006$SB + 0.5*(Batting2006$SH + Batting2006$SF) - 0.3*(Batting2006$SO)
C_2006 <- Batting2006$AB + Batting2006$BB + Batting2006$HBP + Batting2006$SH + Batting2006$SF
RC2006 <- ((2.4*C_2006 + A_2006)*(3*C_2006 + B_2006)/(9*C_2006)) - 0.9*C_2006

Advanced2006 <- data.frame(AVG2006, OBP2006, SLG2006, OPS2006, IsoP2006, IsoD2006,
                           SecA2006, EqA2006, OPS_Plus_2006, RC2006)
colnames(Advanced2006) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced2006 <- round(Advanced2006, 3)
Batting2006 <- cbind(Batting2006, Advanced2006)

# 2005
Batting2005 <- ds2[yearID=='2005',]
AVG2005 <- (Batting2005$H)/(Batting2005$AB)
OBP2005 <- (Batting2005$H + Batting2005$BB + Batting2005$HBP)/(Batting2005$AB + Batting2005$BB + Batting2005$HBP + Batting2005$SF)
X1B <- Batting2005$H - (Batting2005$X2B + Batting2005$X3B + Batting2005$HR)
TB2005 <- 1*X1B + 2*Batting2005$X2B + 3*Batting2005$X3B + 4*Batting2005$HR
SLG2005 <- TB2005/Batting2005$AB
OPS2005 <- OBP2005 + SLG2005
IsoP2005 <- SLG2005 - AVG2005
IsoD2005 <- OBP2005 - AVG2005
SecA2005 <- (Batting2005$BB + (TB2005 - Batting2005$H) + (Batting2005$SB - Batting2005$CS)) / Batting2005$AB
EqA2005 <- (Batting2005$H + TB2005 + 1.5*(Batting2005$BB + Batting2005$HBP) + Batting2005$SB) / (Batting2005$AB + Batting2005$BB + Batting2005$HBP + Batting2005$CS + (Batting2005$SB/3))
OPS_Plus_2005 <- ((OBP2005/(sum(OBP2005, na.rm = T)/length(OBP2005))) + (SLG2005/(sum(SLG2005, na.rm = T)/length(SLG2005))) -1)
A_2005 <- Batting2005$H + Batting2005$BB + Batting2005$HBP - Batting2005$CS - Batting2005$GIDP
B_2005 <- TB2005 + 0.24*(Batting2005$BB - Batting2005$IBB + Batting2005$HBP) + 0.62*Batting2005$SB + 0.5*(Batting2005$SH + Batting2005$SF) - 0.3*(Batting2005$SO)
C_2005 <- Batting2005$AB + Batting2005$BB + Batting2005$HBP + Batting2005$SH + Batting2005$SF
RC2005 <- ((2.4*C_2005 + A_2005)*(3*C_2005 + B_2005) / (9*C_2005)) - 0.9*C_2005

Advanced2005 <- data.frame(AVG2005, OBP2005, SLG2005, OPS2005, IsoP2005, IsoD2005,
                           SecA2005, EqA2005, OPS_Plus_2005, RC2005)
colnames(Advanced2005) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced2005 <- round(Advanced2005, 3)
Batting2005 <- cbind(Batting2005, Advanced2005)

# 2004
Batting2004 <- ds2[yearID=='2004',]
AVG2004 <- (Batting2004$H)/(Batting2004$AB)
OBP2004 <- (Batting2004$H + Batting2004$BB + Batting2004$HBP)/(Batting2004$AB + Batting2004$BB + Batting2004$HBP + Batting2004$SF)
X1B <- Batting2004$H - (Batting2004$X2B + Batting2004$X3B + Batting2004$HR)
TB2004 <- 1*X1B + 2*Batting2004$X2B + 3*Batting2004$X3B + 4*Batting2004$HR
SLG2004 <- TB2004/Batting2004$AB
OPS2004 <- OBP2004 + SLG2004
IsoP2004 <- SLG2004 - AVG2004
IsoD2004 <- OBP2004 - AVG2004
SecA2004 <- (Batting2004$BB + (TB2004 - Batting2004$H) + (Batting2004$SB - Batting2004$CS)) / Batting2004$AB
EqA2004 <- (Batting2004$H + TB2004 + 1.5*(Batting2004$BB + Batting2004$HBP) + Batting2004$SB) / (Batting2004$AB + Batting2004$BB + Batting2004$HBP + Batting2004$CS + (Batting2004$SB/3))
OPS_Plus_2004 <- ((OBP2004/(sum(OBP2004, na.rm = T)/length(OBP2004))) + (SLG2004/(sum(SLG2004, na.rm = T)/length(SLG2004))) -1)
A_2004 <- Batting2004$H + Batting2004$BB + Batting2004$HBP - Batting2004$CS - Batting2004$GIDP
B_2004 <- TB2004 + 0.24*(Batting2004$BB - Batting2004$IBB + Batting2004$HBP) + 0.62*Batting2004$SB + 0.5*(Batting2004$SH + Batting2004$SF) - 0.3*(Batting2004$SO)
C_2004 <- Batting2004$AB + Batting2004$BB + Batting2004$HBP + Batting2004$SH + Batting2004$SF
RC2004 <- ((2.4*C_2004 + A_2004)*(3*C_2004 + B_2004) / (9*C_2004)) - 0.9*C_2004

Advanced2004<- data.frame(AVG2004, OBP2004, SLG2004, OPS2004, IsoP2004, IsoD2004,
                          SecA2004, EqA2004, OPS_Plus_2004, RC2004)
colnames(Advanced2004) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced2004 <- round(Advanced2004, 3)
Batting2004 <- cbind(Batting2004, Advanced2004)

# 2003
Batting2003 <- ds2[yearID=='2003',]
AVG2003 <- (Batting2003$H)/(Batting2003$AB)
OBP2003 <- (Batting2003$H + Batting2003$BB + Batting2003$HBP)/(Batting2003$AB + Batting2003$BB + Batting2003$HBP + Batting2003$SF)
X1B <- Batting2003$H - (Batting2003$X2B + Batting2003$X3B + Batting2003$HR)
TB2003 <- 1*X1B + 2*Batting2003$X2B + 3*Batting2003$X3B + 4*Batting2003$HR
SLG2003 <- TB2003/Batting2003$AB
OPS2003 <- OBP2003 + SLG2003
IsoP2003 <- SLG2003 - AVG2003
IsoD2003 <- OBP2003 - AVG2003
SecA2003 <- (Batting2003$BB + (TB2003 - Batting2003$H) + (Batting2003$SB - Batting2003$CS)) / Batting2003$AB
EqA2003 <- (Batting2003$H + TB2003 + 1.5*(Batting2003$BB + Batting2003$HBP) + Batting2003$SB) / (Batting2003$AB + Batting2003$BB + Batting2003$HBP + Batting2003$CS + (Batting2003$SB/3))
OPS_Plus_2003 <- ((OBP2003/(sum(OBP2003, na.rm = T)/length(OBP2003))) + (SLG2003/(sum(SLG2003, na.rm = T)/length(SLG2003))) -1)
A_2003 <- Batting2003$H + Batting2003$BB + Batting2003$HBP - Batting2003$CS - Batting2003$GIDP
B_2003 <- TB2003 + 0.24*(Batting2003$BB - Batting2003$IBB + Batting2003$HBP) + 0.62*Batting2003$SB + 0.5*(Batting2003$SH + Batting2003$SF) - 0.3*(Batting2003$SO)
C_2003<- Batting2003$AB + Batting2003$BB + Batting2003$HBP + Batting2003$SH + Batting2003$SF
RC2003 <- ((2.4*C_2003 + A_2003)*(3*C_2003 + B_2003) / (9*C_2003)) - 0.9*C_2003

Advanced2003<- data.frame(AVG2003, OBP2003, SLG2003, OPS2003, IsoP2003, IsoD2003,
                          SecA2003, EqA2003, OPS_Plus_2003, RC2003)
colnames(Advanced2003) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced2003 <- round(Advanced2003, 3)
Batting2003 <- cbind(Batting2003, Advanced2003)

# 2002
Batting2002 <- ds2[yearID=='2002',]
AVG2002 <- (Batting2002$H)/(Batting2002$AB)
OBP2002 <- (Batting2002$H + Batting2002$BB + Batting2002$HBP)/(Batting2002$AB + Batting2002$BB + Batting2002$HBP + Batting2002$SF)
X1B <- Batting2002$H - (Batting2002$X2B + Batting2002$X3B + Batting2002$HR)
TB2002 <- 1*X1B + 2*Batting2002$X2B + 3*Batting2002$X3B + 4*Batting2002$HR
SLG2002 <- TB2002/Batting2002$AB
OPS2002 <- OBP2002 + SLG2002
IsoP2002 <- SLG2002 - AVG2002
IsoD2002 <- OBP2002 - AVG2002
SecA2002 <- (Batting2002$BB + (TB2002 - Batting2002$H) + (Batting2002$SB - Batting2002$CS)) / Batting2002$AB
EqA2002 <- (Batting2002$H + TB2002 + 1.5*(Batting2002$BB + Batting2002$HBP) + Batting2002$SB) / (Batting2002$AB + Batting2002$BB + Batting2002$HBP + Batting2002$CS + (Batting2002$SB/3))
OPS_Plus_2002 <- ((OBP2002/(sum(OBP2002, na.rm = T)/length(OBP2002))) + (SLG2002/(sum(SLG2002, na.rm = T)/length(SLG2002))) -1)
A_2002 <- Batting2002$H + Batting2002$BB + Batting2002$HBP - Batting2002$CS - Batting2002$GIDP
B_2002 <- TB2002 + 0.24*(Batting2002$BB - Batting2002$IBB + Batting2002$HBP) + 0.62*Batting2002$SB + 0.5*(Batting2002$SH + Batting2002$SF) - 0.3*(Batting2002$SO)
C_2002 <- Batting2002$AB + Batting2002$BB + Batting2002$HBP + Batting2002$SH + Batting2002$SF
RC2002 <- ((2.4*C_2002 + A_2002)*(3*C_2002 + B_2002) / (9*C_2002)) - 0.9*C_2002

Advanced2002<- data.frame(AVG2002, OBP2002, SLG2002, OPS2002, IsoP2002, IsoD2002,
                          SecA2002, EqA2002, OPS_Plus_2002, RC2002)
colnames(Advanced2002) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced2002 <- round(Advanced2002, 3)
Batting2002 <- cbind(Batting2002, Advanced2002)

# 2001
Batting2001 <- ds2[yearID=='2001',]
AVG2001 <- (Batting2001$H)/(Batting2001$AB)
OBP2001 <- (Batting2001$H + Batting2001$BB + Batting2001$HBP)/(Batting2001$AB + Batting2001$BB + Batting2001$HBP + Batting2001$SF)
X1B <- Batting2001$H - (Batting2001$X2B + Batting2001$X3B + Batting2001$HR)
TB2001 <- 1*X1B + 2*Batting2001$X2B + 3*Batting2001$X3B + 4*Batting2001$HR
SLG2001 <- TB2001/Batting2001$AB
OPS2001 <- OBP2001 + SLG2001
IsoP2001 <- SLG2001 - AVG2001
IsoD2001 <- OBP2001 - AVG2001
SecA2001 <- (Batting2001$BB + (TB2001 - Batting2001$H) + (Batting2001$SB - Batting2001$CS)) / Batting2001$AB
EqA2001 <- (Batting2001$H + TB2001 + 1.5*(Batting2001$BB + Batting2001$HBP) + Batting2001$SB) / (Batting2001$AB + Batting2001$BB + Batting2001$HBP + Batting2001$CS + (Batting2001$SB/3))
OPS_Plus_2001 <- ((OBP2001/(sum(OBP2001, na.rm = T)/length(OBP2001))) + (SLG2001/(sum(SLG2001, na.rm = T)/length(SLG2001))) -1)
A_2001 <- Batting2001$H + Batting2001$BB + Batting2001$HBP - Batting2001$CS - Batting2001$GIDP
B_2001 <- TB2001 + 0.24*(Batting2001$BB - Batting2001$IBB + Batting2001$HBP) + 0.62*Batting2001$SB + 0.5*(Batting2001$SH + Batting2001$SF) - 0.3*(Batting2001$SO)
C_2001 <- Batting2001$AB + Batting2001$BB + Batting2001$HBP + Batting2001$SH + Batting2001$SF
RC2001 = A_2001 * B_2001 / C_2001

Advanced2001<- data.frame(AVG2001, OBP2001, SLG2001, OPS2001, IsoP2001, IsoD2001,
                          SecA2001, EqA2001, OPS_Plus_2001, RC2001)
colnames(Advanced2001) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced2001 <- round(Advanced2001, 3)
Batting2001 <- cbind(Batting2001, Advanced2001)

# 2000
Batting2000 <- ds2[yearID=='2000',]
AVG2000 <- (Batting2000$H)/(Batting2000$AB)
OBP2000 <- (Batting2000$H + Batting2000$BB + Batting2000$HBP)/(Batting2000$AB + Batting2000$BB + Batting2000$HBP + Batting2000$SF)
X1B <- Batting2000$H - (Batting2000$X2B + Batting2000$X3B + Batting2000$HR)
TB2000 <- 1*X1B + 2*Batting2000$X2B + 3*Batting2000$X3B + 4*Batting2000$HR
SLG2000 <- TB2000/Batting2000$AB
OPS2000 <- OBP2000 + SLG2000
IsoP2000 <- SLG2000- AVG2000
IsoD2000 <- OBP2000 - AVG2000
SecA2000 <- (Batting2000$BB + (TB2000 - Batting2000$H) + (Batting2000$SB - Batting2000$CS)) / Batting2000$AB
EqA2000 <- (Batting2000$H + TB2000 + 1.5*(Batting2000$BB + Batting2000$HBP) + Batting2000$SB) / (Batting2000$AB + Batting2000$BB + Batting2000$HBP + Batting2000$CS + (Batting2000$SB/3))
OPS_Plus_2000 <- ((OBP2000/(sum(OBP2000, na.rm = T)/length(OBP2000))) + (SLG2000/(sum(SLG2000, na.rm = T)/length(SLG2000))) -1)
A_2000 <- Batting2000$H + Batting2000$BB + Batting2000$HBP - Batting2000$CS - Batting2000$GIDP
B_2000 <- TB2000 + 0.24*(Batting2000$BB - Batting2000$IBB + Batting2000$HBP) + 0.62*Batting2000$SB + 0.5*(Batting2000$SH + Batting2000$SF) - 0.3*(Batting2000$SO)
C_2000 <- Batting2000$AB + Batting2000$BB + Batting2000$HBP + Batting2000$SH + Batting2000$SF
RC2000 = A_2000 * B_2000 / C_2000

Advanced2000<- data.frame(AVG2000, OBP2000, SLG2000, OPS2000, IsoP2000, IsoD2000,
                          SecA2000, EqA2000, OPS_Plus_2000, RC2000)
colnames(Advanced2000) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced2000 <- round(Advanced2000, 3)
Batting2000 <- cbind(Batting2000, Advanced2000)

# 1999
Batting1999 <- ds2[yearID=='1999',]
AVG1999 <- (Batting1999$H)/(Batting1999$AB)
OBP1999 <- (Batting1999$H + Batting1999$BB + Batting1999$HBP)/(Batting1999$AB + Batting1999$BB + Batting1999$HBP + Batting1999$SF)
X1B <- Batting1999$H - (Batting1999$X2B + Batting1999$X3B + Batting1999$HR)
TB1999 <- 1*X1B + 2*Batting1999$X2B + 3*Batting1999$X3B + 4*Batting1999$HR
SLG1999 <- TB1999/Batting1999$AB
OPS1999 <- OBP1999 + SLG1999
IsoP1999 <- SLG1999 - AVG1999
IsoD1999 <- OBP1999 - AVG1999
SecA1999 <- (Batting1999$BB + (TB1999 - Batting1999$H) + (Batting1999$SB - Batting1999$CS)) / Batting1999$AB
EqA1999 <- (Batting1999$H + TB1999 + 1.5*(Batting1999$BB + Batting1999$HBP) + Batting1999$SB) / (Batting1999$AB + Batting1999$BB + Batting1999$HBP + Batting1999$CS + (Batting1999$SB/3))
OPS_Plus_1999 <- ((OBP1999/(sum(OBP1999, na.rm = T)/length(OBP1999))) + (SLG1999/(sum(SLG1999, na.rm = T)/length(SLG1999))) -1)
A_1999 <- Batting1999$H + Batting1999$BB + Batting1999$HBP - Batting1999$CS - Batting1999$GIDP
B_1999 <- TB1999 + 0.24*(Batting1999$BB - Batting1999$IBB + Batting1999$HBP) + 0.62*Batting1999$SB + 0.5*(Batting1999$SH + Batting1999$SF) - 0.3*(Batting1999$SO)
C_1999 <- Batting1999$AB + Batting1999$BB + Batting1999$HBP + Batting1999$SH + Batting1999$SF
RC1999 = A_1999 * B_1999 / C_1999

Advanced1999<- data.frame(AVG1999, OBP1999, SLG1999, OPS1999, IsoP1999, IsoD1999,
                          SecA1999, EqA1999, OPS_Plus_1999, RC1999)
colnames(Advanced1999) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced1999 <- round(Advanced1999, 3)
Batting1999 <- cbind(Batting1999, Advanced1999)

# A = H + BB + HBP – CS – GIDP						　
# B = TB + 0.24x(BB – IBB + HBP) + 0.62xSB + 0.5x(SH + SF) — 0.3xSO			　
# C = AB + BB + HBP + SH + SF						　
# RC(1) = A x B / C 		###2002年前					　
# RC(2) = [(2.4xC + A) x (3xC + B) / 9xC] — 0.9Xc	###2002後				　


# 1998
Batting1998 <- ds2[yearID=='1998',]
AVG1998 <- (Batting1998$H)/(Batting1998$AB)
OBP1998 <- (Batting1998$H + Batting1998$BB + Batting1998$HBP)/(Batting1998$AB + Batting1998$BB + Batting1998$HBP + Batting1998$SF)
X1B <- Batting1998$H - (Batting1998$X2B + Batting1998$X3B + Batting1998$HR)
TB1998 <- 1*X1B + 2*Batting1998$X2B + 3*Batting1998$X3B + 4*Batting1998$HR
SLG1998 <- TB1998/Batting1998$AB
OPS1998 <- OBP1998 + SLG1998
IsoP1998 <- SLG1998 - AVG1998
IsoD1998 <- OBP1998 - AVG1998
SecA1998 <- (Batting1998$BB + (TB1998 - Batting1998$H) + (Batting1998$SB - Batting1998$CS)) / Batting1998$AB
EqA1998 <- (Batting1998$H + TB1998 + 1.5*(Batting1998$BB + Batting1998$HBP) + Batting1998$SB) / (Batting1998$AB + Batting1998$BB + Batting1998$HBP + Batting1998$CS + (Batting1998$SB/3))
OPS_Plus_1998 <- ((OBP1998/(sum(OBP1998, na.rm = T)/length(OBP1998))) + (SLG1998/(sum(SLG1998, na.rm = T)/length(SLG1998))) -1)
A_1998 <- Batting1998$H + Batting1998$BB + Batting1998$HBP - Batting1998$CS - Batting1998$GIDP
B_1998 <- TB1998 + 0.24*(Batting1998$BB - Batting1998$IBB + Batting1998$HBP) + 0.62*Batting1998$SB + 0.5*(Batting1998$SH + Batting1998$SF) - 0.3*(Batting1998$SO)
C_1998 <- Batting1998$AB + Batting1998$BB + Batting1998$HBP + Batting1998$SH + Batting1998$SF
RC1998 = A_1998 * B_1998 / C_1998

Advanced1998<- data.frame(AVG1998, OBP1998, SLG1998, OPS1998, IsoP1998, IsoD1998,
                          SecA1998, EqA1998, OPS_Plus_1998, RC1998)
colnames(Advanced1998) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced1998 <- round(Advanced1998, 3)
Batting1998 <- cbind(Batting1998, Advanced1998)


# 1997
Batting1997 <- ds2[yearID=='1997',]
AVG1997 <- (Batting1997$H)/(Batting1997$AB)
OBP1997 <- (Batting1997$H + Batting1997$BB + Batting1997$HBP)/(Batting1997$AB + Batting1997$BB + Batting1997$HBP + Batting1997$SF)
X1B <- Batting1997$H - (Batting1997$X2B + Batting1997$X3B + Batting1997$HR)
TB1997 <- 1*X1B + 2*Batting1997$X2B + 3*Batting1997$X3B + 4*Batting1997$HR
SLG1997 <- TB1997/Batting1997$AB
OPS1997 <- OBP1997 + SLG1997
IsoP1997 <- SLG1997 - AVG1997
IsoD1997 <- OBP1997 - AVG1997
SecA1997 <- (Batting1997$BB + (TB1997 - Batting1997$H) + (Batting1997$SB - Batting1997$CS)) / Batting1997$AB
EqA1997 <- (Batting1997$H + TB1997 + 1.5*(Batting1997$BB + Batting1997$HBP) + Batting1997$SB) / (Batting1997$AB + Batting1997$BB + Batting1997$HBP + Batting1997$CS + (Batting1997$SB/3))
OPS_Plus_1997 <- ((OBP1997/(sum(OBP1997, na.rm = T)/length(OBP1997))) + (SLG1997/(sum(SLG1997, na.rm = T)/length(SLG1997))) -1)
A_1997 <- Batting1997$H + Batting1997$BB + Batting1997$HBP - Batting1997$CS - Batting1997$GIDP
B_1997 <- TB1997 + 0.24*(Batting1997$BB - Batting1997$IBB + Batting1997$HBP) + 0.62*Batting1997$SB + 0.5*(Batting1997$SH + Batting1997$SF) - 0.3*(Batting1997$SO)
C_1997 <- Batting1997$AB + Batting1997$BB + Batting1997$HBP + Batting1997$SH + Batting1997$SF
RC1997 = A_1997 * B_1997 / C_1997

Advanced1997<- data.frame(AVG1997, OBP1997, SLG1997, OPS1997, IsoP1997, IsoD1997,
                          SecA1997, EqA1997, OPS_Plus_1997, RC1997)
colnames(Advanced1997) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced1997 <- round(Advanced1997, 3)
Batting1997 <- cbind(Batting1997, Advanced1997)


# 1996
Batting1996 <- ds2[yearID=='1996',]
AVG1996 <- (Batting1996$H)/(Batting1996$AB)
OBP1996 <- (Batting1996$H + Batting1996$BB + Batting1996$HBP)/(Batting1996$AB + Batting1996$BB + Batting1996$HBP + Batting1996$SF)
X1B <- Batting1996$H - (Batting1996$X2B + Batting1996$X3B + Batting1996$HR)
TB1996 <- 1*X1B + 2*Batting1996$X2B + 3*Batting1996$X3B + 4*Batting1996$HR
SLG1996 <- TB1996/Batting1996$AB
OPS1996 <- OBP1996 + SLG1996
IsoP1996 <- SLG1996 - AVG1996
IsoD1996 <- OBP1996 - AVG1996
SecA1996 <- (Batting1996$BB + (TB1996 - Batting1996$H) + (Batting1996$SB - Batting1996$CS)) / Batting1996$AB
EqA1996 <- (Batting1996$H + TB1996 + 1.5*(Batting1996$BB + Batting1996$HBP) + Batting1996$SB) / (Batting1996$AB + Batting1996$BB + Batting1996$HBP + Batting1996$CS + (Batting1996$SB/3))
OPS_Plus_1996 <- ((OBP1996/(sum(OBP1996, na.rm = T)/length(OBP1996))) + (SLG1996/(sum(SLG1996, na.rm = T)/length(SLG1996))) -1)
A_1996 <- Batting1996$H + Batting1996$BB + Batting1996$HBP - Batting1996$CS - Batting1996$GIDP
B_1996 <- TB1996 + 0.24*(Batting1996$BB - Batting1996$IBB + Batting1996$HBP) + 0.62*Batting1996$SB + 0.5*(Batting1996$SH + Batting1996$SF) - 0.3*(Batting1996$SO)
C_1996 <- Batting1996$AB + Batting1996$BB + Batting1996$HBP + Batting1996$SH + Batting1996$SF
RC1996 = A_1996 * B_1996 / C_1996

Advanced1996<- data.frame(AVG1996, OBP1996, SLG1996, OPS1996, IsoP1996, IsoD1996,
                          SecA1996, EqA1996, OPS_Plus_1996, RC1996)
colnames(Advanced1996) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced1996 <- round(Advanced1996, 3)
Batting1996 <- cbind(Batting1996, Advanced1996)

# 1995
Batting1995 <- ds2[yearID=='1995',]
AVG1995 <- (Batting1995$H)/(Batting1995$AB)
OBP1995 <- (Batting1995$H + Batting1995$BB + Batting1995$HBP)/(Batting1995$AB + Batting1995$BB + Batting1995$HBP + Batting1995$SF)
X1B <- Batting1995$H - (Batting1995$X2B + Batting1995$X3B + Batting1995$HR)
TB1995 <- 1*X1B + 2*Batting1995$X2B + 3*Batting1995$X3B + 4*Batting1995$HR
SLG1995 <- TB1995/Batting1995$AB
OPS1995 <- OBP1995 + SLG1995
IsoP1995 <- SLG1995 - AVG1995
IsoD1995 <- OBP1995 - AVG1995
SecA1995 <- (Batting1995$BB + (TB1995 - Batting1995$H) + (Batting1995$SB - Batting1995$CS)) / Batting1995$AB
EqA1995 <- (Batting1995$H + TB1995 + 1.5*(Batting1995$BB + Batting1995$HBP) + Batting1995$SB) / (Batting1995$AB + Batting1995$BB + Batting1995$HBP + Batting1995$CS + (Batting1995$SB/3))
OPS_Plus_1995 <- ((OBP1995/(sum(OBP1995, na.rm = T)/length(OBP1995))) + (SLG1995/(sum(SLG1995, na.rm = T)/length(SLG1995))) -1)
A_1995 <- Batting1995$H + Batting1995$BB + Batting1995$HBP - Batting1995$CS - Batting1995$GIDP
B_1995 <- TB1995 + 0.24*(Batting1995$BB - Batting1995$IBB + Batting1995$HBP) + 0.62*Batting1995$SB + 0.5*(Batting1995$SH + Batting1995$SF) - 0.3*(Batting1995$SO)
C_1995 <- Batting1995$AB + Batting1995$BB + Batting1995$HBP + Batting1995$SH + Batting1995$SF
RC1995 = A_1995 * B_1995 / C_1995

Advanced1995<- data.frame(AVG1995, OBP1995, SLG1995, OPS1995, IsoP1995, IsoD1995,
                          SecA1995, EqA1995, OPS_Plus_1995, RC1995)
colnames(Advanced1995) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced1995 <- round(Advanced1995, 3)
Batting1995 <- cbind(Batting1995, Advanced1995)

# 1994
Batting1994 <- ds2[yearID=='1994',]
AVG1994 <- (Batting1994$H)/(Batting1994$AB)
OBP1994 <- (Batting1994$H + Batting1994$BB + Batting1994$HBP)/(Batting1994$AB + Batting1994$BB + Batting1994$HBP + Batting1994$SF)
X1B <- Batting1994$H - (Batting1994$X2B + Batting1994$X3B + Batting1994$HR)
TB1994 <- 1*X1B + 2*Batting1994$X2B + 3*Batting1994$X3B + 4*Batting1994$HR
SLG1994 <- TB1994/Batting1994$AB
OPS1994 <- OBP1994 + SLG1994
IsoP1994 <- SLG1994 - AVG1994
IsoD1994 <- OBP1994 - AVG1994
SecA1994 <- (Batting1994$BB + (TB1994 - Batting1994$H) + (Batting1994$SB - Batting1994$CS)) / Batting1994$AB
EqA1994 <- (Batting1994$H + TB1994 + 1.5*(Batting1994$BB + Batting1994$HBP) + Batting1994$SB) / (Batting1994$AB + Batting1994$BB + Batting1994$HBP + Batting1994$CS + (Batting1994$SB/3))
OPS_Plus_1994 <- ((OBP1994/(sum(OBP1994, na.rm = T)/length(OBP1994))) + (SLG1994/(sum(SLG1994, na.rm = T)/length(SLG1994))) -1)
A_1994 <- Batting1994$H + Batting1994$BB + Batting1994$HBP - Batting1994$CS - Batting1994$GIDP
B_1994 <- TB1994 + 0.24*(Batting1994$BB - Batting1994$IBB + Batting1994$HBP) + 0.62*Batting1994$SB + 0.5*(Batting1994$SH + Batting1994$SF) - 0.3*(Batting1994$SO)
C_1994 <- Batting1994$AB + Batting1994$BB + Batting1994$HBP + Batting1994$SH + Batting1994$SF
RC1994 = A_1994 * B_1994 / C_1994

Advanced1994<- data.frame(AVG1994, OBP1994, SLG1994, OPS1994, IsoP1994, IsoD1994,
                          SecA1994, EqA1994, OPS_Plus_1994, RC1994)
colnames(Advanced1994) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced1994 <- round(Advanced1994, 3)
Batting1994 <- cbind(Batting1994, Advanced1994)


# 1993
Batting1993 <- ds2[yearID=='1993',]
AVG1993 <- (Batting1993$H)/(Batting1993$AB)
OBP1993 <- (Batting1993$H + Batting1993$BB + Batting1993$HBP)/(Batting1993$AB + Batting1993$BB + Batting1993$HBP + Batting1993$SF)
X1B <- Batting1993$H - (Batting1993$X2B + Batting1993$X3B + Batting1993$HR)
TB1993 <- 1*X1B + 2*Batting1993$X2B + 3*Batting1993$X3B + 4*Batting1993$HR
SLG1993 <- TB1993/Batting1993$AB
OPS1993 <- OBP1993 + SLG1993
IsoP1993 <- SLG1993 - AVG1993
IsoD1993 <- OBP1993 - AVG1993
SecA1993 <- (Batting1993$BB + (TB1993 - Batting1993$H) + (Batting1993$SB - Batting1993$CS)) / Batting1993$AB
EqA1993 <- (Batting1993$H + TB1993 + 1.5*(Batting1993$BB + Batting1993$HBP) + Batting1993$SB) / (Batting1993$AB + Batting1993$BB + Batting1993$HBP + Batting1993$CS + (Batting1993$SB/3))
OPS_Plus_1993 <- ((OBP1993/(sum(OBP1993, na.rm = T)/length(OBP1993))) + (SLG1993/(sum(SLG1993, na.rm = T)/length(SLG1993))) -1)
A_1993 <- Batting1993$H + Batting1993$BB + Batting1993$HBP - Batting1993$CS - Batting1993$GIDP
B_1993 <- TB1993 + 0.24*(Batting1993$BB - Batting1993$IBB + Batting1993$HBP) + 0.62*Batting1993$SB + 0.5*(Batting1993$SH + Batting1993$SF) - 0.3*(Batting1993$SO)
C_1993 <- Batting1993$AB + Batting1993$BB + Batting1993$HBP + Batting1993$SH + Batting1993$SF
RC1993 = A_1993 * B_1993 / C_1993

Advanced1993<- data.frame(AVG1993, OBP1993, SLG1993, OPS1993, IsoP1993, IsoD1993,
                          SecA1993, EqA1993, OPS_Plus_1993, RC1993)
colnames(Advanced1993) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced1993 <- round(Advanced1993, 3)
Batting1993 <- cbind(Batting1993, Advanced1993)

# 1992
Batting1992 <- ds2[yearID=='1992',]
AVG1992 <- (Batting1992$H)/(Batting1992$AB)
OBP1992 <- (Batting1992$H + Batting1992$BB + Batting1992$HBP)/(Batting1992$AB + Batting1992$BB + Batting1992$HBP + Batting1992$SF)
X1B <- Batting1992$H - (Batting1992$X2B + Batting1992$X3B + Batting1992$HR)
TB1992 <- 1*X1B + 2*Batting1992$X2B + 3*Batting1992$X3B + 4*Batting1992$HR
SLG1992 <- TB1992/Batting1992$AB
OPS1992 <- OBP1992 + SLG1992
IsoP1992 <- SLG1992 - AVG1992
IsoD1992 <- OBP1992 - AVG1992
SecA1992 <- (Batting1992$BB + (TB1992 - Batting1992$H) + (Batting1992$SB - Batting1992$CS)) / Batting1992$AB
EqA1992 <- (Batting1992$H + TB1992 + 1.5*(Batting1992$BB + Batting1992$HBP) + Batting1992$SB) / (Batting1992$AB + Batting1992$BB + Batting1992$HBP + Batting1992$CS + (Batting1992$SB/3))
OPS_Plus_1992 <- ((OBP1992/(sum(OBP1992, na.rm = T)/length(OBP1992))) + (SLG1992/(sum(SLG1992, na.rm = T)/length(SLG1992))) -1)
A_1992 <- Batting1992$H + Batting1992$BB + Batting1992$HBP - Batting1992$CS - Batting1992$GIDP
B_1992 <- TB1992 + 0.24*(Batting1992$BB - Batting1992$IBB + Batting1992$HBP) + 0.62*Batting1992$SB + 0.5*(Batting1992$SH + Batting1992$SF) - 0.3*(Batting1992$SO)
C_1992 <- Batting1992$AB + Batting1992$BB + Batting1992$HBP + Batting1992$SH + Batting1992$SF
RC1992 = A_1992 * B_1992 / C_1992

Advanced1992<- data.frame(AVG1992, OBP1992, SLG1992, OPS1992, IsoP1992, IsoD1992,
                          SecA1992, EqA1992, OPS_Plus_1992, RC1992)
colnames(Advanced1992) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced1992 <- round(Advanced1992, 3)
Batting1992 <- cbind(Batting1992, Advanced1992)

# 1991
Batting1991 <- ds2[yearID=='1991',]
AVG1991 <- (Batting1991$H)/(Batting1991$AB)
OBP1991 <- (Batting1991$H + Batting1991$BB + Batting1991$HBP)/(Batting1991$AB + Batting1991$BB + Batting1991$HBP + Batting1991$SF)
X1B <- Batting1991$H - (Batting1991$X2B + Batting1991$X3B + Batting1991$HR)
TB1991 <- 1*X1B + 2*Batting1991$X2B + 3*Batting1991$X3B + 4*Batting1991$HR
SLG1991 <- TB1991/Batting1991$AB
OPS1991 <- OBP1991 + SLG1991
IsoP1991 <- SLG1991 - AVG1991
IsoD1991 <- OBP1991 - AVG1991
SecA1991 <- (Batting1991$BB + (TB1991 - Batting1991$H) + (Batting1991$SB - Batting1991$CS)) / Batting1991$AB
EqA1991 <- (Batting1991$H + TB1991 + 1.5*(Batting1991$BB + Batting1991$HBP) + Batting1991$SB) / (Batting1991$AB + Batting1991$BB + Batting1991$HBP + Batting1991$CS + (Batting1991$SB/3))
OPS_Plus_1991 <- ((OBP1991/(sum(OBP1991, na.rm = T)/length(OBP1991))) + (SLG1991/(sum(SLG1991, na.rm = T)/length(SLG1991))) -1)
A_1991 <- Batting1991$H + Batting1991$BB + Batting1991$HBP - Batting1991$CS - Batting1991$GIDP
B_1991 <- TB1991 + 0.24*(Batting1991$BB - Batting1991$IBB + Batting1991$HBP) + 0.62*Batting1991$SB + 0.5*(Batting1991$SH + Batting1991$SF) - 0.3*(Batting1991$SO)
C_1991 <- Batting1991$AB + Batting1991$BB + Batting1991$HBP + Batting1991$SH + Batting1991$SF
RC1991 = A_1991 * B_1991 / C_1991

Advanced1991<- data.frame(AVG1991, OBP1991, SLG1991, OPS1991, IsoP1991, IsoD1991,
                          SecA1991, EqA1991, OPS_Plus_1991, RC1991)
colnames(Advanced1991) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced1991 <- round(Advanced1991, 3)
Batting1991 <- cbind(Batting1991, Advanced1991)

# 1990
Batting1990 <- ds2[yearID=='1990',]
AVG1990 <- (Batting1990$H)/(Batting1990$AB)
OBP1990 <- (Batting1990$H + Batting1990$BB + Batting1990$HBP)/(Batting1990$AB + Batting1990$BB + Batting1990$HBP + Batting1990$SF)
X1B <- Batting1990$H - (Batting1990$X2B + Batting1990$X3B + Batting1990$HR)
TB1990 <- 1*X1B + 2*Batting1990$X2B + 3*Batting1990$X3B + 4*Batting1990$HR
SLG1990 <- TB1990/Batting1990$AB
OPS1990 <- OBP1990 + SLG1990
IsoP1990 <- SLG1990 - AVG1990
IsoD1990 <- OBP1990 - AVG1990
SecA1990 <- (Batting1990$BB + (TB1990 - Batting1990$H) + (Batting1990$SB - Batting1990$CS)) / Batting1990$AB
EqA1990 <- (Batting1990$H + TB1990 + 1.5*(Batting1990$BB + Batting1990$HBP) + Batting1990$SB) / (Batting1990$AB + Batting1990$BB + Batting1990$HBP + Batting1990$CS + (Batting1990$SB/3))
OPS_Plus_1990 <- ((OBP1990/(sum(OBP1990, na.rm = T)/length(OBP1990))) + (SLG1990/(sum(SLG1990, na.rm = T)/length(SLG1990))) -1)
A_1990 <- Batting1990$H + Batting1990$BB + Batting1990$HBP - Batting1990$CS - Batting1990$GIDP
B_1990 <- TB1990 + 0.24*(Batting1990$BB - Batting1990$IBB + Batting1990$HBP) + 0.62*Batting1990$SB + 0.5*(Batting1990$SH + Batting1990$SF) - 0.3*(Batting1990$SO)
C_1990 <- Batting1990$AB + Batting1990$BB + Batting1990$HBP + Batting1990$SH + Batting1990$SF
RC1990 = A_1990 * B_1990 / C_1990

Advanced1990<- data.frame(AVG1990, OBP1990, SLG1990, OPS1990, IsoP1990, IsoD1990,
                          SecA1990, EqA1990, OPS_Plus_1990, RC1990)
colnames(Advanced1990) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced1990 <- round(Advanced1990, 3)
Batting1990 <- cbind(Batting1990, Advanced1990)

# 1989
Batting1989 <- ds2[yearID=='1989',]
AVG1989 <- (Batting1989$H)/(Batting1989$AB)
OBP1989 <- (Batting1989$H + Batting1989$BB + Batting1989$HBP)/(Batting1989$AB + Batting1989$BB + Batting1989$HBP + Batting1989$SF)
X1B <- Batting1989$H - (Batting1989$X2B + Batting1989$X3B + Batting1989$HR)
TB1989 <- 1*X1B + 2*Batting1989$X2B + 3*Batting1989$X3B + 4*Batting1989$HR
SLG1989 <- TB1989/Batting1989$AB
OPS1989 <- OBP1989 + SLG1989
IsoP1989 <- SLG1989 - AVG1989
IsoD1989 <- OBP1989 - AVG1989
SecA1989 <- (Batting1989$BB + (TB1989 - Batting1989$H) + (Batting1989$SB - Batting1989$CS)) / Batting1989$AB
EqA1989 <- (Batting1989$H + TB1989 + 1.5*(Batting1989$BB + Batting1989$HBP) + Batting1989$SB) / (Batting1989$AB + Batting1989$BB + Batting1989$HBP + Batting1989$CS + (Batting1989$SB/3))
OPS_Plus_1989 <- ((OBP1989/(sum(OBP1989, na.rm = T)/length(OBP1989))) + (SLG1989/(sum(SLG1989, na.rm = T)/length(SLG1989))) -1)
A_1989 <- Batting1989$H + Batting1989$BB + Batting1989$HBP - Batting1989$CS - Batting1989$GIDP
B_1989 <- TB1989 + 0.24*(Batting1989$BB - Batting1989$IBB + Batting1989$HBP) + 0.62*Batting1989$SB + 0.5*(Batting1989$SH + Batting1989$SF) - 0.3*(Batting1989$SO)
C_1989 <- Batting1989$AB + Batting1989$BB + Batting1989$HBP + Batting1989$SH + Batting1989$SF
RC1989 = A_1989 * B_1989 / C_1989

Advanced1989<- data.frame(AVG1989, OBP1989, SLG1989, OPS1989, IsoP1989, IsoD1989,
                          SecA1989, EqA1989, OPS_Plus_1989, RC1989)
colnames(Advanced1989) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced1989 <- round(Advanced1989, 3)
Batting1989 <- cbind(Batting1989, Advanced1989)


# 1988
Batting1988 <- ds2[yearID=='1988',]
AVG1988 <- (Batting1988$H)/(Batting1988$AB)
OBP1988 <- (Batting1988$H + Batting1988$BB + Batting1988$HBP)/(Batting1988$AB + Batting1988$BB + Batting1988$HBP + Batting1988$SF)
X1B <- Batting1988$H - (Batting1988$X2B + Batting1988$X3B + Batting1988$HR)
TB1988 <- 1*X1B + 2*Batting1988$X2B + 3*Batting1988$X3B + 4*Batting1988$HR
SLG1988 <- TB1988/Batting1988$AB
OPS1988 <- OBP1988 + SLG1988
IsoP1988 <- SLG1988 - AVG1988
IsoD1988 <- OBP1988 - AVG1988
SecA1988 <- (Batting1988$BB + (TB1988 - Batting1988$H) + (Batting1988$SB - Batting1988$CS)) / Batting1988$AB
EqA1988 <- (Batting1988$H + TB1988 + 1.5*(Batting1988$BB + Batting1988$HBP) + Batting1988$SB) / (Batting1988$AB + Batting1988$BB + Batting1988$HBP + Batting1988$CS + (Batting1988$SB/3))
OPS_Plus_1988 <- ((OBP1988/(sum(OBP1988, na.rm = T)/length(OBP1988))) + (SLG1988/(sum(SLG1988, na.rm = T)/length(SLG1988))) -1)
A_1988 <- Batting1988$H + Batting1988$BB + Batting1988$HBP - Batting1988$CS - Batting1988$GIDP
B_1988 <- TB1988 + 0.24*(Batting1988$BB - Batting1988$IBB + Batting1988$HBP) + 0.62*Batting1988$SB + 0.5*(Batting1988$SH + Batting1988$SF) - 0.3*(Batting1988$SO)
C_1988 <- Batting1988$AB + Batting1988$BB + Batting1988$HBP + Batting1988$SH + Batting1988$SF
RC1988 = A_1988 * B_1988 / C_1988

Advanced1988<- data.frame(AVG1988, OBP1988, SLG1988, OPS1988, IsoP1988, IsoD1988,
                          SecA1988, EqA1988, OPS_Plus_1988, RC1988)
colnames(Advanced1988) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced1988 <- round(Advanced1988, 3)
Batting1988 <- cbind(Batting1988, Advanced1988)


# 1987
Batting1987 <- ds2[yearID=='1987',]
AVG1987 <- (Batting1987$H)/(Batting1987$AB)
OBP1987 <- (Batting1987$H + Batting1987$BB + Batting1987$HBP)/(Batting1987$AB + Batting1987$BB + Batting1987$HBP + Batting1987$SF)
X1B <- Batting1987$H - (Batting1987$X2B + Batting1987$X3B + Batting1987$HR)
TB1987 <- 1*X1B + 2*Batting1987$X2B + 3*Batting1987$X3B + 4*Batting1987$HR
SLG1987 <- TB1987/Batting1987$AB
OPS1987 <- OBP1987 + SLG1987
IsoP1987 <- SLG1987 - AVG1987
IsoD1987 <- OBP1987 - AVG1987
SecA1987 <- (Batting1987$BB + (TB1987 - Batting1987$H) + (Batting1987$SB - Batting1987$CS)) / Batting1987$AB
EqA1987 <- (Batting1987$H + TB1987 + 1.5*(Batting1987$BB + Batting1987$HBP) + Batting1987$SB) / (Batting1987$AB + Batting1987$BB + Batting1987$HBP + Batting1987$CS + (Batting1987$SB/3))
OPS_Plus_1987 <- ((OBP1987/(sum(OBP1987, na.rm = T)/length(OBP1987))) + (SLG1987/(sum(SLG1987, na.rm = T)/length(SLG1987))) -1)
A_1987 <- Batting1987$H + Batting1987$BB + Batting1987$HBP - Batting1987$CS - Batting1987$GIDP
B_1987 <- TB1987 + 0.24*(Batting1987$BB - Batting1987$IBB + Batting1987$HBP) + 0.62*Batting1987$SB + 0.5*(Batting1987$SH + Batting1987$SF) - 0.3*(Batting1987$SO)
C_1987 <- Batting1987$AB + Batting1987$BB + Batting1987$HBP + Batting1987$SH + Batting1987$SF
RC1987 = A_1987 * B_1987 / C_1987

Advanced1987<- data.frame(AVG1987, OBP1987, SLG1987, OPS1987, IsoP1987, IsoD1987,
                          SecA1987, EqA1987, OPS_Plus_1987, RC1987)
colnames(Advanced1987) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced1987 <- round(Advanced1987, 3)
Batting1987 <- cbind(Batting1987, Advanced1987)

# 1986
Batting1986 <- ds2[yearID=='1986',]
AVG1986 <- (Batting1986$H)/(Batting1986$AB)
OBP1986 <- (Batting1986$H + Batting1986$BB + Batting1986$HBP)/(Batting1986$AB + Batting1986$BB + Batting1986$HBP + Batting1986$SF)
X1B <- Batting1986$H - (Batting1986$X2B + Batting1986$X3B + Batting1986$HR)
TB1986 <- 1*X1B + 2*Batting1986$X2B + 3*Batting1986$X3B + 4*Batting1986$HR
SLG1986 <- TB1986/Batting1986$AB
OPS1986 <- OBP1986 + SLG1986
IsoP1986 <- SLG1986 - AVG1986
IsoD1986 <- OBP1986 - AVG1986
SecA1986 <- (Batting1986$BB + (TB1986 - Batting1986$H) + (Batting1986$SB - Batting1986$CS)) / Batting1986$AB
EqA1986 <- (Batting1986$H + TB1986 + 1.5*(Batting1986$BB + Batting1986$HBP) + Batting1986$SB) / (Batting1986$AB + Batting1986$BB + Batting1986$HBP + Batting1986$CS + (Batting1986$SB/3))
OPS_Plus_1986 <- ((OBP1986/(sum(OBP1986, na.rm = T)/length(OBP1986))) + (SLG1986/(sum(SLG1986, na.rm = T)/length(SLG1986))) -1)
A_1986 <- Batting1986$H + Batting1986$BB + Batting1986$HBP - Batting1986$CS - Batting1986$GIDP
B_1986 <- TB1986 + 0.24*(Batting1986$BB - Batting1986$IBB + Batting1986$HBP) + 0.62*Batting1986$SB + 0.5*(Batting1986$SH + Batting1986$SF) - 0.3*(Batting1986$SO)
C_1986 <- Batting1986$AB + Batting1986$BB + Batting1986$HBP + Batting1986$SH + Batting1986$SF
RC1986 = A_1986 * B_1986 / C_1986

Advanced1986<- data.frame(AVG1986, OBP1986, SLG1986, OPS1986, IsoP1986, IsoD1986,
                          SecA1986, EqA1986, OPS_Plus_1986, RC1986)
colnames(Advanced1986) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced1986 <- round(Advanced1986, 3)
Batting1986 <- cbind(Batting1986, Advanced1986)

# 1985
Batting1985 <- ds2[yearID=='1985',]
AVG1985 <- (Batting1985$H)/(Batting1985$AB)
OBP1985 <- (Batting1985$H + Batting1985$BB + Batting1985$HBP)/(Batting1985$AB + Batting1985$BB + Batting1985$HBP + Batting1985$SF)
X1B <- Batting1985$H - (Batting1985$X2B + Batting1985$X3B + Batting1985$HR)
TB1985 <- 1*X1B + 2*Batting1985$X2B + 3*Batting1985$X3B + 4*Batting1985$HR
SLG1985 <- TB1985/Batting1985$AB
OPS1985 <- OBP1985 + SLG1985
IsoP1985 <- SLG1985 - AVG1985
IsoD1985 <- OBP1985 - AVG1985
SecA1985 <- (Batting1985$BB + (TB1985 - Batting1985$H) + (Batting1985$SB - Batting1985$CS)) / Batting1985$AB
EqA1985 <- (Batting1985$H + TB1985 + 1.5*(Batting1985$BB + Batting1985$HBP) + Batting1985$SB) / (Batting1985$AB + Batting1985$BB + Batting1985$HBP + Batting1985$CS + (Batting1985$SB/3))
OPS_Plus_1985 <- ((OBP1985/(sum(OBP1985, na.rm = T)/length(OBP1985))) + (SLG1985/(sum(SLG1985, na.rm = T)/length(SLG1985))) -1)
A_1985 <- Batting1985$H + Batting1985$BB + Batting1985$HBP - Batting1985$CS - Batting1985$GIDP
B_1985 <- TB1985 + 0.24*(Batting1985$BB - Batting1985$IBB + Batting1985$HBP) + 0.62*Batting1985$SB + 0.5*(Batting1985$SH + Batting1985$SF) - 0.3*(Batting1985$SO)
C_1985 <- Batting1985$AB + Batting1985$BB + Batting1985$HBP + Batting1985$SH + Batting1985$SF
RC1985 = A_1985 * B_1985 / C_1985

Advanced1985<- data.frame(AVG1985, OBP1985, SLG1985, OPS1985, IsoP1985, IsoD1985,
                          SecA1985, EqA1985, OPS_Plus_1985, RC1985)
colnames(Advanced1985) <- c('AVG', 'OBP', 'SLG', 'OPS', 'IsoP', 'IsoD', 'SecA',
                            'EqA', 'OPS+', 'RC')
Advanced1985 <- round(Advanced1985, 3)
Batting1985 <- cbind(Batting1985, Advanced1985)

Batting1985_1986 <- rbind(Batting1985, Batting1986)
Batting1985_1987 <- rbind(Batting1985_1986, Batting1987)
Batting1985_1988 <- rbind(Batting1985_1987, Batting1988)
Batting1985_1989 <- rbind(Batting1985_1988, Batting1989)
Batting1985_1990 <- rbind(Batting1985_1989, Batting1990)
Batting1985_1991 <- rbind(Batting1985_1990, Batting1991)
Batting1985_1992 <- rbind(Batting1985_1991, Batting1992)
Batting1985_1993 <- rbind(Batting1985_1992, Batting1993)
Batting1985_1994 <- rbind(Batting1985_1993, Batting1994)
Batting1985_1995 <- rbind(Batting1985_1994, Batting1995)
Batting1985_1996 <- rbind(Batting1985_1995, Batting1996)
Batting1985_1997 <- rbind(Batting1985_1996, Batting1997)
Batting1985_1998 <- rbind(Batting1985_1997, Batting1998)
Batting1985_1999 <- rbind(Batting1985_1998, Batting1999)
Batting1985_2000 <- rbind(Batting1985_1999, Batting2000)
Batting1985_2001 <- rbind(Batting1985_2000, Batting2001)
Batting1985_2002 <- rbind(Batting1985_2001, Batting2002)
Batting1985_2003 <- rbind(Batting1985_2002, Batting2003)
Batting1985_2004 <- rbind(Batting1985_2003, Batting2004)
Batting1985_2005 <- rbind(Batting1985_2004, Batting2005)
Batting1985_2006 <- rbind(Batting1985_2005, Batting2006)
Batting1985_2007 <- rbind(Batting1985_2006, Batting2007)
Batting1985_2008 <- rbind(Batting1985_2007, Batting2008)
Batting1985_2009 <- rbind(Batting1985_2008, Batting2009)
Batting1985_2010 <- rbind(Batting1985_2009, Batting2010)
Batting1985_2011 <- rbind(Batting1985_2010, Batting2011)
Batting1985_2012 <- rbind(Batting1985_2011, Batting2012)
Batting1985_2013 <- rbind(Batting1985_2012, Batting2013)
Batting1985_2014 <- rbind(Batting1985_2013, Batting2014)
Batting1985_2015 <- rbind(Batting1985_2014, Batting2015)
Batting1985_2016 <- rbind(Batting1985_2015, Batting2016)
detach(ds2)


# performance vs salary
batting_salary <- merge(Batting1985_2016, Salaries,by=c("playerID", "yearID", "teamID", "lgID"))
colnames(batting_salary) <- c("playerID", "yearID", "teamID", "lgID", "stint", "G" , "AB", "R", "H", "X2B", "X3B", "HR", "RBI", "SB", "CS", "BB", "SO", "IBB", "HBP", "SH", "SF", "GIDP", "AVG", "OBP", "SLG", "OPS", "IsoP", "IsoD", "SecA",
                              "EqA", "OPS_plus", "RC", "salary")
attach(batting_salary)
batting_salary <- batting_salary %>%
  filter(AB >= 502) 
# attach(batting_salary)

## Descriptive Statistics
# describe(batting_salary)
dfSummary(batting_salary)
view(dfSummary(batting_salary))

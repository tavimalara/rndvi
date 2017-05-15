satData <- read.delim(text ="satID satNumber year1 week1 year2 week2
NC 07 1981 35 1984 49
NF 09 1985 09 1988 44
NH 11 1988 46 1994 36
NJ 14 1995 04 2000 52
NL 16 2001 01 2004 01
NL 16 2004 05 2004 10
NL 16 2004 25 2004 28
NL 16 2004 30 2005 23
NN 18 2005 24 2010 52
NP 19 2011 01 9999 99",sep=" ", colClasses=c("character","character","character","character","character","character"), header=T)

# From HERE: http://www.star.nesdis.noaa.gov/smcd/emb/vci/images/rvh/VHP.G04.C07.NC.P1982001.ND.META.xml

# this section control which satellite will be used for calculating ND, SM and VH 
# satID satNumber yearWeek1 yearWeek2 
# NC 07 198135 198449 
# NF 09 198509 198844 
# NH 11 198846 199436 
# NJ 14 199504 200052 
# NL 16 200101 200401 
# NL 16 200405 200410 
# NL 16 200425 200428 
# NL 16 200430 200523 
# NN 18 200524 399999 
# NP 19 201201 399999 
# [Periods of AVHRR data used for GVI climatology] 


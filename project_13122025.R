library(WDI)

### Choosing and Downloading Data ###
# Choose indicators (rgdp = "Real GDP", pop = "Population", bach_% = 
#                   "Bachelor percentage of Population +25years", mast_% = 
#                   "Master percentage of Population +25years", enroll_% =
#                   "School Enrollment, Tertiary", lab_f = "Labor force")
#
# Download data
data_2010 <- WDI(indicator = c("rgdp" = "NY.GDP.MKTP.KD",
                               "pop" = "SP.POP.TOTL",
                               "bach_%" = "SE.TER.CUAT.BA.ZS",
                               "mast_%" = "SE.TER.CUAT.MS.ZS",
                               "enroll_%" = "SE.TER.ENRR",
                               "lab_f" = "SL.TLF.TOTL.IN"),
                 start = 2010, end = 2010,
                 extra = TRUE)

data_2015 <- WDI(indicator = c("rgdp" = "NY.GDP.MKTP.KD",
                               "pop" = "SP.POP.TOTL",
                               "bach_%" = "SE.TER.CUAT.BA.ZS",
                               "mast_%" = "SE.TER.CUAT.MS.ZS", 
                               "enroll_%" = "SE.TER.ENRR",
                               "lab_f" = "SL.TLF.TOTL.IN"),
                 start = 2015, end = 2015,
                 extra = TRUE)

# Combine the two datasets into one 
data_combined <- rbind(data_2010, data_2015)

# Remove aggregates 
data_combined <- data_combined[
  !(data_combined$region == "Aggregates" |
      is.na(data_combined$region)),]

# Define a vector of small countries to exclude
# (excluding very small and economically less relevant countries)

small_countries <- c("American Samoa", "Andorra", "Antigua and Barbuda", "Aruba", 
                     "Bahamas, The", "Bahrain", "Barbados", "Belize", "Bhutan", 
                     "Maldives", "Malta", "Monaco", "Nauru", "Palau", "San Marino", 
                     "Sao Tome and Principe", "Seychelles", "Singapore", "St. Kitts and Nevis", 
                     "St. Lucia", "St. Martin (French part)", "St. Vincent and the Grenadines", 
                     "Tuvalu", "Vanuatu", "Virgin Islands (U.S.)", "West Bank and Gaza",
                     "Liechtenstein","Faroe Islands","Kosovo","Gibraltar","Greenland","Grenada",
                     "Dominica","Curacao","Sint Marteen","Anguilla","Montserrat",
                     "Turks and Caicos Islands","Bermuda","Kiribati","Marshallislands","Fidschi",
                     "Samoa","Tonga","Micronesia","Cook Islands","Niue","Tokelau","Brunei",
                     "Comoren","Mauritius","Cape Verde","Timor-Leste","Qatar","Kuwait","Dschibuti",
                     "Dschibuti","Eswaitini","Lesotho","Gambia","Botswana")

# Exclude small countries

data_combined <- data_combined [!data_combined$country %in% small_countries,]

# Reshape data

data_wide <- reshape(data_combined, idvar = "country", timevar = "year", direction ="wide")

# Calculate further needed variables

data_wide$rgdppc.2010 <- data_wide$rgdp.2010/data_wide$pop.2010
data_wide$rgdppc.2015 <- data_wide$rgdp.2015/data_wide$pop.2015
data_wide$pop_growth <- data_wide$pop.2015 - data_wide$pop.2010
education.2010 <- 

###!!!!!! Define indicators
data_bach <- data_wide[c(1,2,7,8,23,24,34,35,36)]
data_bach <- na.omit(data_bach)
# lost observations 147 (due to NA)

data_master <- data_wide[c(1,2,7,9,23,25,34,35,36)]
data_master <- na.omit(data_master)
# lost observations 153
 
data_enroll <- data_wide[c(1,2,7,10,23,26,34,35,36)]
data_enroll <- na.omit(data_enroll)
# lost observations 68

data_labf <- data_wide[c(1,2,7,11,23,27,34,35,36)]
data_labf <- na.omit(data_labf)
# lost observations 12

# Calculate regression formula

ln_rgdppc ~ beta_0+ln_education+enrollment+labf+pop_growth

#Aus welchen Indikatoren könnte man education formen? 




# Reshape data 

#data_wide <- reshape(data_combined, idvar = "country", timevar = "year", direction = "wide")
# data_sorted <- data_combined[,names(data_combined) %in% c("country","year","rgdp","pop")]
#final_data <- subset(data_wide, 
#complete.cases(data_wide[, c("rgdp.2010", "rgdp.2015")]))

# Calculating rgdppc

final_data$rgdppc.2010 <- final_data$rgdp.2010/final_data$pop.2010
final_data$rgdppc.2015 <- final_data$rgdp.2015/final_data$pop.2015
rgdppc.2010 <- c(final_data$rgdppc.2010)
rgdppc.2015 <- c(final_data$rgdppc.2015)
rgdppc_combined <- rbind(rgdppc.2010,rgdppc.2015)



# Plots
rgdp <- c(data_combined$rgdp)
pop <- c(data_combined$pop)
scatter.smooth(rgdp,pop)

bach <- c(data_bach_comb$"bach_%" [!data_bach_comb$country %in% "China"])
pop1 <- c(data_bach_comb$pop [!data_bach_comb$country %in% "China"])
scatter.smooth(bach,pop1)






















#logarithmieren mit ln-funktion

# Plot-functions

install.packages("MASS")
library(MASS)

truehist(data_combined$rgdp)
scatter.smooth((data_combined$rgdp))
boxplot(data_combined$rgdp)

x <- c(data_combined$rgdp)
y <- c(data_combined$pop)
scatter.smooth(x,y)

# statistic numbers

install.packages("moments")
library(moments)

mean(data$gdppc, na.rm = TRUE)
var(data$gdppc, na.rm = TRUE)
sd(data$gdppc, na.rm = TRUE)
min(data$gdppc)
max(data$gdppc)

skewness(data$gdppc, na.rm = TRUE) #positiver Wert über 0 = linksschief
kurtosis(data$gdppc, na.rm = TRUE) # NRMVT hat kurt von 3

min(data$gdppc)
max(data$gdppc)

# Hypothesentest

t.test(data$gdppc[data$region == "Latin America & Caribbean"], mu = 20000)
mean((data$gdppc[data$region == "Latin America & Caribbean"])

# Linear regression
     
reg1 <- lm(ahe ~ , data = ) # sogenanntes "lm- Objekt" 
summary(reg1)

# test_data

data_literacy <- WDI(indicator = c("lite" = "SE.ADT.1524.LT.ZS",
                                   "pop" = "SP.POP.TOTL"),
                      start = 2010, end = 2010,
                      extra = TRUE)

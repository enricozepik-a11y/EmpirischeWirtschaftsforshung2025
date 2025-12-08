library(WDI)

# Download data for 2010 and 2015
data_2010 <- WDI(indicator = c("rgdp" = "NY.GDP.MKTP.KD",
                               "pop" = "SP.POP.TOTL"),
                 start = 2010, end = 2010,
                 extra = TRUE)

data_2015 <- WDI(indicator = c("rgdp" = "NY.GDP.MKTP.KD",
                               "pop" = "SP.POP.TOTL"),
                 start = 2015, end = 2015,
                 extra = TRUE)

# Combine the two datasets into one 
data_combined <- rbind(data_2010, data_2015)

# Remove aggregates 
data_combined <- data_combined[
  !(data_combined$region == "Aggregates" |
      is.na(data_combined$region)),]

# Define a vector of small countries to exclude 

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

data_wide <- reshape(data_combined, idvar = "country", timevar = "year", direction = "wide")
complete.cases(data_wide) 
data_sorted <- data_combined[,names(data_combined) %in% c("country","year","rgdp","pop")]

# Plots

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

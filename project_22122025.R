install.packages("WDI")
library(WDI)

data_2010 <- WDI(indicator = c("rgdp" = "NY.GDP.MKTP.KD",
                               "pop" = "SP.POP.TOTL",
                               "persist" = "SE.PRM.PRSL.ZS"),
                 start = 2010, end = 2010)

data_2015 <- WDI(indicator = c("rgdp" = "NY.GDP.MKTP.KD",
                               "pop" = "SP.POP.TOTL",
                               "persist" = "SE.PRM.PRSL.ZS"),
                 start = 2015, end = 2015)

# WDI country metadata (you can add more arguments in the c()-function if you want)
meta <- WDI_data$country[, c("iso2c", "region", "income")]

# Combine the two datasets into one and check them
data_combined <- rbind(data_2010, data_2015)

# Merge with meta data
data_combined <- merge(data_combined, meta, by = "iso2c", all.x = TRUE)

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

data_wide <- reshape(data_combined, idvar = "country", timevar = "year", direction = "wide")
data_wide <- na.omit(data_wide)

#data_wide$rgdppc.2010 <- data_wide$rgdp.2010/data_wide$pop.2010
data_wide$rgdppc.2015 <- data_wide$rgdp.2015/data_wide$pop.2015
data_wide$pop_growth <- data_wide$pop.2015 - data_wide$pop.2010

###!!!!!! Define indicators
data_persist <- data_wide[c(1,2,4,5,6,12,13,14,18,19,20)]
data_persist <- na.omit(data_persist)


install.packages("MASS")
library(MASS)
install.packages("AER")
library(AER)
install.packages("moments")
library(moments)

attach(data_wide)

reg <- lm(log(rgdppc.2015) ~ persist.2015 + pop_growth)
coef(reg)

plot(rgdppc.2015 ~ persist.2015, data = data_wide, 
     main = "scatter Plot rGDPpc und Bildung (persistence to last grade)",
     xlab = "Persistence to last grade of primary",
     ylab = "Real GDP per capita")

abline(lm(rgdppc.2015 ~ persist.2015, data = data_wide),lwd = 2)

persist_seq <- seq(
  min(persist.2015),
  max(persist.2015),
  length.out = 100)
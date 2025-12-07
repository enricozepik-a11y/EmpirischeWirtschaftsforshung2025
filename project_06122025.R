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

# Define a vector of countries to exclude (just an example, please check carefully!!)

small_countries_2 <- c("American Samoa", "Andorra", "Antigua and Barbuda", "Aruba", 
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

list(small_countries_2)

data_wide <- reshape(data_combined, idvar = "country", timevar = "year", direction = "wide")

complete.cases(data_wide) 


data2 <- data_combined[,names(data_combined) %in% c("country","year","rgdp","pop")]

install.packages("MASS")
library(MASS)

truehist(data_combined$rgdp)
scatter.smooth((data_combined$rgdp))
boxplot(data_combined$rgdp)

list(data_combined$country)

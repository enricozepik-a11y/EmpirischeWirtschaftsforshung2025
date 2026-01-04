
### 1. Install needed Packages ###

library(WDI)# WDI: World Bank Open Data
library(moments)# moments: Schiefe und Kurtosis
library(sandwich)
library(lmtest)# sandwich & lmtest: robuste Standardfehler
library(car)# car: Multikollinearität (VIF)

### 2. Download World Bank Data ### 

data_2010 <- WDI(indicator = c("rgdp" = "NY.GDP.MKTP.KD",      # rgdp = reales BIP 
                               "pop" = "SP.POP.TOTL",          # pop = Gesamtbevölkerung
                               "persist" = "SE.PRM.PRSL.ZS"),  # persist = Persistence to last grade of primary (%)
                 start = 2010, end = 2010)

data_2015 <- WDI(indicator = c("rgdp" = "NY.GDP.MKTP.KD",
                               "pop" = "SP.POP.TOTL",
                               "persist" = "SE.PRM.PRSL.ZS"),
                 start = 2015, end = 2015)

### 3. Download Countries metadata ###

meta <- WDI_data$country[, c("iso2c", "region", "income")]

### 4. Combine and clean up data ###

data_combined <- rbind(data_2010, data_2015)

# Metadaten hinzufügen
data_combined <- merge(data_combined, meta, by = "iso2c", all.x = TRUE)

# Aggregate entfernen 
data_combined <- data_combined[
  !(data_combined$region == "Aggregates" |
      is.na(data_combined$region)),]

### 5. Exclude Small Countries ###

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
                     "Comoren","Mauritius","Cape Verde","Timor-Leste","Qatar","Kuwait",
                     "Dschibuti","Eswaitini","Lesotho","Gambia","Botswana")

data_combined <- data_combined [!data_combined$country %in% small_countries,]   #(45 lost observations)

### 6. Reshape and clean up data ###

data_wide <- reshape(data_combined, idvar = "country", timevar = "year", direction = "wide")
data_wide <- na.omit(data_wide)       # (109 lost obersvations)

# 63 Remaining Observations

### 7. Define relevant variables ###

#data_wide$rgdppc.2010 <- data_wide$rgdp.2010/data_wide$pop.2010
data_wide$rgdppc.2015 <- data_wide$rgdp.2015/data_wide$pop.2015
data_wide$pop_growth <- (data_wide$pop.2015 - data_wide$pop.2010)/data_wide$pop.2010*100

### 8. Regression model ###

# Abhängige Variable: log(BIP pro Kopf) 
# Hauptregressor: Bildungsindikator (Persistence)
# Kontrollvariable: Bevölkerungswachstum

reg <- lm(log(rgdppc.2015) ~ persist.2015 + pop_growth, data = data_wide)

summary(reg)
coef(reg)

### 9. Graphical Representation

persist_seq <- seq(
  min(data_wide$persist.2015),
  max(data_wide$persist.2015),
  length.out = 100)

data_plot <- data.frame(
  persist.2015 = persist_seq,
  pop_growth = mean(data_wide$pop_growth)
)

pred <- predict(reg, newdata = data_plot)

plot(log(rgdppc.2015) ~ persist.2015, data = data_wide, 
     main = "Scatter Plot: BIP pro Kopf und Bildung (persistence to last grade)",
     xlab = "Persistence to last grade of primary (%)",
     ylab = "Log BIP pro Kopf")

lines(persist_seq,pred, lwd =2)
mtext("Regressionslinie bei konstantem Bevölkerungswachstum", side = 3, cex = 0.8)

# Index von Minimum und Maximum der Regression
idx_min <- which.min(pred)
idx_max <- which.max(pred)

# Koordinaten
x_min <- persist_seq[idx_min]
y_min <- pred[idx_min]
x_max <- persist_seq[idx_max]
y_max <- pred[idx_max]

# Punkte hinzufügen
points(x_min, y_min, pch = 19, cex = 1.3)
points(x_max, y_max, pch = 19, cex = 1.3)

### 10. Statistical diagnostics

# Mittelwerte
mean_persist <- mean(data_wide$persist.2015, na.rm = TRUE)
mean_persist
mean_log_rgdppc <- mean(log(data_wide$rgdppc.2015), na.rm = TRUE)
mean_log_rgdppc
mean_pop_growth <- mean(data_wide$pop_growth, na.rm = TRUE)
mean_pop_growth

# Minima und Mixima

min_persist <- min(data_wide$persist.2015, na.rm = TRUE)
max_persist <- max(data_wide$persist.2015, na.rm = TRUE)
min_log_rgdppc <- min(log(data_wide$rgdppc.2015), na.rm = TRUE)
max_log_rgdppc <- max(log(data_wide$rgdppc.2015), na.rm = TRUE)
min_pop_growth <- min(data_wide$pop_growth, na.rm = TRUE)
max_pop_growth <- max(data_wide$pop_growth, na.rm = TRUE)

summary_stats <- data.frame(
  Variable = c("Persistence","log(BIP pro Kopf)","Pop. Growth"),
  Mean = c(mean_persist, mean_log_rgdppc, mean_pop_growth),
  Min = c(min_persist, min_log_rgdppc, min_pop_growth),
  Max = c(max_persist, max_log_rgdppc, max_pop_growth)
)

summary_stats

min_pred <- min(pred, na.rm = TRUE)
max_pred <- max(pred, na.rm = TRUE)
min_pred
max_pred

min_pred_level <- exp(min_pred)
max_pred_level <- exp(max_pred)
min_pred_level
max_pred_level

# Residuenanalyse
skewness(residuals(reg))      # linksschiefe Verteilung 
kurtosis(residuals(reg))      # Kurtosis < 3 (kleiner als Normalverteilung)

# Vorhergesagte Werte und Residuen
rgdppc.2015.hat <- fitted(reg)
u.hat <- resid(reg)

# Heteroskedastizitätsrobuste Standardfehler (HC1)
coeftest(reg, vcov. = vcovHC, type = "HC1")

# Multikollinearität (VIF-Werte)
vif(reg)      # Testing multicollinearity (VIF <= 5), daher keine Multikollinearität



# Fazit:
# Die Regression zeigt einen statistisch signifikanten und positiven Zusammenhang
# zwischen dem Bildungsniveau (Persistence to last grade of primary) 
# und dem BIP pro Kopf.
# Länder mit einer höheren Verbleibsquote weisen im Durchschnitt ein höheres 
# BIP pro Kopf auf, ceteris paribus.

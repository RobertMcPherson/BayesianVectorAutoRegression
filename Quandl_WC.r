
##Remove All Objects before Running the Analysis
rm(list=ls())

#getwd()

#install.packages("Hmisc")
#install.packages("devtools")
#install.packages("Quandl")
#install.packages("car")
#install.packages("fread")
#install.packages("curl")

##Load Packages
library(Hmisc)
library(devtools)
library(Quandl) #for data sources
library(car) #for scatterplots
library(curl) #for downloand data from a url

##Cellular Phone Use in US
indep.var <- Quandl("UICT/CELL_USA", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual")
str(indep.var)
indep.var2 <- indep.var[nrow(indep.var):1,] #reverse order to ascending...oldest at beginning
str(indep.var2)
write.csv(file="MobilePhoneSubscriptionsPer100InhabitantsUS.csv", x=indep.var2)

##Producer Price Index by Commodity for Fuels and Related Products and Power: No. 2 Diesel Fuel
indep.var <- Quandl("FRED/WPU05730302", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual")
#str(indep.var)
indep.var2 <- indep.var[nrow(indep.var):1,] #reverse order to ascending...oldest at beginning
write.csv(file="ProducerPriceIndesDieselFuel.csv", x=indep.var2)

##OPEC Oil Prices
#?Quandl
indep.var <- Quandl("OPEC/ORB", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", order="asc")
write.csv(file="OilPrices_OPEC.csv", x=indep.var)
#str(indep.var)

##GDP
indep.var <- Quandl("BEA/GDP", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", order="asc")
#str(indep.var)
write.csv(file="GDP.csv", x=indep.var)

##CPI
indep.var <- Quandl("BEA/T20804_M", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", order="asc")
write.csv(file="CPI.csv", x=indep.var)

##Personal Consumption Expenditures
indep.var <- Quandl("BEA/T20805_M", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", order="asc")
write.csv(file="PersonalConsumption.csv", x=indep.var)

##Wages and Salaries
indep.var <- Quandl("BEA/T20700B_M", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", order="asc")
write.csv(file="WagesAndSalaries.csv", x=indep.var)

##Motor Vehicle Output
indep.var <- Quandl("BEA/T70205B_A", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", order="asc")
write.csv(file="MotorVehicleOutput.csv", x=indep.var)

##Price Index for Motor Vehicle Output
indep.var <- Quandl("BEA/T70204B_A", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", order="asc")
write.csv(file="PriceIndexForMotorVehicleOutput.csv", x=indep.var)

##Corporate Profits
indep.var <- Quandl("BEA/T61600D_A", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", order="asc")
write.csv(file="CorporateProfits.csv", x=indep.var)

##Real Private Fixed Investment by Type, Quantity Indexes (Annual data from 1947 to 2017)
indep.var <- Quandl("BEA/T50303_A", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", order="asc")
write.csv(file="RealPrivateFixedInvestment.csv", x=indep.var)

##Saving and Investment by Sector (Annual data from 1929 to 2017)
indep.var <- Quandl("BEA/T50100_A", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", order="asc")
write.csv(file="SavingAndInvestmentBySector.csv", x=indep.var)

##Exports and Imports of Goods and Services by Type of Product (Annual data from 1967 to 2017)
indep.var <- Quandl("BEA/T40205_A", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", order="asc")
write.csv(file="ExportsAndImports.csv", x=indep.var)

##Federal Government Current Receipts and Expenditures (Annual data from 1929 to 2017)
indep.var <- Quandl("BEA/T30200_A", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", order="asc")
write.csv(file="FedGovCurrentReciptsAndExpenditures.csv", x=indep.var)

##National Income by Type of Income (Quarterly data from 1947Q1 to 2017Q4)
indep.var <- Quandl("BEA/T11200_Q", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", order="asc")
write.csv(file="NationalIncomeByType.csv", x=indep.var)

##Transactions of Defined Contribution Pension Plans (Annual data from 1984 to 2016)
indep.var <- Quandl("BEA/T72500_A", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", order="asc")
write.csv(file="DefinedContributionPlanTransactions.csv", x=indep.var)

##Transactions of Defined Benefit Pension Plans (Annual data from 1984 to 2016)
indep.var <- Quandl("BEA/T72100_A", api_key="DJGcfzQc5RYP1JSycMBv", collapse="annual", order="asc")
write.csv(file="DefinedBenefitPlanTransactions.csv", x=indep.var)






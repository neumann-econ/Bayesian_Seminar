#### Load Libraries ####
library(readxl)
install.packages("glmnet")
library(glmnet)
library(readr)
install.packages("openxlsx")
library(openxlsx)
library(pls)
library(lars)
library(MASS)
install.packages("monomvn")
library(monomvn)
library(HDInterval)
library(utils)
install.packages("Rlab")
library(Rlab)
install.packages("invgamma")
library(invgamma)


#### Read in Data ####
file= paste("C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Independent Study\\Data\\For Use\\Annual Big Data Variables.xlsx");
Data = read_excel(file)
attach(Data)

file1= paste("C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Independent Study\\Data\\For Use\\Industry Portfolios - For Regressions.xlsx");
Industries = read_excel(file1)
attach(Industries)

Data <- data.matrix(Data)
train_data <- Data[1:45,] # Split the dataset into the training and testing samples
test_data <- Data[46:55,]
Industries <- data.matrix(Industries) # convert the dataframe to a numeric matrix for the elastic net calculation
Industries <- Industries[,-1] # remove the date column from the data
Industries <- Industries - RF # create excess returns by subtracting the risk-free rate
Industries <- Industries[,-31] # remove the risk-free rate from the matrix
train_industries <- Industries[1:45,]
test_industries <- Industries[46:55,]

comp1 = c(1:30) # classical lasso one industry at a time
comp2 = c(1:30) # Bayesian lasso one industry at a time
comp3 = c(1:30) # Bayesian variable selection using the method of Chen et al. (2014)


scale.test_data <- scale(test_data)
scale.test_industries <- scale(test_industries)


#### Classical Lasso ####
foodcvfit = cv.glmnet(train_data, train_industries[,1], alpha = 1, family = "gaussian")
plot(foodcvfit)
coeff_food <- coef(foodcvfit,s = "lambda.min")
coeff.food <- coeff_food@i # Extract the index numbers of the nonzero coefficients
write.xlsx(coeff.food, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Food Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
foodpred <- predict(foodcvfit, test_data, s = "lambda.min") # Predict returns using the test data
fooderror <- test_industries[,1] - foodpred
fooderror2 = fooderror^2
fooderrorsum = sum(fooderror2)
foodret2 = test_industries[,1]^2
foodretsum = sum(foodret2)
foodoosr_sqr = 1- (fooderrorsum/foodretsum) # Calculate the out of sample R-squared
comp1[1] = foodoosr_sqr

# The process of predicting values is repeated for all 30 industry portfolios
beercvfit = cv.glmnet(train_data, train_industries[,2], alpha = 1, family = "gaussian")
plot(beercvfit)
coeff_beer <- coef(beercvfit,s = "lambda.min")
coeff.beer <- coeff_beer@i
write.xlsx(coeff.beer, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Beer Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
beerpred <- predict(beercvfit, test_data, s = "lambda.min")
beererror <- test_industries[,2] - beerpred
beererror2 = beererror^2
beererrorsum = sum(beererror2)
beerret2 = test_industries[,2]^2
beerretsum = sum(beerret2)
beeroosr_sqr = 1- (beererrorsum/beerretsum)
comp1[2] = beeroosr_sqr


smokcvfit = cv.glmnet(train_data, train_industries[,3], alpha = 1, family = "gaussian")
plot(smokcvfit)
coeff_smok <- coef(smokcvfit,s = "lambda.min")
coeff.smok <- coeff_smok@i
write.xlsx(coeff.smok, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Smoke Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
smokpred <- predict(smokcvfit, test_data, s = "lambda.min")
smokerror <- test_industries[,3] - smokpred
smokerror2 = smokerror^2
smokerrorsum = sum(smokerror2)
smokret2 = test_industries[,3]^2
smokretsum = sum(smokret2)
smokoosr_sqr = 1- (smokerrorsum/smokretsum)
comp1[3] = smokoosr_sqr


gamecvfit = cv.glmnet(train_data, train_industries[,4], alpha = 1, family = "gaussian")
plot(gamecvfit)
coeff_game <- coef(gamecvfit,s = "lambda.min")
coeff.game <- coeff_game@i
write.xlsx(coeff.game, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Games Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
gamepred <- predict(gamecvfit, test_data, s = "lambda.min")
gameerror <- test_industries[,4] - gamepred
gameerror2 = gameerror^2
gameerrorsum = sum(gameerror2)
gameret2 = test_industries[,4]^2
gameretsum = sum(gameret2)
gameoosr_sqr = 1- (gameerrorsum/gameretsum)
comp1[4] = gameoosr_sqr


bookcvfit = cv.glmnet(train_data, train_industries[,5], alpha = 1, family = "gaussian")
plot(bookcvfit)
coeff_book <- coef(bookcvfit,s = "lambda.min")
coeff.book <- coeff_book@i
write.xlsx(coeff.book, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Books Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
bookpred <- predict(bookcvfit, test_data, s = "lambda.min")
bookerror <- test_industries[,5] - bookpred
bookerror2 = bookerror^2
bookerrorsum = sum(bookerror2)
bookret2 = test_industries[,5]^2
bookretsum = sum(bookret2)
bookoosr_sqr = 1- (bookerrorsum/bookretsum)
comp1[5] = bookoosr_sqr


houscvfit = cv.glmnet(train_data, train_industries[,6], alpha = 1, family = "gaussian")
plot(houscvfit)
coeff_hous <- coef(houscvfit,s = "lambda.min")
coeff.hous <- coeff_hous@i
write.xlsx(coeff.hous, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Household Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
houspred <- predict(houscvfit, test_data, s = "lambda.min")
houserror <- test_industries[,6] - houspred
houserror2 = houserror^2
houserrorsum = sum(houserror2)
housret2 = test_industries[,6]^2
housretsum = sum(housret2)
housoosr_sqr = 1- (houserrorsum/housretsum)
comp1[6] = housoosr_sqr


clotcvfit = cv.glmnet(train_data, train_industries[,7], alpha = 1, family = "gaussian")
plot(clotcvfit)
coeff_clot <- coef(clotcvfit,s = "lambda.min")
coeff.clot <- coeff_clot@i
write.xlsx(coeff.clot, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Clothing Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
clotpred <- predict(clotcvfit, test_data, s = "lambda.min")
cloterror <- test_industries[,7] - clotpred
cloterror2 = cloterror^2
cloterrorsum = sum(cloterror2)
clotret2 = test_industries[,7]^2
clotretsum = sum(clotret2)
clotoosr_sqr = 1- (cloterrorsum/clotretsum)
comp1[7] = clotoosr_sqr


healcvfit = cv.glmnet(train_data, train_industries[,8], alpha = 1, family = "gaussian")
plot(healcvfit)
coeff_heal <- coef(healcvfit,s = "lambda.min")
coeff.heal <- coeff_heal@i
write.xlsx(coeff.heal, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Health Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
healpred <- predict(healcvfit, test_data, s = "lambda.min")
healerror <- test_industries[,8] - healpred
healerror2 = healerror^2
healerrorsum = sum(healerror2)
healret2 = test_industries[,8]^2
healretsum = sum(healret2)
healoosr_sqr = 1- (healerrorsum/healretsum)
comp1[8] = healoosr_sqr


chemcvfit = cv.glmnet(train_data, train_industries[,9], alpha = 1, family = "gaussian")
plot(chemcvfit)
coeff_chem <- coef(chemcvfit,s = "lambda.min")
coeff.chem <- coeff_chem@i
write.xlsx(coeff.chem, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Chemicals Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
chempred <- predict(chemcvfit, test_data, s = "lambda.min")
chemerror <- test_industries[,9] - chempred
chemerror2 = chemerror^2
chemerrorsum = sum(chemerror2)
chemret2 = test_industries[,9]^2
chemretsum = sum(chemret2)
chemoosr_sqr = 1- (chemerrorsum/chemretsum)
comp1[9] = chemoosr_sqr


textcvfit = cv.glmnet(train_data, train_industries[,10], alpha = 1, family = "gaussian")
plot(textcvfit)
coeff_text <- coef(textcvfit,s = "lambda.min")
coeff.text <- coeff_text@i
write.xlsx(coeff.text, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Textiles Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
textpred <- predict(textcvfit, test_data, s = "lambda.min")
texterror <- test_industries[,10] - textpred
texterror2 = texterror^2
texterrorsum = sum(texterror2)
textret2 = test_industries[,10]^2
textretsum = sum(textret2)
textoosr_sqr = 1- (texterrorsum/textretsum)
comp1[10] = textoosr_sqr


conscvfit = cv.glmnet(train_data, train_industries[,11], alpha = 1, family = "gaussian")
plot(conscvfit)
coeff_cons <- coef(conscvfit,s = "lambda.min")
coeff.cons <- coeff_cons@i
write.xlsx(coeff.cons, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Construction Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
conspred <- predict(conscvfit, test_data, s = "lambda.min")
conserror <- test_industries[,11] - conspred
conserror2 = conserror^2
conserrorsum = sum(conserror2)
consret2 = test_industries[,11]^2
consretsum = sum(consret2)
consoosr_sqr = 1- (conserrorsum/consretsum)
comp1[11] = consoosr_sqr


steecvfit = cv.glmnet(train_data, train_industries[,12], alpha = 1, family = "gaussian")
plot(steecvfit)
coeff_stee <- coef(steecvfit,s = "lambda.min")
coeff.stee <- coeff_stee@i
write.xlsx(coeff.stee, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Steel Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
steepred <- predict(steecvfit, test_data, s = "lambda.min")
steeerror <- test_industries[,12] - steepred
steeerror2 = steeerror^2
steeerrorsum = sum(steeerror2)
steeret2 = test_industries[,12]^2
steeretsum = sum(steeret2)
steeoosr_sqr = 1- (steeerrorsum/steeretsum)
comp1[12] = steeoosr_sqr


fabpcvfit = cv.glmnet(train_data, train_industries[,13], alpha = 1, family = "gaussian")
plot(fabpcvfit)
coeff_fabp <- coef(fabpcvfit,s = "lambda.min")
coeff.fabp <- coeff_fabp@i
write.xlsx(coeff.fabp, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Fabricated Products Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
fabppred <- predict(fabpcvfit, test_data, s = "lambda.min")
fabperror <- test_industries[,13] - fabppred
fabperror2 = fabperror^2
fabperrorsum = sum(fabperror2)
fabpret2 = test_industries[,13]^2
fabpretsum = sum(fabpret2)
fabpoosr_sqr = 1- (fabperrorsum/fabpretsum)
comp1[13] = fabpoosr_sqr


eleccvfit = cv.glmnet(train_data, train_industries[,14], alpha = 1, family = "gaussian")
plot(eleccvfit)
coeff_elec <- coef(eleccvfit,s = "lambda.min")
coeff.elec <- coeff_elec@i
write.xlsx(coeff.elec, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Electrical Equipment Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
elecpred <- predict(eleccvfit, test_data, s = "lambda.min")
elecerror <- test_industries[,14] - elecpred
elecerror2 = elecerror^2
elecerrorsum = sum(elecerror2)
elecret2 = test_industries[,14]^2
elecretsum = sum(elecret2)
elecoosr_sqr = 1- (elecerrorsum/elecretsum)
comp1[14] = elecoosr_sqr


autocvfit = cv.glmnet(train_data, train_industries[,15], alpha = 1, family = "gaussian")
plot(autocvfit)
coeff_auto <- coef(autocvfit,s = "lambda.min")
coeff.auto <- coeff_auto@i
write.xlsx(coeff.auto, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Automobile Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
autopred <- predict(autocvfit, test_data, s = "lambda.min")
autoerror <- test_industries[,15] - autopred
autoerror2 = autoerror^2
autoerrorsum = sum(autoerror2)
autoret2 = test_industries[,15]^2
autoretsum = sum(autoret2)
autooosr_sqr = 1- (autoerrorsum/autoretsum)
comp1[15] = autooosr_sqr


carrcvfit = cv.glmnet(train_data, train_industries[,16], alpha = 1, family = "gaussian")
plot(carrcvfit)
coeff_carr <- coef(carrcvfit,s = "lambda.min")
coeff.carr <- coeff_carr@i
write.xlsx(coeff.carr, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Carry-Shipping Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
carrpred <- predict(carrcvfit, test_data, s = "lambda.min")
carrerror <- test_industries[,16] - carrpred
carrerror2 = carrerror^2
carrerrorsum = sum(carrerror2)
carrret2 = test_industries[,16]^2
carrretsum = sum(carrret2)
carroosr_sqr = 1- (carrerrorsum/carrretsum)
comp1[16] = carroosr_sqr


minecvfit = cv.glmnet(train_data, train_industries[,17], alpha = 1, family = "gaussian")
plot(minecvfit)
coeff_mine <- coef(minecvfit,s = "lambda.min")
coeff.mine <- coeff_mine@i
write.xlsx(coeff.mine, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Mining Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
minepred <- predict(minecvfit, test_data, s = "lambda.min")
mineerror <- test_industries[,17] - minepred
mineerror2 = mineerror^2
mineerrorsum = sum(mineerror2)
mineret2 = test_industries[,17]^2
mineretsum = sum(mineret2)
mineoosr_sqr = 1- (mineerrorsum/mineretsum)
comp1[17] = mineoosr_sqr


coalcvfit = cv.glmnet(train_data, train_industries[,18], alpha = 1, family = "gaussian")
plot(coalcvfit)
coeff_coal <- coef(coalcvfit,s = "lambda.min")
coeff.coal <- coeff_coal@i
write.xlsx(coeff.coal, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Coal Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
coalpred <- predict(coalcvfit, test_data, s = "lambda.min")
coalerror <- test_industries[,18] - coalpred
coalerror2 = coalerror^2
coalerrorsum = sum(coalerror2)
coalret2 = test_industries[,18]^2
coalretsum = sum(coalret2)
coaloosr_sqr = 1- (coalerrorsum/coalretsum)
comp1[18] = coaloosr_sqr


oilcvfit = cv.glmnet(train_data, train_industries[,19], alpha = 1, family = "gaussian")
plot(oilcvfit)
coeff_oil <- coef(oilcvfit,s = "lambda.min")
coeff.oil <- coeff_oil@i
write.xlsx(coeff.oil, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Oil Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
oilpred <- predict(oilcvfit, test_data, s = "lambda.min")
oilerror <- test_industries[,19] - oilpred
oilerror2 = oilerror^2
oilerrorsum = sum(oilerror2)
oilret2 = test_industries[,19]^2
oilretsum = sum(oilret2)
oiloosr_sqr = 1- (oilerrorsum/oilretsum)
comp1[19] = oiloosr_sqr


telecvfit = cv.glmnet(train_data, train_industries[,20], alpha = 1, family = "gaussian")
plot(telecvfit)
coeff_tele <- coef(telecvfit,s = "lambda.min")
coeff.tele <- coeff_tele@i
write.xlsx(coeff.tele, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Telecommunication Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
telepred <- predict(telecvfit, test_data, s = "lambda.min")
teleerror <- test_industries[,20] - telepred
teleerror2 = teleerror^2
teleerrorsum = sum(teleerror2)
teleret2 = test_industries[,20]^2
teleretsum = sum(teleret2)
teleoosr_sqr = 1- (teleerrorsum/teleretsum)
comp1[20] = teleoosr_sqr


servcvfit = cv.glmnet(train_data, train_industries[,21], alpha = 1, family = "gaussian")
plot(servcvfit)
coeff_serv <- coef(servcvfit,s = "lambda.min")
coeff.serv <- coeff_serv@i
write.xlsx(coeff.serv, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Services Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
servpred <- predict(servcvfit, test_data, s = "lambda.min")
serverror <- test_industries[,21] - servpred
serverror2 = serverror^2
serverrorsum = sum(serverror2)
servret2 = test_industries[,21]^2
servretsum = sum(servret2)
servoosr_sqr = 1- (serverrorsum/servretsum)
comp1[21] = servoosr_sqr


busicvfit = cv.glmnet(train_data, train_industries[,22], alpha = 1, family = "gaussian")
plot(busicvfit)
coeff_busi <- coef(busicvfit,s = "lambda.min")
coeff.busi <- coeff_busi@i
write.xlsx(coeff.busi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Business Equipment Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
busipred <- predict(busicvfit, test_data, s = "lambda.min")
busierror <- test_industries[,22] - busipred
busierror2 = busierror^2
busierrorsum = sum(busierror2)
busiret2 = test_industries[,22]^2
busiretsum = sum(busiret2)
busioosr_sqr = 1- (busierrorsum/busiretsum)
comp1[22] = busioosr_sqr


papecvfit = cv.glmnet(train_data, train_industries[,23], alpha = 1, family = "gaussian")
plot(papecvfit)
coeff_pape <- coef(papecvfit,s = "lambda.min")
coeff.pape <- coeff_pape@i
write.xlsx(coeff.pape, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Paper Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
papepred <- predict(papecvfit, test_data, s = "lambda.min")
papeerror <- test_industries[,23] - papepred
papeerror2 = papeerror^2
papeerrorsum = sum(papeerror2)
paperet2 = test_industries[,23]^2
paperetsum = sum(paperet2)
papeoosr_sqr = 1- (papeerrorsum/paperetsum)
comp1[23] = papeoosr_sqr


trancvfit = cv.glmnet(train_data, train_industries[,24], alpha = 1, family = "gaussian")
plot(trancvfit)
coeff_tran <- coef(trancvfit,s = "lambda.min")
coeff.tran <- coeff_tran@i
write.xlsx(coeff.tran, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Transportation Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
tranpred <- predict(trancvfit, test_data, s = "lambda.min")
tranerror <- test_industries[,24] - tranpred
tranerror2 = tranerror^2
tranerrorsum = sum(tranerror2)
tranret2 = test_industries[,24]^2
tranretsum = sum(tranret2)
tranoosr_sqr = 1- (tranerrorsum/tranretsum)
comp1[24] = tranoosr_sqr


wholcvfit = cv.glmnet(train_data, train_industries[,25], alpha = 1, family = "gaussian")
plot(wholcvfit)
coeff_whol <- coef(wholcvfit,s = "lambda.min")
coeff.whol <- coeff_whol@i
write.xlsx(coeff.whol, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Wholesale Trade Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
wholpred <- predict(wholcvfit, test_data, s = "lambda.min")
wholerror <- test_industries[,25] - wholpred
wholerror2 = wholerror^2
wholerrorsum = sum(wholerror2)
wholret2 = test_industries[,25]^2
wholretsum = sum(wholret2)
wholoosr_sqr = 1- (wholerrorsum/wholretsum)
comp1[25] = wholoosr_sqr


retacvfit = cv.glmnet(train_data, train_industries[,26], alpha = 1, family = "gaussian")
plot(retacvfit)
coeff_reta <- coef(retacvfit,s = "lambda.min")
coeff.reta <- coeff_reta@i
write.xlsx(coeff.reta, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Retail Trade Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
retapred <- predict(retacvfit, test_data, s = "lambda.min")
retaerror <- test_industries[,26] - retapred
retaerror2 = retaerror^2
retaerrorsum = sum(retaerror2)
retaret2 = test_industries[,26]^2
retaretsum = sum(retaret2)
retaoosr_sqr = 1- (retaerrorsum/retaretsum)
comp1[26] = retaoosr_sqr


mealcvfit = cv.glmnet(train_data, train_industries[,27], alpha = 1, family = "gaussian")
plot(mealcvfit)
coeff_meal <- coef(mealcvfit,s = "lambda.min")
coeff.meal <- coeff_meal@i
write.xlsx(coeff.meal, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Meals Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
mealpred <- predict(mealcvfit, test_data, s = "lambda.min")
mealerror <- test_industries[,27] - mealpred
mealerror2 = mealerror^2
mealerrorsum = sum(mealerror2)
mealret2 = test_industries[,27]^2
mealretsum = sum(mealret2)
mealoosr_sqr = 1- (mealerrorsum/mealretsum)
comp1[27] = mealoosr_sqr


othecvfit = cv.glmnet(train_data, train_industries[,28], alpha = 1, family = "gaussian")
plot(othecvfit)
coeff_othe <- coef(othecvfit,s = "lambda.min")
coeff.othe <- coeff_othe@i
write.xlsx(coeff.othe, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Other Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
othepred <- predict(othecvfit, test_data, s = "lambda.min")
otheerror <- test_industries[,28] - othepred
otheerror2 = otheerror^2
otheerrorsum = sum(otheerror2)
otheret2 = test_industries[,28]^2
otheretsum = sum(otheret2)
otheoosr_sqr = 1- (otheerrorsum/otheretsum)
comp1[28] = otheoosr_sqr


finacvfit = cv.glmnet(train_data, train_industries[,29], alpha = 1, family = "gaussian")
plot(finacvfit)
coeff_fina <- coef(finacvfit,s = "lambda.min")
coeff.fina <- coeff_fina@i
write.xlsx(coeff.fina, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Finance Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
finapred <- predict(finacvfit, test_data, s = "lambda.min")
finaerror <- test_industries[,29] - finapred
finaerror2 = finaerror^2
finaerrorsum = sum(finaerror2)
finaret2 = test_industries[,29]^2
finaretsum = sum(finaret2)
finaoosr_sqr = 1- (finaerrorsum/finaretsum)
comp1[29] = finaoosr_sqr


utilcvfit = cv.glmnet(train_data, train_industries[,30], alpha = 1, family = "gaussian")
plot(utilcvfit)
coeff_util <- coef(utilcvfit,s = "lambda.min")
coeff.util <- coeff_util@i
write.xlsx(coeff.util, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Utilities Lasso Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
utilpred <- predict(utilcvfit, test_data, s = "lambda.min")
utilerror <- test_industries[,30] - utilpred
utilerror2 = utilerror^2
utilerrorsum = sum(utilerror2)
utilret2 = test_industries[,30]^2
utilretsum = sum(utilret2)
utiloosr_sqr = 1- (utilerrorsum/utilretsum)
comp1[30] = utiloosr_sqr


write.xlsx(comp1, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Classical Lasso OOS R-Squared.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)


##Multi-Reponse##
# The glmnet function actually allows for multiple response vectors simultaneously
cvmfit = cv.glmnet(train_data, train_industries, alpha = 1, family = "mgaussian")
plot(cvmfit)
multicoeff <- coef(cvmfit,s = "lambda.min")
multi.coeff <- multicoeff[["Beer"]]@i #since all industries share the same selected coefficients it does not matter which industry I choose to display the coefficients for
yhat <- predict(cvmfit, test_data, s = "lambda.min")
yhat1 <- yhat[,1:30,1]
predictdiff <- test_industries - yhat1
predictdiff2 = predictdiff^2
diffsum = sum(predictdiff2)
ret2 = test_industries^2
retsum = sum(ret2)
multioor-sqr = 1 - (diffsum/retsum)



#### Bayesian Lasso ####
bfood <- blasso(train_data, train_industries[,1], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
foodcoeff <- bfood[["beta"]] # Extract the coefficients
bfoodhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bfoodhdi[i,] = hdi(density(foodcoeff[,i]), credMass = 0.60) # Calculate the highest density region to determine which coefficients have been moved sufficiently far from 0
} # I used a credible mass of 0.60 because the 0.95% interval included 0 for all coefficients
bfoodvars <- test_data[,c(538,3633,4641,2523,1145,2003)] # The list of variables moved away from 0
bfoodbeta1 = c(1:6)
foodvar1 <- density(foodcoeff[,538])
plot(foodvar1)
foodvar1max <- which.max(density(foodcoeff[,538])$y) # Determines the x-axis location of the mode of the distribution
foodvar1maxval <- density(foodcoeff[,538])$x[foodvar1max] # Determines the coefficient value at the mode of the simulated distribution
bfoodbeta1[1] = foodvar1maxval
foodvar2 <- density(foodcoeff[,3633])
plot(foodvar2)
foodvar2max <- which.max(density(foodcoeff[,3633])$y)
foodvar2maxval <- density(foodcoeff[,3633])$x[foodvar2max]
bfoodbeta1[2] = foodvar2maxval
foodvar3 <- density(foodcoeff[,4641])
plot(foodvar3)
foodvar3max <- which.max(density(foodcoeff[,4641])$y)
foodvar3maxval <- density(foodcoeff[,4641])$x[foodvar3max]
bfoodbeta1[3] = foodvar3maxval
foodvar4 <- density(foodcoeff[,2523])
plot(foodvar4)
foodvar4max <- which.max(density(foodcoeff[,2523])$y)
foodvar4maxval <- density(foodcoeff[,2523])$x[foodvar4max]
bfoodbeta1[4] = foodvar4maxval
foodvar5 <- density(foodcoeff[,1145])
plot(foodvar5)
foodvar5max <- which.max(density(foodcoeff[,1145])$y)
foodvar5maxval <- density(foodcoeff[,1145])$x[foodvar5max]
bfoodbeta1[5] = foodvar5maxval
foodvar6 <- density(foodcoeff[,2003])
plot(foodvar6)
foodvar6max <- which.max(density(foodcoeff[,2003])$y)
foodvar6maxval <- density(foodcoeff[,2003])$x[foodvar6max]
bfoodbeta1[6] = foodvar6maxval
bfoodpred = t(bfoodbeta1%*%t(bfoodvars))
bfooddiff = bfoodpred - test_industries[,1]
bfooddiff2 = bfooddiff^2
pfoodpredsum = sum(bfooddiff2)
bfoodret2 = test_industries[,1]^2
bfoodretsum = sum(bfoodret2)
bfoodr_sqr = 1-(pfoodpredsum/bfoodretsum)
comp2[1] = bfoodr_sqr

# The above process is repeated for each of the 30 industry coefficients
bbeer <- blasso(train_data, train_industries[,2], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
beercoeff <- bbeer[["beta"]]
bbeerhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bbeerhdi[i,] = hdi(density(beercoeff[,i]), credMass = 0.60)
}
bbeervars <- test_data[,c(4505,51,4499)]
bbeerbeta1 = c(1:3)
beervar1 <- density(beercoeff[,4505])
plot(beervar1)
beervar1max <- which.max(density(beercoeff[,4505])$y)
beervar1maxval <- density(beercoeff[,4505])$x[beervar1max]
bbeerbeta1[1] = beervar1maxval
beervar2 <- density(beercoeff[,51])
plot(beervar2)
beervar2max <- which.max(density(beercoeff[,51])$y)
beervar2maxval <- density(beercoeff[,51])$x[beervar2max]
bbeerbeta1[2] = beervar2maxval
beervar3 <- density(beercoeff[,4499])
plot(beervar3)
beervar3max <- which.max(density(beercoeff[,4499])$y)
beervar3maxval <- density(beercoeff[,4499])$x[beervar3max]
bbeerbeta1[3] = beervar3maxval
bbeerpred = t(bbeerbeta1%*%t(bbeervars))
bbeerdiff = bbeerpred - test_industries[,2]
bbeerdiff2 = bbeerdiff^2
bbeerpredsum = sum(bbeerdiff2)
bbeerret2 = test_industries[,2]^2
bbeerretsum = sum(bbeerret2)
bbeerr_sqr = 1-(pbeerpredsum/bbeerretsum)
comp2[2] = bbeerr_sqr


bsmok <- blasso(train_data, train_industries[,3], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
smokcoeff <- bsmok[["beta"]]
bsmokhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bsmokhdi[i,] = hdi(density(smokcoeff[,i]), credMass = 0.60)
}
bsmokvars <- test_data[,c(890,814,1478,2312)]
bsmokbeta1 = c(1:4)
smokvar1 <- density(smokcoeff[,890])
plot(smokvar1)
smokvar1max <- which.max(density(smokcoeff[,890])$y)
smokvar1maxval <- density(smokcoeff[,890])$x[smokvar1max]
bsmokbeta1[1] = smokvar1maxval
smokvar2 <- density(smokcoeff[,814])
plot(smokvar2)
smokvar2max <- which.max(density(smokcoeff[,814])$y)
smokvar2maxval <- density(smokcoeff[,814])$x[smokvar2max]
bsmokbeta1[2] = smokvar2maxval
smokvar3 <- density(smokcoeff[,1478])
plot(smokvar3)
smokvar3max <- which.max(density(smokcoeff[,1478])$y)
smokvar3maxval <- density(smokcoeff[,1478])$x[smokvar3max]
bsmokbeta1[3] = smokvar3maxval
smokvar4 <- density(smokcoeff[,2312])
plot(smokvar4)
smokvar4max <- which.max(density(smokcoeff[,2312])$y)
smokvar4maxval <- density(smokcoeff[,2312])$x[smokvar4max]
bsmokbeta1[4] = smokvar4maxval
bsmokpred = t(bsmokbeta1%*%t(bsmokvars))
bsmokdiff = bsmokpred - test_industries[,3]
bsmokdiff2 = bsmokdiff^2
bsmokpredsum = sum(bsmokdiff2)
bsmokret2 = test_industries[,3]^2
bsmokretsum = sum(bsmokret2)
bsmokr_sqr = 1-(bsmokpredsum/bsmokretsum)
comp2[3] = bsmokr_sqr


bgame <- blasso(train_data, train_industries[,4], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
gamecoeff <- bgame[["beta"]]
bgamehdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bgamehdi[i,] = hdi(density(gamecoeff[,i]), credMass = 0.60)
}
bgamevars <- test_data[,c(4110,3653,3711,4610,1543)]
bgamebeta1 = c(1:5)
gamevar1 <- density(gamecoeff[,4110])
plot(gamevar1)
gamevar1max <- which.max(density(gamecoeff[,4110])$y)
gamevar1maxval <- density(gamecoeff[,4110])$x[gamevar1max]
bgamebeta1[1] = gamevar1maxval
gamevar2 <- density(gamecoeff[,3653])
plot(gamevar2)
gamevar2max <- which.max(density(gamecoeff[,3653])$y)
gamevar2maxval <- density(gamecoeff[,3653])$x[gamevar2max]
bgamebeta1[2] = gamevar2maxval
gamevar3 <- density(gamecoeff[,3711])
plot(gamevar3)
gamevar3max <- which.max(density(gamecoeff[,3711])$y)
gamevar3maxval <- density(gamecoeff[,3711])$x[gamevar3max]
bgamebeta1[3] = gamevar3maxval
gamevar4 <- density(gamecoeff[,4610])
plot(gamevar4)
gamevar4max <- which.max(density(gamecoeff[,4610])$y)
gamevar4maxval <- density(gamecoeff[,4610])$x[gamevar4max]
bgamebeta1[4] = gamevar4maxval
gamevar5 <- density(gamecoeff[,1543])
plot(gamevar5)
gamevar5max <- which.max(density(gamecoeff[,1543])$y)
gamevar5maxval <- density(gamecoeff[,1543])$x[gamevar5max]
bgamebeta1[5] = gamevar5maxval
bgamepred = t(bgamebeta1%*%t(bgamevars))
bgamediff = bgamepred - test_industries[,4]
bgamediff2 = bgamediff^2
bgamepredsum = sum(bgamediff2)
bgameret2 = test_industries[,4]^2
bgameretsum = sum(bgameret2)
bgamer_sqr = 1-(bgamepredsum/bgameretsum)
comp2[4] = bgamer_sqr


bbook <- blasso(train_data, train_industries[,5], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
bookcoeff <- bbook[["beta"]]
bbookhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bbookhdi[i,] = hdi(density(bookcoeff[,i]), credMass = 0.60)
}
bbookvars <- test_data[,c(3505,1214,1396,2410,3712,3678)]
bbookbeta1 = c(1:6)
bookvar1 <- density(bookcoeff[,3505])
plot(bookvar1)
bookvar1max <- which.max(density(bookcoeff[,3505])$y)
bookvar1maxval <- density(bookcoeff[,3505])$x[bookvar1max]
bbookbeta1[1] = bookvar1maxval
bookvar2 <- density(bookcoeff[,1214])
plot(bookvar2)
bookvar2max <- which.max(density(bookcoeff[,1214])$y)
bookvar2maxval <- density(bookcoeff[,1214])$x[bookvar2max]
bbookbeta1[2] = bookvar2maxval
bookvar3 <- density(bookcoeff[,1396])
plot(bookvar3)
bookvar3max <- which.max(density(bookcoeff[,1396])$y)
bookvar3maxval <- density(bookcoeff[,1396])$x[bookvar3max]
bbookbeta1[3] = bookvar3maxval
bookvar4 <- density(bookcoeff[,2410])
plot(bookvar4)
bookvar4max <- which.max(density(bookcoeff[,2410])$y)
bookvar4maxval <- density(bookcoeff[,2410])$x[bookvar4max]
bbookbeta1[4] = bookvar4maxval
bookvar5 <- density(bookcoeff[,3712])
plot(bookvar5)
bookvar5max <- which.max(density(bookcoeff[,3712])$y)
bookvar5maxval <- density(bookcoeff[,3712])$x[bookvar5max]
bbookbeta1[5] = bookvar5maxval
bookvar6 <- density(bookcoeff[,3678])
plot(bookvar6)
bookvar6max <- which.max(density(bookcoeff[,3678])$y)
bookvar6maxval <- density(bookcoeff[,3678])$x[bookvar6max]
bbookbeta1[6] = bookvar6maxval
bbookpred = t(bbookbeta1%*%t(bbookvars))
bbookdiff = bbookpred - test_industries[,5]
bbookdiff2 = bbookdiff^2
bbookpredsum = sum(bbookdiff2)
bbookret2 = test_industries[,5]^2
bbookretsum = sum(bbookret2)
bbookr_sqr = 1-(bbookpredsum/bbookretsum)
comp2[5] = bbookr_sqr


bhous <- blasso(train_data, train_industries[,6], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
houscoeff <- bhous[["beta"]]
bhoushdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bhoushdi[i,] = hdi(density(houscoeff[,i]), credMass = 0.60)
}
bhousvars <- test_data[,c(3425,1736)]
bhousbeta1 = c(1:2)
housvar1 <- density(houscoeff[,3425])
plot(housvar1)
housvar1max <- which.max(density(houscoeff[,3425])$y)
housvar1maxval <- density(houscoeff[,3425])$x[housvar1max]
bhousbeta1[1] = housvar1maxval
housvar2 <- density(houscoeff[,1736])
plot(housvar2)
housvar2max <- which.max(density(houscoeff[,1736])$y)
housvar2maxval <- density(houscoeff[,1736])$x[housvar2max]
bhousbeta1[2] = housvar2maxval
bhouspred = t(bhousbeta1%*%t(bhousvars))
bhousdiff = bhouspred - test_industries[,6]
bhousdiff2 = bhousdiff^2
bhouspredsum = sum(bhousdiff2)
bhousret2 = test_industries[,6]^2
bhousretsum = sum(bhousret2)
bhousr_sqr = 1-(bhouspredsum/bhousretsum)
comp2[6] = bhousr_sqr


bclot <- blasso(train_data, train_industries[,7], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
clotcoeff <- bclot[["beta"]]
bclothdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bclothdi[i,] = hdi(density(clotcoeff[,i]), credMass = 0.60)
}
bclotvars <- test_data[,c(4173,2908,3641)]
bclotbeta1 = c(1:3)
clotvar1 <- density(clotcoeff[,4173])
plot(clotvar1)
clotvar1max <- which.max(density(clotcoeff[,4173])$y)
clotvar1maxval <- density(clotcoeff[,4173])$x[clotvar1max]
bclotbeta1[1] = clotvar1maxval
clotvar2 <- density(clotcoeff[,2908])
plot(clotvar2)
clotvar2max <- which.max(density(clotcoeff[,2908])$y)
clotvar2maxval <- density(clotcoeff[,2908])$x[clotvar2max]
bclotbeta1[2] = clotvar2maxval
clotvar3 <- density(clotcoeff[,3641])
plot(clotvar3)
clotvar3max <- which.max(density(clotcoeff[,3641])$y)
clotvar3maxval <- density(clotcoeff[,3641])$x[clotvar3max]
bclotbeta1[3] = clotvar3maxval
bclotpred = t(bclotbeta1%*%t(bclotvars))
bclotdiff = bclotpred - test_industries[,7]
bclotdiff2 = bclotdiff^2
bclotpredsum = sum(bclotdiff2)
bclotret2 = test_industries[,7]^2
bclotretsum = sum(bclotret2)
bclotr_sqr = 1-(bclotpredsum/bclotretsum)
comp2[7] = bclotr_sqr


bheal <- blasso(train_data, train_industries[,8], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
healcoeff <- bheal[["beta"]]
bhealhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bhealhdi[i,] = hdi(density(healcoeff[,i]), credMass = 0.60)
}
bhealvars <- test_data[,c(900,4114,1232,2147,4273)]
bhealbeta1 = c(1:5)
healvar1 <- density(healcoeff[,900])
plot(healvar1)
healvar1max <- which.max(density(healcoeff[,900])$y)
healvar1maxval <- density(healcoeff[,900])$x[healvar1max]
bhealbeta1[1] = healvar1maxval
healvar2 <- density(healcoeff[,4114])
plot(healvar2)
healvar2max <- which.max(density(healcoeff[,4114])$y)
healvar2maxval <- density(healcoeff[,4114])$x[healvar2max]
bhealbeta1[2] = healvar2maxval
healvar3 <- density(healcoeff[,1232])
plot(healvar3)
healvar3max <- which.max(density(healcoeff[,1232])$y)
healvar3maxval <- density(healcoeff[,1232])$x[healvar3max]
bhealbeta1[3] = healvar3maxval
healvar4 <- density(healcoeff[,2147])
plot(healvar4)
healvar4max <- which.max(density(healcoeff[,2147])$y)
healvar4maxval <- density(healcoeff[,2147])$x[healvar4max]
bhealbeta1[4] = healvar4maxval
healvar5 <- density(healcoeff[,4273])
plot(healvar5)
healvar5max <- which.max(density(healcoeff[,4273])$y)
healvar5maxval <- density(healcoeff[,4273])$x[healvar5max]
bhealbeta1[5] = healvar5maxval
bhealpred = t(bhealbeta1%*%t(bhealvars))
bhealdiff = bhealpred - test_industries[,8]
bhealdiff2 = bhealdiff^2
bhealpredsum = sum(bhealdiff2)
bhealret2 = test_industries[,8]^2
bhealretsum = sum(bhealret2)
bhealr_sqr = 1-(bhealpredsum/bhealretsum)
comp2[8] = bhealr_sqr


bchem <- blasso(train_data, train_industries[,9], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
chemcoeff <- bchem[["beta"]]
bchemhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bchemhdi[i,] = hdi(density(chemcoeff[,i]), credMass = 0.60)
}
bchemvars <- test_data[,c(2954,2040,3169,3645,4654)]
bchembeta1 = c(1:5)
chemvar1 <- density(chemcoeff[,2954])
plot(chemvar1)
chemvar1max <- which.max(density(chemcoeff[,2954])$y)
chemvar1maxval <- density(chemcoeff[,2954])$x[chemvar1max]
bchembeta1[1] = chemvar1maxval
chemvar2 <- density(chemcoeff[,2040])
plot(chemvar2)
chemvar2max <- which.max(density(chemcoeff[,2040])$y)
chemvar2maxval <- density(chemcoeff[,2040])$x[chemvar2max]
bchembeta1[2] = chemvar2maxval
chemvar3 <- density(chemcoeff[,3169])
plot(chemvar3)
chemvar3max <- which.max(density(chemcoeff[,3169])$y)
chemvar3maxval <- density(chemcoeff[,3169])$x[chemvar3max]
bchembeta1[3] = chemvar3maxval
chemvar4 <- density(chemcoeff[,3645])
plot(chemvar4)
chemvar4max <- which.max(density(chemcoeff[,3645])$y)
chemvar4maxval <- density(chemcoeff[,3645])$x[chemvar4max]
bchembeta1[4] = chemvar4maxval
chemvar5 <- density(chemcoeff[,4654])
plot(chemvar5)
chemvar5max <- which.max(density(chemcoeff[,4654])$y)
chemvar5maxval <- density(chemcoeff[,4654])$x[chemvar5max]
bchembeta1[5] = chemvar5maxval
bchempred = t(bchembeta1%*%t(bchemvars))
bchemdiff = bchempred - test_industries[,9]
bchemdiff2 = bchemdiff^2
bchempredsum = sum(bchemdiff2)
bchemret2 = test_industries[,9]^2
bchemretsum = sum(bchemret2)
bchemr_sqr = 1-(bchempredsum/bchemretsum)
comp2[9] = bchemr_sqr


btext <- blasso(train_data, train_industries[,10], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
textcoeff <- btext[["beta"]]
btexthdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  btexthdi[i,] = hdi(density(textcoeff[,i]), credMass = 0.60)
}
btextvars <- test_data[,c(4176,4676,490)]
btextbeta1 = c(1:3)
textvar1 <- density(textcoeff[,4176])
plot(textvar1)
textvar1max <- which.max(density(textcoeff[,4176])$y)
textvar1maxval <- density(textcoeff[,4176])$x[textvar1max]
btextbeta1[1] = textvar1maxval
textvar2 <- density(textcoeff[,4676])
plot(textvar2)
textvar2max <- which.max(density(textcoeff[,4676])$y)
textvar2maxval <- density(textcoeff[,4676])$x[textvar2max]
btextbeta1[2] = textvar2maxval
textvar3 <- density(textcoeff[,490])
plot(textvar3)
textvar3max <- which.max(density(textcoeff[,490])$y)
textvar3maxval <- density(textcoeff[,490])$x[textvar3max]
btextbeta1[3] = textvar3maxval
btextpred = t(btextbeta1%*%t(btextvars))
btextdiff = btextpred - test_industries[,10]
btextdiff2 = btextdiff^2
btextpredsum = sum(btextdiff2)
btextret2 = test_industries[,10]^2
btextretsum = sum(btextret2)
btextr_sqr = 1-(btextpredsum/btextretsum)
comp2[10] = btextr_sqr


bcons <- blasso(train_data, train_industries[,11], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
conscoeff <- bcons[["beta"]]
bconshdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bconshdi[i,] = hdi(density(conscoeff[,i]), credMass = 0.60)
}
bconsvars <- test_data[,c(4632,1586,2697,3659,1289)]
bconsbeta1 = c(1:5)
consvar1 <- density(conscoeff[,4632])
plot(consvar1)
consvar1max <- which.max(density(conscoeff[,4632])$y)
consvar1maxval <- density(conscoeff[,4632])$x[consvar1max]
bconsbeta1[1] = consvar1maxval
consvar2 <- density(conscoeff[,1586])
plot(consvar2)
consvar2max <- which.max(density(conscoeff[,1586])$y)
consvar2maxval <- density(conscoeff[,1586])$x[consvar2max]
bconsbeta1[2] = consvar2maxval
consvar3 <- density(conscoeff[,2697])
plot(consvar3)
consvar3max <- which.max(density(conscoeff[,2697])$y)
consvar3maxval <- density(conscoeff[,2697])$x[consvar3max]
bconsbeta1[3] = consvar3maxval
consvar4 <- density(conscoeff[,3659])
plot(consvar4)
consvar4max <- which.max(density(conscoeff[,3659])$y)
consvar4maxval <- density(conscoeff[,3659])$x[consvar4max]
bconsbeta1[4] = consvar4maxval
consvar5 <- density(conscoeff[,1289])
plot(consvar5)
consvar5max <- which.max(density(conscoeff[,1289])$y)
consvar5maxval <- density(conscoeff[,1289])$x[consvar5max]
bconsbeta1[5] = consvar5maxval
bconspred = t(bconsbeta1%*%t(bconsvars))
bconsdiff = bconspred - test_industries[,11]
bconsdiff2 = bconsdiff^2
bconspredsum = sum(bconsdiff2)
bconsret2 = test_industries[,11]^2
bconsretsum = sum(bconsret2)
bconsr_sqr = 1-(bconspredsum/bconsretsum)
comp2[11] = bconsr_sqr


bstee <- blasso(train_data, train_industries[,12], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
steecoeff <- bstee[["beta"]]
bsteehdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bsteehdi[i,] = hdi(density(steecoeff[,i]), credMass = 0.60)
}
bsteevars <- test_data[,c(4118,3828,3615,3491,525,409,397)]
bsteebeta1 = c(1:7)
steevar1 <- density(steecoeff[,4118])
plot(steevar1)
steevar1max <- which.max(density(steecoeff[,4118])$y)
steevar1maxval <- density(steecoeff[,4118])$x[steevar1max]
bsteebeta1[1] = steevar1maxval
steevar2 <- density(steecoeff[,3828])
plot(steevar2)
steevar2max <- which.max(density(steecoeff[,3828])$y)
steevar2maxval <- density(steecoeff[,3828])$x[steevar2max]
bsteebeta1[2] = steevar2maxval
steevar3 <- density(steecoeff[,3615])
plot(steevar3)
steevar3max <- which.max(density(steecoeff[,3615])$y)
steevar3maxval <- density(steecoeff[,3615])$x[steevar3max]
bsteebeta1[3] = steevar3maxval
steevar4 <- density(steecoeff[,3491])
plot(steevar4)
steevar4max <- which.max(density(steecoeff[,3491])$y)
steevar4maxval <- density(steecoeff[,3491])$x[steevar4max]
bsteebeta1[4] = steevar4maxval
steevar5 <- density(steecoeff[,525])
plot(steevar5)
steevar5max <- which.max(density(steecoeff[,525])$y)
steevar5maxval <- density(steecoeff[,525])$x[steevar5max]
bsteebeta1[5] = steevar5maxval
steevar6 <- density(steecoeff[,409])
plot(steevar6)
steevar6max <- which.max(density(steecoeff[,409])$y)
steevar6maxval <- density(steecoeff[,409])$x[steevar6max]
bsteebeta1[6] = steevar6maxval
steevar7 <- density(steecoeff[,397])
plot(steevar7)
steevar7max <- which.max(density(steecoeff[,397])$y)
steevar7maxval <- density(steecoeff[,397])$x[steevar7max]
bsteebeta1[7] = steevar7maxval
bsteepred = t(bsteebeta1%*%t(bsteevars))
bsteediff = bsteepred - test_industries[,12]
bsteediff2 = bsteediff^2
bsteepredsum = sum(bsteediff2)
bsteeret2 = test_industries[,12]^2
bsteeretsum = sum(bsteeret2)
bsteer_sqr = 1-(bsteepredsum/bsteeretsum)
comp2[12] = bsteer_sqr


bfabp <- blasso(train_data, train_industries[,13], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
fabpcoeff <- bfabp[["beta"]]
bfabphdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bfabphdi[i,] = hdi(density(fabpcoeff[,i]), credMass = 0.60)
}
bfabpvars <- test_data[,c(3161,2737,1550,3099,3441,3423)]
bfabpbeta1 = c(1:6)
fabpvar1 <- density(fabpcoeff[,3161])
plot(fabpvar1)
fabpvar1max <- which.max(density(fabpcoeff[,3161])$y)
fabpvar1maxval <- density(fabpcoeff[,3161])$x[fabpvar1max]
bfabpbeta1[1] = fabpvar1maxval
fabpvar2 <- density(fabpcoeff[,2737])
plot(fabpvar2)
fabpvar2max <- which.max(density(fabpcoeff[,2737])$y)
fabpvar2maxval <- density(fabpcoeff[,2737])$x[fabpvar2max]
bfabpbeta1[2] = fabpvar2maxval
fabpvar3 <- density(fabpcoeff[,1550])
plot(fabpvar3)
fabpvar3max <- which.max(density(fabpcoeff[,1550])$y)
fabpvar3maxval <- density(fabpcoeff[,1550])$x[fabpvar3max]
bfabpbeta1[3] = fabpvar3maxval
fabpvar4 <- density(fabpcoeff[,3099])
plot(fabpvar4)
fabpvar4max <- which.max(density(fabpcoeff[,3099])$y)
fabpvar4maxval <- density(fabpcoeff[,3099])$x[fabpvar4max]
bfabpbeta1[4] = fabpvar4maxval
fabpvar5 <- density(fabpcoeff[,3441])
plot(fabpvar5)
fabpvar5max <- which.max(density(fabpcoeff[,3441])$y)
fabpvar5maxval <- density(fabpcoeff[,3441])$x[fabpvar5max]
bfabpbeta1[5] = fabpvar5maxval
fabpvar6 <- density(fabpcoeff[,3423])
plot(fabpvar6)
fabpvar6max <- which.max(density(fabpcoeff[,3423])$y)
fabpvar6maxval <- density(fabpcoeff[,3423])$x[fabpvar6max]
bfabpbeta1[6] = fabpvar6maxval
bfabppred = t(bfabpbeta1%*%t(bfabpvars))
bfabpdiff = bfabppred - test_industries[,13]
bfabpdiff2 = bfabpdiff^2
bfabppredsum = sum(bfabpdiff2)
bfabpret2 = test_industries[,13]^2
bfabpretsum = sum(bfabpret2)
bfabpr_sqr = 1-(bfabppredsum/bfabpretsum)
comp2[13] = bfabpr_sqr


belec <- blasso(train_data, train_industries[,14], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
eleccoeff <- belec[["beta"]]
belechdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  belechdi[i,] = hdi(density(eleccoeff[,i]), credMass = 0.60)
}
belecvars <- test_data[,c(2700,1232,4579,396,718)]
belecbeta1 = c(1:5)
elecvar1 <- density(eleccoeff[,2700])
plot(elecvar1)
elecvar1max <- which.max(density(eleccoeff[,2700])$y)
elecvar1maxval <- density(eleccoeff[,2700])$x[elecvar1max]
belecbeta1[1] = elecvar1maxval
elecvar2 <- density(eleccoeff[,1232])
plot(elecvar2)
elecvar2max <- which.max(density(eleccoeff[,1232])$y)
elecvar2maxval <- density(eleccoeff[,1232])$x[elecvar2max]
belecbeta1[2] = elecvar2maxval
elecvar3 <- density(eleccoeff[,4579])
plot(elecvar3)
elecvar3max <- which.max(density(eleccoeff[,4579])$y)
elecvar3maxval <- density(eleccoeff[,4579])$x[elecvar3max]
belecbeta1[3] = elecvar3maxval
elecvar4 <- density(eleccoeff[,396])
plot(elecvar4)
elecvar4max <- which.max(density(eleccoeff[,396])$y)
elecvar4maxval <- density(eleccoeff[,396])$x[elecvar4max]
belecbeta1[4] = elecvar4maxval
elecvar5 <- density(eleccoeff[,718])
plot(elecvar5)
elecvar5max <- which.max(density(eleccoeff[,718])$y)
elecvar5maxval <- density(eleccoeff[,718])$x[elecvar5max]
belecbeta1[5] = elecvar5maxval
belecpred = t(belecbeta1%*%t(belecvars))
belecdiff = belecpred - test_industries[,14]
belecdiff2 = belecdiff^2
belecpredsum = sum(belecdiff2)
belecret2 = test_industries[,14]^2
belecretsum = sum(belecret2)
belecr_sqr = 1-(belecpredsum/belecretsum)
comp2[14] = belecr_sqr


bauto <- blasso(train_data, train_industries[,15], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
autocoeff <- bauto[["beta"]]
bautohdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bautohdi[i,] = hdi(density(autocoeff[,i]), credMass = 0.60)
}
bautovars <- test_data[,c(914,2840,1824,2578,3477,54,2388,2203)]
bautobeta1 = c(1:8)
autovar1 <- density(autocoeff[,914])
plot(autovar1)
autovar1max <- which.max(density(autocoeff[,914])$y)
autovar1maxval <- density(autocoeff[,914])$x[autovar1max]
bautobeta1[1] = autovar1maxval
autovar2 <- density(autocoeff[,2840])
plot(autovar2)
autovar2max <- which.max(density(autocoeff[,2840])$y)
autovar2maxval <- density(autocoeff[,2840])$x[autovar2max]
bautobeta1[2] = autovar2maxval
autovar3 <- density(autocoeff[,1824])
plot(autovar3)
autovar3max <- which.max(density(autocoeff[,1824])$y)
autovar3maxval <- density(autocoeff[,1824])$x[autovar3max]
bautobeta1[3] = autovar3maxval
autovar4 <- density(autocoeff[,2578])
plot(autovar4)
autovar4max <- which.max(density(autocoeff[,2578])$y)
autovar4maxval <- density(autocoeff[,2578])$x[autovar4max]
bautobeta1[4] = autovar4maxval
autovar5 <- density(autocoeff[,3477])
plot(autovar5)
autovar5max <- which.max(density(autocoeff[,3477])$y)
autovar5maxval <- density(autocoeff[,3477])$x[autovar5max]
bautobeta1[5] = autovar5maxval
autovar6 <- density(autocoeff[,54])
plot(autovar6)
autovar6max <- which.max(density(autocoeff[,54])$y)
autovar6maxval <- density(autocoeff[,54])$x[autovar6max]
bautobeta1[6] = autovar6maxval
autovar7 <- density(autocoeff[,2388])
plot(autovar7)
autovar7max <- which.max(density(autocoeff[,2388])$y)
autovar7maxval <- density(autocoeff[,2388])$x[autovar7max]
bautobeta1[7] = autovar7maxval
autovar8 <- density(autocoeff[,2203])
plot(autovar8)
autovar8max <- which.max(density(autocoeff[,2203])$y)
autovar8maxval <- density(autocoeff[,2203])$x[autovar8max]
bautobeta1[8] = autovar8maxval
bautopred = t(bautobeta1%*%t(bautovars))
bautodiff = bautopred - test_industries[,15]
bautodiff2 = bautodiff^2
bautopredsum = sum(bautodiff2)
bautoret2 = test_industries[,15]^2
bautoretsum = sum(bautoret2)
bautor_sqr = 1-(bautopredsum/bautoretsum)
comp2[15] = bautor_sqr


bcarr <- blasso(train_data, train_industries[,16], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
carrcoeff <- bcarr[["beta"]]
bcarrhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bcarrhdi[i,] = hdi(density(carrcoeff[,i]), credMass = 0.60)
}
bcarrvars <- test_data[,c(4493)]
bcarrbeta1 = c(1)
carrvar1 <- density(carrcoeff[,4493])
plot(carrvar1)
carrvar1max <- which.max(density(carrcoeff[,4493])$y)
carrvar1maxval <- density(carrcoeff[,4493])$x[carrvar1max]
bcarrbeta1[1] = carrvar1maxval
bcarrpred = t(bcarrbeta1%*%t(bcarrvars))
bcarrdiff = bcarrpred - test_industries[,16]
bcarrdiff2 = bcarrdiff^2
bcarrpredsum = sum(bcarrdiff2)
bcarrret2 = test_industries[,16]^2
bcarrretsum = sum(bcarrret2)
bcarrr_sqr = 1-(bcarrpredsum/bcarrretsum)
comp2[16] = bcarrr_sqr


bmine <- blasso(train_data, train_industries[,17], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
minecoeff <- bmine[["beta"]]
bminehdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bminehdi[i,] = hdi(density(minecoeff[,i]), credMass = 0.60)
}
bminevars <- test_data[,c(4183,1733,4667,1713)]
bminebeta1 = c(1:4)
minevar1 <- density(minecoeff[,4183])
plot(minevar1)
minevar1max <- which.max(density(minecoeff[,4183])$y)
minevar1maxval <- density(minecoeff[,4183])$x[minevar1max]
bminebeta1[1] = minevar1maxval
minevar2 <- density(minecoeff[,1733])
plot(minevar2)
minevar2max <- which.max(density(minecoeff[,1733])$y)
minevar2maxval <- density(minecoeff[,1733])$x[minevar2max]
bminebeta1[2] = minevar2maxval
minevar3 <- density(minecoeff[,4667])
plot(minevar3)
minevar3max <- which.max(density(minecoeff[,4667])$y)
minevar3maxval <- density(minecoeff[,4667])$x[minevar3max]
bminebeta1[3] = minevar3maxval
minevar4 <- density(minecoeff[,1713])
plot(minevar4)
minevar4max <- which.max(density(minecoeff[,1713])$y)
minevar4maxval <- density(minecoeff[,1713])$x[minevar4max]
bminebeta1[4] = minevar4maxval
bminepred = t(bminebeta1%*%t(bminevars))
bminediff = bminepred - test_industries[,17]
bminediff2 = bminediff^2
bminepredsum = sum(bminediff2)
bmineret2 = test_industries[,17]^2
bmineretsum = sum(bmineret2)
bminer_sqr = 1-(bminepredsum/bmineretsum)
comp2[17] = bminer_sqr


bcoal <- blasso(train_data, train_industries[,18], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
coalcoeff <- bcoal[["beta"]]
bcoalhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bcoalhdi[i,] = hdi(density(coalcoeff[,i]), credMass = 0.60)
}
bcoalvars <- test_data[,c(585,1444,1116,1567,4024,2156,735,789,2462,195)]
bcoalbeta1 = c(1:10)
coalvar1 <- density(coalcoeff[,585])
plot(coalvar1)
coalvar1max <- which.max(density(coalcoeff[,585])$y)
coalvar1maxval <- density(coalcoeff[,585])$x[coalvar1max]
bcoalbeta1[1] = coalvar1maxval
coalvar2 <- density(coalcoeff[,1444])
plot(coalvar2)
coalvar2max <- which.max(density(coalcoeff[,1444])$y)
coalvar2maxval <- density(coalcoeff[,1444])$x[coalvar2max]
bcoalbeta1[2] = coalvar2maxval
coalvar3 <- density(coalcoeff[,1116])
plot(coalvar3)
coalvar3max <- which.max(density(coalcoeff[,1116])$y)
coalvar3maxval <- density(coalcoeff[,1116])$x[coalvar3max]
bcoalbeta1[3] = minevar3maxval
coalvar4 <- density(coalcoeff[,1567])
plot(coalvar4)
coalvar4max <- which.max(density(coalcoeff[,1567])$y)
coalvar4maxval <- density(coalcoeff[,1567])$x[coalvar4max]
bcoalbeta1[4] = coalvar4maxval
coalvar5 <- density(coalcoeff[,4024])
plot(coalvar5)
coalvar5max <- which.max(density(coalcoeff[,4024])$y)
coalvar5maxval <- density(coalcoeff[,4024])$x[coalvar5max]
bcoalbeta1[5] = coalvar5maxval
coalvar6 <- density(coalcoeff[,2156])
plot(coalvar6)
coalvar6max <- which.max(density(coalcoeff[,2156])$y)
coalvar6maxval <- density(coalcoeff[,2156])$x[coalvar6max]
bcoalbeta1[6] = coalvar6maxval
coalvar7 <- density(coalcoeff[,753])
plot(coalvar7)
coalvar7max <- which.max(density(coalcoeff[,753])$y)
coalvar7maxval <- density(coalcoeff[,753])$x[coalvar7max]
bcoalbeta1[7] = coalvar7maxval
coalvar8 <- density(coalcoeff[,789])
plot(coalvar8)
coalvar8max <- which.max(density(coalcoeff[,789])$y)
coalvar8maxval <- density(coalcoeff[,789])$x[coalvar8max]
bcoalbeta1[8] = coalvar8maxval
coalvar9 <- density(coalcoeff[,2462])
plot(coalvar9)
coalvar9max <- which.max(density(coalcoeff[,2462])$y)
coalvar9maxval <- density(coalcoeff[,2462])$x[coalvar9max]
bcoalbeta1[9] = coalvar9maxval
coalvar10 <- density(coalcoeff[,195])
plot(coalvar10)
coalvar10max <- which.max(density(coalcoeff[,195])$y)
coalvar10maxval <- density(coalcoeff[,195])$x[coalvar10max]
bcoalbeta1[10] = coalvar10maxval
bcoalpred = t(bcoalbeta1%*%t(bcoalvars))
bcoaldiff = bcoalpred - test_industries[,18]
bcoaldiff2 = bcoaldiff^2
bcoalpredsum = sum(bcoaldiff2)
bcoalret2 = test_industries[,18]^2
bcoalretsum = sum(bcoalret2)
bcoalr_sqr = 1-(bcoalpredsum/bcoalretsum)
comp2[18] = bcoalr_sqr


boil <- blasso(train_data, train_industries[,19], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
oilcoeff <- boil[["beta"]]
boilhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  boilhdi[i,] = hdi(density(oilcoeff[,i]), credMass = 0.60)
}
boilvars <- test_data[,c(2327,389)]
boilbeta1 = c(1:2)
oilvar1 <- density(oilcoeff[,2327])
plot(oilvar1)
oilvar1max <- which.max(density(oilcoeff[,2327])$y)
oilvar1maxval <- density(oilcoeff[,2327])$x[oilvar1max]
boilbeta1[1] = oilvar1maxval
oilvar2 <- density(oilcoeff[,389])
plot(oilvar2)
oilvar2max <- which.max(density(oilcoeff[,389])$y)
oilvar2maxval <- density(oilcoeff[,389])$x[oilvar2max]
boilbeta1[2] = oilvar2maxval
boilpred = t(boilbeta1%*%t(boilvars))
boildiff = boilpred - test_industries[,19]
boildiff2 = boildiff^2
boilpredsum = sum(boildiff2)
boilret2 = test_industries[,19]^2
boilretsum = sum(boilret2)
boilr_sqr = 1-(boilpredsum/boilretsum)
comp2[19] = boilr_sqr


btele <- blasso(train_data, train_industries[,20], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
telecoeff <- btele[["beta"]]
btelehdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  btelehdi[i,] = hdi(density(telecoeff[,i]), credMass = 0.60)
}
btelevars <- test_data[,c(926,939)]
btelebeta1 = c(1:2)
televar1 <- density(telecoeff[,926])
plot(televar1)
televar1max <- which.max(density(telecoeff[,926])$y)
televar1maxval <- density(telecoeff[,926])$x[televar1max]
btelebeta1[1] = televar1maxval
televar2 <- density(telecoeff[,939])
plot(televar2)
televar2max <- which.max(density(telecoeff[,939])$y)
televar2maxval <- density(telecoeff[,939])$x[televar2max]
btelebeta1[2] = televar2maxval
btelepred = t(btelebeta1%*%t(btelevars))
btelediff = btelepred - test_industries[,20]
btelediff2 = btelediff^2
btelepredsum = sum(btelediff2)
bteleret2 = test_industries[,20]^2
bteleretsum = sum(bteleret2)
bteler_sqr = 1-(btelepredsum/bteleretsum)
comp2[20] = bteler_sqr


bserv <- blasso(train_data, train_industries[,21], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
servcoeff <- bserv[["beta"]]
bservhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bservhdi[i,] = hdi(density(servcoeff[,i]), credMass = 0.60)
}
bservvars <- test_data[,c(435,4513,71,3572,83)]
bservbeta1 = c(1:5)
servvar1 <- density(servcoeff[,435])
plot(servvar1)
servvar1max <- which.max(density(servcoeff[,435])$y)
servvar1maxval <- density(servcoeff[,435])$x[servvar1max]
bservbeta1[1] = servvar1maxval
servvar2 <- density(servcoeff[,4513])
plot(servvar2)
servvar2max <- which.max(density(servcoeff[,4513])$y)
servvar2maxval <- density(servcoeff[,4513])$x[servvar2max]
bservbeta1[2] = servvar2maxval
servvar3 <- density(servcoeff[,71])
plot(servvar3)
servvar3max <- which.max(density(servcoeff[,71])$y)
servvar3maxval <- density(servcoeff[,71])$x[servvar3max]
bservbeta1[3] = servvar3maxval
servvar4 <- density(servcoeff[,3572])
plot(servvar4)
servvar4max <- which.max(density(servcoeff[,3572])$y)
servvar4maxval <- density(servcoeff[,3572])$x[servvar4max]
bservbeta1[4] = servvar4maxval
servvar5 <- density(servcoeff[,83])
plot(servvar5)
servvar5max <- which.max(density(servcoeff[,83])$y)
servvar5maxval <- density(servcoeff[,83])$x[servvar5max]
bservbeta1[5] = servvar5maxval
bservpred = t(bservbeta1%*%t(bservvars))
bservdiff = bservpred - test_industries[,21]
bservdiff2 = bservdiff^2
bservpredsum = sum(bservdiff2)
bservret2 = test_industries[,21]^2
bservretsum = sum(bservret2)
bservr_sqr = 1-(bservpredsum/bservretsum)
comp2[21] = bservr_sqr


bbusi <- blasso(train_data, train_industries[,22], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
busicoeff <- bbusi[["beta"]]
bbusihdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bbusihdi[i,] = hdi(density(busicoeff[,i]), credMass = 0.60)
}
bbusivars <- test_data[,c(2278,2043,3521,1633,2816,1269)]
bbusibeta1 = c(1:6)
busivar1 <- density(busicoeff[,2278])
plot(busivar1)
busivar1max <- which.max(density(busicoeff[,2278])$y)
busivar1maxval <- density(busicoeff[,2278])$x[busivar1max]
bbusibeta1[1] = busivar1maxval
busivar2 <- density(busicoeff[,2043])
plot(busivar2)
busivar2max <- which.max(density(busicoeff[,2043])$y)
busivar2maxval <- density(busicoeff[,2043])$x[busivar2max]
bbusibeta1[2] = busivar2maxval
busivar3 <- density(busicoeff[,3521])
plot(busivar3)
busivar3max <- which.max(density(busicoeff[,3521])$y)
busivar3maxval <- density(busicoeff[,3521])$x[busivar3max]
bbusibeta1[3] = busivar3maxval
busivar4 <- density(busicoeff[,1633])
plot(busivar4)
busivar4max <- which.max(density(busicoeff[,1633])$y)
busivar4maxval <- density(busicoeff[,1633])$x[busivar4max]
bbusibeta1[4] = busivar4maxval
busivar5 <- density(busicoeff[,2816])
plot(busivar5)
busivar5max <- which.max(density(busicoeff[,2816])$y)
busivar5maxval <- density(busicoeff[,2816])$x[busivar5max]
bbusibeta1[5] = busivar5maxval
busivar6 <- density(busicoeff[,1269])
plot(busivar6)
busivar6max <- which.max(density(busicoeff[,1269])$y)
busivar6maxval <- density(busicoeff[,1269])$x[busivar6max]
bbusibeta1[6] = busivar6maxval
bbusipred = t(bbusibeta1%*%t(bbusivars))
bbusidiff = bbusipred - test_industries[,22]
bbusidiff2 = bbusidiff^2
bbusipredsum = sum(bbusidiff2)
bbusiret2 = test_industries[,22]^2
bbusiretsum = sum(bbusiret2)
bbusir_sqr = 1-(bbusipredsum/bbusiretsum)
comp2[22] = bbusir_sqr


bpape <- blasso(train_data, train_industries[,23], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
papecoeff <- bpape[["beta"]]
bpapehdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bpapehdi[i,] = hdi(density(papecoeff[,i]), credMass = 0.60)
}
bpapevars <- test_data[,c(932,1780,2525)]
bpapebeta1 = c(1:3)
papevar1 <- density(papecoeff[,932])
plot(papevar1)
papevar1max <- which.max(density(papecoeff[,932])$y)
papevar1maxval <- density(papecoeff[,932])$x[papevar1max]
bpapebeta1[1] = papevar1maxval
papevar2 <- density(papecoeff[,1780])
plot(papevar2)
papevar2max <- which.max(density(papecoeff[,1780])$y)
papevar2maxval <- density(papecoeff[,1780])$x[papevar2max]
bpapebeta1[2] = papevar2maxval
papevar3 <- density(papecoeff[,2525])
plot(papevar3)
papevar3max <- which.max(density(papecoeff[,2525])$y)
papevar3maxval <- density(papecoeff[,2525])$x[papevar3max]
bpapebeta1[3] = papevar3maxval
bpapepred = t(bpapebeta1%*%t(bpapevars))
bpapediff = bpapepred - test_industries[,23]
bpapediff2 = bpapediff^2
bpapepredsum = sum(bpapediff2)
bpaperet2 = test_industries[,23]^2
bpaperetsum = sum(bpaperet2)
bpaper_sqr = 1-(bpapepredsum/bpaperetsum)
comp2[23] = bpaper_sqr


btran <- blasso(train_data, train_industries[,24], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
trancoeff <- btran[["beta"]]
btranhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  btranhdi[i,] = hdi(density(trancoeff[,i]), credMass = 0.60)
}
btranvars <- test_data[,c(4131,3255,3542,1729,3805,3751)]
btranbeta1 = c(1:6)
tranvar1 <- density(trancoeff[,4131])
plot(tranvar1)
tranvar1max <- which.max(density(trancoeff[,4131])$y)
tranvar1maxval <- density(trancoeff[,4131])$x[tranvar1max]
btranbeta1[1] = tranvar1maxval
tranvar2 <- density(trancoeff[,3255])
plot(tranvar2)
tranvar2max <- which.max(density(trancoeff[,3255])$y)
tranvar2maxval <- density(trancoeff[,3255])$x[tranvar2max]
btranbeta1[2] = tranvar2maxval
tranvar3 <- density(trancoeff[,3542])
plot(tranvar3)
tranvar3max <- which.max(density(trancoeff[,3542])$y)
tranvar3maxval <- density(trancoeff[,3542])$x[tranvar3max]
btranbeta1[3] = tranvar3maxval
tranvar4 <- density(trancoeff[,1729])
plot(tranvar4)
tranvar4max <- which.max(density(trancoeff[,1729])$y)
tranvar4maxval <- density(trancoeff[,1729])$x[tranvar4max]
btranbeta1[4] = tranvar4maxval
tranvar5 <- density(trancoeff[,3805])
plot(tranvar5)
tranvar5max <- which.max(density(trancoeff[,3805])$y)
tranvar5maxval <- density(trancoeff[,3805])$x[tranvar5max]
btranbeta1[5] = tranvar5maxval
tranvar6 <- density(trancoeff[,3751])
plot(tranvar6)
tranvar6max <- which.max(density(trancoeff[,3751])$y)
tranvar6maxval <- density(trancoeff[,3751])$x[tranvar6max]
btranbeta1[6] = tranvar6maxval
btranpred = t(btranbeta1%*%t(btranvars))
btrandiff = btranpred - test_industries[,24]
btrandiff2 = btrandiff^2
btranpredsum = sum(btrandiff2)
btranret2 = test_industries[,24]^2
btranretsum = sum(btranret2)
btranr_sqr = 1-(btranpredsum/btranretsum)
comp2[24] = btranr_sqr


bwhol <- blasso(train_data, train_industries[,25], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
wholcoeff <- bwhol[["beta"]]
bwholhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bwholhdi[i,] = hdi(density(wholcoeff[,i]), credMass = 0.60)
}
bwholvars <- test_data[,c(4117,87,4018)]
bwholbeta1 = c(1:3)
wholvar1 <- density(wholcoeff[,4117])
plot(wholvar1)
wholvar1max <- which.max(density(wholcoeff[,4117])$y)
wholvar1maxval <- density(wholcoeff[,4117])$x[wholvar1max]
bwholbeta1[1] = wholvar1maxval
wholvar2 <- density(wholcoeff[,87])
plot(wholvar2)
wholvar2max <- which.max(density(wholcoeff[,87])$y)
wholvar2maxval <- density(wholcoeff[,87])$x[wholvar2max]
bwholbeta1[2] = wholvar2maxval
wholvar3 <- density(wholcoeff[,4018])
plot(wholvar3)
wholvar3max <- which.max(density(wholcoeff[,4018])$y)
wholvar3maxval <- density(wholcoeff[,4018])$x[wholvar3max]
bwholbeta1[3] = wholvar3maxval
bwholpred = t(bwholbeta1%*%t(bwholvars))
bwholdiff = bwholpred - test_industries[,25]
bwholdiff2 = bwholdiff^2
bwholpredsum = sum(bwholdiff2)
bwholret2 = test_industries[,25]^2
bwholretsum = sum(bwholret2)
bwholr_sqr = 1-(bwholpredsum/bwholretsum)
comp2[25] = bwholr_sqr


breta <- blasso(train_data, train_industries[,26], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
retacoeff <- breta[["beta"]]
bretahdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bretahdi[i,] = hdi(density(retacoeff[,i]), credMass = 0.60)
}
bretavars <- test_data[,c(938,2312,3557,4603,809)]
bretabeta1 = c(1:5)
retavar1 <- density(retacoeff[,938])
plot(retavar1)
retavar1max <- which.max(density(retacoeff[,938])$y)
retavar1maxval <- density(retacoeff[,938])$x[retavar1max]
bretabeta1[1] = retavar1maxval
retavar2 <- density(retacoeff[,2312])
plot(retavar2)
retavar2max <- which.max(density(retacoeff[,2312])$y)
retavar2maxval <- density(retacoeff[,2312])$x[retavar2max]
bretabeta1[2] = retavar2maxval
retavar3 <- density(retacoeff[,3557])
plot(retavar3)
retavar3max <- which.max(density(retacoeff[,3557])$y)
retavar3maxval <- density(retacoeff[,3557])$x[retavar3max]
bretabeta1[3] = retavar3maxval
retavar4 <- density(retacoeff[,4603])
plot(retavar4)
retavar4max <- which.max(density(retacoeff[,4603])$y)
retavar4maxval <- density(retacoeff[,4603])$x[retavar4max]
bretabeta1[4] = retavar4maxval
retavar5 <- density(retacoeff[,809])
plot(retavar5)
retavar5max <- which.max(density(retacoeff[,809])$y)
retavar5maxval <- density(retacoeff[,809])$x[retavar5max]
bretabeta1[5] = retavar5maxval
bretapred = t(bretabeta1%*%t(bretavars))
bretadiff = bretapred - test_industries[,26]
bretadiff2 = bretadiff^2
bretapredsum = sum(bretadiff2)
bretaret2 = test_industries[,26]^2
bretaretsum = sum(bretaret2)
bretar_sqr = 1-(bretapredsum/bretaretsum)
comp2[26] = bretar_sqr


bmeal <- blasso(train_data, train_industries[,27], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
mealcoeff <- bmeal[["beta"]]
bmealhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bmealhdi[i,] = hdi(density(mealcoeff[,i]), credMass = 0.60)
}
bmealvars <- test_data[,c(3615,1826,798,4517)]
bmealbeta1 = c(1:4)
mealvar1 <- density(mealcoeff[,3615])
plot(mealvar1)
mealvar1max <- which.max(density(mealcoeff[,3615])$y)
mealvar1maxval <- density(mealcoeff[,3615])$x[mealvar1max]
bmealbeta1[1] = mealvar1maxval
mealvar2 <- density(mealcoeff[,1826])
plot(mealvar2)
mealvar2max <- which.max(density(mealcoeff[,1826])$y)
mealvar2maxval <- density(mealcoeff[,1826])$x[mealvar2max]
bmealbeta1[2] = mealvar2maxval
mealvar3 <- density(mealcoeff[,798])
plot(mealvar3)
mealvar3max <- which.max(density(mealcoeff[,798])$y)
mealvar3maxval <- density(mealcoeff[,798])$x[mealvar3max]
bmealbeta1[3] = mealvar3maxval
mealvar4 <- density(mealcoeff[,4517])
plot(mealvar4)
mealvar4max <- which.max(density(mealcoeff[,4517])$y)
mealvar4maxval <- density(mealcoeff[,4517])$x[mealvar4max]
bmealbeta1[4] = mealvar4maxval
bmealpred = t(bmealbeta1%*%t(bmealvars))
bmealdiff = bmealpred - test_industries[,27]
bmealdiff2 = bmealdiff^2
bmealpredsum = sum(bmealdiff2)
bmealret2 = test_industries[,27]^2
bmealretsum = sum(bmealret2)
bmealr_sqr = 1-(bmealpredsum/bmealretsum)
comp2[27] = bmealr_sqr


bothe <- blasso(train_data, train_industries[,28], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
othecoeff <- bothe[["beta"]]
bothehdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bothehdi[i,] = hdi(density(othecoeff[,i]), credMass = 0.60)
}
bothevars <- test_data[,c(2920,3461,1376,2886,287)]
bothebeta1 = c(1:5)
othevar1 <- density(othecoeff[,2920])
plot(othevar1)
othevar1max <- which.max(density(othecoeff[,2920])$y)
othevar1maxval <- density(othecoeff[,2920])$x[othevar1max]
bothebeta1[1] = othevar1maxval
othevar2 <- density(othecoeff[,3461])
plot(othevar2)
othevar2max <- which.max(density(othecoeff[,3461])$y)
othevar2maxval <- density(othecoeff[,3461])$x[othevar2max]
bothebeta1[2] = othevar2maxval
othevar3 <- density(othecoeff[,1376])
plot(othevar3)
othevar3max <- which.max(density(othecoeff[,1376])$y)
othevar3maxval <- density(othecoeff[,1376])$x[othevar3max]
bothebeta1[3] = othevar3maxval
othevar4 <- density(othecoeff[,2886])
plot(othevar4)
othevar4max <- which.max(density(othecoeff[,2886])$y)
othevar4maxval <- density(othecoeff[,2886])$x[othevar4max]
bothebeta1[4] = othevar4maxval
othevar5 <- density(othecoeff[,287])
plot(othevar5)
othevar5max <- which.max(density(othecoeff[,287])$y)
othevar5maxval <- density(othecoeff[,287])$x[othevar5max]
bothebeta1[5] = othevar5maxval
bothepred = t(bothebeta1%*%t(bothevars))
bothediff = bothepred - test_industries[,28]
bothediff2 = bothediff^2
bothepredsum = sum(bothediff2)
botheret2 = test_industries[,28]^2
botheretsum = sum(botheret2)
bother_sqr = 1-(bothepredsum/botheretsum)
comp2[28] = bother_sqr


bfina <- blasso(train_data, train_industries[,29], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
finacoeff <- bfina[["beta"]]
bfinahdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bfinahdi[i,] = hdi(density(finacoeff[,i]), credMass = 0.60)
}
bfinavars <- test_data[,c(3504)]
bfinabeta1 = c(1)
finavar1 <- density(finacoeff[,3504])
plot(finavar1)
finavar1max <- which.max(density(finacoeff[,3504])$y)
finavar1maxval <- density(finacoeff[,3504])$x[finavar1max]
bfinabeta1[1] = finavar1maxval
bfinapred = t(bfinabeta1%*%t(bfinavars))
bfinadiff = bfinapred - test_industries[,29]
bfinadiff2 = bfinadiff^2
bfinapredsum = sum(bfinadiff2)
bfinaret2 = test_industries[,29]^2
bfinaretsum = sum(bfinaret2)
bfinar_sqr = 1-(bfinapredsum/bfinaretsum)
comp2[29] = bfinar_sqr


butil <- blasso(train_data, train_industries[,30], T=10000, thin=500, RJ=TRUE, beta= rep(0, ncol(train_data)), lambda2 = 0.1)
utilcoeff <- butil[["beta"]]
butilhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  butilhdi[i,] = hdi(density(utilcoeff[,i]), credMass = 0.60)
}
butilvars <- test_data[,c(4126,924,2704,4107,2692,2828)]
butilbeta1 = c(1:6)
utilvar1 <- density(utilcoeff[,4126])
plot(utilvar1)
utilvar1max <- which.max(density(utilcoeff[,4126])$y)
utilvar1maxval <- density(utilcoeff[,4126])$x[utilvar1max]
butilbeta1[1] = utilvar1maxval
utilvar2 <- density(utilcoeff[,924])
plot(utilvar2)
utilvar2max <- which.max(density(utilcoeff[,924])$y)
utilvar2maxval <- density(utilcoeff[,924])$x[utilvar2max]
butilbeta1[2] = utilvar2maxval
utilvar3 <- density(utilcoeff[,2704])
plot(utilvar3)
utilvar3max <- which.max(density(utilcoeff[,2704])$y)
utilvar3maxval <- density(utilcoeff[,2704])$x[utilvar3max]
butilbeta1[3] = utilvar3maxval
utilvar4 <- density(utilcoeff[,4107])
plot(utilvar4)
utilvar4max <- which.max(density(utilcoeff[,4107])$y)
utilvar4maxval <- density(utilcoeff[,4107])$x[utilvar4max]
butilbeta1[4] = utilvar4maxval
utilvar5 <- density(utilcoeff[,2692])
plot(utilvar5)
utilvar5max <- which.max(density(utilcoeff[,2692])$y)
utilvar5maxval <- density(utilcoeff[,2692])$x[utilvar5max]
butilbeta1[5] = utilvar5maxval
utilvar6 <- density(utilcoeff[,2828])
plot(utilvar6)
utilvar6max <- which.max(density(utilcoeff[,2828])$y)
utilvar6maxval <- density(utilcoeff[,2828])$x[utilvar6max]
butilbeta1[6] = utilvar6maxval
butilpred = t(butilbeta1%*%t(butilvars))
butildiff = butilpred - test_industries[,30]
butildiff2 = butildiff^2
butilpredsum = sum(butildiff2)
butilret2 = test_industries[,30]^2
butilretsum = sum(butilret2)
butilr_sqr = 1-(butilpredsum/butilretsum)
comp2[30] = butilr_sqr


write.xlsx(comp2, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Bayesian Lasso OOS R-Squared.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)


#### Bayesian Variable Selection ####
# With multiple response vectors simultaneously
# Create empty arrays to store simulated and calculated variables
betamat <- array(rep(NaN, 2000*4676*30), c(2000, 4676, 30)) # number of simulations times number of variables times number of portfolios
sig2 <- rinvgamma(1, shape = 0.1, rate = .0005, scale = 2000) # This is simulated once and applied to all variables and response vectors
deltamat <- matrix(c(1:9352000), nrow = 2000) # number of simulations times number of variables
Rmat <- array(rep(NaN, 45*4676*30), c(45, 4676, 30)) # number of observations times number of variables times number of portfolios
rmat <- array(rep(NaN, 45*4676*30), c(45, 4676, 30)) # number of observations times number of variables times number of portfolios
sigstarmat <- array(rep(NaN, 45*4676*30), c(45, 4676, 30)) # number of observations times number of variables times number of portfolios
resmat <- array(rep(NaN, 2000*4676*30), c(2000, 4676, 30)) # number of simulations times number of variables times number of portfolios
Zmat <- array(rep(NaN, 45*4676*30), c(45, 4676, 30)) # number of observations times number of variables times number of portfolios
Zmat1 <- matrix(c(1:210420), nrow = 45) # number of observations times number of variables
tau2 <- 20 # This is assumed constant and the same for each variable and response vector

# Simuated parameters using the method of Algorithm 1 in Chen et al. (2014)
# See that paper for further details of the equations
for (j in 1:4676){
  for (m in 1:30){
    Rmat[,j,m] <- train_industries[,m] #because we start with the null model
    rmat[,j,m] <- (t(train_industries[,m])%*%train_data[,j]*tau2)/(sig2[1]+t(train_data[,j]%*%train_data[,j]*tau2))
    sigstarmat[,j,m] <- (sig2*tau2)/(t(train_data[,j])%*%train_data[,j]*tau2)
    Zmat[,j,m] <- sqrt(sigstarmat[,j,m]/tau2)*exp(rmat[,j,m]/(2*sigstarmat[,j,m]))
  }
}
Zmat1 <- apply(Zmat, c(1,2), prod)
Zmat2 <- Zmat1[1,]
for (j in 1:4676){
  for (k in 1:2000){
    deltamat[k,j] <- rbern(1,(0.5*Zmat2[j])/(0.5*Zmat2[j] + 0.5))
  }
}
deltamean <- colMeans(deltamat)
write.xlsx(deltamean, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Simulated Indicators.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
for (j in 1:4676){
  for (m in 1:30){
    for (k in 1:2000){
      betamat[k,j,m] <- rnorm(1,sqrt(rmat[,j,m]),sigstarmat[,j,m])
    }
  }
}

# Separate each of the 30 portfolio coefficients into their own dataframes and replace any possible NAN values
betafood <- betamat[,,1]
betafood[is.na(betafood)] <- 0
betabeer <- betamat[,,2]
betabeer[is.na(betabeer)] <- 0
betasmok <- betamat[,,3]
betasmok[is.na(betasmok)] <- 0
betagame <- betamat[,,4]
betagame[is.na(betagame)] <- 0
betabook <- betamat[,,5]
betabook[is.na(betabook)] <- 0
betahous <- betamat[,,6]
betahous[is.na(betahous)] <- 0
betaclot <- betamat[,,7]
betaclot[is.na(betaclot)] <- 0
betaheal <- betamat[,,8]
betaheal[is.na(betaheal)] <- 0
betachem <- betamat[,,9]
betachem[is.na(betachem)] <- 0
betatext <- betamat[,,10]
betatext[is.na(betatext)] <- 0
betacons <- betamat[,,11]
betacons[is.na(betacons)] <- 0
betastee <- betamat[,,12]
betastee[is.na(betastee)] <- 0
betafabp <- betamat[,,13]
betafabp[is.na(betafabp)] <- 0
betaelec <- betamat[,,14]
betaelec[is.na(betaelec)] <- 0
betaauto <- betamat[,,15]
betaauto[is.na(betaauto)] <- 0
betacarr <- betamat[,,16]
betacarr[is.na(betacarr)] <- 0
betamine <- betamat[,,17]
betamine[is.na(betamine)] <- 0
betacoal <- betamat[,,18]
betacoal[is.na(betacoal)] <- 0
betaoil <- betamat[,,19]
betaoil[is.na(betaoil)] <- 0
betatele <- betamat[,,20]
betatele[is.na(betatele)] <- 0
betaserv <- betamat[,,21]
betaserv[is.na(betaserv)] <- 0
betabusi <- betamat[,,22]
betabusi[is.na(betabusi)] <- 0
betapape <- betamat[,,23]
betapape[is.na(betapape)] <- 0
betatran <- betamat[,,24]
betatran[is.na(betatran)] <- 0
betawhol <- betamat[,,25]
betawhol[is.na(betawhol)] <- 0
betareta <- betamat[,,26]
betareta[is.na(betareta)] <- 0
betameal <- betamat[,,27]
betameal[is.na(betameal)] <- 0
betaothe <- betamat[,,28]
betaothe[is.na(betaothe)] <- 0
betafina <- betamat[,,29]
betafina[is.na(betafina)] <- 0
betautil <- betamat[,,30]
betautil[is.na(betautil)] <- 0

# As in the single response vector case, use the highest density region to determine which coefficients have moved sufficiently far from zero
bfoodhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bfoodhdi[i,] = hdi(density(betafood[,i]), credMass = 0.95)
}
write.xlsx(bfoodhdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\FoodHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
# Repeast this process for each of the 30 industry portfolios
bbeerhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bbeerhdi[i,] = hdi(density(betabeer[,i]), credMass = 0.95)
}
write.xlsx(bbeerhdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\BeerHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bsmokhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bsmokhdi[i,] = hdi(density(betasmok[,i]), credMass = 0.95)
}
write.xlsx(bsmokhdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\SmokeHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bbookhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bbookhdi[i,] = hdi(density(betabook[,i]), credMass = 0.95)
}
write.xlsx(bbookhdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\BooksHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bhoushdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bhoushdi[i,] = hdi(density(betahous[,i]), credMass = 0.95)
}
write.xlsx(bhoushdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\HouseholdHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bclothdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bclothdi[i,] = hdi(density(betaclot[,i]), credMass = 0.95)
}
write.xlsx(bclothdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\ClothingHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bhealhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bhealhdi[i,] = hdi(density(betaheal[,i]), credMass = 0.95)
}
write.xlsx(bhealhdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\HealthHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bchemhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bchemhdi[i,] = hdi(density(betachem[,i]), credMass = 0.95)
}
write.xlsx(bchemhdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\ChemicalsHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

btexthdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  btexthdi[i,] = hdi(density(betatext[,i]), credMass = 0.95)
}
write.xlsx(btexthdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\TextilesHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bconshdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bconshdi[i,] = hdi(density(betacons[,i]), credMass = 0.95)
}
write.xlsx(bconshdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\ConstructionHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bsteehdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bsteehdi[i,] = hdi(density(betastee[,i]), credMass = 0.95)
}
write.xlsx(bsteehdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\SteelHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bfabphdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bfabphdi[i,] = hdi(density(betafabp[,i]), credMass = 0.95)
}
write.xlsx(bfabphdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\FabricatedProductsHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

belechdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  belechdi[i,] = hdi(density(betaelec[,i]), credMass = 0.95)
}
write.xlsx(belechdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\ElectricalEquipmentHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bautohdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bautohdi[i,] = hdi(density(betaauto[,i]), credMass = 0.95)
}
write.xlsx(bautohdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\AutomobileHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bcarrhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bcarrhdi[i,] = hdi(density(betacarr[,i]), credMass = 0.95)
}
write.xlsx(bcarrhdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\CarryHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bminehdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bminehdi[i,] = hdi(density(betamine[,i]), credMass = 0.95)
}
write.xlsx(bminehdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\MiningHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bcoalhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bcoalhdi[i,] = hdi(density(betacoal[,i]), credMass = 0.95)
}
write.xlsx(bcoalhdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\CoalHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

boilhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  boilhdi[i,] = hdi(density(betaoil[,i]), credMass = 0.95)
}
write.xlsx(boilhdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\OilHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

btelehdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  btelehdi[i,] = hdi(density(betatele[,i]), credMass = 0.95)
}
write.xlsx(btelehdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\TelecommunicationsHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bservhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bservhdi[i,] = hdi(density(betaserv[,i]), credMass = 0.95)
}
write.xlsx(bservhdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\ServicesHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bbusihdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bbusihdi[i,] = hdi(density(betabusi[,i]), credMass = 0.95)
}
write.xlsx(bbusihdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\BusinessEquipmentHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bpapehdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bpapehdi[i,] = hdi(density(betapape[,i]), credMass = 0.95)
}
write.xlsx(bpapehdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\PaperHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

btranhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  btranhdi[i,] = hdi(density(betatran[,i]), credMass = 0.95)
}
write.xlsx(btranhdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\TransportationHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bwholhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bwholhdi[i,] = hdi(density(betawhol[,i]), credMass = 0.95)
}
write.xlsx(bwholhdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\WholesaleTradeHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bretahdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bretahdi[i,] = hdi(density(betareta[,i]), credMass = 0.95)
}
write.xlsx(bretahdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\RetailTradeHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bmealhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bmealhdi[i,] = hdi(density(betameal[,i]), credMass = 0.95)
}
write.xlsx(bmealhdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\MealsHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bothehdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bothehdi[i,] = hdi(density(betaothe[,i]), credMass = 0.95)
}
write.xlsx(bothehdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\OtherHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

bfinahdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  bfinahdi[i,] = hdi(density(betafina[,i]), credMass = 0.95)
}
write.xlsx(bfinahdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\FinanceHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

butilhdi <- matrix(c(1:9352), nrow = 4676)
for (i in 1:4676){
  butilhdi[i,] = hdi(density(betautil[,i]), credMass = 0.95)
}
write.xlsx(butilhdi, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\UtilitiesHDI Variables.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

# Create the vector of independent variables used for the predictions
multibayesianvars <- test_data[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                                  205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                                  1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                                  1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                                  1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                                  1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                                  1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                                  2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                                  2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                                  3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                                  4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                                  4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                                  4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                                  4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                                  4577)]

# Create matrix of coefficients used for predictions
betafoodpred <- betafood[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                           205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                           1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                           1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                           1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                           1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                           1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                           2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                           2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                           3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                           4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                           4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                           4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                           4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                           4577)]
foodbetacoeff <- c(1:190)
foodbetamax <- c(1:190)
for (i in 1:190){
  foodbetamax[i] <- which.max(density(betafoodpred[,i])$y) # Determines the x-axis location of the mode of the distribution
  foodbetacoeff[i] <- density(betafoodpred[,i])$x[foodbetamax[i]] # Determines the coefficient value at the mode of the simulated distribution
}
foodbetacoeff <- as.data.frame(foodbetacoeff)
food.pred = t(foodbetacoeff)%*%t(multibayesianvars) # Prediction
food.diff = food.pred - test_industries[,1] # Calculate difference between the prediction and the actual value
food.diff2 = food.diff^2
food.sum = sum(food.diff2)
foodret2 = test_industries[,1]^2
foodretsum = sum(foodret2)
food.r_sqr = 1-(food.sum/foodretsum) # Calculate the R-squared
comp3[1] = food.r_sqr

# Repeat this process for all 30 industry portfolios
betabeerpred <- betabeer[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
beerbetacoeff <- c(1:190)
beerbetamax <- c(1:190)
for (i in 1:190){
  beerbetamax[i] <- which.max(density(betabeerpred[,i])$y)
  beerbetacoeff[i] <- density(betabeerpred[,i])$x[beerbetamax[i]]
}
beerbetacoeff <- as.data.frame(beerbetacoeff)
beer.pred = t(beerbetacoeff)%*%t(multibayesianvars)
beer.pred = t(beer.pred)
beer.diff = beer.pred - test_industries[,2]
beer.diff2 = beer.diff^2
beer.sum = sum(beer.diff2)
beerret2 = test_industries[,2]^2
beerretsum = sum(beerret2)
beer.r_sqr = 1-(beer.sum/beerretsum)
comp3[2] = beer.r_sqr


betasmokpred <- betasmok[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
smokbetacoeff <- c(1:190)
smokbetamax <- c(1:190)
for (i in 1:190){
  smokbetamax[i] <- which.max(density(betasmokpred[,i])$y)
  smokbetacoeff[i] <- density(betasmokpred[,i])$x[smokbetamax[i]]
}
smokbetacoeff <- as.data.frame(smokbetacoeff)
smok.pred = t(smokbetacoeff)%*%t(multibayesianvars)
smok.pred = t(smok.pred)
smok.diff = smok.pred - test_industries[,3]
smok.diff2 = smok.diff^2
smok.sum = sum(smok.diff2)
smokret2 = test_industries[,3]^2
smokretsum = sum(smokret2)
smok.r_sqr = 1-(smok.sum/smokretsum)
comp3[3] = smok.r_sqr


betagamepred <- betagame[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
gamebetacoeff <- c(1:190)
gamebetamax <- c(1:190)
for (i in 1:190){
  gamebetamax[i] <- which.max(density(betagamepred[,i])$y)
  gamebetacoeff[i] <- density(betagamepred[,i])$x[gamebetamax[i]]
}
gamebetacoeff <- as.data.frame(gamebetacoeff)
game.pred = t(gamebetacoeff)%*%t(multibayesianvars)
game.pred = t(game.pred)
game.diff = game.pred - test_industries[,4]
game.diff2 = game.diff^2
game.sum = sum(game.diff2)
gameret2 = test_industries[,4]^2
gameretsum = sum(gameret2)
game.r_sqr = 1-(game.sum/gameretsum)
comp3[4] = game.r_sqr


betabookpred <- betabook[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
bookbetacoeff <- c(1:190)
bookbetamax <- c(1:190)
for (i in 1:190){
  bookbetamax[i] <- which.max(density(betabookpred[,i])$y)
  bookbetacoeff[i] <- density(betabookpred[,i])$x[bookbetamax[i]]
}
bookbetacoeff <- as.data.frame(bookbetacoeff)
book.pred = t(bookbetacoeff)%*%t(multibayesianvars)
book.pred = t(book.pred)
book.diff = book.pred - test_industries[,5]
book.diff2 = book.diff^2
book.sum = sum(book.diff2)
bookret2 = test_industries[,5]^2
bookretsum = sum(bookret2)
book.r_sqr = 1-(book.sum/bookretsum)
comp3[5] = book.r_sqr


betahouspred <- betahous[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
housbetacoeff <- c(1:190)
housbetamax <- c(1:190)
for (i in 1:190){
  housbetamax[i] <- which.max(density(betahouspred[,i])$y)
  housbetacoeff[i] <- density(betahouspred[,i])$x[housbetamax[i]]
}
housbetacoeff <- as.data.frame(housbetacoeff)
hous.pred = t(housbetacoeff)%*%t(multibayesianvars)
hous.pred = t(hous.pred)
hous.diff = hous.pred - test_industries[,6]
hous.diff2 = hous.diff^2
hous.sum = sum(hous.diff2)
housret2 = test_industries[,6]^2
housretsum = sum(housret2)
hous.r_sqr = 1-(hous.sum/housretsum)
comp3[6] = hous.r_sqr


betaclotpred <- betaclot[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
clotbetacoeff <- c(1:190)
clotbetamax <- c(1:190)
for (i in 1:190){
  clotbetamax[i] <- which.max(density(betaclotpred[,i])$y)
  clotbetacoeff[i] <- density(betaclotpred[,i])$x[clotbetamax[i]]
}
clotbetacoeff <- as.data.frame(clotbetacoeff)
clot.pred = t(clotbetacoeff)%*%t(multibayesianvars)
clot.pred = t(clot.pred)
clot.diff = clot.pred - test_industries[,7]
clot.diff2 = clot.diff^2
clot.sum = sum(clot.diff2)
clotret2 = test_industries[,7]^2
clotretsum = sum(clotret2)
clot.r_sqr = 1-(clot.sum/clotretsum)
comp3[7] = clot.r_sqr


betahealpred <- betaheal[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
healbetacoeff <- c(1:190)
healbetamax <- c(1:190)
for (i in 1:190){
  healbetamax[i] <- which.max(density(betahealpred[,i])$y)
  healbetacoeff[i] <- density(betahealpred[,i])$x[healbetamax[i]]
}
healbetacoeff <- as.data.frame(healbetacoeff)
heal.pred = t(healbetacoeff)%*%t(multibayesianvars)
heal.pred = t(heal.pred)
heal.diff = heal.pred - test_industries[,8]
heal.diff2 = heal.diff^2
heal.sum = sum(heal.diff2)
healret2 = test_industries[,8]^2
healretsum = sum(healret2)
heal.r_sqr = 1-(heal.sum/healretsum)
comp3[8] = heal.r_sqr


betachempred <- betachem[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
chembetacoeff <- c(1:190)
chembetamax <- c(1:190)
for (i in 1:190){
  chembetamax[i] <- which.max(density(betachempred[,i])$y)
  chembetacoeff[i] <- density(betachempred[,i])$x[chembetamax[i]]
}
chembetacoeff <- as.data.frame(chembetacoeff)
chem.pred = t(chembetacoeff)%*%t(multibayesianvars)
chem.pred = t(chem.pred)
chem.diff = chem.pred - test_industries[,9]
chem.diff2 = chem.diff^2
chem.sum = sum(chem.diff2)
chemret2 = test_industries[,9]^2
chemretsum = sum(chemret2)
chem.r_sqr = 1-(chem.sum/chemretsum)
comp3[9] = chem.r_sqr


betatextpred <- betatext[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
textbetacoeff <- c(1:190)
textbetamax <- c(1:190)
for (i in 1:190){
  textbetamax[i] <- which.max(density(betatextpred[,i])$y)
  textbetacoeff[i] <- density(betatextpred[,i])$x[textbetamax[i]]
}
textbetacoeff <- as.data.frame(textbetacoeff)
text.pred = t(textbetacoeff)%*%t(multibayesianvars)
text.pred = t(text.pred)
text.diff = text.pred - test_industries[,10]
text.diff2 = text.diff^2
text.sum = sum(text.diff2)
textret2 = test_industries[,10]^2
textretsum = sum(textret2)
text.r_sqr = 1-(text.sum/textretsum)
comp3[10] = text.r_sqr


betaconspred <- betacons[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
consbetacoeff <- c(1:190)
consbetamax <- c(1:190)
for (i in 1:190){
  consbetamax[i] <- which.max(density(betaconspred[,i])$y)
  consbetacoeff[i] <- density(betaconspred[,i])$x[consbetamax[i]]
}
consbetacoeff <- as.data.frame(consbetacoeff)
cons.pred = t(consbetacoeff)%*%t(multibayesianvars)
cons.pred = t(cons.pred)
cons.diff = cons.pred - test_industries[,11]
cons.diff2 = cons.diff^2
cons.sum = sum(cons.diff2)
consret2 = test_industries[,11]^2
consretsum = sum(consret2)
cons.r_sqr = 1-(cons.sum/consretsum)
comp3[11] = cons.r_sqr


betasteepred <- betastee[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
steebetacoeff <- c(1:190)
steebetamax <- c(1:190)
for (i in 1:190){
  steebetamax[i] <- which.max(density(betasteepred[,i])$y)
  steebetacoeff[i] <- density(betasteepred[,i])$x[steebetamax[i]]
}
steebetacoeff <- as.data.frame(steebetacoeff)
stee.pred = t(steebetacoeff)%*%t(multibayesianvars)
stee.pred = t(stee.pred)
stee.diff = stee.pred - test_industries[,12]
stee.diff2 = stee.diff^2
stee.sum = sum(stee.diff2)
steeret2 = test_industries[,12]^2
steeretsum = sum(steeret2)
stee.r_sqr = 1-(stee.sum/steeretsum)
comp3[12] = stee.r_sqr


betafabppred <- betafabp[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
fabpbetacoeff <- c(1:190)
fabpbetamax <- c(1:190)
for (i in 1:190){
  fabpbetamax[i] <- which.max(density(betafabppred[,i])$y)
  fabpbetacoeff[i] <- density(betafabppred[,i])$x[fabpbetamax[i]]
}
fabpbetacoeff <- as.data.frame(fabpbetacoeff)
fabp.pred = t(fabpbetacoeff)%*%t(multibayesianvars)
fabp.pred = t(fabp.pred)
fabp.diff = fabp.pred - test_industries[,13]
fabp.diff2 = fabp.diff^2
fabp.sum = sum(fabp.diff2)
fabpret2 = test_industries[,13]^2
fabpretsum = sum(fabpret2)
fabp.r_sqr = 1-(fabp.sum/fabpretsum)
comp3[13] = fabp.r_sqr


betaelecpred <- betaelec[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
elecbetacoeff <- c(1:190)
elecbetamax <- c(1:190)
for (i in 1:190){
  elecbetamax[i] <- which.max(density(betaelecpred[,i])$y)
  elecbetacoeff[i] <- density(betaelecpred[,i])$x[elecbetamax[i]]
}
elecbetacoeff <- as.data.frame(elecbetacoeff)
elec.pred = t(elecbetacoeff)%*%t(multibayesianvars)
elec.pred = t(elec.pred)
elec.diff = elec.pred - test_industries[,14]
elec.diff2 = elec.diff^2
elec.sum = sum(elec.diff2)
elecret2 = test_industries[,14]^2
elecretsum = sum(elecret2)
elec.r_sqr = 1-(elec.sum/elecretsum)
comp3[14] = elec.r_sqr


betaautopred <- betaauto[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
autobetacoeff <- c(1:190)
autobetamax <- c(1:190)
for (i in 1:190){
  autobetamax[i] <- which.max(density(betaautopred[,i])$y)
  autobetacoeff[i] <- density(betaautopred[,i])$x[autobetamax[i]]
}
autobetacoeff <- as.data.frame(autobetacoeff)
auto.pred = t(autobetacoeff)%*%t(multibayesianvars)
auto.pred = t(auto.pred)
auto.diff = auto.pred - test_industries[,15]
auto.diff2 = auto.diff^2
auto.sum = sum(auto.diff2)
autoret2 = test_industries[,15]^2
autoretsum = sum(autoret2)
auto.r_sqr = 1-(auto.sum/autoretsum)
comp3[15] = auto.r_sqr


betacarrpred <- betacarr[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
carrbetacoeff <- c(1:190)
carrbetamax <- c(1:190)
for (i in 1:190){
  carrbetamax[i] <- which.max(density(betacarrpred[,i])$y)
  carrbetacoeff[i] <- density(betacarrpred[,i])$x[carrbetamax[i]]
}
carrbetacoeff <- as.data.frame(carrbetacoeff)
carr.pred = t(carrbetacoeff)%*%t(multibayesianvars)
carr.pred = t(carr.pred)
carr.diff = carr.pred - test_industries[,16]
carr.diff2 = carr.diff^2
carr.sum = sum(carr.diff2)
carrret2 = test_industries[,16]^2
carrretsum = sum(carrret2)
carr.r_sqr = 1-(carr.sum/carrretsum)
comp3[16] = carr.r_sqr


betaminepred <- betamine[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
minebetacoeff <- c(1:190)
minebetamax <- c(1:190)
for (i in 1:190){
  minebetamax[i] <- which.max(density(betaminepred[,i])$y)
  minebetacoeff[i] <- density(betaminepred[,i])$x[minebetamax[i]]
}
minebetacoeff <- as.data.frame(minebetacoeff)
mine.pred = t(minebetacoeff)%*%t(multibayesianvars)
mine.pred = t(mine.pred)
mine.diff = mine.pred - test_industries[,17]
mine.diff2 = mine.diff^2
mine.sum = sum(mine.diff2)
mineret2 = test_industries[,17]^2
mineretsum = sum(mineret2)
mine.r_sqr = 1-(mine.sum/mineretsum)
comp3[17] = mine.r_sqr


betacoalpred <- betacoal[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
coalbetacoeff <- c(1:190)
coalbetamax <- c(1:190)
for (i in 1:190){
  coalbetamax[i] <- which.max(density(betacoalpred[,i])$y)
  coalbetacoeff[i] <- density(betacoalpred[,i])$x[coalbetamax[i]]
}
coalbetacoeff <- as.data.frame(coalbetacoeff)
coal.pred = t(coalbetacoeff)%*%t(multibayesianvars)
coal.pred = t(coal.pred)
coal.diff = coal.pred - test_industries[,18]
coal.diff2 = coal.diff^2
coal.sum = sum(coal.diff2)
coalret2 = test_industries[,18]^2
coalretsum = sum(coalret2)
coal.r_sqr = 1-(coal.sum/coalretsum)
comp3[18] = coal.r_sqr


betaoilpred <- betaoil[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
oilbetacoeff <- c(1:190)
oilbetamax <- c(1:190)
for (i in 1:190){
  oilbetamax[i] <- which.max(density(betaoilpred[,i])$y)
  oilbetacoeff[i] <- density(betaoilpred[,i])$x[oilbetamax[i]]
}
oilbetacoeff <- as.data.frame(oilbetacoeff)
oil.pred = t(oilbetacoeff)%*%t(multibayesianvars)
oil.pred = t(oil.pred)
oil.diff = oil.pred - test_industries[,19]
oil.diff2 = oil.diff^2
oil.sum = sum(oil.diff2)
oilret2 = test_industries[,19]^2
oilretsum = sum(oilret2)
oil.r_sqr = 1-(oil.sum/oilretsum)
comp3[19] = oil.r_sqr


betatelepred <- betatele[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                          205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                          1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                          1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                          1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                          1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                          1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                          2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                          2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                          3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                          4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                          4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                          4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                          4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                          4577)]
telebetacoeff <- c(1:190)
telebetamax <- c(1:190)
for (i in 1:190){
  telebetamax[i] <- which.max(density(betatelepred[,i])$y)
  telebetacoeff[i] <- density(betatelepred[,i])$x[telebetamax[i]]
}
telebetacoeff <- as.data.frame(telebetacoeff)
tele.pred = t(telebetacoeff)%*%t(multibayesianvars)
tele.pred = t(tele.pred)
tele.diff = tele.pred - test_industries[,20]
tele.diff2 = tele.diff^2
tele.sum = sum(tele.diff2)
teleret2 = test_industries[,20]^2
teleretsum = sum(teleret2)
tele.r_sqr = 1-(tele.sum/teleretsum)
comp3[20] = tele.r_sqr


betaservpred <- betaserv[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
servbetacoeff <- c(1:190)
servbetamax <- c(1:190)
for (i in 1:190){
  servbetamax[i] <- which.max(density(betaservpred[,i])$y)
  servbetacoeff[i] <- density(betaservpred[,i])$x[servbetamax[i]]
}
servbetacoeff <- as.data.frame(servbetacoeff)
serv.pred = t(servbetacoeff)%*%t(multibayesianvars)
serv.pred = t(serv.pred)
serv.diff = serv.pred - test_industries[,21]
serv.diff2 = serv.diff^2
serv.sum = sum(serv.diff2)
servret2 = test_industries[,21]^2
servretsum = sum(servret2)
serv.r_sqr = 1-(serv.sum/servretsum)
comp3[21] = serv.r_sqr


betabusipred <- betabusi[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
busibetacoeff <- c(1:190)
busibetamax <- c(1:190)
for (i in 1:190){
  busibetamax[i] <- which.max(density(betabusipred[,i])$y)
  busibetacoeff[i] <- density(betabusipred[,i])$x[busibetamax[i]]
}
busibetacoeff <- as.data.frame(busibetacoeff)
busi.pred = t(busibetacoeff)%*%t(multibayesianvars)
busi.pred = t(busi.pred)
busi.diff = busi.pred - test_industries[,22]
busi.diff2 = busi.diff^2
busi.sum = sum(busi.diff2)
busiret2 = test_industries[,22]^2
busiretsum = sum(busiret2)
busi.r_sqr = 1-(busi.sum/busiretsum)
comp3[22] = busi.r_sqr


betapapepred <- betapape[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
papebetacoeff <- c(1:190)
papebetamax <- c(1:190)
for (i in 1:190){
  papebetamax[i] <- which.max(density(betapapepred[,i])$y)
  papebetacoeff[i] <- density(betapapepred[,i])$x[papebetamax[i]]
}
papebetacoeff <- as.data.frame(papebetacoeff)
pape.pred = t(papebetacoeff)%*%t(multibayesianvars)
pape.pred = t(pape.pred)
pape.diff = pape.pred - test_industries[,23]
pape.diff2 = pape.diff^2
pape.sum = sum(pape.diff2)
paperet2 = test_industries[,23]^2
paperetsum = sum(paperet2)
pape.r_sqr = 1-(pape.sum/paperetsum)
comp3[23] = pape.r_sqr


betatranpred <- betatran[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
tranbetacoeff <- c(1:190)
tranbetamax <- c(1:190)
for (i in 1:190){
  tranbetamax[i] <- which.max(density(betatranpred[,i])$y)
  tranbetacoeff[i] <- density(betatranpred[,i])$x[tranbetamax[i]]
}
tranbetacoeff <- as.data.frame(tranbetacoeff)
tran.pred = t(tranbetacoeff)%*%t(multibayesianvars)
tran.pred = t(tran.pred)
tran.diff = tran.pred - test_industries[,24]
tran.diff2 = tran.diff^2
tran.sum = sum(tran.diff2)
tranret2 = test_industries[,24]^2
tranretsum = sum(tranret2)
tran.r_sqr = 1-(tran.sum/tranretsum)
comp3[24] = tran.r_sqr


betawholpred <- betawhol[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
wholbetacoeff <- c(1:190)
wholbetamax <- c(1:190)
for (i in 1:190){
  wholbetamax[i] <- which.max(density(betawholpred[,i])$y)
  wholbetacoeff[i] <- density(betawholpred[,i])$x[wholbetamax[i]]
}
wholbetacoeff <- as.data.frame(wholbetacoeff)
whol.pred = t(wholbetacoeff)%*%t(multibayesianvars)
whol.pred = t(whol.pred)
whol.diff = whol.pred - test_industries[,25]
whol.diff2 = whol.diff^2
whol.sum = sum(whol.diff2)
wholret2 = test_industries[,25]^2
wholretsum = sum(wholret2)
whol.r_sqr = 1-(whol.sum/wholretsum)
comp3[25] = whol.r_sqr


betaretapred <- betareta[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
retabetacoeff <- c(1:190)
retabetamax <- c(1:190)
for (i in 1:190){
  retabetamax[i] <- which.max(density(betaretapred[,i])$y)
  retabetacoeff[i] <- density(betaretapred[,i])$x[retabetamax[i]]
}
retabetacoeff <- as.data.frame(retabetacoeff)
reta.pred = t(retabetacoeff)%*%t(multibayesianvars)
reta.pred = t(reta.pred)
reta.diff = reta.pred - test_industries[,26]
reta.diff2 = reta.diff^2
reta.sum = sum(reta.diff2)
retaret2 = test_industries[,26]^2
retaretsum = sum(retaret2)
reta.r_sqr = 1-(reta.sum/retaretsum)
comp3[26] = reta.r_sqr


betamealpred <- betameal[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
mealbetacoeff <- c(1:190)
mealbetamax <- c(1:190)
for (i in 1:190){
  mealbetamax[i] <- which.max(density(betamealpred[,i])$y)
  mealbetacoeff[i] <- density(betamealpred[,i])$x[mealbetamax[i]]
}
mealbetacoeff <- as.data.frame(mealbetacoeff)
meal.pred = t(mealbetacoeff)%*%t(multibayesianvars)
meal.pred = t(meal.pred)
meal.diff = meal.pred - test_industries[,27]
meal.diff2 = meal.diff^2
meal.sum = sum(meal.diff2)
mealret2 = test_industries[,27]^2
mealretsum = sum(mealret2)
meal.r_sqr = 1-(meal.sum/mealretsum)
comp3[27] = meal.r_sqr


betaothepred <- betaothe[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
othebetacoeff <- c(1:190)
othebetamax <- c(1:190)
for (i in 1:190){
  othebetamax[i] <- which.max(density(betaothepred[,i])$y)
  othebetacoeff[i] <- density(betaothepred[,i])$x[othebetamax[i]]
}
othebetacoeff <- as.data.frame(othebetacoeff)
othe.pred = t(othebetacoeff)%*%t(multibayesianvars)
othe.pred = t(othe.pred)
othe.diff = othe.pred - test_industries[,28]
othe.diff2 = othe.diff^2
othe.sum = sum(othe.diff2)
otheret2 = test_industries[,28]^2
otheretsum = sum(otheret2)
othe.r_sqr = 1-(othe.sum/otheretsum)
comp3[28] = othe.r_sqr


betafinapred <- betafina[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
finabetacoeff <- c(1:190)
finabetamax <- c(1:190)
for (i in 1:190){
  finabetamax[i] <- which.max(density(betafinapred[,i])$y)
  finabetacoeff[i] <- density(betafinapred[,i])$x[finabetamax[i]]
}
finabetacoeff <- as.data.frame(finabetacoeff)
fina.pred = t(finabetacoeff)%*%t(multibayesianvars)
fina.pred = t(fina.pred)
fina.diff = fina.pred - test_industries[,29]
fina.diff2 = fina.diff^2
fina.sum = sum(fina.diff2)
finaret2 = test_industries[,29]^2
finaretsum = sum(finaret2)
fina.r_sqr = 1-(fina.sum/finaretsum)
comp3[29] = fina.r_sqr


betautilpred <- betautil[,c(1,13,15,45,85,105,129,145,165,169,171,179,181,189,191,193,195,203,
                            205,259,433,459,509,571,769,993,997,1001,1007,1029,1033,1067,1069,
                            1073,1083,1125,1127,1131,1133,1135,1137,1143,1145,1147,1151,1153,
                            1155,1157,1171,1181,1219,1263,1487,1489,1493,1497,1531,1539,1543,
                            1545,1551,1561,1565,1567,1569,1575,1591,1593,1595,1623,1639,1663,
                            1673,1683,1691,1697,1719,1723,1803,1819,1893,1967,1969,1973,1977,
                            1979,1985,1989,1991,1995,2011,2015,2019,2025,2027,2029,2035,2041,
                            2045,2049,2055,2071,2073,2075,2173,2185,2187,2189,2193,2197,2201,
                            2536,2691,3436,3440,3446,3452,3456,3459,3724,3736,3737,3738,3739,
                            3791,3793,3796,3797,3798,3799,4077,4084,4087,4089,4095,4096,4097,
                            4098,4099,4103,4137,4144,4145,4147,4149,4155,4156,4157,4158,4159,
                            4160,4161,4162,4163,4198,4199,4200,4201,4202,4203,4207,4208,4210,
                            4212,4221,4222,4224,4226,4258,4260,4261,4263,4270,4272,4282,4284,
                            4286,4442,4445,4449,4451,4452,4455,4457,4458,4463,4505,4513,4576,
                            4577)]
utilbetacoeff <- c(1:190)
utilbetamax <- c(1:190)
for (i in 1:190){
  utilbetamax[i] <- which.max(density(betautilpred[,i])$y)
  utilbetacoeff[i] <- density(betautilpred[,i])$x[utilbetamax[i]]
}
utilbetacoeff <- as.data.frame(utilbetacoeff)
util.pred = t(utilbetacoeff)%*%t(multibayesianvars)
util.pred = t(util.pred)
util.diff = util.pred - test_industries[,30]
util.diff2 = util.diff^2
util.sum = sum(util.diff2)
utilret2 = test_industries[,30]^2
utilretsum = sum(utilret2)
util.r_sqr = 1-(util.sum/utilretsum)
comp3[30] = util.r_sqr

write.xlsx(comp3, file = "C:\\Users\\Jesse Work PC\\Documents\\Rutgers\\Academic_Documents\\Seminar in Econometrics\\Term Paper\\Output Data\\Multiple-Response Bayesian OOS R-Squared.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)
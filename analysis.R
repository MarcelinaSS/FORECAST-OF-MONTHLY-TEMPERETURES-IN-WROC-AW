#wczytanie danych
temp <- read.table("C:/Users/marce/OneDrive/Pulpit/SRUDIA/pakiety/temp.txt", header=FALSE)

#szereg
temp.ts=ts(temp, start = c(2009,1),frequency=12)
plot(temp.ts,xlab = "Czas", ylab = "Temperatura w stopniach Celsujsza")

#podzia� na zbi�r ucz�cy i testowy
temp.train <- window(temp.ts, end = c(2016, 12)) 
temp.test <- window(temp.ts, start = c(2017, 1))

#wykres dla danych ucz�cych
plot(temp.train, xlab = "Czas", ylab = "Temperatura w stopniach Celsujsza")

library(forecast)

#identyfikacja waha� sezonowych
mon = monthplot(temp.train, xlab = "miesi�ce", ylab = "Temperatura w stopniach Celsujsza")
season = seasonplot(temp.train, col = rainbow(8), year.labels = TRUE, xlab = "miesi�ce", ylab = "Temperatura w stopniach Celsujsza")

#identyfikacja autokorelacji
lag.plot(temp.train, do.lines = FALSE, lags = 12)
acf = Acf(temp.train)
pcf = Pacf(temp.train)

#dekompozycja
temp.train.de = decompose(temp.train)
plot(temp.train.de, xlab = "czas")

#weryfikacja opoznien po dekompozycji
temp.train.de = decompose(temp.train)$random
temp.train.de = na.omit(temp.train.de)
lag.plot(temp.train.de, do.lines = FALSE, lags = 12)

tsdisplay(temp.train.de)

#sprawdzenie czy wyst�puj� dane odstaj�ce
tsoutliers(temp.train)

#korekta kalendarzowa
temp.train.k = temp.train*(365.25/12)/monthdays(temp.train)
ts.plot(temp.train, temp.train.k, col=c("red","blue")) 
temp.train = temp.train*(365.25/12)/monthdays(temp.train)


#usuni�cie trendu i sezonowo�ci
temp.train.diff = diff(temp.train)
temp.train.diff.diff12 = diff(temp.train.diff, lag=12)

par(mfrow = c(3,1))
ts.plot(temp.train, ylab="oryginalny szereg", xlab = "czas")
ts.plot(temp.train.diff, ylab="I r�nicowanie (op�nienie 1)", xlab = "czas")
ts.plot(temp.train.diff.diff12, ylab="II r�nicowanie (op�nienie 12)", xlab = "czas")

tsdisplay(temp.train.diff.diff12)
lag.plot(temp.train.diff.diff12, do.lines = FALSE, lags = 12)

"por�wnanie por�wna� metod� dekompozycji klasycznej na podstawie ruchomej �redniej
z dekompozycj� na podstawie modelu regresji oraz z r�nicowaniem."

temp.train.tslm = tslm(temp.train ~ trend + season)
temp.train.tslm.res = residuals(temp.train.tslm)

par(mfrow = c(3,1))
Acf(temp.train.de, main=paste("szerego po dekompozycji klasycznej"))
Acf(temp.train.diff.diff12, main=paste("szereg po r�nicowaniu"))
Acf(temp.train.tslm.res, main=paste("szerego po dekompozycji na podstawie modelu regresji"))

par(mfrow = c(3,1))
Pacf(temp.train.de, main=paste("szerego po dekompozycji klasycznej"))
Pacf(temp.train.diff.diff12, main=paste("szereg po r�nicowaniu"))
Pacf(temp.train.tslm.res, main=paste("szerego po dekompozycji na podstawie modelu regresji"))

#dopasowanie modelu - dekompozycje tak samo
#MA
model1.MA = Arima(temp.train, order = c(0,1,15), seasonal = c(0,1,0))
model2.MA = Arima(temp.train, order = c(0,1,12), seasonal = c(0,1,0))

summary(model1.MA)
summary(model2.MA)

#AR

model1.AR = Arima(temp.train, order = c(15,1,0), seasonal = c(0,1,0))
model2.AR = Arima(temp.train, order = c(11,1,0), seasonal = c(0,1,0))

summary(model1.AR)
summary(model2.AR)

#auto
model = auto.arima(temp.train)
summary(model)

#residua dla modeli

tsdisplay(residuals(model1.MA))
tsdisplay(residuals(model2.MA))
tsdisplay(residuals(model1.AR))
tsdisplay(residuals(model2.AR))
tsdisplay(residuals(model))
#prognoza
x.zakres = c(2016, 2020)
y.zakres = c(-15, 35)

prognoza.auto = forecast(model, h=36)
plot(prognoza.auto, xlim = x.zakres, ylim = y.zakres)
lines(temp.test, col ="red", lty=2)

prognoza.ma1 = forecast(model1.MA, h=36)
plot(prognoza.ma1, xlim = x.zakres, ylim = y.zakres)
lines(temp.test, col ="red", lty=2)

prognoza.ma2 = forecast(model2.MA, h=36)
plot(prognoza.ma2, xlim = x.zakres, ylim = y.zakres)
lines(temp.test, col ="red", lty=2)

prognoza.ar1 = forecast(model1.AR, h=36)
plot(prognoza.ar1, xlim = x.zakres, ylim = y.zakres)
lines(temp.test, col ="red", lty=2)

prognoza.ar2 = forecast(model2.AR, h=36)
plot(prognoza.ar2, xlim = x.zakres, ylim = y.zakres)
lines(temp.test, col ="red", lty=2)

#por�wnanie prognoz z zbiorem testowym

kryteria = c("RMSE", "MAE", "MAPE", "MASE")
accuracy(prognoza.ma1, temp.test)[, kryteria]
accuracy(prognoza.ma2, temp.test)[, kryteria]
accuracy(prognoza.ar1, temp.test)[, kryteria]
accuracy(prognoza.ar2, temp.test)[, kryteria]
accuracy(prognoza.auto, temp.test)[, kryteria]


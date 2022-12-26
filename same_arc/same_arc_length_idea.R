# biblioteki / libraries ----
library(data.table)
library(ggplot2)
library(cowplot)
library(showtext)

# czcionki / fonts ----
font_add_google(name = "Montserrat", family = "montserrat", regular.wt = 400) # need package 'curl for this
font_add_google(name = "Quicksand", family = "quicksand", regular.wt = 400)
showtext_auto()

# funkcje / functions ----
# oblicza odleglosc miedzy dwoma punktami / calculating distance between two points
calc_dist <- function(p1, p2)
{
    sqrt( (p1[1] - p2[1]) ^ 2 + (p1[2] - p2[2]) ^ 2 )
}

# tworzy zbior punktow / creates dataset of points
tworz_zbior_bubles <- function(data, katStartowy = 1, dlugoscPozadana = 12, krokKatowy = .0125)
{
    liczbaDocelowaPunktow <- nrow(data) - 1
    
    ostatniPunkt <- c(katStartowy * cos(katStartowy), katStartowy * sin(katStartowy))
    zbiorPunktow <- data.table(x = ostatniPunkt[1], y = ostatniPunkt[2], t = katStartowy, odl = NA)
    odleglosciBiezacegoOdcinka <- c()
    biezacyKat <- katStartowy
    while (nrow(zbiorPunktow) < liczbaDocelowaPunktow + 1)
    {
        while (sum(odleglosciBiezacegoOdcinka) < dlugoscPozadana)
        {
            biezacyKat <- biezacyKat + krokKatowy / biezacyKat
            dodanyPunkt <- c(biezacyKat * cos(biezacyKat), biezacyKat * sin(biezacyKat))
            odleglosciBiezacegoOdcinka <- c(odleglosciBiezacegoOdcinka, calc_dist(ostatniPunkt, dodanyPunkt))
            ostatniPunkt <- dodanyPunkt
        }
        zbiorPunktow <- rbind(zbiorPunktow, data.table(x = ostatniPunkt[1], y = ostatniPunkt[2], t = biezacyKat, odl = sum(odleglosciBiezacegoOdcinka)))
        odleglosciBiezacegoOdcinka <- c()
    }
    
    
    zbiorPunktowPlot <- cbind(zbiorPunktow, data)
    zbiorPunktowPlot[, fill2 := cut(positive_rate, c(-Inf, seq(.05, .5, by = .05), Inf))]
    zbiorPunktowPlot[, ods := new_tests / max(new_tests, na.rm = T)]
    return(zbiorPunktowPlot)
}

# tworzy zbior reprezentujacy sciezke / creates dataset resembling for path
tworz_path <- function(data, lO = 1000)
{
    dataPlotDens2 <- data.table(t = seq(from = min(data$t), to = max(data$t), length.out = lO))
    dataPlotDens2[, x := t * cos(t)]
    dataPlotDens2[, y := t * sin(t)]
    
    wart <- unlist(lapply(X = 1:nrow(dataPlotDens2), FUN = function(i){
        tt <- dataPlotDens2[i]$t
        tett <- c(data$t[max(which(data$t <= tt))], data$t[min(which(data$t >= tt))])
        mean(data[t %in% tett]$positive_rate, na.rm = T)
    }))
    dataPlotDens2$w <- wart
    
    return(dataPlotDens2)
}

# glowna funkcja do wykresu / main function for a graph
rysuj_plot <- function(dataPlotDens2, zbiorPunktowPlot, maxColor = max(zbiorPunktowPlot$positive_rate), 
                       pointSizeRange = c(1, 6),
                       colorBreaks = seq(0, ceiling(maxColor / .05) * .05, by = .05), colorNUmber = 20, 
                       colorValues = rev(RColorBrewer::brewer.pal(10, "RdBu")),
                       alphaPathBase = .0175, alphaPathSpecial = .075, sizePathBase = 2, sizePathSpecial = 1,
                       tytul = "tytul", podTytul = "podtytul", bgColor = "#FFFFFF")
{
    colorsForScale <- colorRampPalette(colors = colorValues)(colorNUmber)
    dd <- dataPlotDens2[,.(x, y, w <= .05)]
    dd[, ff := rleid(V3)]
    ddP <- dd[,.(.N), by = .(ff)]
    
    # wybranie odpowiednich odcinkow
    gg <- lapply(
        X = sort(unique(ddP[N > 1][ff %in% dd[V3 == TRUE]$ff]$ff)), 
        FUN = function(i){
            geom_path(data = dd[ff == i], aes(x = x, y = y, group = ff), alpha = alphaPathSpecial, size = sizePathSpecial)
        }
    )
    
    ggplot() +
        geom_path(data = dataPlotDens2, aes(x = x, y = y), alpha = alphaPathBase, size = sizePathBase) +
        gg +
        geom_point(data = zbiorPunktowPlot, aes(x = x, y = y, size = ods, color = positive_rate)) + 
        theme_void() +
        scale_color_gradientn(
            limits = c(0, ceiling(maxColor / .05) * .05), breaks = colorBreaks,
            colors = colorsForScale, labels = function(x) paste0(100 * x, "%")
        ) +
        scale_size_continuous(limits = c(0, 1), range = pointSizeRange) + 
        guides(
            color = guide_colorbar(
                barwidth = 24, title = "% testów pozytywnych", title.position = "top", 
                title.vjust = -3, title.hjust = 1, label.vjust = 6
            ), 
            size = guide_none()
        ) +
        ggtitle(label = tytul, subtitle = podTytul) +
        labs(caption = "Vis: @DominikZabinski\nData: https://ourworldindata.org/coronavirus-testing") +
        theme(
            aspect.ratio = 1, legend.position = "bottom",
            text = element_text(family = "montserrat"),
            legend.title = element_text(size = 10 * ggplot2::.pt, colour = "#395877", face = "bold"),
            legend.text = element_text(size = 8 * ggplot2::.pt),
            plot.title = element_text(size = 12 * ggplot2::.pt, colour = "#395877", face = "bold"),
            plot.subtitle = element_text(size = 10 * ggplot2::.pt, colour = "#395877", face = "bold"),
            panel.background = element_rect(fill = bgColor, color = bgColor),
            plot.background = element_rect(fill = bgColor, color = bgColor),
            plot.caption = element_text(size = 8 * ggplot2::.pt, colour = "#000000", face = "bold", lineheight = .5)
        )
}

# analiza danych / data analysis ----
# dane pochodza z https://ourworldindata.org/coronavirus-testing, stan na 05.05.2021 / dane pochodza z https://ourworldindata.org/coronavirus-testing, 05.05.2021
covidData <- data.table(read.csv(file = "same_arc/owid-covid-data.csv"))
covidData[1]
# Poland
covidDataPoland <- covidData[location == "Poland"][,.(date, new_tests, new_tests_smoothed, positive_rate)]
covidDataPoland[, date := as.Date(date)]
# NA's
summary(covidDataPoland)
covidDataPoland[new_tests < 0]
nn <- covidDataPoland[new_tests < 0]$date
covidDataPoland[date %in% seq(nn - 2, nn + 2, by = 1)]
# reczna poprawa danych / manually fixing data
covidDataPoland[date %in% as.Date(c("2021-01-29", "2021-01-30")), new_tests := (327863 - 239172) / 2]
summary(covidDataPoland)

# odrzucam pierwsze braki danych / discarding first missing values
covidDataPoland <- covidDataPoland[min(which(!is.na(covidDataPoland$new_tests))):nrow(covidDataPoland)]
summary(covidDataPoland)

covidDataPoland[is.na(new_tests)]

covidDataPoland[is.na(new_tests), new_tests := new_tests_smoothed]

covidDataPoland <- covidDataPoland[!is.na(new_tests)]

covidDataPoland[is.na(positive_rate)]
# uzupelnianie brakow / filling the gaps
listP <- covidDataPoland[is.na(positive_rate)]$date
while (length(listP) > 0)
{
    i <- listP[1]
    sr <- mean(covidDataPoland[date %in% c(i-1, i +1)]$positive_rate, na.rm = T)
    if (is.nan(sr))
    {
        listP <- c(setdiff(listP, i), i)
    } else
    {
        covidDataPoland[date == i, positive_rate := sr]
        listP <- setdiff(listP, i)
    }
}
covidDataPoland[is.na(positive_rate)]

# % testow pozytywnych a 5% prog wg WHO/ lets look at positiv rate in respect of WHO 5% threshold
ggplot(data = covidDataPoland, aes(x = date, y = positive_rate)) + 
    geom_line() + 
    geom_abline(intercept = .05, slope = 0)

# a liczba testow? / how about number of test
ggplot(data = covidDataPoland, aes(x = date, y = new_tests)) + 
    geom_line()

# odsetek i odsetek skorygowany / rate and corrected rate
ggplot() + 
    geom_line(data = covidDataPoland, aes(x = date, y = sqrt(new_tests/max(covidDataPoland$new_tests,na.rm = T))), color = "red") + 
    geom_line(data = covidDataPoland, aes(x = date, y = new_tests/max(covidDataPoland$new_tests,na.rm = T)), color = "blue")

# tworze zbiory do wykresu/ creating datasets for plot
zbiorPunktowPlot <- tworz_zbior_bubles(data = covidDataPoland)
dataPlotDens2 <- tworz_path(data = zbiorPunktowPlot)

rysuj_plot(dataPlotDens2 = dataPlotDens2, zbiorPunktowPlot = zbiorPunktowPlot)

# jak w innych krajach? / how about other countries?
covidDataOther <- copy(covidData)[,.(date, new_tests, new_tests_smoothed, positive_rate, location)]
covidDataOther[, date := as.Date(date)]
covidDataOther <- covidDataOther[date %in% covidDataPoland$date]

# wezmy pod uwage tylko kraje bez brakow danych /  lets look onyl at countries without missing data
toWalid <- covidDataOther[,.(walidTest = sum(!is.na(new_tests)) / .N, walidRate = sum(!is.na(positive_rate)) / .N), by = .(location)]

toWalid[walidTest == 1 & walidRate == 1]

# wykresy / graphs ----
zbiorPunktowPlot <- tworz_zbior_bubles(data = covidDataPoland)
dataPlotDens2 <- tworz_path(data = zbiorPunktowPlot)
fPlot <- rysuj_plot(
    dataPlotDens2 = dataPlotDens2, 
    zbiorPunktowPlot = zbiorPunktowPlot, 
    tytul = "Polska", 
    podTytul = sprintf("Maksymalna liczba testów: %s", format(max(zbiorPunktowPlot$new_tests, na.rm = T), big.mark = " "))
)
ggsave(plot = fPlot, filename = sprintf("same_arc/%s.png", "Poland"), width = 8, height = 8)

# wykresy dla Węgier, Izraela i Słowacji / creating plots for Hungary, Israel and Slovakia
countries <- list(
    "Hungary" = "Węgry",
    "Slovakia" = "Słowacja",
    "Israel" = "Izrael"
)
for (i in names(countries))
{
    zbiorPunktowPlot <- tworz_zbior_bubles(data = covidDataOther[location == i])
    dataPlotDens2 <- tworz_path(data = zbiorPunktowPlot)
    fPlot <- rysuj_plot(
        dataPlotDens2 = dataPlotDens2, 
        zbiorPunktowPlot = zbiorPunktowPlot, 
        tytul = countries[[i]], 
        podTytul = sprintf("Maksymalna liczba testów: %s", format(max(zbiorPunktowPlot$new_tests, na.rm = T), big.mark = " "))
    )
    ggsave(plot = fPlot, filename = sprintf("same_arc/%s.png", i), width = 8, height = 8)
}

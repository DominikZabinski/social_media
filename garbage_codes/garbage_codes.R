# libraries ----
library(openxlsx)
library(data.table)
library(ggplot2)
library(extrafont)

# globals ----
loadfonts(device = "win") # load fonts to session

# data wrangling ----
file <- "garbage_codes/garbage_codes_lista_rozszerzona_2000_2005_2010-2018.xlsx"
getSheetNames(file = file)
d <- data.table(read.xlsx(xlsxFile = file, sheet = "2010_lista rozszerzona", startRow = 4))
head(d)
setnames(d, "X3", "Polska")
d <- melt(data = d, id.vars = c("Kod.ICD", "Opis"), variable.factor = F, value.factor = F)
head(d)

dataAll <- rbindlist(
    l = lapply(
        X = 2010:2018,
        FUN = function(x)
        {
            sName <- sprintf("%s_lista rozszerzona", x)
            message(x)
            d <- data.table(read.xlsx(xlsxFile = file, sheet = sName, startRow = ifelse(x < 2013, 4, 6)))
            setnames(d, "X3", "Polska")
            d <- melt(data = d, id.vars = c("Kod.ICD", "Opis"), variable.factor = F, value.factor = F)
            d[, Rok := x]
            d
        }
    )
)

str(dataAll)
length(unique(dataAll[is.na(as.numeric(value))]$value))
unique(dataAll[is.na(as.numeric(value))]$value)
dataAll[, value := as.numeric(value)]

dataChart <- dataAll[Kod.ICD == "w % do zgonów ogółem"]
chosen <- c("Polska", "Pomorskie", "Opolskie")
ggplot() + 
    geom_line(data = dataChart[!variable %in% chosen], aes(x = Rok, y = value, group = variable), color = "#9abad6", size = .51) +
    geom_line(data = dataChart[variable == "Opolskie"], aes(x = Rok, y = value, group = variable), color = "#000004CC", size = 1.51, alpha = .17) +
    geom_point(data = dataChart[variable == "Opolskie"], aes(x = Rok, y = value, group = variable), color = "#000004CC", size = 5) +
    geom_line(data = dataChart[variable == "Polska"], aes(x = Rok, y = value, group = variable), color = "#BB3754CC", size = 1.51, alpha = .17) +
    geom_point(data = dataChart[variable == "Polska"], aes(x = Rok, y = value, group = variable), color = "#BB3754CC", size = 5) +
    geom_line(data = dataChart[variable == "Pomorskie"], aes(x = Rok, y = value, group = variable), color = "#FCFFA4CC", size = 1.51, alpha = .17) +
    geom_point(data = dataChart[variable == "Pomorskie"], aes(x = Rok, y = value, group = variable), color = "#FCFFA4CC", size = 5) + 
    theme(
        line = element_line(colour = "grey30"),
        text = element_text(size = 18, family = "Open Sans Light"), 
        title = element_text(family = "Open Sans Light", lineheight = 1.25),
        axis.title = element_text(family = "Open Sans Semibold"),
        axis.ticks = element_blank(), 
        legend.background = element_blank(), 
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey90"),
        plot.background = element_rect(colour = "white", fill = "#add8e6"),
        plot.caption = element_text(size = 9, hjust = 0),
        plot.subtitle = element_text(size = 11, margin = margin(0, 0, 20, 0)), 
        plot.title = element_text(family = "Roboto", face = "bold", margin = margin(0, 0, 15, 0)),
        strip.background = element_blank()
    ) + 
    scale_y_continuous(limits = function(x) c(0, max(x) * 1.05)) + 
    # scale_x_continuous(expand = c(0, 0)) +
    labs(
        x = "Rok", 
        y = "% zgonów ogółem", 
        title = "Garbage (codes) in - garbage (actions) out", 
        subtitle = "Garbage codes - WHO określa tak kody, które uniemożliwiają precyzyjne określenie i zakodowanie przyczyny zgonu. Wyróżnia się dwie listy garbage codes:\npodstawową i rozszerzoną. Wykres przedstawia % zgonów raportowanych w latach 2010-2018 właśnie za pomocą garbage codes z listy rozszerzonej.\nLinie przedstawiają wartości dla poszczególnych województw, wyróżniono średnią ogólnopolską.",
        caption = "Źródło: Opracowanie własne na podstawie danych GUS:\nhttps://stat.gov.pl/obszary-tematyczne/ludnosc/statystyka-przyczyn-zgonow/zgony-wedlug-przyczyn-okreslanych-jako-garbage-codes,3,3.html"
    ) + 
    annotate(geom = "text", label = "Pomorskie", x = 2015, y = 10, color = "#FCFFA4CC", fontface = 2, size = 6) + 
    annotate(geom = "text", label = "Opolskie", x = 2017, y = 50, color = "#000004CC", fontface = 2, size = 6) + 
    annotate(geom = "text", label = "Polska", x = 2017, y = 27, color = "#BB3754CC", fontface = 2, size = 6)
szer <- 1200 * 1 / 96
ggsave(filename = "garbage_codes/2010-2018_garbage_all.png", width = szer, height = szer * 9 / 16, units = "in", dpi = 720)
file.show("garbage_codes/2010-2018_garbage_all.png")

# lvls
head(dataAll)
garbageLvls <- data.table(read.table(file = "garbage_codes/garbage_lvls.txt", sep = ";", stringsAsFactors = F))
str(garbageLvls)
dataAll[Kod.ICD == "A40"]
dataAll[Kod.ICD %like% "A40"]

sort(unique(dataAll$Kod.ICD))
dataAll[, ICD := trimws(Kod.ICD)]
sort(unique(dataAll$ICD))

dataIcd <- dataAll[!ICD %in% c("Garbage codes", "w % do zgonów ogółem", "w tym:", "Zgony OGÓłEM")]
dataIcd[is.na(value), value := 0]
ag <- dataIcd[,.(value = sum(value)), by = .(Rok, variable)]
f <- merge(ag, dataAll[ICD == "Garbage codes",.(Rok, variable, r = value)], by = c("Rok", "variable"))
f[r != value]

splitted <- trimws(unlist(strsplit(x = garbageLvls[V1 == 1]$V2, split = ",")))
lapply(X = splitted, FUN = function(x){
    print(x)
    g <- trimws(unlist(strsplit(x = x, split = "-")))
    if (length(g) == 1)
    {
      return(g)  
    } else
    {
        base <- substr(g[1], 1, 3)
        start <- substr(g[1], 5, 5)
        end <- substr(g[2], 5, 5)
        paste0(base, ".", c(as.numeric(start):as.numeric(end)))
    }
    })

lapply(X = splitted, FUN = function(x){
    print(x)
    g <- trimws(unlist(strsplit(x = x, split = "-")))
    if (length(g) == 1)
    {
        return(g)  
    } else
    {
        if (nchar(g[1]) == 3)
        {
            return(g)
        } else
        {
            base <- substr(g[1], 1, 3)
            start <- substr(g[1], 5, 5)
            end <- substr(g[2], 5, 5)
            paste0(base, ".", c(as.numeric(start):as.numeric(end)))
        }
        
    }
})

garbageLvlsSplitted <- rbindlist(l = lapply(
    X = 1:4,
    FUN = function(lvl){
        splitted <- trimws(unlist(strsplit(x = garbageLvls[V1 == lvl]$V2, split = ",")))
        icdVector <- unlist(lapply(X = splitted, FUN = function(x){
            g <- trimws(unlist(strsplit(x = x, split = "-")))
            if (length(g) == 1)
            {
                return(g)  
            } else
            {
                if (nchar(g[1]) == 3)
                {
                    return(g)
                } else
                {
                    base <- substr(g[1], 1, 3)
                    start <- substr(g[1], 5, 5)
                    end <- substr(g[2], 5, 5)
                    paste0(base, ".", c(as.numeric(start):as.numeric(end)))
                }
                
            }
        }))
        data.table(lvl = lvl, icd = icdVector)
    }
))
garbageLvlsSplitted[, base := substr(icd, 1, 3)]
nrow(garbageLvlsSplitted[,.(length(unique(lvl))), by = .(base)][V1 > 1])

table(garbageLvlsSplitted[,.(length(unique(lvl)), paste0(sort(unique(lvl)), collapse = "|")), by = .(base)][V1 > 1]$V2)

# can we separate this
merge(garbageLvlsSplitted[,.(length(unique(lvl)), paste0(sort(unique(lvl)), collapse = "|")), by = .(base)][V1 > 1],
      dataIcd[,.(sum(value)), by = .(base = substr(ICD, 1, 3))], by = "base")

problematic <- merge(garbageLvlsSplitted[,.(length(unique(lvl)), paste0(sort(unique(lvl)), collapse = "|")), by = .(base)][V1 > 1],
                     dataIcd[,.(sum(value)), by = .(base = substr(ICD, 1, 3))], by = "base")

dataIcd[substr(ICD,1,3) %in% problematic$base & variable == "Polska",.(sum(value)), by = .(ICD)]
garbageLvlsSplitted[icd %in% dataIcd[substr(ICD,1,3) %in% problematic$base & variable == "Polska",.(sum(value)), by = .(ICD)]$ICD]

dataIcdLvls <- merge(dataIcd[,.(Rok, variable, value, ICD)], garbageLvlsSplitted[,.(ICD = icd, lvl)], all.x = T)
dataIcdLvls[variable == "Polska" & is.na(lvl),.(sum(value)), by = .(Rok)]
sort(unique(dataIcdLvls[is.na(lvl)]$ICD))
garbageLvlsSplitted[icd %like% "I51"]

# imputing data
dataIcdLvls[ICD %in% c("N18", "R95", "Y15"), lvl := 1]
dcast(dataIcdLvls[is.na(lvl)][variable == "Polska"], ICD ~ Rok, value.var = "value")

# i'm not a medic
dataIcdLvls <- dataIcdLvls[!is.na(lvl)]
dataIcdLvls <- dataIcdLvls[,.(sum(value)), by = .(Rok, variable, lvl)]
dataIcdLvls <- merge(dataIcdLvls, dataAll[ICD == 'Zgony OGÓŁEM',.(Rok, variable, ogol = value)], by = c("Rok", "variable"))
dataIcdLvls[, ods := V1 / ogol]

lvls_status <- c(
    `1` = "Poziom I",
    `2` = "Poziom II",
    `3` = "Poziom III",
    `4` = "Poziom IV"
)

ggplot()+
    geom_point(data = dataIcdLvls[variable != "Polska"], aes(x = Rok, y = ods, group = lvl), size = 2, color = "#9abad6") + 
    geom_line(data = dataIcdLvls[variable == "Polska"], aes(x = Rok, y = ods, group = lvl, color = as.factor(lvl)), size = 1.51, alpha = .17) + 
    geom_point(data = dataIcdLvls[variable == "Polska"], aes(x = Rok, y = ods, group = lvl, color = as.factor(lvl)), size = 5) + 
    # scale_color_manual(values = viridis::magma(8, begin = .2, end = .8)[1:4]) +
    scale_color_brewer(palette = "YlOrRd", direction = -1)+
    facet_wrap(~lvl, scales = "free_y", labeller = labeller(lvl = lvls_status)) +
    theme(
        line = element_line(colour = "grey30"),
        text = element_text(size = 18, family = "Open Sans Light"), 
        title = element_text(family = "Open Sans Light", lineheight = 1.25),
        axis.title = element_text(family = "Open Sans Semibold"),
        axis.ticks = element_blank(), 
        legend.background = element_blank(), 
        legend.key = element_blank(),
        legend.text = element_text(size = 10),
        panel.background = element_blank(), 
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "grey90"),
        plot.background = element_rect(colour = "white", fill = "lightblue"),
        plot.caption = element_text(size = 9, hjust = 0),
        plot.subtitle = element_text(size = 11, margin = margin(0, 0, 20, 0)), 
        plot.title = element_text(family = "Roboto", face = "bold", margin = margin(0, 0, 15, 0)),
        strip.background = element_blank(),
        legend.position = "none"
    ) + 
    scale_y_continuous(limits = function(x) c(0, max(x) * 1.05), labels = function(x) format(100 * x)) + 
    labs(
        x = "Rok",
        y = "% zgon?w og??em",
        title = "Garbage garbage nier?wny", 
        subtitle = "Garbage codes mo?na podzieli? ze wzgl?du na to jak wielki wp?yw ma ich u?ywanie na podejmowane dzia?ania i planowanie w zakresie zdrowia publicznego.Wyr??niane s?\n4 poziomy garbage codes gdzie 1 oznacza kody o najwi?kszym potencjale, kryj?ce najwi?cej informacji, za? 4 oznacza kod, kt?rego dok?adno?? nie wp?ywa znacznie na planowanie\nw zakresie zdrowia publicznego. Wykres przedstawia % zgon?w raportowanych w latach 2010-2018 w podziale na 4 poziomy garbage codes. Kropki przedstawiaj? warto?ci\ndla poszczeg?lnych wojew?dztw, wyr??niono ?redni? og?lnopolsk?.",
        caption = "?r?d?o: Opracowanie w?asne na podstawie danych GUS:\nhttps://stat.gov.pl/obszary-tematyczne/ludnosc/statystyka-przyczyn-zgonow/zgony-wedlug-przyczyn-okreslanych-jako-garbage-codes,3,3.html i inspirowane artyku?em\nIburg KM, Mikkelsen L, Adair T, Lopez AD (2020) Are cause of death data fit for purpose? evidence from 20 countries at different levels of socio-economic development."
    )
szer <- 1200 * 1 / 96
ggsave(filename = "garbage_codes/2010-2018_garbage_lvls.png", width = szer, height = szer * 9 / 16, units = "in", dpi = 720)
file.show("garbage_codes/2010-2018_garbage_lvls.png")

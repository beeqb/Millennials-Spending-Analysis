#########################################
##    Millennials Spending Analysis    ##
##  Programmed by: Roberto Fierimonte  ##
##    (roberto.fierimonte@gmail.com)   ##
#########################################

## Import required libraries
libs <- c('data.table', 'plyr', 'ggplot2', 'ggvis', 'dplyr', 'xlsx', 'tidyr', 'stringr')
new.packages <- libs[!(libs %in% installed.packages()[,"Package"])]

if(length(new.packages)) 
    suppressWarnings(install.packages(new.packages))

suppressMessages(lapply(libs, require, character.only = TRUE))

rm(libs)
rm(new.packages)

## Set Working Directory
setwd('~/Downloads/Spending Analysis/')

## Import and clean data
folders <- list.dirs(full.names = FALSE)
folders <- folders[grepl('^[0-9]', folders)]

tab.list <- list()

for (j in seq(folders)) {
    folder.name <- paste(getwd(), folders[j], sep = '/')
    files <- list.files(path = folder.name, full.names = TRUE, pattern = '*.xls(x)?')
    
    for (i in seq(files)) {
        file.name <- files[i]
        tab <- xlsx::read.xlsx(files[i],
                               sheetIndex = 1,
                               startRow = 3,
                               colClasses = rep("character", 11),
                               stringsAsFactors = FALSE)
        
        tab <- tab[1:161,]
        tab <- data.frame(apply(tab, 2, function(x) {gsub('a/|b/|d/', '', x)}))
        if (grepl('xunder25', file.name)) {
            new.colnames = c('Metric', 'Total', 'Less than 5000$', '5000$ to 9999$', '10000$ to 14999$', 
                             '15000$ to 19999$', '20000$ to 29999$', '30000$ to 39999$', 'Over 40000$')
        }   else {
            new.colnames = c('Metric', 'Total', 'Less than 5000$', '5000$ to 9999$', '10000$ to 14999$', 
                             '15000$ to 19999$', '20000$ to 29999$', '30000$ to 39999$', '40000$ to 49999$',
                             '50000$ to 69999$', 'Over 70000$')       
        }
        
        setnames(tab, colnames(tab), new.colnames)
        
        suppressWarnings(tab$Year <- folders[j])
        
        if (grepl('xunder25', file.name)) {
            suppressWarnings(tab$Age.Group <- 'Less than 25 years')
        }    else if (grepl('x25to34', file.name)) {
            suppressWarnings(tab$Age.Group <- '25 to 34 years')
        }    else if (grepl('x35to44', file.name)) {
            suppressWarnings(tab$Age.Group <- '35 to 44 years')
        }    else if (grepl('x45to54', file.name)) {
            suppressWarnings(tab$Age.Group <- '45 to 54 years')
        }    else if (grepl('x55to64', file.name)) {
            suppressWarnings(tab$Age.Group <- '55 to 64 years')
        }
        
        tab$Metric <- str_trim(tab$Metric)
        
        tab <- tab %>% 
            dplyr::filter(Metric %in% c('Income after taxes', 'Male', 'Men', 'Homeowner', 'With mortgage', 'College',
                                        'At least one vehicle owned or leased', 'Food', 'Food at home', 'Alcoholic beverages', 'Housing', 'Apparel and services',
                                        'Transportation', 'Healthcare', 'Entertainment', 'Personal care products and services',
                                        'Reading', 'Education', 'Tobacco products and smoking supplies', 'Miscellaneous',
                                        'Cash contributions', 'Personal insurance and pensions')
                         )
        
        names <- colnames(tab)
        if (sum(grepl('Men', names)) > 0)   {
            setnames(tab, names[grepl('Men', names)], 'Male')    
        }
        tab$Metric <- as.factor(tab$Metric)
        tab$Year <- as.factor(tab$Year)
        tab$Age.Group <- as.factor(tab$Age.Group)
        
        tab <- tab %>% 
            tidyr::gather_('Income Group', 'Value', gather_cols = new.colnames[-1])
        
        tab$`Income Group` <- as.factor(tab$`Income Group`)
        
        tab <- tab %>% tidyr::spread(Metric, Value, drop = FALSE)
        
        tab.list[[(j - 1)*length(files) + i]] <- tab
    }
}

## Merge Yearly Data
df <- rbindlist(tab.list)
setnames(df, colnames(df), sub(' ', '.', colnames(df)))

df <- df %>% tidyr::gather(Spending.Type, Amount, -c())

## Process data
col.to.change <- colnames(df)[-1:-3]

df <- data.frame(df)

df[, -1:-3] <- sapply(df[, -1:-3], as.numeric)
# In case I wanted to keep using dplyr I should use df[, -1:-3, with = FALSE]

df <- within(df, Total.spending <- Alcoholic.beverages + Apparel.and.services + Cash.contributions +
             Education + Entertainment + Food + Healthcare + Housing +
             Miscellaneous + Personal.care.products.and.services + Personal.insurance.and.pensions +
             Reading + Tobacco.products.and.smoking.supplies + Transportation)

df <- within(df, Profit <- Income.after.taxes - Total.spending)
df <- within(df, Relative.profit <- Profit/Income.after.taxes)

spending.df <- data.frame(df)
cor <- df %>% dplyr::group_by(Year, Age.Group) %>% 
    summarise(COR = cor(Income.after.taxes, Total.spending))

a <- df %>% dplyr::group_by(Year, Age.Group) %>% 
    do(data.frame(Cor = t(cor(.[, 4:16], .[,4:16]))))

## Perform some statistic tests
young <- df[df$Age.Group == 'Less than 25 years',]
millennials <- df[df$Age.Group == '25 to 34 years',]

summary(aov(Relative.profit ~ Year + Age.Group, data = df))

tidy.df <- df %>% tidyr::gather_('Metric', 'Value', gather_cols = colnames(df)[-1:-3])

cor <- df %>% summarise(cor = cor(Income.after.taxes, Total.spending))

## Plot results

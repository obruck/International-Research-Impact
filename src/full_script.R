rm(list=ls())

# Set working directory
# Change working directory
## Get current file location
getCurrentFileLocation <-  function()
{
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0)
  {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}
setwd(getCurrentFileLocation())


# Create dirs
dir.create("data", recursive = TRUE)
dir.create("data_export", recursive = TRUE)
dir.create("results/capital", recursive = TRUE)
dir.create("results/continent", recursive = TRUE)
dir.create("results/gender", recursive = TRUE)
dir.create("results/map", recursive = TRUE)
dir.create("results/publications", recursive = TRUE)
dir.create("results/time", recursive = TRUE)


########################################## LIBRARIES ##########################################


library(tidyverse)
library(readxl)
library(janitor)
library(fastDummies)
library(maps)
library(reshape2)
library(ggmap)
library(data.table)
library(countrycode)
library(genderizeR)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(rstatix)
library(ggdendro)
library(dendextend)
library(writexl)


########################################## FUNCTIONS ##########################################


# Euclidean distance 
mydist <- function(a, b, df1, x, y){
  
  dt <- data.table(sqrt((df1[[x]]-a)^2 + (df1[[y]]-b)^2))
  
  return(data.table(Closest.V1  = which.min(dt$V1),
                    Distance    = dt[which.min(dt$V1)]))
}



# Split into multiple columns
split_into_multiple <- function(column, pattern = ", ", into_prefix){
  cols <- str_split_fixed(column, pattern, n = Inf)
  # Sub out the ""'s returned by filling the matrix to the right, with NAs which are useful
  cols[which(cols == "")] <- NA
  cols <- as.tibble(cols)
  # name the 'cols' tibble as 'into_prefix_1', 'into_prefix_2', ..., 'into_prefix_m' 
  # where m = # columns of 'cols'
  m <- dim(cols)[2]
  
  names(cols) <- paste(into_prefix, 1:m, sep = "_")
  return(cols)
}


# World map
## Citations
plot_mapdata <- function(df)
{
  states <- ggplot2::map_data("world")
  
  ggplot2::ggplot(data = df, ggplot2::aes(x = long, y = lat)) + 
    # ggplot2::lims(x = c(-25, 47), y = c(35, 80)) +
    # ggplot2::lims(x = c(-180, 180), y = c(-140, 140)) +
    coord_fixed(1.3) +
    ggplot2::geom_polygon(data = states, ggplot2::aes(x = long, y = lat, group = group), 
                          color = "black", fill="grey", alpha=0.1) +
    geom_point(shape = 16,
               # size = 4,
               aes(color = times_cited_all_databases, size = times_cited_all_databases), alpha = 0.6) +
    scale_color_distiller(palette = 'Spectral', name="# Citations") +
    scale_size_continuous(range=c(2,5), guide = "none") +
    plain +
    ggplot2::theme(legend.position = "bottom")
}
## Publications
plot_mapdata_pub <- function(df)
{
  states <- ggplot2::map_data("world")
  
  ggplot2::ggplot(data = df, ggplot2::aes(x = long, y = lat)) + 
    # ggplot2::lims(x = c(-25, 47), y = c(35, 80)) +
    # ggplot2::lims(x = c(-180, 180), y = c(-140, 140)) +
    coord_fixed(1.3) +
    ggplot2::geom_polygon(data = states, ggplot2::aes(x = long, y = lat, group = group), 
                          color = "black", fill="grey", alpha=0.1) +
    stat_density2d(aes(fill = ..level..), alpha = 1, geom="polygon") +
    geom_point(aes(color=times_cited_all_databases), size=2.5, alpha=1, shape = 16) +
    # geom_point(aes(alpha=times_cited_all_databases), size=2.5, shape = 16) +
    scale_fill_distiller(palette = 'Reds', name="# Publications", direction = 1) +
    scale_color_distiller(palette = 'Purples', name="# Citations", direction = 1) +
    # scale_size_continuous(range=c(3,6), guide = "none") +
    plain +
    guides(fill = FALSE) +
    ggplot2::theme(legend.position = "bottom")
}

# USA map
## Citations
plot_mapdata_usa_cit <- function(df)
{
  states <- ggplot2::map_data("world")  #These include maps::county(), maps::france(), maps::italy(), maps::nz(), maps::state(), maps::usa(), maps::world(), maps::world2()
  states <- states %>% 
    dplyr::filter(region == "USA")
  
  ggplot2::ggplot(data = df, ggplot2::aes(x = long, y = lat)) + 
    ggplot2::lims(x = c(-140, -60), y = c(20, 60)) +
    coord_fixed(1.3) +
    ggplot2::geom_polygon(data = states, ggplot2::aes(x = long, y = lat, group = group), 
                          color = "black", fill="grey", alpha=0.1) +
    geom_point(shape = 16,
               # size = 4,
               aes(color = times_cited_all_databases, size = times_cited_all_databases), alpha = 0.6) +
    scale_color_distiller(palette = 'Spectral', name="# Citations") +
    scale_size_continuous(range=c(3,6), guide = "none") +
    plain +
    ggplot2::theme(legend.position = "bottom")
}
## Publications
plot_mapdata_usa_pub <- function(df)
{
  states <- ggplot2::map_data("world")  #These include maps::county(), maps::france(), maps::italy(), maps::nz(), maps::state(), maps::usa(), maps::world(), maps::world2()
  states <- states %>% 
    dplyr::filter(region == "USA")
  
  ggplot2::ggplot(data = df, ggplot2::aes(x = long, y = lat)) + 
    ggplot2::lims(x = c(-140, -60), y = c(20, 60)) +
    coord_fixed(1.3) +
    ggplot2::geom_polygon(data = states, ggplot2::aes(x = long, y = lat, group = group), 
                          color = "black", fill="grey", alpha=0.1) +
    stat_density2d(aes(fill = ..level..), alpha = 1, geom="polygon") +
    geom_point(aes(color=times_cited_all_databases), size=2.5, alpha=1, shape = 16) +
    # geom_point(aes(alpha=times_cited_all_databases), size=2.5, shape = 16) +
    scale_fill_distiller(palette = 'Reds', name="# Publications", direction = 1) +
    scale_color_distiller(palette = 'Purples', name="# Citations", direction = 1) +
    # scale_size_continuous(range=c(3,6), guide = "none") +
    plain +
    guides(fill = FALSE) +
    ggplot2::theme(legend.position = "bottom")
}

# Europe map
## Citations
plot_mapdata_eu <- function(df)
{
  states <- ggplot2::map_data("world")  #These include maps::county(), maps::france(), maps::italy(), maps::nz(), maps::state(), maps::usa(), maps::world(), maps::world2()
  tmp <- df_world %>% dplyr::filter(continent == "Europe") %>% dplyr::distinct(region)
  states <- states %>% 
    dplyr::filter(region %in% tmp$region )
  
  ggplot2::ggplot(data = df, ggplot2::aes(x = long, y = lat)) + 
    ggplot2::lims(x = c(-25, 47), y = c(35, 80)) +
    coord_fixed(1.3) +
    ggplot2::geom_polygon(data = states, ggplot2::aes(x = long, y = lat, group = group), 
                          color = "black", fill="grey", alpha=0.1) +
    geom_point(shape = 16,
               # size = 4,
               aes(color = times_cited_all_databases, size = times_cited_all_databases), alpha = 0.6) +
    scale_color_distiller(palette = 'Spectral', name="# Citations") +
    scale_size_continuous(range=c(3,6), guide = "none") +
    plain +
    ggplot2::theme(legend.position = "bottom")
}
## Publications
plot_mapdata_eu_pub <- function(df)
{
  states <- ggplot2::map_data("world")  #These include maps::county(), maps::france(), maps::italy(), maps::nz(), maps::state(), maps::usa(), maps::world(), maps::world2()
  tmp <- df_world %>% dplyr::filter(continent == "Europe") %>% dplyr::distinct(region)
  states <- states %>% 
    dplyr::filter(region %in% tmp$region )
  
  ggplot2::ggplot(data = df, ggplot2::aes(x = long, y = lat)) + 
    ggplot2::lims(x = c(-25, 47), y = c(35, 80)) +
    coord_fixed(1.3) +
    ggplot2::geom_polygon(data = states, ggplot2::aes(x = long, y = lat, group = group), 
                          color = "black", fill="grey", alpha=0.1) +
    stat_density2d(aes(fill = ..level..), alpha = 1, geom="polygon") +
    geom_point(aes(color=times_cited_all_databases), size=2.5, alpha=1, shape = 16) +
    scale_fill_distiller(palette = 'Reds', name="# Publications", direction = 1) +
    scale_color_distiller(palette = 'Purples', name="# Citations", direction = 1) +
    plain +
    guides(fill = FALSE) +
    ggplot2::theme(legend.position = "bottom")
}

# PLOTTING PARAMETERS
plain <- theme(
  axis.text = element_blank(),
  legend.title = element_text(size=14, colour = "black", face = "bold", vjust = 0.75),
  legend.text = element_text(size=14, colour = "black", vjust = 0.75),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

options(scipen = 999) ## To disable scientific notation


########################################## API ##########################################


register_google(key="AIzaSyDv5Iu2OGBGsJ4b4YMog11QmHk4WSVdunM", write = TRUE)


########################################## DATA ##########################################


# World population
world_pop <- read.csv("data/world_population.csv")

# World map
world <- map_data("world")
data(world.cities)

# Citations
df <- readxl::read_xlsx("International-Research-Impact/data/data.xlsx")


########################################## PROCESS DATA (GENERAL) ##########################################


# Calculate basic metrics
df <- df %>%
  dplyr::mutate(authors_tmp = str_count(authors, ";")+1,
                addresses_tmp = str_count(addresses, ";")+1,
                abstract_tmp = nchar(abstract),
                reprint_addresses_tmp = str_count(reprint_addresses, ";")+1,
                open_access_designations_tmp = ifelse(str_detect(open_access_designations, "Green Published"), "Green", open_access_designations),
                open_access_designations_tmp = ifelse(is.na(open_access_designations), NA,
                                                      ifelse(open_access_designations_tmp == "Green", "Green", "Bronze")),
                times_cited_all_databases_original = times_cited_all_databases,
                times_cited_all_databases = times_cited_all_databases / (2021-publication_year),
                author_per_address  = authors_tmp/addresses_tmp)


# Rewrite correspondence addresses
df <- df %>%
  dplyr::mutate(
    reprint_addresses = gsub("England", "UK", reprint_addresses),
    reprint_addresses = gsub("Scotland", "UK", reprint_addresses),
    reprint_addresses = gsub("Northern Ireland", "UK", reprint_addresses),
    reprint_addresses = gsub("North Ireland", "UK", reprint_addresses),
    reprint_addresses = gsub("Taiwan", "China", reprint_addresses),
    reprint_addresses = gsub("Wales", "UK", reprint_addresses))

# Split correspondence address by ;
df1 <- df %>% 
  bind_cols(split_into_multiple(.$reprint_addresses, ";", "type")) %>% 
  select(starts_with("type_"))

# Modify text of the splitted variables
for (i in 1:ncol(df1)) {
  df1[[i]] <- ifelse(str_detect(df1[[i]], "corresponding"), df1[[i]], NA)
  if (i!=ncol(df1)) {
    df1[[i]] <- ifelse(is.na(df1[[i]]), df1[[i+1]], df1[[i]])
  }
  df1[[i]] <- substr(df1[[i]], nchar(df1[[i]])-35, nchar(df1[[i]]))
  df1[[i]] <- sub('[[:digit:]]{1,7}', "", df1[[i]])
  df1[[i]] <- sub('[[:digit:]]{1,7}', "", df1[[i]])
  df1[[i]] <- sub('[[:digit:]]{1,7}', "", df1[[i]])
  df1[[i]] <- sub('[[:digit:]]{1,7}', "", df1[[i]])
  df1[[i]] <- sub(".*?,", "", df1[[i]])
  df1[[i]] <- sub("^[^ ].*?,", "", df1[[i]])
  df1[[i]] <- sub('^ ', "", df1[[i]])
  df1[[i]] <- sub("\\.$", "", df1[[i]])
  df1[[i]] <- sub('^[[:alnum:]]{1,2}-', "", df1[[i]])
  df1[[i]] <- sub('^ [[:alpha:]]{2} ', "", df1[[i]])
  df1[[i]] <- sub('^ ', "", df1[[i]])
  df1[[i]] <- sub("  ", " ", df1[[i]])
  df1[[i]] <- sub(' \\,', ",", df1[[i]])
  df1[[i]] <- sub("^.*- ", "", df1[[i]])
  df1[[i]] <- sub(" [A-Z]{1,3} ", " ", df1[[i]])
  df1[[i]] <- sub(" [A-Z]{1,3} ", " ", df1[[i]])
  df1[[i]] <- sub(" [A-Z]{1,4},", ",", df1[[i]])
  df1[[i]] <- sub(',,', ",", df1[[i]])
  df1[[i]] <- sub('^, ', "", df1[[i]])
  df1[[i]] <- ifelse(str_count(df1[[i]], ",")>1, gsub("^.*?, ", "", df1[[i]]), df1[[i]])
  df1[[i]] <- sub("Peoples China", "China", df1[[i]])
}

# Replace duplicated values row-wise
df2 <- t(apply(df1, 1, function(x) replace(x, duplicated(x), NA)))


########################################## PROCESS DATA (CORRESPONDENCE ADDRESS) ##########################################


# Continue separately for city (df2) and country (df3) data
df2 <- as.data.frame(df2)
df3 <- df2
## City
colnames(df2) <- gsub("type", "City", colnames(df2))
### Remove country information
for (i in 1:ncol(df2)) {
  df2[[i]] <- gsub(",[[:print:]]*", "", df2[[i]])
}
### Replace duplicated values with NA
for (i in 2:ncol(df2)) {
  for (j in 1:nrow(df2)) {
    df2[j,i] = ifelse(df2[j,i] == df2[j,1], NA, df2[j,i])
  }
}
### Each city as separate categorical variable
df2 <- dummy_cols(df2[colnames(df2)]) %>%
  dplyr::select(-ends_with("_NA"))
## Finalize
colnames(df2) <- gsub("City_[[:digit:]]{1,2}_", "", colnames(df2))
df2 <- cbind(df2[,(grep("City_", colnames(df2), invert = FALSE))], t(rowsum(t(df2[,(grep("City_", colnames(df2), invert = TRUE))]), group = gsub("\\.[[:digit:]]$", "", unique(colnames(df2[,(grep("City_", colnames(df2), invert = TRUE))]))), na.rm = T)))
for (i in grep("City_", colnames(df2), invert = TRUE)) {
  df2[[i]] <- ifelse(df2[[i]] > 1, 1, df2[[i]])
}
df2 <- as.data.frame(df2)



## Country
colnames(df3) <- gsub("type", "Country", colnames(df3))
### Remove city information
for (i in 1:ncol(df3)) {
  df3[[i]] <- gsub("[[:print:]]*, ", "", df3[[i]])
  df3[[i]] <- gsub("[[:alpha:]]{1,3} USA", "USA", df3[[i]])
  df3[[i]] <- ifelse(nchar(df3[[i]]) < 3 & !df3[[i]]=="UK", NA, df3[[i]])
  df3[[i]] <- ifelse(str_detect(df3[[i]], "\\,"), NA, df3[[i]])
}
### Replace duplicated values with NA
for (i in 2:ncol(df3)) {
  for (i in 2:ncol(df3)) {
    df3[[i]] = ifelse(df3[[i]] == df3[[1]], NA, df3[[i]])
  }
}
### Each country as separate categorical variable
df3 <- dummy_cols(df3[colnames(df3)]) %>%
  dplyr::select(-ends_with("_NA"))
## Finalize
colnames(df3) <- gsub("Country_[[:digit:]]{1,3}_", "", colnames(df3))
df3 <- cbind(df3[,(grep("Country_", colnames(df3), invert = FALSE))], t(rowsum(t(df3[,(grep("Country_", colnames(df3), invert = TRUE))]), group = gsub("\\.[[:digit:]]$", "", unique(colnames(df3[,(grep("Country_", colnames(df3), invert = TRUE))]))), na.rm = T)))
for (i in grep("Country_", colnames(df3), invert = TRUE)) {
  df3[[i]] <- ifelse(df3[[i]] > 1, 1, df3[[i]])
}
df3 <- as.data.frame(df3)


## Remove duplicates
df2 <- df2[!colnames(df2) %in% colnames(df3)]
# Combine city and country information back to df
df <- cbind(cbind(df, df2), df3)


########################################## CITATIONS AND ARTICLES BY COUNTRIES ##########################################


# Preprocess countries data into long format
df_long_country <- df %>% 
  dplyr::select(article_title, authors, starts_with("Country"), times_cited_all_databases) %>%
  melt(id.vars = c("times_cited_all_databases", "article_title", "authors"), variable.name = "Country") %>%
  dplyr::group_by(article_title, authors, times_cited_all_databases) %>%
  dplyr::summarise(
    region = unique(value)
  ) %>%
  ungroup() %>%
  dplyr::filter(!is.na(region)) %>%
  dplyr::select(-c(article_title, authors)) %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(
    times_cited_all_databases_sum = sum(times_cited_all_databases),
    times_cited_all_databases = median(times_cited_all_databases),
    n_articles = n()) %>% 
  ungroup()


# World population
world_pop <- read.csv("data/world_population.csv") %>%
  dplyr::rename(country.etc = Country.Name,
                pop = X2021) %>%
  dplyr::mutate(country.etc = gsub("United States", "USA",
                                   gsub("United Kingdom", "UK",
                                        gsub("Czechia", "Czech Republic",
                                             # gsub("Korea", "South Korea",
                                             gsub("Russian Federation", "Russia",
                                                  gsub("Serbia", "Serbia and Montenegro",
                                                       gsub("Slovak Republic", "Slovakia",
                                                            gsub("Syrian Arab Republic", "Syria",
                                                                 gsub("Turkiye", "Turkey",
                                                                      gsub("St\\. Vincent and the Grenadines", "Saint Vincent and The Grenadines",
                                                                           gsub("St\\.", "Saint",
                                                                                gsub("Lao PDR", "Laos",
                                                                                     gsub("Kyrgyz Republic", "Kyrgyzstan",
                                                                                          gsub("Cabo Verde", "Cape Verde",
                                                                                               gsub("Cote d'Ivoire", "Ivory Coast",
                                                                                                    gsub("North Macedonia", "Macedonia",
                                                                                                         # gsub("Korea South", "South Korea",
                                                                                                         gsub(",[[:print:]]*", "",
                                                                                                              gsub("Korea, Dem. People's Rep.", "North Korea", 
                                                                                                                   gsub("Korea, Rep.", "South Korea", country.etc)))))))))))))))))))




## Join
worldpop <- world.cities %>%
  dplyr::select(-pop) %>%
  dplyr::full_join(world_pop)

## Summarise
worldpop <- worldpop %>%
  group_by(country.etc) %>%
  summarise(pop = unique(pop))

# Combine world population to countries data
df_long_country1 <- df_long_country %>%
  dplyr::rename(country.etc = region) %>%
  dplyr::left_join(worldpop) %>%
  dplyr::mutate(
    times_cited_all_databases_per_art_per_pop = times_cited_all_databases*1000000,
    times_cited_all_databases_per_art_per_pop = times_cited_all_databases_per_art_per_pop/pop,
    n_articles_per_pop = (n_articles/pop)*1000000
  )
# Number of articles/population and citations/article by countries
a <- df_long_country1 %>% dplyr::filter(n_articles >= 10) %>% dplyr::filter(!is.na(pop))
sum(df_long_country1$n_articles)
sum(a$n_articles)
a$prop <- a$n_articles/sum(a$n_articles)*100

# Correlation
cor.test(a$n_articles_per_pop, a$n_articles, method = "spearman")

# Over and underachivers
a$achiever2 <- as.factor(ifelse(summary(lm(log(a$n_articles, 10)~log(a$times_cited_all_databases_sum, 10)))$residuals > 0, 1, 0))

# Linear regression model for number of articles
fit <- lm(log(a$n_articles, 10)~log(a$times_cited_all_databases_sum, 10))
## Define predicted values
a$predicted <- predict(fit)
## Define difference between actual and predicted
(10^a$predicted)-a$n_articles
a$n_articles_diff <- (10^a$predicted)-a$n_articles
a$n_articles_diff_prop <- a$n_articles_diff/a$n_articles
## Median of difference between actual and predicted
median(abs(a$n_articles_diff))
## Sum of article in excess
sum(a[a$n_articles_diff<0,]$n_articles_diff)
(1174+410)/sum(a[a$n_articles_diff<0,]$n_articles_diff)


# Export
a_export <- a %>%
  dplyr::select(country.etc, times_cited_all_databases_sum, times_cited_all_databases, n_articles, pop, times_cited_all_databases_per_art_per_pop, n_articles_per_pop, n_articles_diff, n_articles_diff_prop) %>%
  dplyr::mutate(n_articles_diff_prop = n_articles_diff_prop*100,
                pop = pop / 1000000)
colnames(a_export) <- c("Country", "# Citations", "# Citations/Year", "# Articles", "# Population", "# Citations/Article/Population/Year", "# Articles/Population", "# Excess Articles", "% Excess Articles")
a_export[2:ncol(a_export)] <- sapply(a_export[2:ncol(a_export)], function(x) round(x, 1))
a_export[c(2,3,5:9)] <- sapply(a_export[c(2,3,5:9)], function(x) ifelse(str_detect(x, "\\."), x, paste0(x, ".0")))
writexl::write_xlsx(a_export, "results/table1.xlsx")



## Cit ~ n of publications
g <- ggplot(a, aes(y = log(n_articles, 10), x = log(times_cited_all_databases_sum, 10), label = paste0(country.etc, "\n", scales::percent(a$n_articles_diff_prop)))) +
  geom_smooth(method = "lm") +
  geom_point(aes(fill=log(n_articles_per_pop, 10), size=-n_articles_diff), shape = 21, color="black", alpha = 0.85) +
  scale_fill_distiller(palette = "RdBu", direction = -1, name="# Publications/1 000 000 people (LOG10)", breaks = c(-1, 0, 1), labels = c(-1, 0, 1)) +
  scale_color_brewer(palette = "Set1", direction = 1, guide = "none") + 
  geom_label_repel(aes(color=achiever2), min.segment.length = 0, seed = 42, box.padding = 1) +
  scale_size(range = c(1, 20), name="# Publication excess", breaks = c(10, 100, 1000)) +  # , guide = "none"
  labs(x="# Citations (LOG10)", y="# Publications (LOG10)") +
  guides(
    size = guide_legend(title.vjust = 0.55, ncol=3)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=12, colour = "black"),
        axis.text.y = element_text(size=12, colour = "black"),
        axis.title.y = element_text(size=16, face="bold", colour = "black"),
        axis.title.x = element_text(size=16, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black", vjust = 0.8),
        legend.text = element_text(size=14, colour = "black"),
        legend.box = 'vertical',
        legend.spacing.y = unit(0.1, 'cm'),
        legend.position = "bottom")
ggsave(plot = g, filename = "results/publications/Correlation_publications_articles2.png", width = 8, height = 9, dpi = 300, units = "in")


## N of publications per 1 000 000 people ~ n of publications
g <- ggplot(a, aes(log(n_articles, 10), log(n_articles_per_pop, 10), label = country.etc)) +
  geom_abline(intercept = -1.55, slope = 0.95, color="black", size = 1, linetype = "solid") +
  geom_point(aes(fill=times_cited_all_databases), size = 14, shape = 21, color="black", alpha = 0.85) +
  geom_label_repel(aes(color=achiever3), min.segment.length = 0, seed = 42, box.padding = 0.5) +
  scale_fill_distiller(palette = "RdBu", direction = -1, name="# Citations") +
  scale_color_brewer(palette = "Set1", direction = -1, guide = "none") +
  labs(y="# Publications/1 000 000 people (LOG10)", x="# Publications (LOG10)") +
  theme_bw() +
  theme(axis.text.x = element_text(size=12, colour = "black"),
        axis.text.y = element_text(size=12, colour = "black"),
        axis.title.y = element_text(size=16, face="bold", colour = "black"),
        axis.title.x = element_text(size=16, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black", vjust = 0.8),
        legend.text = element_text(size=14, colour = "black"),
        legend.position = "bottom") +
  scale_size(range = c(1, 20), guide = "none")
ggsave(plot = g, filename = "results/publications/Correlation_publications_articles.png", width = 8, height = 8, dpi = 300, units = "in")


########################################## CITATIONS AND ARTICLES BY CITIES ##########################################


# Split correspondence addresses to columns
df_long_city <- df %>% 
  bind_cols(split_into_multiple(.$reprint_addresses, ";", "type")) %>%
  select(starts_with("type_"))

# Remove redundant information
df_long_city <- sapply(df_long_city, function(x) gsub("^[[:print:]]{1,30}\\(corresponding author\\), ", "", x) )

# Transform to long format
df_long_city = df %>%
  dplyr::select(article_title, authors, times_cited_all_databases) %>%
  cbind(df_long_city) %>%
  melt(id.vars = c("times_cited_all_databases", "article_title", "authors"), variable.name = "City") %>%
  dplyr::group_by(article_title, authors, times_cited_all_databases) %>%
  dplyr::summarise(
    name = unique(value)
  ) %>%
  ungroup() %>%
  dplyr::select(-c(article_title, authors)) %>%
  dplyr::filter(!is.na(name)) %>%
  dplyr::mutate(n_of_articles = 1) %>%
  dplyr::group_by(name) %>%
  dplyr::summarise(times_cited_all_databases = median(times_cited_all_databases),
                   n_of_articles = sum(n_of_articles)) %>%
  ungroup() %>%
  dplyr::filter(nchar(name) > 20)


# Geolocate round 1 = each city by their address and Google API
df_long_city <- df_long_city %>%
  mutate(coor = geocode(name))

# Geolocation round 2 = Use only last 30 characters of the corresponding author address
df_long_cityzz <- df_long_city %>%
  filter(is.na(coor$lon)) %>%
  dplyr::select(-coor) %>%
  dplyr::mutate(
    name = str_sub(name,-30,-1)) %>%
  mutate(coor = geocode(name))

# Geolocation round 3 = Use only last 20 characters of the corresponding author address
df_long_cityzzz <- df_long_cityzz %>%
  filter(is.na(coor$lon)) %>%
  dplyr::select(-coor) %>%
  dplyr::mutate(
    name = str_sub(name,-20,-1)) %>%
  mutate(coor = geocode(name))

# Geolocation round 4 = Use only last 15 characters of the corresponding author address
df_long_cityzzzz <- df_long_cityzzz %>%
  filter(is.na(coor$lon)) %>%
  dplyr::select(-coor) %>%
  dplyr::mutate(
    name = str_sub(name,-15,-1)) %>%
  mutate(coor = geocode(name))

# Combine
df_long_city_final <- full_join(
  full_join(
    full_join(
      df_long_city %>% dplyr::filter(!is.na(coor$lon)),
      df_long_cityzz %>% dplyr::filter(!is.na(coor$lon))),
    df_long_cityzzz %>% dplyr::filter(!is.na(coor$lon))),
  df_long_cityzzzz)


# Save to avoid rerunning this
saveRDS(df_long_city_final, "data_export/combined.rds")
df_long_city <- readRDS("data_export/combined.rds")


########################################## MAP OF CITATIONS AND ARTICLES BY COUNTRIES ##########################################


# Join world and country data
df_world <- world %>%
  dplyr::left_join(df_long_country)

# Stratify citations into 8 groups
df_world1 <- df_world %>%
  group_by(region) %>%
  summarise(times_cited_all_databases = unique(times_cited_all_databases),
            n_articles = unique(n_articles))

# Modify publications and citations
df_world1 <- df_world1 %>%
  # Citations
  dplyr::mutate(times_cited_all_databases = ifelse(is.na(times_cited_all_databases), 0, times_cited_all_databases)) %>%
  dplyr::mutate(n_articles = ifelse(is.na(n_articles), 0, n_articles)) %>%
  dplyr::mutate(times_cited_all_databases3 = log(times_cited_all_databases)) %>%
  dplyr::mutate(n_articles3 = log(n_articles)) %>%
  dplyr::mutate(times_cited_all_databases1 = ntile(times_cited_all_databases, 8)) %>%
  ## Keep only countries with >5 publications in 10 years
  dplyr::mutate(times_cited_all_databases2 = ifelse(n_articles > 5, times_cited_all_databases, NA)) %>%
  dplyr::mutate(times_cited_all_databases2 = ntile(times_cited_all_databases2, 8)) %>%
  dplyr::mutate(times_cited_all_databases2 = ifelse(is.na(times_cited_all_databases2), 0, times_cited_all_databases2)) %>%
  # Publications
  dplyr::mutate(n_articles1 = ntile(n_articles, 8)) %>%
  ## Keep only countries with >5 publications in 10 years
  dplyr::mutate(n_articles2 = ifelse(n_articles > 5, n_articles, NA)) %>%
  dplyr::mutate(n_articles2 = ntile(n_articles2, 8)) %>%
  dplyr::mutate(n_articles2 = ifelse(is.na(n_articles2), 0, n_articles2))

# Join
df_world <- df_world %>%
  dplyr::select(-c(times_cited_all_databases, n_articles)) %>%
  dplyr::left_join(df_world1)



# Plot by citations
worldHDI <- ggplot(data = df_world, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = times_cited_all_databases3)) +
  scale_fill_distiller(palette ="Spectral", direction = -1, name="# Citations") + # or direction=1
  plain
ggsave(plot = worldHDI, filename = "results/map/Map_country_number_of_citations_raw.png", width = 10, height = 8, dpi = 300, units = "in")
worldHDI <- ggplot(data = df_world, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = times_cited_all_databases1)) +
  scale_fill_distiller(palette ="Spectral", direction = -1, name="# Citations") + # or direction=1
  plain
ggsave(plot = worldHDI, filename = "results/map/Map_country_number_of_citations.png", width = 10, height = 8, dpi = 300, units = "in")
worldHDI <- ggplot(data = df_world, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = times_cited_all_databases2)) +
  scale_fill_distiller(palette ="Spectral", direction = -1, name="# Citations") + # or direction=1
  plain
ggsave(plot = worldHDI, filename = "results/map/Map_country_number_of_citations_min5articles.png", width = 10, height = 8, dpi = 300, units = "in")



# Plot by articles
worldHDI <- ggplot(data = df_world, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = n_articles3)) +
  scale_fill_distiller(palette ="Spectral", direction = -1, name="# Publications") + # or direction=1
  plain
ggsave(plot = worldHDI, filename = "results/map/Map_country_number_of_articles_raw.png", width = 10, height = 8, dpi = 300, units = "in")
worldHDI <- ggplot(data = df_world, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = n_articles1)) +
  scale_fill_distiller(palette ="Spectral", direction = -1, name="# Publications") + # or direction=1
  plain
ggsave(plot = worldHDI, filename = "results/map/Map_country_number_of_articles.png", width = 10, height = 8, dpi = 300, units = "in")
worldHDI <- ggplot(data = df_world, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = n_articles2)) +
  scale_fill_distiller(palette ="Spectral", direction = -1, name="# Publications") + # or direction=1
  plain
ggsave(plot = worldHDI, filename = "results/map/Map_country_number_of_articles_min5articles.png", width = 10, height = 8, dpi = 300, units = "in")


########################################## CITATIONS AND ARTICLES BY CONTINENTS ##########################################


# Continent
df_world$continent <- countrycode(sourcevar = df_world[, "region"],
                                  origin = "country.name",
                                  destination = "continent")
df_continent <- df_world %>%
  dplyr::filter(!is.na(times_cited_all_databases)) %>%
  distinct(region, .keep_all=TRUE) %>%
  dplyr::group_by(continent) %>%
  dplyr::summarise(times_cited_all_databases_sum = sum(times_cited_all_databases*n_articles),
                   n_articles_sum = sum(n_articles)
  ) %>%
  dplyr::mutate(times_cited_all_databases_median = times_cited_all_databases_sum / n_articles_sum)

# Statistics
df_continent$times_cited_all_databases_sum/sum(df_continent$times_cited_all_databases_sum)
31.8/median(df$times_cited_all_databases)



# Barplot
g <- ggplot(df_continent %>% dplyr::filter(!is.na(continent)), aes(x = reorder(continent, -n_articles_sum), y = n_articles_sum, fill = continent)) +
  geom_bar(stat = "identity", color="black") +
  labs(y="# Publications") +
  scale_fill_brewer(palette="Set1", name="Continent") +
  geom_text(aes(label = n_articles_sum, y = n_articles_sum + 50),
            position = position_dodge(0.9), vjust=0) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/continent/Barplot_continent_n_articles.png", width = 5, height = 4, units = "in", dpi = 300)


g <- ggplot(df_continent %>% dplyr::filter(!is.na(continent)), aes(x = reorder(continent, -times_cited_all_databases_sum), y = times_cited_all_databases_sum, fill = continent)) +
  geom_bar(stat = "identity", color="black") +
  labs(y="# Citations") +
  scale_fill_brewer(palette="Set1", name="Continent") +
  geom_text(aes(label = round(times_cited_all_databases_sum), y = times_cited_all_databases_sum + 1000),
            position = position_dodge(0.9), vjust=0) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/continent/Barplot_continent_n_citations.png", width = 5, height = 4, units = "in", dpi = 300)


g <- ggplot(df_continent %>% dplyr::filter(!is.na(continent)), aes(x = reorder(continent, -times_cited_all_databases_median), y = times_cited_all_databases_median, fill = continent)) +
  geom_bar(stat = "identity", color="black") +
  labs(y="# Citations/# Publications") +
  scale_fill_brewer(palette="Set1", name="Continent") +
  geom_text(aes(label = round(times_cited_all_databases_median, 1), y = times_cited_all_databases_median + 0.5),
            position = position_dodge(0.9), vjust=0) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/continent/Barplot_continent_n_citations_per_articles.png", width = 5, height = 4, units = "in", dpi = 300)


########################################## MAP OF CITATIONS AND ARTICLES BY CITIES ##########################################


# Preprocess
df_long_city <- bind_cols(df_long_city[c(1:3)], reduce(df_long_city[4], tibble))
colnames(df_long_city)[4] <- "long"

# Find city based on coordinates
df_long_city1 <- setDT(df_long_city)[, j = mydist(lat, long, setDT(world.cities), 
                                                  "lat", "long"), 
                                     by = list(name, lat, long)]

## Join city and coordinates
df_long_city <- df_long_city %>%
  dplyr::left_join(df_long_city1) %>%
  dplyr::mutate(lat = round(lat, 2),
                long = round(long, 2))

# Rownames to column in world.cities
world.cities <- world.cities %>% 
  rownames_to_column("Closest.V1") %>%
  dplyr::mutate(Closest.V1 = as.integer(Closest.V1))

# Join world.cities with city data
df_world_city <- world.cities %>%
  dplyr::left_join(df_long_city %>% dplyr::select(-lat, -long) %>% rename(adress = name)) %>%
  ## Round to the nearest 0.5 decimal value
  dplyr::mutate(lat = round(lat/5, 1)*5,
                long = round(long/5, 1)*5)

# Summarise by long and lat values
df_world_city1 <- df_world_city %>%
  dplyr::group_by(long, lat) %>%
  dplyr::summarise(times_cited_all_databases = median(times_cited_all_databases, na.rm = TRUE),
                   n_articles = median(n_of_articles, na.rm = TRUE))

# Join
df_world_city2 <- df_world_city %>%
  dplyr::distinct(long, lat, .keep_all=FALSE) %>%
  dplyr::left_join(df_world_city1)

# Join metadata
df_world_city3 <- df_world_city2 %>%
  dplyr::filter(!is.na(times_cited_all_databases)) %>%
  dplyr::left_join(df_world_city %>% dplyr::select(lat, long, country.etc, capital, pop))



# World map
## Stratify citations into 8 groups
worldHDI <- plot_mapdata(df_world_city3 %>%
                           dplyr::mutate(times_cited_all_databases = ntile(log(times_cited_all_databases), 20)))
worldHDI2 <- plot_mapdata_pub(df_world_city3 %>%
                                dplyr::mutate(times_cited_all_databases = ntile(log(times_cited_all_databases), 20)))

## Plot citations
ggsave(plot = worldHDI, filename = "results/map/map_city_citation.png", units = "in", dpi = 300, width = 20, height = 12)
## Plot publications
ggsave(plot = worldHDI2, filename = "results/map/map_city_citation_publications.png", units = "in", dpi = 300, width = 20, height = 12)


# USA map
## Citations
worldHDI1 <- plot_mapdata_usa_cit(df_world_city3 %>%
                                    dplyr::filter(country.etc == "USA") %>%
                                    dplyr::mutate(times_cited_all_databases = ntile(log(times_cited_all_databases), 20)))
## Publications
worldHDI2 <- plot_mapdata_usa_pub(df_world_city3 %>%
                                    dplyr::filter(country.etc == "USA") %>%
                                    dplyr::mutate(times_cited_all_databases = ntile(log(times_cited_all_databases, 10), 20)))
## Plot citations
ggsave(plot = worldHDI1, filename = "results/map/map_city_USA_citations.png", units = "in", dpi = 300, width = 10, height = 8)
## Plot publications
ggsave(plot = worldHDI2, filename = "results/map/map_city_USA_publications.png", units = "in", dpi = 300, width = 10, height = 7)



# Europe map
states <- ggplot2::map_data("world")  #These include maps::county(), maps::france(), maps::italy(), maps::nz(), maps::state(), maps::usa(), maps::world(), maps::world2()
tmp <- df_world %>% dplyr::filter(continent == "Europe") %>% dplyr::distinct(region)
states <- states %>% 
  dplyr::filter(region %in% tmp$region )
## Citations
worldHDI <- plot_mapdata_eu(df_world_city3 %>%
                              dplyr::filter(country.etc %in% states$region) %>%
                              dplyr::mutate(times_cited_all_databases = ntile(log(times_cited_all_databases), 20)))
## Publications
worldHDI2 <- plot_mapdata_eu_pub(df_world_city3 %>%
                                   dplyr::filter(country.etc %in% states$region) %>%
                                   dplyr::mutate(times_cited_all_databases = ntile(log(times_cited_all_databases), 20)))
## Plot citations
ggsave(plot = worldHDI, filename = "results/map/map_city_Europe.png", units = "in", dpi = 300, width = 10, height = 10)
## Plot publications
ggsave(plot = worldHDI2, filename = "results/map/map_city_Europe_publications.png", units = "in", dpi = 300, width = 10, height = 10)


######################################### STATISTICS BY CAPITAL ##########################################


# Compare data by studies done in the capital vs. non-capital
df_world_city3$capital <- ifelse(df_world_city3$capital > 1, 1, df_world_city3$capital)
df_world_city3$cit_per_art <- df_world_city3$times_cited_all_databases / df_world_city3$n_articles
df_world_city3 <- df_world_city3 %>%
  dplyr::mutate(capital = ifelse(capital == 0, "Non capital", "Capital"),
                capital = factor(capital))

# Summarise per country and capital
df_world_city4 <- df_world_city3 %>%
  group_by(country.etc, capital) %>%
  summarise(times_cited_all_databases = sum(times_cited_all_databases),
            n_articles = sum(n_articles)) %>%
  dplyr::mutate(cit_per_art = times_cited_all_databases / n_articles)

# Remove countries with only Capital or Non-Capital data
df_world_city4_countries <- unique(df_world_city4$country.etc)[table(df_world_city4$country.etc)==2]


# Statistic
## Overall
g <- ggplot(df_world_city4 %>% dplyr::filter(!is.na(capital)) %>% dplyr::filter(country.etc %in% df_world_city4_countries), aes(x = capital, y = log(times_cited_all_databases, 10))) +
  geom_jitter(size=1, width = 0.2, fill="black", shape = 21, color = "black") +
  geom_boxplot(outlier.shape = NA, aes(fill=capital), alpha = 0.5) + 
  labs(y="# Citations (LOG10)", x="City") +
  scale_fill_brewer(palette="Set1", name="City") +
  theme_bw() +
  stat_compare_means(method = "wilcox.test", paired = TRUE, label = "p.signif",
                     label.y = 0.9*max(log(df_world_city4$times_cited_all_databases, 10), na.rm=TRUE),
                     label.x = 1.45,
                     size = 6) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/capital/Boxplot_capital_n_citations.png", width = 4, height = 4, units = "in", dpi = 300)

g <- ggplot(df_world_city4 %>% dplyr::filter(!is.na(capital)) %>% dplyr::filter(country.etc %in% df_world_city4_countries), aes(x = capital, y = log(n_articles, 10))) +
  geom_jitter(size=1, width = 0.2, fill="black", shape = 21, color = "black") +
  geom_boxplot(outlier.shape = NA, aes(fill=capital), alpha = 0.5) + 
  labs(y="# Publications (LOG10)", x="City") +
  scale_fill_brewer(palette="Set1", name="City") +
  theme_bw() +
  stat_compare_means(method = "wilcox.test",  paired = TRUE,                      label = "p.signif",
                     # label.y = 0.9*max(df_world_city4$n_articles, na.rm=TRUE),
                     label.x = 1.45,
                     size = 6) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/capital/Boxplot_capital_n_articles.png", width = 4, height = 4, units = "in", dpi = 300)

g <- ggplot(df_world_city4 %>% dplyr::filter(!is.na(capital)) %>% dplyr::filter(country.etc %in% df_world_city4_countries), aes(x = capital, y = cit_per_art)) +
  geom_jitter(size=1, width = 0.2, fill="black", shape = 21, color = "black") +
  geom_boxplot(outlier.shape = NA, aes(fill=capital), alpha = 0.5) + 
  labs(y="# Citations/# Publications", x="City") +
  scale_fill_brewer(palette="Set1", name="City") +
  theme_bw() +
  stat_compare_means(method = "wilcox.test",   paired = TRUE,                     label = "p.signif",
                     label.y = 0.9*max(df_world_city4$cit_per_art, na.rm=TRUE),
                     label.x = 1.45,
                     size = 6) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/capital/Boxplot_capital_n_citations_per_articles.png", width = 4, height = 4, units = "in", dpi = 300)

# wilcox.test(df_world_city3$times_cited_all_databases ~ df_world_city3$capital)
# median(df_world_city3[df_world_city3$capital==0,]$times_cited_all_databases, na.rm=TRUE)
# median(df_world_city3[df_world_city3$capital==1,]$times_cited_all_databases, na.rm=TRUE)
# wilcox.test(df_world_city3[(df_world_city3$country.etc %in% tmp$region),]$times_cited_all_databases ~ df_world_city3[(df_world_city3$country.etc %in% tmp$region),]$capital)
# median(df_world_city3[((df_world_city3$country.etc %in% tmp$region) & df_world_city3$capital==0),]$times_cited_all_databases, na.rm=TRUE)
# median(df_world_city3[((df_world_city3$country.etc %in% tmp$region) & df_world_city3$capital==1),]$times_cited_all_databases, na.rm=TRUE)


# Loop over continents
for (i in unique(df_world[!is.na(df_world$continent),]$continent)) {
  
  # Statistics
  tmp <- df_world %>% dplyr::filter(continent == i) %>% dplyr::distinct(region)
  median(df_world_city3[((df_world_city3$country.etc %in% tmp$region) & df_world_city3$capital=="Non capital"),]$times_cited_all_databases, na.rm=TRUE)
  median(df_world_city3[((df_world_city3$country.etc %in% tmp$region) & df_world_city3$capital=="Capital"),]$times_cited_all_databases, na.rm=TRUE)
  
  # Plot
  ## N of citations
  g <- ggplot(df_world_city4 %>% dplyr::filter(country.etc %in% tmp$region) %>% dplyr::filter(!is.na(capital)) %>% dplyr::filter(country.etc %in% df_world_city4_countries), aes(x = capital, y = log(times_cited_all_databases, 10))) +
    geom_jitter(size=1, width = 0.2, fill="black", shape = 21, color = "black") +
    geom_boxplot(outlier.shape = NA, aes(fill=capital), alpha = 0.5) + 
    labs(y="# Citations (LOG10)", x="City") +
    scale_fill_brewer(palette="Set1", name="City") +
    theme_bw() +
    stat_compare_means(method = "wilcox.test",    paired=TRUE,                   label = "p.signif",
                       # label.y = 0.9*max(log(df_world_city4[df_world_city4$country.etc %in% tmp$region,]$times_cited_all_databases, 10), na.rm=TRUE),
                       label.x = 1.45,
                       size = 6) +
    theme_bw() +
    theme(axis.text.x = element_text(size=14, colour = "black"),
          axis.text.y = element_text(size=14, colour = "black"),
          axis.title.y = element_text(size=14, face="bold", colour = "black"),
          axis.title.x = element_text(size=14, face="bold", colour = "black"),
          legend.title = element_text(size=14, face="bold", colour = "black"),
          legend.text = element_text(size=14, colour = "black"),
          legend.key = element_rect(color="black"),
          legend.position = "NULL")
  ggsave(plot = g, paste0("results/capital/Boxplot_capital_n_citations_", i, ".png"), width = 4, height = 4, units = "in", dpi = 300)
  
  ## N of articles
  g <- ggplot(df_world_city4 %>% dplyr::filter(country.etc %in% tmp$region) %>% dplyr::filter(!is.na(capital)) %>% dplyr::filter(country.etc %in% df_world_city4_countries), aes(x = capital, y = log(n_articles, 10))) +
    geom_jitter(size=1, width = 0.2, fill="black", shape = 21, color = "black") +
    geom_boxplot(outlier.shape = NA, aes(fill=capital), alpha = 0.5) + 
    labs(y="# Publications (LOG10)", x="City") +
    scale_fill_brewer(palette="Set1", name="City") +
    theme_bw() +
    stat_compare_means(method = "wilcox.test",    paired=TRUE,                   label = "p.signif",
                       # label.y = 0.9*max(df_world_city4[df_world_city4$country.etc %in% tmp$region,]$n_articles, na.rm=TRUE),
                       label.x = 1.45,
                       size = 6) +
    theme_bw() +
    theme(axis.text.x = element_text(size=14, colour = "black"),
          axis.text.y = element_text(size=14, colour = "black"),
          axis.title.y = element_text(size=14, face="bold", colour = "black"),
          axis.title.x = element_text(size=14, face="bold", colour = "black"),
          legend.title = element_text(size=14, face="bold", colour = "black"),
          legend.text = element_text(size=14, colour = "black"),
          legend.key = element_rect(color="black"),
          legend.position = "NULL")
  ggsave(plot = g, paste0("results/capital/Boxplot_capital_n_articles_", i, ".png"), width = 4, height = 4, units = "in", dpi = 300)
  
  ## N of citations per articles
  g <- ggplot(df_world_city4 %>% dplyr::filter(country.etc %in% tmp$region) %>% dplyr::filter(!is.na(capital)) %>% dplyr::filter(country.etc %in% df_world_city4_countries), aes(x = capital, y = cit_per_art)) +
    geom_jitter(size=1, width = 0.2, fill="black", shape = 21, color = "black") +
    geom_boxplot(outlier.shape = NA, aes(fill=capital), alpha = 0.5) + 
    labs(y="# Citations/# Publications", x="City") +
    scale_fill_brewer(palette="Set1", name="City") +
    theme_bw() +
    stat_compare_means(method = "wilcox.test",        paired=TRUE,               label = "p.signif",
                       label.y = 0.9*max(df_world_city4[df_world_city4$country.etc %in% tmp$region,]$cit_per_art, na.rm=TRUE),
                       label.x = 1.45,
                       size = 6) +
    theme_bw() +
    theme(axis.text.x = element_text(size=14, colour = "black"),
          axis.text.y = element_text(size=14, colour = "black"),
          axis.title.y = element_text(size=14, face="bold", colour = "black"),
          axis.title.x = element_text(size=14, face="bold", colour = "black"),
          legend.title = element_text(size=14, face="bold", colour = "black"),
          legend.text = element_text(size=14, colour = "black"),
          legend.key = element_rect(color="black"),
          legend.position = "NULL")
  ggsave(plot = g, paste0("results/capital/Boxplot_capital_n_citations_per_articles_", i, ".png"), width = 4, height = 4, units = "in", dpi = 300)
  
}

########################################## GENDER STATISTICS ##########################################


# Load data
if (exists("file2")){
  rm(file2)
}

filelist <- list.files("./data/names", pattern = "txt", full.names = TRUE)
for (file1 in filelist) {
  if (!exists("file2")) {
    file2 = data.table::fread(file1)
  } else {
    file2 = rbind(file2, data.table::fread(file1))
  }
}
file2 <- file2[,1:2]


# Extract names
## Split names by ;
df_authors <- df %>% 
  bind_cols(split_into_multiple(.$author_full_names, ";", "type")) %>% 
  select(authors_tmp, starts_with("type_"))

## Identify last and second last authors
df_authors1 <- as.data.frame(df_authors[,1])
df_authors1$author_second_last <- NA
df_authors1$author_last <- NA
for (i in 1:nrow(df_authors)) {
  print(i)
  if(!is.na(df_authors[i,1])) {
    NAindex <- which(df_authors[i,] %in% c(" ", NA, ""))
    df_authors1[i,2] <- df_authors[i, min(NAindex)-2]
    df_authors1[i,3] <- df_authors[i, min(NAindex)-1]
  } else {
    df_authors1[i,2] <- NA
    df_authors1[i,3] <- NA
  }
}


# df_authors0 <- df_authors1

## Identify first and second authors
df_authors1 <- df_authors1[,2:3]
df_authors1 <- cbind(df_authors %>% dplyr::select(2,3), df_authors1)
### Rename
colnames(df_authors1)[1] <- "author_first"
colnames(df_authors1)[2] <- "author_second"

# Preprocess in case there are <4 authors to avoid duplicates
df_authors1$author_second <- ifelse((df_authors1$author_second == df_authors1$author_first) & !is.na(df_authors1$author_second) & !is.na(df_authors1$author_first), NA, df_authors1$author_second)
df_authors1$author_second_last <- ifelse((df_authors1$author_second_last == df_authors1$author_first) & !is.na(df_authors1$author_second_last) & !is.na(df_authors1$author_first), NA, df_authors1$author_second_last)
df_authors1$author_last <- ifelse((df_authors1$author_last == df_authors1$author_first) & !is.na(df_authors1$author_last) & !is.na(df_authors1$author_first), NA, df_authors1$author_last)
df_authors1$author_second_last <- ifelse((df_authors1$author_second_last == df_authors1$author_second)  & !is.na(df_authors1$author_second_last) & !is.na(df_authors1$author_second), NA, df_authors1$author_second_last)
df_authors1$author_last <- ifelse((df_authors1$author_last == df_authors1$author_second) & !is.na(df_authors1$author_last) & !is.na(df_authors1$author_second), NA, df_authors1$author_last)
df_authors1$author_last <- ifelse((df_authors1$author_last == df_authors1$author_second_last) & !is.na(df_authors1$author_last) & !is.na(df_authors1$author_second_last), NA, df_authors1$author_last)


# Modify names
for (i in 1:ncol(df_authors1)) {
  df_authors1[[i]] = gsub(" $", "", 
                          gsub("^ ", "", 
                               gsub("' ", "'", 
                                    gsub("- ", "-", 
                                         gsub("[[:alpha:]]\\.", "", 
                                              gsub("  ", " ", 
                                                   gsub("^[[:alnum:][:space:]-]*,", "", 
                                                        gsub("'", "", df_authors1[[i]] ))))))))
}
for (i in 1:ncol(df_authors1)) {
  df_authors1[[i]] = gsub(" $", "", 
                          gsub("^\\-", "", 
                               gsub("^\\ ", "", 
                                    gsub(",[[:space:]]{0,2}[[:alpha:]]{0,3}$", "", 
                                         gsub("  ", " ",
                                              gsub(" $", "", 
                                                   gsub("[^A-Za-z]$", "", 
                                                        gsub("^\\ ", "", df_authors1[[i]] ))))))))
}

# For difficult names, try to match just the first part of the name
for (i in 1:ncol(df_authors1)) {
  df_authors1[[i]] = gsub(" [[:print:]]*", "", df_authors1[[i]] )
}

# If there are 2 authors, convert 2nd author to last author
df_authors1 <- df_authors2
df_authors1$author_last_tmp <- ifelse(is.na(df_authors1$author_second_last) & is.na(df_authors1$author_last) & !is.na(df_authors1$author_second), df_authors1$author_second, NA)
df_authors1$author_second <- ifelse(is.na(df_authors1$author_second_last) & is.na(df_authors1$author_last) & !is.na(df_authors1$author_second), NA, df_authors1$author_second)
df_authors1$author_last <- ifelse(is.na(df_authors1$author_second_last) & is.na(df_authors1$author_last) & !is.na(df_authors1$author_last_tmp), df_authors1$author_last_tmp, df_authors1$author_last)
df_authors1$author_last_tmp <- NULL


# List of unique first names
firstnames = unique(x = c(df_authors1[[1]], df_authors1[[2]], df_authors1[[3]], df_authors1[[4]]))

# sum(df_authors1[[1]]=="", na.rm = TRUE)/nrow(df_authors1)
# sum(df_authors1[[2]]=="", na.rm = TRUE)/nrow(df_authors1)
# sum(df_authors1[[3]]=="", na.rm = TRUE)/nrow(df_authors1)
# sum(df_authors1[[4]]=="", na.rm = TRUE)/nrow(df_authors1)

# Load previously analyzed names
givenNames0 <- readRDS("data/combined_names.rds")
## Keep only unique new names
firstnames <- firstnames[!firstnames %in% givenNames0$probability]
## Analyze new names
givenNames = findGivenNames(firstnames, progress = FALSE, ssl.verifypeer = FALSE, textPrepare = FALSE, apikey = "836042cbb6d757723eee9316e3046780") %>%
  dplyr::distinct(probability, gender, count, .keep_all=FALSE)
## Combine with previously analyzed names
givenNames1 <- rbind(givenNames0, givenNames)
## Save for future use
saveRDS(givenNames1, "data/combined_names.rds")


# Keep genders
df_authors1 <- df_authors1 %>%
  dplyr::left_join(givenNames1 %>% select(-count), by=c("author_first" = "probability")) %>%
  dplyr::rename(author_first_gender = gender)
df_authors1 <- df_authors1 %>%
  dplyr::left_join(givenNames1 %>% select(-count), by=c("author_second" = "probability")) %>%
  dplyr::rename(author_second_gender = gender)
df_authors1 <- df_authors1 %>%
  dplyr::left_join(givenNames1 %>% select(-count), by=c("author_second_last" = "probability")) %>%
  dplyr::rename(author_second_last_gender = gender)
df_authors1 <- df_authors1 %>%
  dplyr::left_join(givenNames1 %>% select(-count), by=c("author_last" = "probability")) %>%
  dplyr::rename(author_last_gender = gender)
## Combine with df
df <- cbind(df, df_authors1)



# Statistics
if (exists("aa4")) { rm(aa4) }
for (i in c("author_first_gender", "author_last_gender")) {
  print(i)
  df$tmp <- df[[i]]
  df %>%
    group_by(tmp) %>%
    summarise(med_cit = median(times_cited_all_databases, na.rm = TRUE),
              iqr25_cit = quantile(times_cited_all_databases, na.rm = TRUE)[2],
              iqr75_cit = quantile(times_cited_all_databases, na.rm = TRUE)[4],
              med_authors = median(authors_tmp, na.rm = TRUE),
              iqr25_authors = quantile(authors_tmp, na.rm = TRUE)[2],
              iqr75_authors = quantile(authors_tmp, na.rm = TRUE)[4],
              med_pages = median(number_of_pages, na.rm = TRUE),
              iqr25_pages = quantile(number_of_pages, na.rm = TRUE)[2],
              iqr75_pages = quantile(number_of_pages, na.rm = TRUE)[4]) %>%
    print() %>%
    ungroup()
  
  aa1 <- df %>%
    filter(!is.na(tmp)) %>%
    rstatix::wilcox_test(times_cited_all_databases~tmp)
  
  aa2 <- df %>%
    filter(!is.na(tmp)) %>%
    rstatix::wilcox_test(authors_tmp~tmp)
  
  aa3 <- df %>%
    filter(!is.na(tmp)) %>%
    rstatix::wilcox_test(number_of_pages~tmp)
  
  aa3 <- rbind(rbind(aa1, aa2), aa3)
  
  if (exists("aa4")) {
    aa4 <- rbind(aa4, aa3) %>%
      mutate(padjust = adjust_pvalue(p, method="BH"))
  } else {
    aa4 <- aa3
  }
  
}
print(aa4)
table(df$author_first_gender); table(df$author_last_gender)


## Citations
g <- ggplot(df %>% dplyr::filter(!is.na(author_first_gender)), aes(x = author_first_gender, y = log(times_cited_all_databases, 10))) +
  geom_jitter(size=1, width = 0.2, fill="black", shape = 21, color = "black") +
  geom_boxplot(outlier.shape = NA, aes(fill=author_first_gender), alpha = 0.5) + 
  labs(y="# Citations (Log10)", x="Gender") +
  scale_fill_brewer(palette="Set1", name="Gender") +
  theme_bw() +
  stat_compare_means(method = "wilcox.test",
                     label = "p.signif",
                     label.x = 1.45,
                     size = 6) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/gender/Boxplot_n_citations_first_author_gender.png", width = 4, height = 4, units = "in", dpi = 300)

g <- ggplot(df %>% dplyr::filter(!is.na(author_second_gender)), aes(x = author_second_gender, y = log(times_cited_all_databases, 10))) +
  geom_jitter(size=1, width = 0.2, fill="black", shape = 21, color = "black") +
  geom_boxplot(outlier.shape = NA, aes(fill=author_second_gender), alpha = 0.5) + 
  labs(y="# Citations (Log10)", x="Gender") +
  scale_fill_brewer(palette="Set1", name="Gender") +
  theme_bw() +
  stat_compare_means(method = "wilcox.test",
                     label = "p.signif",
                     label.x = 1.45,
                     size = 6) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/gender/Boxplot_n_citations_second_author_gender.png", width = 4, height = 4, units = "in", dpi = 300)

g <- ggplot(df %>% dplyr::filter(!is.na(author_second_last_gender)), aes(x = author_second_last_gender, y = log(times_cited_all_databases, 10))) +
  geom_jitter(size=1, width = 0.2, fill="black", shape = 21, color = "black") +
  geom_boxplot(outlier.shape = NA, aes(fill=author_second_last_gender), alpha = 0.5) + 
  labs(y="# Citations (Log10)", x="Gender") +
  scale_fill_brewer(palette="Set1", name="Gender") +
  theme_bw() +
  stat_compare_means(method = "wilcox.test",
                     label = "p.signif",
                     label.x = 1.45,
                     size = 6) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/gender/Boxplot_n_citations_second_last_author_gender.png", width = 4, height = 4, units = "in", dpi = 300)

g <- ggplot(df %>% dplyr::filter(!is.na(author_last_gender)), aes(x = author_last_gender, y = log(times_cited_all_databases, 10))) +
  geom_jitter(size=1, width = 0.2, fill="black", shape = 21, color = "black") +
  geom_boxplot(outlier.shape = NA, aes(fill=author_last_gender), alpha = 0.5) + 
  labs(y="# Citations (Log10)", x="Gender") +
  scale_fill_brewer(palette="Set1", name="Gender") +
  theme_bw() +
  stat_compare_means(method = "wilcox.test",
                     label = "p.signif",
                     label.x = 1.45,
                     size = 6) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/gender/Boxplot_n_citations_last_author_gender.png", width = 4, height = 4, units = "in", dpi = 300)


## Number of pages
g <- ggplot(df %>% dplyr::filter(!is.na(author_first_gender)), aes(x = author_first_gender, y = log(number_of_pages, 10))) +
  geom_jitter(size=1, width = 0.2, fill="black", shape = 21, color = "black") +
  geom_boxplot(outlier.shape = NA, aes(fill=author_first_gender), alpha = 0.5) + 
  labs(y="# Pages (Log10)", x="Gender") +
  scale_fill_brewer(palette="Set1", name="Gender") +
  theme_bw() +
  stat_compare_means(method = "wilcox.test",
                     label = "p.signif",
                     label.x = 1.45,
                     size = 6) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/gender/Boxplot_n_pages_first_author_gender.png", width = 4, height = 4, units = "in", dpi = 300)

g <- ggplot(df %>% dplyr::filter(!is.na(author_second_gender)), aes(x = author_second_gender, y = log(number_of_pages, 10))) +
  geom_jitter(size=1, width = 0.2, fill="black", shape = 21, color = "black") +
  geom_boxplot(outlier.shape = NA, aes(fill=author_second_gender), alpha = 0.5) + 
  labs(y="# Pages (Log10)", x="Gender") +
  scale_fill_brewer(palette="Set1", name="Gender") +
  theme_bw() +
  stat_compare_means(method = "wilcox.test",
                     label = "p.signif",
                     label.x = 1.45,
                     size = 6) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/gender/Boxplot_n_pages_second_author_gender.png", width = 4, height = 4, units = "in", dpi = 300)

g <- ggplot(df %>% dplyr::filter(!is.na(author_second_last_gender)), aes(x = author_second_last_gender, y = log(number_of_pages, 10))) +
  geom_jitter(size=1, width = 0.2, fill="black", shape = 21, color = "black") +
  geom_boxplot(outlier.shape = NA, aes(fill=author_second_last_gender), alpha = 0.5) + 
  labs(y="# Pages (Log10)", x="Gender") +
  scale_fill_brewer(palette="Set1", name="Gender") +
  theme_bw() +
  stat_compare_means(method = "wilcox.test",
                     label = "p.signif",
                     label.x = 1.45,
                     size = 6) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/gender/Boxplot_n_pages_second_last_author_gender.png", width = 4, height = 4, units = "in", dpi = 300)

g <- ggplot(df %>% dplyr::filter(!is.na(author_last_gender)), aes(x = author_last_gender, y = log(number_of_pages, 10))) +
  geom_jitter(size=1, width = 0.2, fill="black", shape = 21, color = "black") +
  geom_boxplot(outlier.shape = NA, aes(fill=author_last_gender), alpha = 0.5) + 
  labs(y="# Pages (Log10)", x="Gender") +
  scale_fill_brewer(palette="Set1", name="Gender") +
  theme_bw() +
  stat_compare_means(method = "wilcox.test",
                     label = "p.signif",
                     label.x = 1.45,
                     size = 6) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/gender/Boxplot_n_pages_last_author_gender.png", width = 4, height = 4, units = "in", dpi = 300)


## Number of authors
g <- ggplot(df %>% dplyr::filter(!is.na(author_first_gender)), aes(x = author_first_gender, y = log(authors_tmp, 10))) +
  geom_jitter(size=1, width = 0.2, fill="black", shape = 21, color = "black") +
  geom_boxplot(outlier.shape = NA, aes(fill=author_first_gender), alpha = 0.5) + 
  labs(y="# Authors (LOG10)", x="Gender") +
  scale_fill_brewer(palette="Set1", name="Gender") +
  theme_bw() +
  stat_compare_means(method = "wilcox.test",
                     label = "p.signif",
                     label.x = 1.45,
                     size = 6) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/gender/Boxplot_n_authors_first_author_gender.png", width = 4, height = 4, units = "in", dpi = 300)

g <- ggplot(df %>% dplyr::filter(!is.na(author_second_gender)), aes(x = author_second_gender, y = log(authors_tmp, 10))) +
  geom_jitter(size=1, width = 0.2, fill="black", shape = 21, color = "black") +
  geom_boxplot(outlier.shape = NA, aes(fill=author_second_gender), alpha = 0.5) + 
  labs(y="# Authors (LOG10)", x="Gender") +
  scale_fill_brewer(palette="Set1", name="Gender") +
  theme_bw() +
  stat_compare_means(method = "wilcox.test",
                     label = "p.signif",
                     label.x = 1.45,
                     size = 6) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/gender/Boxplot_n_authors_second_author_gender.png", width = 4, height = 4, units = "in", dpi = 300)

g <- ggplot(df %>% dplyr::filter(!is.na(author_second_last_gender)), aes(x = author_second_last_gender, y = log(authors_tmp, 10))) +
  geom_jitter(size=1, width = 0.2, fill="black", shape = 21, color = "black") +
  geom_boxplot(outlier.shape = NA, aes(fill=author_second_last_gender), alpha = 0.5) + 
  labs(y="# Authors (LOG10)", x="Gender") +
  scale_fill_brewer(palette="Set1", name="Gender") +
  theme_bw() +
  stat_compare_means(method = "wilcox.test",
                     label = "p.signif",
                     label.x = 1.45,
                     size = 6) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/gender/Boxplot_n_authors_second_last_author_gender.png", width = 4, height = 4, units = "in", dpi = 300)

g <- ggplot(df %>% dplyr::filter(!is.na(author_last_gender)), aes(x = author_last_gender, y = log(authors_tmp, 10))) +
  geom_jitter(size=1, width = 0.2, fill="black", shape = 21, color = "black") +
  geom_boxplot(outlier.shape = NA, aes(fill=author_last_gender), alpha = 0.5) + 
  labs(y="# Authors (LOG10)", x="Gender") +
  scale_fill_brewer(palette="Set1", name="Gender") +
  theme_bw() +
  stat_compare_means(method = "wilcox.test",
                     label = "p.signif",
                     label.x = 1.45,
                     size = 6) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/gender/Boxplot_n_authors_last_author_gender.png", width = 4, height = 4, units = "in", dpi = 300)


wilcox.test(df$times_cited_all_databases~factor(df$author_first_gender))
median(df[df$author_first_gender=="female",]$times_cited_all_databases, na.rm=TRUE)
median(df[df$author_first_gender=="male",]$times_cited_all_databases, na.rm=TRUE)

wilcox.test(df$times_cited_all_databases~factor(df$author_second_gender))
median(df[df$author_second_gender=="female",]$times_cited_all_databases, na.rm=TRUE)
median(df[df$author_second_gender=="male",]$times_cited_all_databases, na.rm=TRUE)

wilcox.test(df$times_cited_all_databases~factor(df$author_second_last_gender))
median(df[df$author_second_last_gender=="female",]$times_cited_all_databases, na.rm=TRUE)
median(df[df$author_second_last_gender=="male",]$times_cited_all_databases, na.rm=TRUE)

wilcox.test(df$times_cited_all_databases~factor(df$author_last_gender))
median(df[df$author_last_gender=="female",]$times_cited_all_databases, na.rm=TRUE)
median(df[df$author_last_gender=="male",]$times_cited_all_databases, na.rm=TRUE)


## Number of pages
wilcox.test(df$number_of_pages~factor(df$author_first_gender))
median(df[df$author_first_gender=="female",]$number_of_pages, na.rm=TRUE)
median(df[df$author_first_gender=="male",]$number_of_pages, na.rm=TRUE)

wilcox.test(df$number_of_pages~factor(df$author_second_gender))
median(df[df$author_second_gender=="female",]$number_of_pages, na.rm=TRUE)
median(df[df$author_second_gender=="male",]$number_of_pages, na.rm=TRUE)

wilcox.test(df$number_of_pages~factor(df$author_second_last_gender))
median(df[df$author_second_last_gender=="female",]$number_of_pages, na.rm=TRUE)
median(df[df$author_second_last_gender=="male",]$number_of_pages, na.rm=TRUE)

wilcox.test(df$number_of_pages~factor(df$author_last_gender))
median(df[df$author_last_gender=="female",]$number_of_pages, na.rm=TRUE)
median(df[df$author_last_gender=="male",]$number_of_pages, na.rm=TRUE)



## Number of articles
table(df$author_first_gender)
table(df$author_second_gender)
table(df$author_second_last_gender)
table(df$author_last_gender)

### First author
df_sr <- df %>%
  group_by(author_first_gender) %>%
  summarise(n_articles = n())

g <- ggplot(df_sr %>% dplyr::filter(!is.na(author_first_gender)), aes(x = author_first_gender, y = n_articles, fill = author_first_gender)) +
  geom_bar(stat = "identity", color="black") +
  labs(y="# Publications") +
  scale_fill_brewer(palette="Set1", name="Gender") +
  geom_text(aes(label = n_articles),
            position = position_stack(vjust = .5)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/gender/Barplot_n_articles_first_author_gender.png", width = 5, height = 4, units = "in", dpi = 300)

### Second author
df_sr <- df %>%
  group_by(author_second_gender) %>%
  summarise(n_articles = n())

g <- ggplot(df_sr %>% dplyr::filter(!is.na(author_second_gender)), aes(x = author_second_gender, y = n_articles, fill = author_second_gender)) +
  geom_bar(stat = "identity", color="black") +
  labs(y="# Publications") +
  scale_fill_brewer(palette="Set1", name="Gender") +
  geom_text(aes(label = n_articles),
            position = position_stack(vjust = .5)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/gender/Barplot_n_articles_second_author_gender.png", width = 5, height = 4, units = "in", dpi = 300)

### Second last author
df_sr <- df %>%
  group_by(author_second_last_gender) %>%
  summarise(n_articles = n())

g <- ggplot(df_sr %>% dplyr::filter(!is.na(author_second_last_gender)), aes(x = author_second_last_gender, y = n_articles, fill = author_second_last_gender)) +
  geom_bar(stat = "identity", color="black") +
  labs(y="# Publications") +
  scale_fill_brewer(palette="Set1", name="Gender") +
  geom_text(aes(label = n_articles),
            position = position_stack(vjust = .5)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/gender/Barplot_n_articles_second_last_author_gender.png", width = 5, height = 4, units = "in", dpi = 300)

### Last author
df_sr <- df %>%
  group_by(author_last_gender) %>%
  summarise(n_articles = n())

g <- ggplot(df_sr %>% dplyr::filter(!is.na(author_last_gender)), aes(x = author_last_gender, y = n_articles, fill = author_last_gender)) +
  geom_bar(stat = "identity", color="black") +
  labs(y="# Publications") +
  geom_text(aes(label = n_articles),
            position = position_stack(vjust = .5)) +
  scale_fill_brewer(palette="Set1", name="Gender") +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black"),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/gender/Barplot_n_articles_last_author_gender.png", width = 5, height = 4, units = "in", dpi = 300)



### First and last author
chisq.test(df$author_first_gender, df$author_last_gender)
df_gender_sr <- rbind.data.frame(table(df$author_first_gender, df$author_last_gender) / rowSums(table(df$author_first_gender, df$author_last_gender)))
df_gender_sr1 <- rbind.data.frame(table(df$author_first_gender, df$author_last_gender))
colnames(df_gender_sr1)[3] <- "Freq1"
df_gender_sr <- full_join(df_gender_sr, df_gender_sr1)
colnames(df_gender_sr)[1:2] <- c("author_first_gender", "author_last_gender")

g <- ggplot(df_gender_sr, aes(x = author_first_gender, y=Freq, fill=author_last_gender)) +
  geom_bar(stat="identity", color="black", position = "stack") +
  scale_y_continuous(labels = scales::percent) +
  labs(y="Proportion (%)", x="First Author Gender") +
  geom_text(aes(label = paste0(Freq1, "\n", scales::percent(Freq))),
            position = position_stack(vjust = .5)) +
  scale_fill_brewer(palette="Set1", name="Last\nAuthor\nGender") +
  theme_bw() +
  theme(axis.text.x = element_text(size=12, colour = "black"),
        axis.text.y = element_text(size=12, colour = "black"),
        axis.title.y = element_text(size=12, face="bold", colour = "black"),
        axis.title.x = element_text(size=12, face="bold", colour = "black"),
        legend.title = element_text(size=12, face="bold", colour = "black", vjust=4.75, hjust=1),
        legend.text = element_text(size=12, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "bottom")
ggsave(plot = g, "results/gender/Barplot_first_last_author_gender.png", width = 3, height = 4, units = "in", dpi = 300)


########################################## GENDER STATISTICS BY PAGENUMBER AND AUTHORNUMBER ##########################################


# Modify data
df <- df %>%
  dplyr::mutate(number_of_pages_cat = ifelse(number_of_pages > median(number_of_pages, na.rm=TRUE), ">10", "≤10"),
                authors_tmp_cat = ifelse(authors_tmp > median(authors_tmp, na.rm=TRUE), ">11", "≤11"),
                author_first_gender = tools::toTitleCase(author_first_gender),
                author_second_gender = tools::toTitleCase(author_second_gender),
                author_second_last_gender = tools::toTitleCase(author_second_last_gender),
                author_last_gender = tools::toTitleCase(author_last_gender))

# Citations ~ Gender + Number of pages
for (i in c("author_first_gender", "author_last_gender")) {
  for (j in c("number_of_pages_cat", "authors_tmp_cat")) {
    
    ## Variables
    df$tmp <- df[[i]]; df$tmp1 <- df[[j]]
    
    ## xlab
    if (j == "number_of_pages_cat") {
      xlab = "# Pages"
    } else if (j == "authors_tmp_cat") {
      xlab = "# Authors"
    }
    
    g <- ggplot(df %>% dplyr::filter(!is.na(tmp)), aes(x = tmp1, y = log(times_cited_all_databases, 10), fill=factor(tmp))) +
      geom_point(position=position_jitterdodge(), size=1, color = "black") +
      geom_boxplot(outlier.shape = NA, alpha = 0.5) + 
      scale_x_discrete(limits = rev(levels(factor(df$tmp1)))) +
      labs(y="# Citations (LOG10)", x=xlab) +
      scale_fill_brewer(palette="Set1", name="Gender") +
      theme_bw() +
      stat_compare_means(method = "wilcox.test",                       label = "p.signif",
                         label.x = 1.45,
                         size = 6) +
      theme_bw() +
      theme(axis.text.x = element_text(size=14, colour = "black"),
            axis.text.y = element_text(size=14, colour = "black"),
            axis.title.y = element_text(size=14, face="bold", colour = "black"),
            axis.title.x = element_text(size=14, face="bold", colour = "black"),
            legend.title = element_text(size=14, face="bold", colour = "black"),
            legend.text = element_text(size=14, colour = "black"),
            legend.position = "bottom")
    ggsave(plot = g, paste0("results/gender/Boxplot_citations_", j, "_", i, ".png"), width = 4, height = 4, units = "in", dpi = 300)
    
  }
}


########################################## GENDER BY COUNTRIES ##########################################


## By countries
countries <- df_long_country1 %>% dplyr::filter(n_articles > 19) %>% dplyr::select(country.etc)


### First author
if (exists("df_plot1")) { rm(df_plot1) }
for (i in c("author_first_gender", "author_second_gender", "author_second_last_gender", "author_last_gender")) {
  
  print(i)
  
  # Set variable
  df$tmp <- df[[i]]
  
  # Process
  df_plot <- df %>%
    dplyr::filter(!is.na(tmp)) %>% 
    dplyr::select(tmp, one_of(a[a$n_articles > 19,]$country.etc)) %>%
    reshape2::melt(id.vars=c("tmp"), variable.name="Country") %>%
    filter(value!=0) %>%
    group_by(Country, tmp) %>%
    summarise(n=n()) %>%
    ungroup() %>%
    dplyr::mutate(tmp = tools::toTitleCase(tmp))
  
  # Dcast
  df_plot <- df_plot %>%
    reshape2::dcast(Country~tmp, value.var = "n", fill = 0)
  
  # Country as factor
  df_plot <- df_plot %>%
    arrange(desc(Male+Female))
  df_plot <- df_plot %>%
    dplyr::mutate(Country = factor(Country, levels = unique(df_plot$Country)),
                  Prop = Female/Male,
                  Author = tools::toTitleCase(gsub("_", " ",
                                                   gsub("author_", "",
                                                        gsub("_gender", "", i)))))
  
  # Save
  if (exists("df_plot1")) {
    df_plot1 <- rbind(df_plot1, df_plot)
  } else {
    df_plot1 <- df_plot
  }
  
}

## Process
df_plot1 <- df_plot1 %>%
  dplyr::mutate(Author = factor(Author, levels = c("First", "Second", "Second Last", "Last")),
                Prop_scale = Prop - 1,
                Prop_scale_abs = abs(Prop_scale))

# Balloonplot
p <- ggballoonplot(df_plot1, y = "Author", x = "Country",
                   fill = "Prop_scale",
                   size = "Prop_scale_abs",
                   # size.range = c(1, 10),
                   ggtheme = theme_bw()) +
  scale_size(breaks = c(0.1, 0.5, 1),
             labels=c(0.1, 0.5, 1),
             range = c(1, 10)) +
  scale_fill_gradientn(colours = c("blue","white","red"),
                       values = scales::rescale(c(-1,-0.6,0.5)),
                       guide = "colorbar") +
  guides(size = guide_legend(title="Female:Male Abs Diff", nrow = 1, title.vjust = 0.5),
         fill = guide_colorbar(title="Female:Male", title.vjust = 0.75)) +
  font("xy.text", size = 10, color = "black", face="plain") +
  theme(axis.title.y = element_text(size=12, colour="black", face="bold", angle = 90),
        axis.text.x = element_text(colour="black", angle = 45, hjust = 1),
        axis.title.x = element_text(size=12, colour="black", face="bold"),
        axis.text.y = element_text(colour="black"),
        legend.title = element_text(size=12, colour="black", face="bold"),
        legend.text = element_text(size=10, colour="black"),
        legend.position = "bottom", legend.direction = "horizontal", legend.box="horizontal", legend.margin=margin())
p
ggsave(plot = p, filename = "results/gender/Balloonplot_gender_country.png", width = 8, height = 3.5, units = "in", dpi = 300)


########################################## KEYWORDS ##########################################


# Split correspondence address by ;
df_kw <- df %>% 
  bind_cols(split_into_multiple(.$keywords_plus, ";", "type")) %>% 
  select(starts_with("type_"))

# Categorical
df_kw1 <- dummy_cols(df_kw[colnames(df_kw)]) %>%
  dplyr::select(-ends_with("_NA"))

## Finalize
colnames(df_kw1) <- gsub("type_[[:digit:]]{1,2}_", "", colnames(df_kw1))
colnames(df_kw1) <- gsub("^ ", "", colnames(df_kw1))
df_kw1 <- cbind(df_kw1[,(grep("type_", colnames(df_kw1), invert = FALSE))], t(rowsum(t(df_kw1[,(grep("type_", colnames(df_kw1), invert = TRUE))]), group = gsub("\\.[[:digit:]]$", "", unique(colnames(df_kw1[,(grep("type_", colnames(df_kw1), invert = TRUE))]))), na.rm = T)))
for (i in grep("type_", colnames(df_kw1), invert = TRUE)) {
  df_kw1[[i]] <- ifelse(df_kw1[[i]] > 1, 1, df_kw1[[i]])
}

## Filter only keywords occurring >9 times
v1 <- colSums(df_kw1[,(grep("type_", colnames(df_kw1), invert = TRUE))])
ww <- v1>19
df_kw2 <- cbind(df_kw1[,(grep("type_", colnames(df_kw1), invert = FALSE))], df_kw1[ncol(df_kw1[,(grep("type_", colnames(df_kw1), invert = FALSE))])+which(ww==TRUE)])

## Combine citation data
df_kw3 <- cbind(df %>% dplyr::select(ends_with("gender"), times_cited_all_databases), df_kw2)

### Mann-Whitney U test
w <- colnames(df_kw2[,(grep("type_", colnames(df_kw2), invert = TRUE))])
multiple_t_tests_p_value <- lapply(df_kw3[w], function(x) wilcox.test(df_kw3$times_cited_all_databases ~ x, na.rm=TRUE, exact=FALSE))
### P-values can be extracted from the result object
pvalue <- data.frame(p.value = sapply(multiple_t_tests_p_value, getElement, name = "p.value"))
### Create a matrix and dataframe of the p-values
pvalue_df_first <- pvalue %>% data.frame() %>% mutate(
  #### Add the p values to a new dataframe
  p_adjusted = p.adjust(p = as.matrix(pvalue), method = "BH"),
  #### Add also the t values, 95%CI to the same dataframe
  Med0 = unlist(lapply(df_kw3[w], function(x) median(df_kw3[x==0,]$times_cited_all_databases, na.rm=TRUE))),
  Med1 = unlist(lapply(df_kw3[w], function(x) median(df_kw3[x==1,]$times_cited_all_databases, na.rm=TRUE)))
) %>%
  ## Rownames to column
  rownames_to_column() %>%
  rename(Keyword = rowname,
         pvalue = p.value) %>%
  dplyr::mutate(
    FC = Med1 / Med0)
rownames(pvalue_df_first) = NULL

# Gender deviation
pvalue_df_gender_first <- df_kw3 %>%
  dplyr::group_by(author_first_gender) %>%
  summarise_at(vars(w), sum, na.rm = TRUE) %>%
  dplyr::filter(author_first_gender %in% c("male", "female")) %>%
  t() %>%
  data.frame() %>%
  rownames_to_column() %>%
  dplyr::mutate(X1 = unlist(X1),
                X2 = unlist(X2))
pvalue_df_gender_last <- df_kw3 %>%
  dplyr::group_by(author_last_gender) %>%
  summarise_at(vars(w), sum, na.rm = TRUE) %>%
  dplyr::filter(author_last_gender %in% c("male", "female")) %>%
  t() %>%
  data.frame() %>%
  rownames_to_column() %>%
  dplyr::mutate(X1 = unlist(X1),
                X2 = unlist(X2))


# Colnames
## First
colnames(pvalue_df_gender_first) <- as.matrix(pvalue_df_gender_first[1,])
colnames(pvalue_df_gender_first)[1] <- "Keyword"
pvalue_df_gender_first <- pvalue_df_gender_first[-1, ]
colnames(pvalue_df_gender_first)[2:3] <- paste0(colnames(pvalue_df_gender_first)[2:3], "_first")
## Last
colnames(pvalue_df_gender_last) <- as.matrix(pvalue_df_gender_last[1,])
colnames(pvalue_df_gender_last)[1] <- "Keyword"
pvalue_df_gender_last <- pvalue_df_gender_last[-1, ]
colnames(pvalue_df_gender_last)[2:3] <- paste0(colnames(pvalue_df_gender_last)[2:3], "_last")

# Join
pvalue_df_gender <- full_join(pvalue_df_gender_first, pvalue_df_gender_last)

# Factor to numeric
pvalue_df_gender$female_first <- as.numeric(as.character(pvalue_df_gender$female_first))
pvalue_df_gender$male_first <- as.numeric(as.character(pvalue_df_gender$male_first))
pvalue_df_gender$female_last <- as.numeric(as.character(pvalue_df_gender$female_last))
pvalue_df_gender$male_last <- as.numeric(as.character(pvalue_df_gender$male_last))

# Calculate proportions
## First author
pvalue_df_gender$female_prop_first <- pvalue_df_gender$female_first / sum(df_kw3$author_first_gender=="female", na.rm = TRUE)
pvalue_df_gender$male_prop_first <- pvalue_df_gender$male_first / sum(df_kw3$author_first_gender=="male", na.rm = TRUE)
pvalue_df_gender$F_to_M_first = ((pvalue_df_gender$female_prop_first / (pvalue_df_gender$male_prop_first + pvalue_df_gender$female_prop_first) * 100)/50)-1
## Last author
pvalue_df_gender$female_prop_last <- pvalue_df_gender$female_last / sum(df_kw3$author_last_gender=="female", na.rm = TRUE)
pvalue_df_gender$male_prop_last <- pvalue_df_gender$male_last / sum(df_kw3$author_last_gender=="male", na.rm = TRUE)
pvalue_df_gender$F_to_M_last = ((pvalue_df_gender$female_prop_last / (pvalue_df_gender$male_prop_last + pvalue_df_gender$female_prop_last) * 100)/50)-1


# Join pvalue_df_first and pvalue_df_gender
pvalue_df_first1 <- full_join(pvalue_df_first, pvalue_df_gender)


# First author
# Top 20
pvalue_df_first_top10 <- pvalue_df_first1 %>% dplyr::filter(p_adjusted < 0.05) %>% dplyr::arrange(desc(FC)) %>% slice(1:20)
pvalue_df_first_top10$prop <- pvalue_df_first_top10$FC/sum(pvalue_df_first_top10$FC)*100
pvalue_df_first_top10$ymax = cumsum(pvalue_df_first_top10$FC)
pvalue_df_first_top10$ymin = c(0, head(pvalue_df_first_top10$ymax, n = -1))
pvalue_df_first_top10_first <- pvalue_df_first_top10


# Plot
top20 <- ggplot(pvalue_df_first_top10_first, aes(fill = F_to_M_first, ymax = ymax, ymin = ymin, xmax = 100, xmin = 80)) +
  geom_rect(colour = "black") +
  coord_polar(theta = "y") + 
  xlim(c(0, 100)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", name="Female:Male",
                       midpoint = 0,
                       limits = c(-1, 1), na.value = "gray") +
  geom_label_repel(aes(label = paste(Keyword, round(prop, 1),"%"), x = 100, y = (ymin + ymax)/2), inherit.aes = F, show.legend = F, size = 5, fontface = "bold",
                   point.padding = 0, # additional padding around each point
                   min.segment.length = 0, # draw all line segments
                   max.iter = 1e5, # stop after 1 second, or after 100,000 iterations
                   box.padding = 0.3 ) + # additional padding around each text label) +
  theme_bw() +
  theme(legend.title = element_text(colour = "black", size = 16, face = "bold"), 
        legend.text = element_text(colour = "black", size = 15), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave(plot = top20, filename = "results/gender/keywords_top20_first_author.png", width = 8, height = 8, units = "in", dpi = 300)


# Low 20
pvalue_df_first_top10 <- pvalue_df_first1 %>% dplyr::filter(p_adjusted < 0.05) %>% dplyr::arrange(FC) %>% slice(1:20) %>% dplyr::mutate(FC = 1/FC)
pvalue_df_first_top10$prop <- pvalue_df_first_top10$FC/sum(pvalue_df_first_top10$FC)*100
pvalue_df_first_top10$ymax = cumsum(pvalue_df_first_top10$FC)
pvalue_df_first_top10$ymin = c(0, head(pvalue_df_first_top10$ymax, n = -1))
pvalue_df_first_low10_first <- pvalue_df_first_top10


# Plot
top20 <- ggplot(pvalue_df_first_low10_first, aes(fill = F_to_M_first, ymax = ymax, ymin = ymin, xmax = 100, xmin = 80)) +
  geom_rect(colour = "black") +
  coord_polar(theta = "y") + 
  xlim(c(0, 100)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", name="Female:Male",
                       midpoint = 0,
                       limits = c(-1, 1), na.value = "gray") +
  geom_label_repel(aes(label = paste(Keyword, round(prop, 1),"%"), x = 100, y = (ymin + ymax)/2), inherit.aes = F, show.legend = F, size = 5, fontface = "bold",
                   point.padding = 0, # additional padding around each point
                   min.segment.length = 0, # draw all line segments
                   max.iter = 1e5, # stop after 1 second, or after 100,000 iterations
                   box.padding = 0.3 ) + # additional padding around each text label) +
  theme_bw() +
  theme(legend.title = element_text(colour = "black", size = 16, face = "bold"), 
        legend.text = element_text(colour = "black", size = 15), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave(plot = top20, filename = "results/gender/keywords_low20_first_author.png", width = 8, height = 8, units = "in", dpi = 300)


# Last author
# Top 20
pvalue_df_first_top10 <- pvalue_df_first1 %>% dplyr::filter(p_adjusted < 0.05) %>% dplyr::arrange(desc(FC)) %>% slice(1:20)
pvalue_df_first_top10$prop <- pvalue_df_first_top10$FC/sum(pvalue_df_first_top10$FC)*100
pvalue_df_first_top10$ymax = cumsum(pvalue_df_first_top10$FC)
pvalue_df_first_top10$ymin = c(0, head(pvalue_df_first_top10$ymax, n = -1))
pvalue_df_first_top10_last <- pvalue_df_first_top10

# Plot
top20 <- ggplot(pvalue_df_first_top10_last, aes(fill = F_to_M_last, ymax = ymax, ymin = ymin, xmax = 100, xmin = 80)) +
  geom_rect(colour = "black") +
  coord_polar(theta = "y") + 
  xlim(c(0, 100)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", name="Female:Male",
                       midpoint = 0,
                       limits = c(-1, 1), na.value = "gray") +
  geom_label_repel(aes(label = paste(Keyword, round(prop, 1),"%"), x = 100, y = (ymin + ymax)/2), inherit.aes = F, show.legend = F, size = 5, fontface = "bold",
                   point.padding = 0, # additional padding around each point
                   min.segment.length = 0, # draw all line segments
                   max.iter = 1e5, # stop after 1 second, or after 100,000 iterations
                   box.padding = 0.3 ) + # additional padding around each text label) +
  theme_bw() +
  theme(legend.title = element_text(colour = "black", size = 16, face = "bold"), 
        legend.text = element_text(colour = "black", size = 15), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave(plot = top20, filename = "results/gender/keywords_top20_last.png", width = 8, height = 8, units = "in", dpi = 300)


# Low 20
pvalue_df_first_top10 <- pvalue_df_first1 %>% dplyr::filter(p_adjusted < 0.05) %>% dplyr::arrange(FC) %>% slice(1:20) %>% dplyr::mutate(FC = 1/FC)
pvalue_df_first_top10$prop <- pvalue_df_first_top10$FC/sum(pvalue_df_first_top10$FC)*100
pvalue_df_first_top10$ymax = cumsum(pvalue_df_first_top10$FC)
pvalue_df_first_top10$ymin = c(0, head(pvalue_df_first_top10$ymax, n = -1))
pvalue_df_first_top10$ymax2 = cumsum(pvalue_df_first_top10$FC)+20
pvalue_df_first_low10_last <- pvalue_df_first_top10

# Plot
top20 <- ggplot(pvalue_df_first_low10_last, aes(fill = F_to_M_last, ymax = ymax, ymin = ymin, xmax = 100, xmin = 80)) +
  geom_rect(colour = "black") +
  coord_polar(theta = "y") + 
  xlim(c(0, 100)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", name="Female:Male",
                       midpoint = 0,
                       limits = c(-1, 1), na.value = "gray") +
  geom_label_repel(aes(label = paste(Keyword, round(prop, 1),"%"), x = 100, y = (ymin + ymax)/2), inherit.aes = F, show.legend = F, size = 5, fontface = "bold",
                   point.padding = 0, # additional padding around each point
                   min.segment.length = 0, # draw all line segments
                   max.iter = 1e5, # stop after 1 second, or after 100,000 iterations
                   box.padding = 0.3 ) + # additional padding around each text label) +
  theme_bw() +
  theme(legend.title = element_text(colour = "black", size = 16, face = "bold"), 
        legend.text = element_text(colour = "black", size = 15), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave(plot = top20, filename = "results/gender/keywords_low20_last.png", width = 8, height = 8, units = "in", dpi = 300)





# LAST COMBINED
pvalue_df_first_top10_last1 <- pvalue_df_first_top10_last %>% dplyr::select(Keyword, F_to_M_first, F_to_M_last, prop, ymax, ymin)
pvalue_df_first_top10_last1 <- melt(pvalue_df_first_top10_last1, id.vars=c("Keyword", "prop", "ymax", "ymin"))

pvalue_df_first_top10_last2 <- pvalue_df_first_top10_last1 %>%
  mutate(Tot = sum(prop)) %>% 
  group_by(Keyword) %>% 
  mutate(CUM = cumsum(prop), DomSize = max(CUM))

# Calculate the bottom edge of the Domains when stacked
DomBot <- unique(select(pvalue_df_first_top10_last2, Keyword, Tot, DomSize)) %>% ungroup() %>% 
  mutate(Bottom = Tot - cumsum(DomSize))

pvalue_df_first_top10_last2 <- inner_join(pvalue_df_first_top10_last2, select(DomBot, Keyword, Bottom))

# Joining, by = "Domain"
pvalue_df_first_top10_last2 <- pvalue_df_first_top10_last2 %>%
  mutate(Pos = Bottom + CUM - prop/2)

## Add pvalues
pvalue_df_first_top10_last2 <- pvalue_df_first_top10_last2 %>%
  dplyr::left_join(pvalue_df_first_top10_last %>%
                     dplyr::select(Keyword, p_adjusted) %>%
                     dplyr::mutate(p_adjusted1 = ifelse(p_adjusted<0.001, "***",
                                                        ifelse(p_adjusted<0.01, "**",
                                                               ifelse(p_adjusted<0.05, "*", "")))))

# Plot
plt <- ggplot() +
  geom_col(aes(x = 2, y = prop*2, fill = value), 
           data = pvalue_df_first_top10_last2[pvalue_df_first_top10_last2$variable=="F_to_M_last",], color = "black") + 
  geom_col(aes(x = 3, y = prop*2, fill = value), 
           data = pvalue_df_first_top10_last2[pvalue_df_first_top10_last2$variable!="F_to_M_last",], color = "black") +
  geom_label_repel(data=pvalue_df_first_top10_last2[pvalue_df_first_top10_last2$variable=="F_to_M_first",],
                   aes(label = paste(gsub("PHASE-", "PHASE ",
                                          gsub("ACQUIRED-", "ACQUIRED\n",
                                               gsub("CYTIC-", "CYTIC\n",
                                                    gsub(" ", "\n", Keyword)))),
                                     round(prop, 1),"%"), x= 3.5, y = max(pvalue_df_first_top10_last2$Pos)-Pos, fill=value), size = 5, fontface = "bold",
                   point.padding = 0, # additional padding around each point
                   min.segment.length = 0, # draw all line segments
                   max.iter = 1e5, # stop after 1 second, or after 100,000 iterations
                   box.padding = 0.0 ) + # additional padding around each text label) +
  xlim(0, 3.5) +
  labs(x = NULL, y = NULL) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", name="Female:Male",
                       midpoint = 0,
                       limits = c(-1, 1), na.value = "gray") +
  theme_bw() +
  theme(legend.title = element_text(colour = "black", size = 16, face = "bold"), 
        legend.text = element_text(colour = "black", size = 15), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
g <- plt + coord_polar(theta = "y") 
g
ggsave(plot = g, filename = "results/gender/keywords_first20_combined.png", width = 10, height = 8, units = "in", dpi = 300)




# LAST COMBINED
pvalue_df_first_low10_first1 <- pvalue_df_first_low10_first %>% dplyr::select(Keyword, F_to_M_first, F_to_M_last, prop, ymax, ymin)
pvalue_df_first_low10_first1 <- melt(pvalue_df_first_low10_first1, id.vars=c("Keyword", "prop", "ymax", "ymin"))

pvalue_df_first_low10_first2 <- pvalue_df_first_low10_first1 %>%
  mutate(Tot = sum(prop)) %>% 
  group_by(Keyword) %>% 
  mutate(CUM = cumsum(prop), DomSize = max(CUM))

#Calculate the bottom edge of the Domains when stacked
DomBot <- unique(select(pvalue_df_first_low10_first2, Keyword, Tot, DomSize)) %>% ungroup() %>% 
  mutate(Bottom = Tot - cumsum(DomSize))

pvalue_df_first_low10_first2 <- inner_join(pvalue_df_first_low10_first2, select(DomBot, Keyword, Bottom))
# Joining, by = "Domain"
pvalue_df_first_low10_first2 <- pvalue_df_first_low10_first2 %>%
  mutate(Pos = Bottom + CUM - prop/2)
## Add pvalues
pvalue_df_first_low10_first2 <- pvalue_df_first_low10_first2 %>%
  dplyr::left_join(pvalue_df_first_low10_first %>%
                     dplyr::select(Keyword, p_adjusted) %>%
                     dplyr::mutate(p_adjusted1 = ifelse(p_adjusted<0.001, "***",
                                                        ifelse(p_adjusted<0.01, "**",
                                                               ifelse(p_adjusted<0.05, "*", "")))))

# Plot
plt <- ggplot() +
  geom_col(aes(x = 2, y = prop*2, fill = value), 
           data = pvalue_df_first_low10_first2[pvalue_df_first_low10_first2$variable=="F_to_M_last",], color = "black") + 
  geom_col(aes(x = 3, y = prop*2, fill = value), 
           data = pvalue_df_first_low10_first2[pvalue_df_first_low10_first2$variable!="F_to_M_last",], color = "black") +
  geom_label_repel(data=pvalue_df_first_low10_first2[pvalue_df_first_low10_first2$variable=="F_to_M_first",],
                   aes(label = paste(Keyword, round(prop, 1),"%"), x= 3.5, y = max(pvalue_df_first_low10_first2$Pos)-Pos, fill=value), size = 5, fontface = "bold",
                   point.padding = 0, # additional padding around each point
                   min.segment.length = 0, # draw all line segments
                   max.iter = 1e5, # stop after 1 second, or after 100,000 iterations
                   box.padding = 0.0 ) + # additional padding around each text label) +
  xlim(0, 3.5) +
  labs(x = NULL, y = NULL) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", name="Female:Male",
                       midpoint = 0,
                       limits = c(-1, 1), na.value = "gray") +
  theme_bw() +
  theme(legend.title = element_text(colour = "black", size = 16, face = "bold"), 
        legend.text = element_text(colour = "black", size = 15), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
g <- plt + coord_polar(theta = "y") 
ggsave(plot = g, filename = "results/gender/keywords_low20_combined.png", width = 10, height = 8, units = "in", dpi = 300)

tt <- data.frame(x=1, y=1)
tt1 <- data.frame(x=2, y=1)
plt <- ggplot() +
  geom_col(aes(x = x, y = y), width=1, size=1.5,
           data = tt, color = "black", fill="white") + 
  geom_col(aes(x = x, y = y), width=1.2, size=1.5,
           data = tt1, color = "black", fill="white") +
  xlim(0, 3) +
  labs(x = NULL, y = NULL) + 
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.text = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
g <- plt + coord_polar(theta = "y") 
ggsave(plot = g, filename = "results/gender/keywords_combined_legend.png", width = 4, height = 4, units = "in", dpi = 300)




# Spiral plot
## First author
pvalue_df_first1_spiral <- pvalue_df_first1
pvalue_df_first1_spiral$prop <- pvalue_df_first1_spiral$FC/sum(pvalue_df_first1_spiral$FC)*100
pvalue_df_first1_spiral$ymax = cumsum(pvalue_df_first1_spiral$FC)
pvalue_df_first1_spiral$ymin = c(0, head(pvalue_df_first1_spiral$ymax, n = -1))
pvalue_df_first1_spiral$ymax2 = cumsum(pvalue_df_first1_spiral$FC)+20
pvalue_df_first1_spiral$temp = rnorm(nrow(pvalue_df_first1_spiral), 10, 2)
pvalue_df_first1_spiral$FC2 = 0.05*(1:nrow(pvalue_df_first1_spiral)) + rnorm(nrow(pvalue_df_first1_spiral), 10, 2)/2
pvalue_df_first1_spiral <- pvalue_df_first1_spiral %>%
  arrange(FC)

# Correlation
cor.test(pvalue_df_first1_spiral$F_to_M_first, pvalue_df_first1_spiral$prop, method="spearman")
cor.test(pvalue_df_first1_spiral$F_to_M_last, pvalue_df_first1_spiral$prop, method="spearman")


g <- ggplot(pvalue_df_first1_spiral,
            aes(x = 1:nrow(pvalue_df_first1_spiral) %% 100, y = 0.05*1:nrow(pvalue_df_first1_spiral) + prop/2, width = 1, height = 5, fill = F_to_M_first)) + 
  geom_tile(color="black") + 
  scale_y_continuous(limits = c(-20, NA)) +
  scale_x_continuous(minor_breaks = NULL) +
  coord_polar() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", name="Female:Male",
                       midpoint = 0,
                       limits = c(-1, 1), na.value = "gray") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(legend.title = element_text(colour = "black", size = 16, face = "bold"), 
        legend.text = element_text(colour = "black", size = 15), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave(plot = g, filename = "results/gender/keywords_all_first_author.png", width = 6, height = 6, units = "in", dpi = 300)


# Spiral plot
## Last author
g <- ggplot(pvalue_df_first1_spiral,
            aes(x = 1:nrow(pvalue_df_first1_spiral) %% 100, y = 0.05*1:nrow(pvalue_df_first1_spiral) + prop/2, width = 1, height = 5, fill = F_to_M_last)) + 
  geom_tile(color="black") + 
  scale_y_continuous(limits = c(-20, NA)) +
  # scale_x_continuous(minor_breaks = NULL) +
  coord_polar() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", name="Female:Male",
                       midpoint = 0,
                       limits = c(-1, 1), na.value = "gray") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(legend.title = element_text(colour = "black", size = 16, face = "bold"), 
        legend.text = element_text(colour = "black", size = 15), 
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
ggsave(plot = g, filename = "results/gender/keywords_all_last_author.png", width = 6, height = 6, units = "in", dpi = 300)



########################################## REGRESSION MODEL ##########################################


df$times_cited_all_databases_log <- as.numeric(gsub("Inf", 0, log(df$times_cited_all_databases, 10)))
model <- lm(times_cited_all_databases_log ~ author_first_gender, data = df)
summary(model)
model <- lm(times_cited_all_databases_log ~ author_last_gender, data = df)
summary(model)
model <- lm(times_cited_all_databases_log ~ authors_tmp, data = df)
summary(model)
model <- lm(times_cited_all_databases_log ~ number_of_pages, data = df)
summary(model)
model <- lm(times_cited_all_databases_log ~ author_first_gender + number_of_pages, data = df)
summary(model)
model <- lm(times_cited_all_databases_log ~ author_first_gender + number_of_pages + author_first_gender*number_of_pages, data = df)
summary(model)
model <- lm(times_cited_all_databases_log ~ author_first_gender + authors_tmp, data = df)
summary(model)
model <- lm(times_cited_all_databases_log ~ author_first_gender + authors_tmp + author_first_gender*authors_tmp, data = df)
summary(model)
model <- lm(times_cited_all_databases_log ~ author_last_gender, data = df)
summary(model)
model <- lm(times_cited_all_databases_log ~ author_last_gender + authors_tmp, data = df)
summary(model)
model <- lm(times_cited_all_databases_log ~ author_last_gender + authors_tmp + author_last_gender*authors_tmp, data = df)
summary(model)


########################################## STATISTICS OF CITIES ##########################################


# City columns
tt <- df %>% dplyr::select(starts_with("City_")) %>% length()
tt1 <- which(colnames(df) == paste0("City_", tt))+1
tt2 <- which(colnames(df) == paste0("Country_1"))-1
df_city <- df[tt1:tt2]

# Country columns
tt <- df %>% dplyr::select(starts_with("Country_")) %>% length()
tt1 <- which(colnames(df) == paste0("Country_", tt))+1
tt2 <- which(colnames(df) == paste0("author_first"))-1
df_city_country <- df[tt1:tt2]


# Filter only cities ≥5 occurrences
v1 <- colSums(df_city)
ww <- v1>4
df_city1 <- df_city[which(ww==TRUE)]


# Identify city names located in two different countries (Cambridge, UK and Cambridge, USA)
duplicate_city_names <- df_world_city %>%
  distinct(name, country.etc) %>%
  group_by(name) %>% 
  filter(n()>1) %>%
  ungroup()
## Filter
duplicate_city_names1 <- names(df_city1)[names(df_city1) %in% duplicate_city_names$name]
duplicate_city_names <- duplicate_city_names %>% dplyr::filter(name %in% duplicate_city_names1)
## Remove countries which are not included in the analysis
duplicate_city_names <- duplicate_city_names %>% 
  dplyr::filter(country.etc %in% names(df_city_country))
## Filter
duplicate_city_names <- duplicate_city_names %>%
  distinct(name, country.etc) %>%
  group_by(name) %>% 
  filter(n()>1) %>%
  ungroup()

# Loop duplicate cities and countries
if (exists("dupl_cities")) {rm(dupl_cities)}
for (i in unique(duplicate_city_names$name)) {
  tmp <- cbind(i, rbind.data.frame(colSums(df_city_country[df_city1[i]==1, duplicate_city_names[duplicate_city_names$name==i,]$country.etc])[colSums(df_city_country[df_city1[i]==1,duplicate_city_names[duplicate_city_names$name==i,]$country.etc])>0]))
  colnames(tmp) <- c("City", names(colSums(df_city_country[df_city1[i]==1, duplicate_city_names[duplicate_city_names$name==i,]$country.etc])[colSums(df_city_country[df_city1[i]==1,duplicate_city_names[duplicate_city_names$name==i,]$country.etc])>0]))
  if (exists("dupl_cities")) {
    dupl_cities <- full_join(dupl_cities, tmp)
  } else {
    dupl_cities <- tmp
  }
}

# Identify cities
dupl_cities1 <- dupl_cities
dupl_cities1$dupl <- NA
dupl_cities1$Country_to_fix <- NA
for (i in 1:nrow(dupl_cities1)) {
  dupl_cities1[i,]$dupl <- ifelse(sum(colSums(!is.na(dupl_cities1[i,which(!colnames(dupl_cities1) %in% c("Country_to_fix", "City", "dupl"))])))>1,
                                  NA,
                                  t(t(apply(as.data.frame(!is.na(dupl_cities1[i,which(!colnames(dupl_cities1) %in% c("Country_to_fix", "City", "dupl"))])), 1, function(u) paste( names(which(u)), collapse=", " )))) )
  dupl_cities1[i,]$Country_to_fix <- ifelse(sum(colSums(!is.na(dupl_cities1[i,which(!colnames(dupl_cities1) %in% c("Country_to_fix", "City", "dupl"))])))>1,
                                            dupl_cities1[i,"Country_to_fix"] <- t(t(apply(as.data.frame(!is.na(dupl_cities1[i,which(!colnames(dupl_cities1) %in% c("City", "dupl", "Country_to_fix"))])), 1, function(u) paste( names(which(u)), collapse=", " )))),
                                            NA)
}
dupl_cities1$dupl <- ifelse(!is.na(dupl_cities1$dupl), paste0(dupl_cities1$City, "_", dupl_cities1$dupl), dupl_cities1$dupl)


# Split Country_to_fix by , to multiple columns
dupl_cities1 <- cbind(dupl_cities1, dupl_cities1 %>%
  bind_cols(split_into_multiple(.$Country_to_fix, ",", "Country_to_fix")) %>%
  select(starts_with("Country_to_fix_"))) %>%
  dplyr::select(-Country_to_fix)

# Specify the country for the rest by using regex and corresponding addresses
for (i in dupl_cities1[is.na(dupl_cities1$dupl),]$City) {
  for (j in colnames(dupl_cities1)[str_detect(colnames(dupl_cities1), "Country_to_fix")]) {
    print(paste0("Processing   ", i, ", ", j))
    assign("tmp", gsub("^ ", "", dupl_cities1[dupl_cities1$City==i,j]))
    tmp2 = ifelse(tmp == "UK",
                  paste0(i, "[^;]*", "UK|", i, "[^;]*", "England|", i, "[^;]*", "Scotland|", i, "[^;]*", "Wales"),
                  paste0(i, "[^;]*", tmp))
    df <- cbind(df, assign("tmp1", ifelse(df[[i]] == 1 & str_detect(df$addresses, tmp2), 1, 0)))
    colnames(df)[ncol(df)] <- paste0("city_", i, "_", gsub(" ", "", tmp))
  }
}

# test = c("Cambridge, Strangeways Res Lab, Dept Publ Hlth & Primary Care, Cambridge CB1 8RN, England")
# i = "Cambridge"
# # str_detect(test, paste0(i, sprintf("[^;]*"), "UK|", i, sprintf("[^;]*"), "England|", i, sprintf("[^;]*"), "Scotland|", i, sprintf("[^;]*"), "Wales"))
# str_detect(test, paste0(i, "[^;]*", "UK|", i, "[^;]*", "England|", i, "[^;]*", "Scotland|", i, "[^;]*", "Wales"))


# Correct colnames for others
for (i in dupl_cities1[!is.na(dupl_cities1$dupl),]$City) {
  colnames(df)[which(colnames(df) == i)] <- dupl_cities1[dupl_cities1$City==i, "dupl"]
}

# # Double-check
# df %>% dplyr::filter(Cambridge==1 & city_Cambridge_USA==0 & city_Cambridge_UK==0) %>% dplyr::select(addresses)
# sum(df$Cambridge)
# sum(df$city_Cambridge_USA)
# sum(df$city_Cambridge_UK)


# Move the new cities
df <- cbind(
  cbind(
    df[1:(which(colnames(df) == "Country_1")-1)],
    df[(which(colnames(df) == "authors_tmp_cat")+1):ncol(df)]),
  df[which(colnames(df) == "Country_1"):which(colnames(df) == "authors_tmp_cat")])
# Remove old cities
df <- df %>% dplyr::select(-one_of(dupl_cities1[is.na(dupl_cities1$dupl),]$City))
# Remove city_ prefix
colnames(df) <- gsub("city_", "", colnames(df))



# City columns
tt <- df %>% dplyr::select(starts_with("City_")) %>% length()
tt1 <- which(colnames(df) == paste0("City_", tt))+1
tt2 <- which(colnames(df) == paste0("Country_1"))-1
df_city <- df[tt1:tt2]

# Country columns
tt <- df %>% dplyr::select(starts_with("Country_")) %>% length()
tt1 <- which(colnames(df) == paste0("Country_", tt))+1
tt2 <- which(colnames(df) == paste0("author_first"))-1
df_city_country <- df[tt1:tt2]


# Filter only cities ≥5 occurrences
v1 <- colSums(df_city, na.rm=TRUE)
ww <- v1>4
df_city1 <- df_city[which(ww==TRUE)]


# Cbind
df_city1 <- cbind(df %>% dplyr::select(times_cited_all_databases, authors_tmp, ends_with("gender"), number_of_pages), df_city1)
df_city1$tmp <- NULL

### Mann-Whitney U test
w <- colnames(df_city1[(which(colnames(df_city1)=="number_of_pages")+1):ncol(df_city1)])
multiple_t_tests_p_value <- lapply(df_city1[w], function(x) wilcox.test(df_city1$times_cited_all_databases ~ x, na.rm=TRUE, exact=FALSE))
### P-values can be extracted from the result object
pvalue <- data.frame(p.value = sapply(multiple_t_tests_p_value, getElement, name = "p.value"))
### Create a matrix and dataframe of the p-values
pvalue_df_first <- pvalue %>% data.frame() %>% mutate(
  #### Add the p values to a new dataframe
  p_adjusted = p.adjust(p = as.matrix(pvalue), method = "BH"),
  #### Add also the t values, 95%CI to the same dataframe
  Med0 = unlist(lapply(df_city1[w], function(x) median(df_city1[x==0,]$times_cited_all_databases, na.rm=TRUE))),
  Med1 = unlist(lapply(df_city1[w], function(x) median(df_city1[x==1,]$times_cited_all_databases, na.rm=TRUE)))
) %>%
  ## Rownames to column
  rownames_to_column() %>%
  rename(City = rowname,
         pvalue = p.value) %>%
  dplyr::mutate(
    FC = Med1 / Med0+Med1)
rownames(pvalue_df_first) = NULL


# Summarise
df_city1_long <- df_city1 %>% 
  melt(id.vars = 1:7, variable.name = "City")
df_city1_sr1 <- df_city1_long %>%
  dplyr::group_by(City, value) %>%
  summarise_at(vars("times_cited_all_databases", "authors_tmp", "number_of_pages"), median, na.rm=TRUE)
df_city1_sr2 <- df_city1_long %>%
  dplyr::group_by(City, value) %>%
  summarise_at(vars("times_cited_all_databases"), sum, na.rm=TRUE) %>%
  rename(times_cited_all_databases_sum = times_cited_all_databases)
df_city1_sr3 <- df_city1_long %>%
  dplyr::group_by(City, value) %>%
  summarise(n=n())
df_city1_sr4 <- df_city1_long %>%
  dplyr::mutate_at(vars("author_first_gender", "author_second_gender", "author_second_last_gender", "author_last_gender"), .funs = function(x) ifelse(is.na(x), NA, ifelse(x=="male", 1, 0))) %>%
  dplyr::group_by(City, value) %>%
  summarise_at(vars("author_first_gender", "author_second_gender", "author_second_last_gender", "author_last_gender"), sum, na.rm=TRUE)

# Join
df_city1_sr <- full_join(full_join(full_join(df_city1_sr1, df_city1_sr2), df_city1_sr3), df_city1_sr4)

# Filter only city data
df_city1_sr_0 <- df_city1_sr %>% 
  dplyr::filter(value==1)

# Filter cities with ≥10 articles
sum(df_city1_sr_0[df_city1_sr_0$n>9,]$n); sum(df_city1_sr_0[df_city1_sr_0$n>9,]$n) / sum(df_city1_sr_0$n)
df_city1_sr_1 <- df_city1_sr_0 %>% 
  dplyr::filter(n>9)

# Add population
df_city1_sr_1 <- df_city1_sr_1 %>%
  dplyr::mutate(City1 = gsub("_[[:print:]]*", "", City)) %>%
  dplyr::left_join(df_world_city %>% dplyr::select(City1 = name, pop) %>% group_by(City1) %>% summarise(pop = max(pop)) %>% ungroup()) %>%
  dplyr::mutate(n_per_pop = n/pop*1000000) %>%
  dplyr::select(-City1)

# Label
df_city1_sr_1$achiever3 <- as.factor(ifelse(summary(lm(log(df_city1_sr_1$n, 10)~log(df_city1_sr_1$times_cited_all_databases_sum, 10)))$residuals > 0, "+", "-"))
df_city1_sr_1$achiever2 <- summary(lm(log(df_city1_sr_1$n, 10)~log(df_city1_sr_1$times_cited_all_databases_sum, 10)))$residuals

plot(log(df$authors_tmp, 10), log(df$times_cited_all_databases, 10))
cor.test(log(df$authors_tmp, 10), log(df$times_cited_all_databases, 10), method="spearman")

plot(log(df$number_of_pages, 10), log(df$times_cited_all_databases, 10))
cor.test(log(df$number_of_pages, 10), log(df$times_cited_all_databases, 10), method="spearman")


# Linear regression model for number of articles
fit <- lm(log(df_city1_sr_1$n, 10)~log(df_city1_sr_1$times_cited_all_databases_sum, 10))
summary(fit)
## Residual analysis
plot(y = fit$residuals, x = log(df_city1_sr_1$n, 10))
cor.test(y = fit$residuals, x = log(df_city1_sr_1$n, 10), method="spearman")
## Define predicted values
df_city1_sr_1$predicted <- predict(fit)
## Define difference between actual and predicted
(10^df_city1_sr_1$predicted)-df_city1_sr_1$n
df_city1_sr_1$n_articles_diff <- (10^df_city1_sr_1$predicted)-df_city1_sr_1$n
df_city1_sr_1$n_articles_diff_prop <- df_city1_sr_1$n_articles_diff/df_city1_sr_1$n
## Median of difference between actual and predicted
median(abs(df_city1_sr_1$n_articles_diff))

# Remove duplicates
df_city1_sr_1 <- df_city1_sr_1 %>%
  distinct()

# Proportion of publications from Boston, London and New York
zz <- df_city1_sr_1[order(df_city1_sr_1$n, decreasing = TRUE),][1:25,]; sum(zz$n_articles_diff>0)
zz <- df_city1_sr_1 %>% arrange(desc(n)); sum(zz[1:10,"n"])/sum(df_city1_sr_0$n)  #9940
df_city1_sr_1[df_city1_sr_1$City=="Boston (US)",]$n; df_city1_sr_1[df_city1_sr_1$City=="Boston (US)",]$n/sum(df_city1_sr_0$n)
df_city1_sr_1[df_city1_sr_1$City=="London (UK)",]$n; df_city1_sr_1[df_city1_sr_1$City=="London (UK)",]$n/sum(df_city1_sr_0$n)
df_city1_sr_1[df_city1_sr_1$City=="New York",]$n; df_city1_sr_1[df_city1_sr_1$City=="New York",]$n/sum(df_city1_sr_0$n)
sum(zz[1:20,]$n_articles_diff>0)
cor.test(df_city1_sr_1$n_per_pop, df_city1_sr_1$n, method = "spearman")

# Correct City names
df_city1_sr_1$City <- ifelse(str_detect(df_city1_sr_1$City, "_"), as.character(paste0(df_city1_sr_1$City, ")")), as.character(df_city1_sr_1$City))
df_city1_sr_1$City <- ifelse(str_detect(df_city1_sr_1$City, "_"), gsub("_", " (", df_city1_sr_1$City), df_city1_sr_1$City)
df_city1_sr_1$City <- gsub("France", "FR", 
                           gsub("Spain", "ES", 
                                gsub("Italy", "IT",
                                     gsub("Belgium", "BE", 
                                          gsub("Netherlands", "NL", 
                                               gsub("Australia", "AU", 
                                                    gsub("USA", "US", 
                                                         gsub("Ireland", "IE", 
                                                              gsub("Germany", "DE", df_city1_sr_1$City)))))))))


# Plot
g <- ggplot(df_city1_sr_1, aes(y = log(n, 10), x = log(times_cited_all_databases_sum, 10),
                               label = paste0(City, "\n", scales::percent(n_articles_diff_prop)))) +
  geom_point(aes(fill=log(n_per_pop, 10), size=-n_articles_diff), shape = 21, color="black", alpha = 0.85) +
  stat_smooth(method = "lm") +
  geom_label_repel(data = df_city1_sr_1[log(df_city1_sr_1$times_cited_all_databases_sum, 10)>4 | df_city1_sr_1$achiever2 <= quantile(df_city1_sr_1$achiever2, 0.10) | df_city1_sr_1$achiever2 >= quantile(df_city1_sr_1$achiever2, 0.9),],
                   aes(color=achiever3),
                   min.segment.length = 0, seed = 42, box.padding = 1) +
  scale_fill_distiller(palette = "RdBu", direction = -1, name="# Publications/1 000 000 people (LOG10)") +
  scale_color_brewer(palette = "Set1", direction = 1, guide = "none") +
  scale_size(range = c(1, 20), name="# Publication excess", breaks = c(10, 100, 500)) +  # , guide = "none"
  guides(
    size = guide_legend(title.vjust = 0.55, ncol=3)) +
  labs(x="# Citations (LOG10)", y="# Publications (LOG10)") +
  theme_bw() +
  theme(axis.text.x = element_text(size=12, colour = "black"),
        axis.text.y = element_text(size=12, colour = "black"),
        axis.title.y = element_text(size=16, face="bold", colour = "black"),
        axis.title.x = element_text(size=16, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black", vjust = 0.8),
        legend.text = element_text(size=14, colour = "black"),
        legend.box = 'vertical',
        legend.spacing.y = unit(0.1, 'cm'),
        legend.position = "bottom")
ggsave(plot = g, filename = "results/publications/Correlation_publications_articles_city.png", width = 8, height = 9, dpi = 300, units = "in")

  
########################################## STATISTICS OF JOURNALS AND THEIR NATIONALITES ##########################################


# COMPARE BY JOURNALS
# Journals
df$nejm <- ifelse(df$journal_abbreviation == "NEW ENGL J MED", 1, 0)
df$natmed <- ifelse(df$journal_abbreviation == "NAT MED", 1, 0)
df$jama <- ifelse(df$journal_abbreviation == "JAMA-J AM MED ASSOC", 1, 0)
df$bmj <- ifelse(df$journal_abbreviation == "BMJ-BRIT MED J", 1, 0)
df$lancet <- ifelse(df$journal_abbreviation == "LANCET", 1, 0)

## NEJM
multiple_t_tests_p_value <- lapply(df[countries$country.etc], function(x) wilcox.test(x ~ df$nejm, na.rm=TRUE, exact=FALSE))
### P-values can be extracted from the result object
pvalue <- data.frame(p.value = sapply(multiple_t_tests_p_value, getElement, name = "p.value"))
### Create a matrix and dataframe of the p-values
pvalue_df_nejm <- pvalue %>% data.frame() %>% mutate(
  #### Add the p values to a new dataframe
  p_adjusted = p.adjust(p = as.matrix(pvalue), method = "BH"),
  #### Add also the t values, 95%CI to the same dataframe
  sum_1 = unlist(lapply(df[countries$country.etc], function(x) sum(df$nejm[x==1], na.rm=TRUE))),
  sum_0 = unlist(lapply(df[countries$country.etc], function(x) sum(df$nejm[x==0], na.rm=TRUE)))
) %>%
  ### Rownames to column
  rownames_to_column() %>%
  rename(Country = rowname,
         pvalue = p.value) %>%
  dplyr::mutate(
    Journal = "NEJM",
    Prop = sum_1 / (sum_1+sum_0) * 100,
    sig = ifelse(p_adjusted < 0.001, "***", ifelse(p_adjusted < 0.01, "**", ifelse(p_adjusted < 0.05, "*", ""))))
rownames(pvalue_df_nejm) = NULL


## JAMA
multiple_t_tests_p_value <- lapply(df[countries$country.etc], function(x) wilcox.test(x ~ df$jama, na.rm=TRUE, exact=FALSE))
### P-values can be extracted from the result object
pvalue <- data.frame(p.value = sapply(multiple_t_tests_p_value, getElement, name = "p.value"))
### Create a matrix and dataframe of the p-values
pvalue_df_jama <- pvalue %>% data.frame() %>% mutate(
  #### Add the p values to a new dataframe
  p_adjusted = p.adjust(p = as.matrix(pvalue), method = "BH"),
  #### Add also the t values, 95%CI to the same dataframe
  sum_1 = unlist(lapply(df[countries$country.etc], function(x) sum(df$jama[x==1], na.rm=TRUE))),
  sum_0 = unlist(lapply(df[countries$country.etc], function(x) sum(df$jama[x==0], na.rm=TRUE)))
) %>%
  ### Rownames to column
  rownames_to_column() %>%
  rename(Country = rowname,
         pvalue = p.value) %>%
  dplyr::mutate(
    Journal = "JAMA",
    Prop = sum_1 / (sum_1+sum_0) * 100,
    sig = ifelse(p_adjusted < 0.001, "***", ifelse(p_adjusted < 0.01, "**", ifelse(p_adjusted < 0.05, "*", ""))))
rownames(pvalue_df_jama) = NULL

## NATMED
multiple_t_tests_p_value <- lapply(df[countries$country.etc], function(x) wilcox.test(x ~ df$natmed, na.rm=TRUE, exact=FALSE))
### P-values can be extracted from the result object
pvalue <- data.frame(p.value = sapply(multiple_t_tests_p_value, getElement, name = "p.value"))
### Create a matrix and dataframe of the p-values
pvalue_df_natmed <- pvalue %>% data.frame() %>% mutate(
  #### Add the p values to a new dataframe
  p_adjusted = p.adjust(p = as.matrix(pvalue), method = "BH"),
  #### Add also the t values, 95%CI to the same dataframe
  sum_1 = unlist(lapply(df[countries$country.etc], function(x) sum(df$natmed[x==1], na.rm=TRUE))),
  sum_0 = unlist(lapply(df[countries$country.etc], function(x) sum(df$natmed[x==0], na.rm=TRUE)))
) %>%
  ### Rownames to column
  rownames_to_column() %>%
  rename(Country = rowname,
         pvalue = p.value) %>%
  dplyr::mutate(
    Journal = "NATMED",
    Prop = sum_1 / (sum_1+sum_0) * 100,
    sig = ifelse(p_adjusted < 0.001, "***", ifelse(p_adjusted < 0.01, "**", ifelse(p_adjusted < 0.05, "*", ""))))
rownames(pvalue_df_natmed) = NULL

## BMJ
multiple_t_tests_p_value <- lapply(df[countries$country.etc], function(x) wilcox.test(x ~ df$bmj, na.rm=TRUE, exact=FALSE))
### P-values can be extracted from the result object
pvalue <- data.frame(p.value = sapply(multiple_t_tests_p_value, getElement, name = "p.value"))
### Create a matrix and dataframe of the p-values
pvalue_df_bmj <- pvalue %>% data.frame() %>% mutate(
  #### Add the p values to a new dataframe
  p_adjusted = p.adjust(p = as.matrix(pvalue), method = "BH"),
  #### Add also the t values, 95%CI to the same dataframe
  sum_1 = unlist(lapply(df[countries$country.etc], function(x) sum(df$bmj[x==1], na.rm=TRUE))),
  sum_0 = unlist(lapply(df[countries$country.etc], function(x) sum(df$bmj[x==0], na.rm=TRUE)))
) %>%
  ### Rownames to column
  rownames_to_column() %>%
  rename(Country = rowname,
         pvalue = p.value) %>%
  dplyr::mutate(
    Journal = "BMJ",
    Prop = sum_1 / (sum_1+sum_0) * 100,
    sig = ifelse(p_adjusted < 0.001, "***", ifelse(p_adjusted < 0.01, "**", ifelse(p_adjusted < 0.05, "*", ""))))
rownames(pvalue_df_bmj) = NULL

## LANCET
multiple_t_tests_p_value <- lapply(df[countries$country.etc], function(x) wilcox.test(x ~ df$lancet, na.rm=TRUE, exact=FALSE))
### P-values can be extracted from the result object
pvalue <- data.frame(p.value = sapply(multiple_t_tests_p_value, getElement, name = "p.value"))
### Create a matrix and dataframe of the p-values
pvalue_df_lancet <- pvalue %>% data.frame() %>% mutate(
  #### Add the p values to a new dataframe
  p_adjusted = p.adjust(p = as.matrix(pvalue), method = "BH"),
  #### Add also the t values, 95%CI to the same dataframe
  sum_1 = unlist(lapply(df[countries$country.etc], function(x) sum(df$lancet[x==1], na.rm=TRUE))),
  sum_0 = unlist(lapply(df[countries$country.etc], function(x) sum(df$lancet[x==0], na.rm=TRUE)))
) %>%
  ### Rownames to column
  rownames_to_column() %>%
  rename(Country = rowname,
         pvalue = p.value) %>%
  dplyr::mutate(
    Journal = "LANCET",
    Prop = sum_1 / (sum_1+sum_0) * 100,
    sig = ifelse(p_adjusted < 0.001, "***", ifelse(p_adjusted < 0.01, "**", ifelse(p_adjusted < 0.05, "*", ""))))
rownames(pvalue_df_lancet) = NULL


# Join
pvalue_df_journals <- full_join(full_join(full_join(full_join(pvalue_df_jama, pvalue_df_nejm), pvalue_df_natmed), pvalue_df_bmj), pvalue_df_lancet)
# 598/(598+1119)
# 691/(691+1771)
# table(df$UK); 1691/(1691+8864)
uk1 <- pvalue_df_journals %>% dplyr::filter(Journal %in% c("JAMA", "NEJM", "NATMED") & Country == "UK")
uk2 <- pvalue_df_journals %>% dplyr::filter(!Journal %in% c("JAMA", "NEJM", "NATMED") & Country == "UK")
mean(uk1$Prop)
mean(uk2$Prop)
table(df[df$nejm==1,]$UK) + table(df[df$natmed==1,]$UK) + table(df[df$jama==1,]$UK); 402/(5973+402)
table(df[df$bmj==1,]$UK) + table(df[df$lancet==1,]$UK); 1289/(2890+1289)
(1289/(2890+1289)) / (402/(5973+402))


# Plot
## NEJM
g <- ggplot(pvalue_df_journals %>% dplyr::filter(Journal=="NEJM"), aes(x = reorder(Country, -Prop), y = Prop, fill = Prop)) +
  geom_bar(stat = "identity", color="black") +
  labs(y="Proportion of Publications (%)") +
  geom_text(aes(label = sig), vjust = -0.1, colour = "black") +
  scale_fill_distiller(palette = "Spectral", name="Country") +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/journals/Barplot_publications_nejm.png", width = 10, height = 4, units = "in", dpi = 300)


## LANCET
g <- ggplot(pvalue_df_journals %>% dplyr::filter(Journal=="LANCET"), aes(x = reorder(Country, -Prop), y = Prop, fill = Prop)) +
  geom_bar(stat = "identity", color="black") +
  labs(y="Proportion of Publications (%)") +
  geom_text(aes(label = sig), vjust = -0.1, colour = "black") +
  scale_fill_distiller(palette = "Spectral", name="Country") +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/journals/Barplot_publications_lancet.png", width = 10, height = 4, units = "in", dpi = 300)


## NATMED
g <- ggplot(pvalue_df_journals %>% dplyr::filter(Journal=="NATMED"), aes(x = reorder(Country, -Prop), y = Prop, fill = Prop)) +
  geom_bar(stat = "identity", color="black") +
  labs(y="Proportion of Publications (%)") +
  geom_text(aes(label = sig), vjust = -0.1, colour = "black") +
  scale_fill_distiller(palette = "Spectral", name="Country") +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/journals/Barplot_publications_natmed.png", width = 10, height = 4, units = "in", dpi = 300)


## BMJ
g <- ggplot(pvalue_df_journals %>% dplyr::filter(Journal=="BMJ"), aes(x = reorder(Country, -Prop), y = Prop, fill = Prop)) +
  geom_bar(stat = "identity", color="black") +
  labs(y="Proportion of Publications (%)") +
  geom_text(aes(label = sig), vjust = -0.1, colour = "black") +
  scale_fill_distiller(palette = "Spectral", name="Country") +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, filename = "results/journals/Barplot_publications_bmj.png", width = 10, height = 4, units = "in", dpi = 300)


## JAMA
g <- ggplot(pvalue_df_journals %>% dplyr::filter(Journal=="JAMA"), aes(x = reorder(Country, -Prop), y = Prop, fill = Prop)) +
  geom_bar(stat = "identity", color="black") +
  labs(y="Proportion of Publications (%)") +
  geom_text(aes(label = sig), vjust = -0.1, colour = "black") +
  scale_fill_distiller(palette = "Spectral", name="Country") +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/journals/Barplot_publications_jama.png", width = 10, height = 4, units = "in", dpi = 300)


# COMPARE BY COUNTRIES
df$journal_abbreviation <- gsub("BMJ-", "BMJ ", df$journal_abbreviation)
multiple_t_tests_p_value <- lapply(df[countries$country.etc], function(x) kruskal.test(df$journal_abbreviation, x, na.rm=TRUE, exact=FALSE))
### P-values can be extracted from the result object
pvalue <- data.frame(p.value = sapply(multiple_t_tests_p_value, getElement, name = "p.value"))
### Create a matrix and dataframe of the p-values
pvalue_df_country <- pvalue %>% data.frame() %>% mutate(
  #### Add the p values to a new dataframe
  p_adjusted = p.adjust(p = as.matrix(pvalue), method = "BH")) %>%
  rownames_to_column(var="Country") %>%
  dplyr::mutate(Country=gsub("\\.", " ", Country))
# Summary data
sr_data <- rbind.data.frame(lapply(df[countries$country.etc], function(x) (table(df$journal_abbreviation, x) / rowSums(table(df$journal_abbreviation, x)))))
sr_data$journal <- rep(rownames(table(df$journal_abbreviation, df$UK)/rowSums(table(df$journal_abbreviation, df$UK))), 2)
## Keep only last 4 rows
sr_data <- sr_data[(length(unique(sr_data$journal))+1):nrow(sr_data),]
## To longer
sr_data_long <- reshape2::melt(sr_data, id.vars="journal", variable.name="Country", value.name="Prop")
## To wider
sr_data_long2 <- reshape2::dcast(sr_data_long, Country~journal, value.var="Prop")
## Rename Country
sr_data_long2 <- sr_data_long2 %>%
  dplyr::mutate(Country=gsub("\\.", " ", Country))
## Join
pvalue_df_country <- full_join(pvalue_df_country, sr_data_long2)

## Filter significant associations
pvalue_df_country1 <- pvalue_df_country %>%
  dplyr::filter(p_adjusted < 0.05)

## To percent
pvalue_df_country1[4:ncol(pvalue_df_country1)] <- pvalue_df_country1[4:ncol(pvalue_df_country1)]*100

## To long
pvalue_df_country2 <- reshape2::melt(pvalue_df_country1, id.vars=1:3, variable.name="Journal", value.name="Prop")

## Modify
pvalue_df_country2 <- pvalue_df_country2 %>%
  dplyr::mutate(Journal = ifelse(Journal=="JAMA-J AM MED ASSOC", "JAMA",
                                 ifelse(Journal=="NAT MED", "NATMED",
                                        ifelse(Journal=="BMJ BRIT MED J", "BMJ",
                                               ifelse(Journal=="NEW ENGL J MED", "NEJM", "LANCET")))),
                sig = ifelse(p_adjusted < 0.001, "***", ifelse(p_adjusted < 0.01, "**", ifelse(p_adjusted < 0.05, "*", ""))),
                Country1 = paste0(Country, " ", sig))


# Plot
g <- ggplot(pvalue_df_country2, aes(x = Journal, y = Prop, fill=Journal)) +
  geom_bar(stat = "identity", color="black") +
  labs(y="Proportion of Publications (%)") +
  scale_fill_brewer(palette="Set1", name="Journal") +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL") +
  facet_wrap("Country1", scales = "free", nrow = 2)
ggsave(plot = g, "results/journals/Barplot_publications_all.png", width = 10, height = 4, units = "in", dpi = 300)







## Gender by journals
multiple_t_tests_p_value <- lapply(df[c(grep("gender", colnames(df)))], function(x) chisq.test(df$journal_abbreviation, x))
### P-values can be extracted from the result object
pvalue <- data.frame(p.value = sapply(multiple_t_tests_p_value, getElement, name = "p.value"))
### Create a matrix and dataframe of the p-values
pvalue_df_gender_journal <- pvalue %>% data.frame() %>% mutate(
  #### Add the p values to a new dataframe
  p_adjusted = p.adjust(p = as.matrix(pvalue), method = "BH")
) %>%
  ### Rownames to column
  rownames_to_column() %>%
  rename(Author = rowname,
         pvalue = p.value)
rownames(pvalue_df_gender_journal) = NULL
### Values
if (exists("pvalue_df_gender_journal3")) { rm(pvalue_df_gender_journal3) }
for (i in grep("gender", colnames(df))) {
  pvalue_df_gender_journal1 <- data.frame(table(df[df[[i]]=="male",]$journal_abbreviation))
  pvalue_df_gender_journal1$Author <- names(df[i])
  pvalue_df_gender_journal1$gender <- "male"
  pvalue_df_gender_journal2 <- data.frame(table(df[df[[i]]=="female",]$journal_abbreviation))
  pvalue_df_gender_journal2$Author <- names(df[i])
  pvalue_df_gender_journal2$gender <- "female"
  if (exists("pvalue_df_gender_journal3")) {
    pvalue_df_gender_journal3 <- rbind(rbind(pvalue_df_gender_journal3, pvalue_df_gender_journal1), pvalue_df_gender_journal2)
  } else {
    pvalue_df_gender_journal3 <- rbind(pvalue_df_gender_journal1, pvalue_df_gender_journal2)
  }
}
### Process data
pvalue_df_gender_journal3 <- pvalue_df_gender_journal3 %>%
  dplyr::left_join(pvalue_df_gender_journal) %>%
  dplyr::rename(Journal = Var1) %>%
  dplyr::mutate(Journal = ifelse(Journal=="JAMA-J AM MED ASSOC", "JAMA",
                                 ifelse(Journal=="NAT MED", "NATMED",
                                        ifelse(Journal=="BMJ BRIT MED J", "BMJ",
                                               ifelse(Journal=="NEW ENGL J MED", "NEJM", "LANCET"))))) %>%
  dplyr::mutate(gender = tools::toTitleCase(gender))
### Transform to proportions and keep only female data
pvalue_df_gender_journal3_plot <- pvalue_df_gender_journal3 %>%
  dplyr::filter(gender == "Female") %>%
  dplyr::rename(Freq_female = Freq) %>%
  dplyr::select(-gender) %>%
  dplyr::left_join(pvalue_df_gender_journal3 %>% dplyr::filter(gender == "Male") %>% dplyr::select(-gender) %>% dplyr::rename(Freq_male = Freq)) %>%
  dplyr::mutate(Prop = Freq_female / (Freq_female + Freq_male) * 100) %>%
  dplyr::mutate(Author = tools::toTitleCase(paste0(gsub("_", " ",
                                                        gsub("author_", "",
                                                             gsub("_gender", "", Author))), " author"))) %>%
  dplyr::mutate(pvalue = ifelse(pvalue<0.001, "***",
                                ifelse(pvalue<0.01, "**",
                                       ifelse(pvalue<0.05, "*", "ns"))),
                Author = factor(Author, levels=c("First Author", "Second Author", "Second Last Author", "Last Author")))

### Plot
g <- ggplot(pvalue_df_gender_journal3_plot, aes(x = Journal, y = Prop, fill=Journal)) +
  geom_bar(stat = "identity", color="black") +
  labs(y="Proportion of Publications (%)") +
  ylim(0, max(round(pvalue_df_gender_journal3_plot$Prop/10, 0)*10)+10 ) +
  scale_fill_brewer(palette="Set1", name="Author") +
  geom_text(aes(label = pvalue, x=3, y = 0.875*(max(round(pvalue_df_gender_journal3_plot$Prop/10, 0)*10)+10)), size=4, vjust = -0.1, colour = "black") +
  geom_text(aes(label = ifelse(str_detect(round(Prop, 1), "\\."), round(Prop, 1), paste0(round(Prop, 1), ".0")),
                               x=Journal, y = Prop), size=2.75, vjust = -0.3, colour = "black") +
  # scale_fill_distiller(palette = "Spectral", name="Country") +
  theme_bw() +
  theme(axis.text.x = element_text(size=12, colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size=12, colour = "black"),
        axis.title.y = element_text(size=11, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_text(size=12, face="bold", colour = "black"),
        legend.text = element_text(size=12, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL") +
  facet_wrap("Author", nrow = 1)
ggsave(plot = g, "results/journals/Gender_publications_all.png", width = 6, height = 2.75, units = "in", dpi = 300)


################################# TIME-ANALYSIS #########################################################################################################


# Save image and reload
save.image(file='data_export/Session.RData')
# load('data_export/Session.RData')


# Plot
for (i in c("author_first_gender", "author_last_gender", "number_of_pages_cat", "authors_tmp_cat")) {
  
  print(i)
  df$tmp <- df[[i]]
  
  # Adjust p values
  pairwise.test = df %>%
    dplyr::filter(!is.na(tmp)) %>%
    group_by(publication_year) %>%
    wilcox_test(times_cited_all_databases~tmp) %>%
    adjust_pvalue(method = 'BH') %>%
    mutate(p.adj = round(p.adj, 2)) %>%
    mutate(p = ifelse(p.adj < 0.001, "***",
                      ifelse(p.adj < 0.01, "**",
                             ifelse(p.adj < 0.05, "*", round(p.adj, 2)))))
  
  # Summarise
  df_plot <- df %>%
    dplyr::filter(!is.na(tmp)) %>%
    group_by(tmp, publication_year) %>%
    summarise(times_cited_all_databases = median(times_cited_all_databases),
              n=n()) %>%
    ungroup() %>%
    dplyr::mutate(publication_year = factor(publication_year),
                  tmp = tools::toTitleCase(tmp))
  
  # Statistics
  df_plot1 <- df_plot %>% 
    mutate(publication_year = as.numeric(as.character(publication_year))-2009)
  
  ## Citations
  zz <- df_plot1 %>% 
    group_by(tmp) %>% 
    do({
      mod = lm(times_cited_all_databases ~ publication_year, data = .)
      data.frame(Intercept = coef(mod)[1],
                 Slope = coef(mod)[2])
    })
  print(zz)
  
  fit1 = lm(times_cited_all_databases ~ publication_year*tmp, data=df_plot1)
  print(summary(fit1)$call)
  print(summary(fit1))
  
  
  ## Number of publications
  zz1 <- df_plot1 %>% 
    group_by(tmp) %>% 
    do({
      mod = lm(n ~ publication_year, data = .)
      data.frame(Intercept = coef(mod)[1],
                 Slope = coef(mod)[2])
    })
  print(zz1)
  
  fit2 = lm(n ~ publication_year*tmp, data=df_plot1)
  print(summary(fit2)$call)
  print(summary(fit2))
  
  # Plot 1
  g <- ggplot(data = df_plot, aes(x = publication_year, y = times_cited_all_databases, group = tmp)) +
    geom_line(size = 1.5) +
    geom_smooth(method="lm", aes(col = tmp), size = 2) +
    xlab("Publication year") +
    ylab("# Citations/# Publication by year") +
    geom_point(aes(fill = tmp), size=4, col="black", shape=21) +
    geom_text(data=zz[2,], aes(label = paste0("Slope ", round(Slope, 2))),
             x = 1,
             vjust = "inward", hjust = "inward",
             y = max(df_plot$times_cited_all_databases),
             size = 5, colour = "#377eb8"
    ) +
    geom_text(data=zz[1,], aes(label = paste0("Slope ", round(Slope, 2))),
              x = 1,
              vjust = "inward", hjust = "inward",
              y = max(df_plot$times_cited_all_databases)-0.075*max(df_plot$times_cited_all_databases),
              size = 5, colour = "#e41a1c"
    ) +
    scale_color_brewer(palette="Set1") +
    scale_fill_brewer(palette="Set1", name=str_to_sentence(gsub("author last", "last author",
                                                                gsub("author first", "first author",
                                                                     # gsub("gender", "sex",
                                                                          gsub("number of pages cat", "# Pages",
                                                                               gsub("authors tmp cat", "# Authors",
                                                                                    gsub("_", " ", i))))))) +
    guides(color = FALSE,
           fill = guide_legend(override.aes = list(size=8), order=1)) +
    theme_bw() +
    theme(axis.text.x = element_text(size=14, colour = "black", angle = 45, hjust = 1),
          axis.text.y = element_text(size=14, colour = "black"),
          axis.title.y = element_text(size=14, face="bold", colour = "black"),
          axis.title.x = element_text(size=14, face="bold", colour = "black"),
          legend.title = element_text(size=14, face="bold", colour = "black"),
          legend.text = element_text(size=14, colour = "black"),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.direction = "horizontal")
  g
  ggsave(plot = g, filename = paste0("results/time/", i, "_citations_by_publication_year.png"), width = 5.5, height = 5, units = "in", dpi = 300)
  
  # Plot 2
  g <- ggplot(data = df_plot, aes(x = publication_year, y = n, group = tmp)) +
    geom_line(size = 1.5) +
    geom_smooth(method="lm", aes(col = tmp), size = 2) +
    xlab("Publication year") +
    ylab("# Publications by year") +
    geom_point(aes(fill = tmp), size=4, col="black", shape=21) +
    geom_text(data=zz1[2,], aes(label = paste0("Slope ", round(Slope, 2))),
              x = 10,
              vjust = "inward", hjust = "inward",
              y = max(df_plot$n),
              size = 5, colour = "#377eb8"
    ) +
    geom_text(data=zz1[1,], aes(label = paste0("Slope ", round(Slope, 2))),
              x = 10,
              vjust = "inward", hjust = "inward",
              y = max(df_plot$n)-0.05*max(df_plot$n),
              size = 5, colour = "#e41a1c"
    ) +
    scale_color_brewer(palette="Set1") +
    scale_fill_brewer(palette="Set1", name=str_to_sentence(gsub("author last", "last author",
                                                                gsub("author first", "first author",
                                                                     # gsub("gender", "sex",
                                                                          gsub("number of pages cat", "# Pages",
                                                                               gsub("authors tmp cat", "# Authors",
                                                                                    gsub("_", " ", i))))))) +
    guides(color = FALSE,
           fill = guide_legend(override.aes = list(size=8))) +
    theme_bw() +
    theme(axis.text.x = element_text(size=14, colour = "black", angle = 45, hjust = 1),
          axis.text.y = element_text(size=14, colour = "black"),
          axis.title.y = element_text(size=14, face="bold", colour = "black"),
          axis.title.x = element_text(size=14, face="bold", colour = "black"),
          legend.title = element_text(size=14, face="bold", colour = "black"),
          legend.text = element_text(size=14, colour = "black"),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.direction = "horizontal")
  g
  ggsave(plot = g, filename = paste0("results/time/", i, "_publications_by_publication_year.png"), width = 5.5, height = 5, units = "in", dpi = 300)
  
}


# Plot by countries
for (i in c("author_first_gender", "author_last_gender")) {
  
  print(i)
  df$tmp <- df[[i]]
  
  # Summarise
  df_plot <- df %>%
    dplyr::filter(!is.na(tmp)) %>%
    dplyr::select(tmp, publication_year, one_of(a$country.etc), times_cited_all_databases) %>%
    reshape2::melt(id.vars=c("tmp", "publication_year", "times_cited_all_databases"), variable.name="Country") %>%
    filter(value!=0) %>%
    group_by(publication_year, Country, tmp) %>%
    summarise(times_cited_all_databases = median(times_cited_all_databases),
              n=n()) %>%
    ungroup() %>%
    dplyr::mutate(publication_year = factor(publication_year),
                  tmp = tools::toTitleCase(tmp))
  df_plot <- df_plot %>%
    arrange(desc(n))
  df_plot <- df_plot %>%
    dplyr::mutate(Country = factor(Country, levels = unique(df_plot$Country)))

  # Statistics
  df_plot1 <- df_plot %>% 
    mutate(publication_year = as.numeric(as.character(publication_year))-2009)
  
  ## Citations
  zz <- df_plot1 %>% 
    group_by(Country, tmp) %>% 
    do({ 
      mod = lm(times_cited_all_databases ~ publication_year, data = .)
      data.frame(Intercept = coef(mod)[1],
                 Slope = coef(mod)[2])
    })
  print(zz)
  
  fit1 = lm(times_cited_all_databases ~ publication_year*tmp, data=df_plot1)
  print(summary(fit1)$call)
  print(summary(fit1))
  
  
  ## Number of publications
  zz1 <- df_plot1 %>% 
    group_by(Country, tmp) %>% 
    do({
      mod = lm(n ~ publication_year, data = .)
      data.frame(Intercept = coef(mod)[1],
                 Slope = coef(mod)[2])
    })
  print(zz1)
  
  fit2 = lm(n ~ publication_year*tmp, data=df_plot1)
  print(summary(fit2)$call)
  print(summary(fit2))
  
  
  # Plot 1
  g <- ggplot(data = df_plot, aes(x = publication_year, y = n, group = tmp)) +
    geom_line(size = 1.5) +
    xlab("Publication year") +
    ylab("# Publication by year") +
    geom_point(aes(fill = tmp), size=4, col="black", shape=21) +
    scale_color_brewer(palette="Set1") +
    scale_fill_brewer(palette="Set1", name=str_to_sentence(gsub("author last", "last author",
                                                                gsub("author first", "first author",
                                                                     # gsub("gender", "sex",
                                                                          gsub("number of pages cat", "# Pages",
                                                                               gsub("authors tmp cat", "# Authors",
                                                                                    gsub("_", " ", i))))))) +
    guides(color = FALSE,
           fill = guide_legend(override.aes = list(size=8), order=1)) +
    theme_bw() +
    theme(axis.text.x = element_text(size=14, colour = "black", angle = 45, hjust = 1),
          axis.text.y = element_text(size=14, colour = "black"),
          axis.title.y = element_text(size=14, face="bold", colour = "black"),
          axis.title.x = element_text(size=14, face="bold", colour = "black"),
          legend.title = element_text(size=14, face="bold", colour = "black"),
          legend.text = element_text(size=14, colour = "black"),
          strip.text.x = element_text(size = 14, face="bold", colour = "black"),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.direction = "horizontal") +
    facet_wrap("Country", scales = "free")
  ggsave(plot = g, filename = paste0("results/time/", i, "_citations_by_publication_year_by_country.png"), width = 20, height = 15, units = "in", dpi = 300)
}

df[df$number_of_pages==86,]$authors_tmp
cor.test(df$authors_tmp, df$number_of_pages, method="spearman")
cor.test(df_plot[df_plot$variable=="# Pages",]$med, df_plot[df_plot$variable=="# Authors",]$med, method="spearman")


# Plot
# Summarise
## number_of_pages
df_plot1 <- df %>%
  group_by(publication_year) %>%
  summarise(med = median(number_of_pages, na.rm=TRUE),
            iqr25 = quantile(number_of_pages, na.rm=TRUE, probs = 0.25),
            iqr75 = quantile(number_of_pages, na.rm=TRUE, probs = 0.75)
  ) %>%
  ungroup() %>%
  dplyr::mutate(publication_year = factor(publication_year),
                variable = "# Pages")
## authors_tmp
df_plot2 <- df %>%
  group_by(publication_year) %>%
  summarise(med = median(authors_tmp, na.rm=TRUE),
            iqr25 = quantile(authors_tmp, na.rm = TRUE, probs = 0.25),
            iqr75 = quantile(authors_tmp, na.rm = TRUE, probs = 0.75)
  ) %>%
  ungroup() %>%
  dplyr::mutate(publication_year = factor(publication_year),
                variable = "# Authors")
## rbind
df_plot <- rbind(df_plot1, df_plot2)

## number_of_pages
zz1 <- df_plot1 %>% 
  mutate(publication_year = as.numeric(as.character(publication_year))) %>%
  do({
    mod = lm(med ~ publication_year, data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2])
  })
print(zz1)
## authors_tmp
zz2 <- df_plot2 %>% 
  mutate(publication_year = as.numeric(as.character(publication_year))) %>%
  do({
    mod = lm(med ~ publication_year, data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2])
  })
print(zz2)



# Plot
g <- ggplot(data = df_plot, aes(x = publication_year, y = med)) +
  geom_smooth(aes(group=variable, color=variable), method="lm", size = 2) +
  xlab("Publication year") +
  ylab("Number") +
  geom_point(aes(fill=variable), size=4, col="black", shape=21) +
  geom_text(data=zz1, aes(label = paste0("Slope ", round(Slope, 2))),
            x = 1,
            vjust = "inward", hjust = "inward",
            y = max(df_plot$med),
            size = 5, colour = "#377eb8"
  ) +
  geom_text(data=zz2, aes(label = paste0("Slope ", round(Slope, 2))),
            x = 1,
            vjust = "inward", hjust = "inward",
            y = max(df_plot$med)-0.05*max(df_plot$med),
            size = 5, colour = "#e41a1c"
  ) +
  scale_fill_brewer(palette="Set1", name="") +
  scale_color_brewer(palette="Set1") +
  guides(fill = guide_legend(override.aes = list(size=8)),
         color = FALSE) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_text(size=14, face="bold", colour = "black"),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.direction = "horizontal")
g
ggsave(plot = g, filename = "results/time/Corplot_authors_and_pages_by_publication_year.png", width = 5.5, height = 5, units = "in", dpi = 300)
  

  

# Same plot for countries
df_country <- df %>% 
  dplyr::select(one_of(a$country.etc), times_cited_all_databases, publication_year) %>%
  melt(id.vars=c("times_cited_all_databases", "publication_year"), variable.name="Country") %>%
  dplyr::filter(value==1) %>%
  dplyr::select(-value)

# Summarise
df_plot <- df_country %>%
  group_by(Country, publication_year) %>%
  summarise(times_cited_all_databases = median(times_cited_all_databases),
            n=n()) %>%
  ungroup() %>%
  dplyr::mutate(Country = factor(Country, levels = a[order(-a$n_articles),]$country.etc))


# Plot
g <- ggplot(data = df_plot, aes(x = publication_year, y = times_cited_all_databases, group = Country)) +
  geom_line(aes(col = Country), size = 1.5) +
  xlab("Publication year") +
  ylab("# Citations/Publication by year") +
  geom_point(col="black", size=2) +
  guides(col = guide_legend(ncol = 7)) +
  theme_bw() +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  facet_wrap("Country", scales = "free", ncol = 7) +
  theme(axis.text.x = element_text(size=20, colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size=20, colour = "black"),
        axis.title.y = element_text(size=24, face="bold", colour = "black"),
        axis.title.x = element_text(size=24, face="bold", colour = "black"),
        legend.title = element_text(size=24, face="bold", colour = "black", angle = 90),
        legend.text = element_text(size=20, colour = "black"),
        legend.position = "none",
        strip.text.x = element_text(size = 15))
ggsave(plot = g, filename = "results/time/Country_citation_year.png", width = 17, height = 12, units = "in", dpi = 300)



# Plot by countries
g <- ggplot(data = df_plot, aes(x = publication_year, y = n, group = Country)) +
  geom_line(aes(col = Country), size = 1.5) +
  xlab("Publication year") +
  ylab("# Publications by year") +
  geom_point(col="black", size=2) +
  guides(col = guide_legend(ncol = 7)) +
  theme_bw() +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  facet_wrap("Country", scales = "free", ncol = 7) +
  theme(axis.text.x = element_text(size=20, colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size=20, colour = "black"),
        axis.title.y = element_text(size=24, face="bold", colour = "black"),
        axis.title.x = element_text(size=24, face="bold", colour = "black"),
        legend.title = element_text(size=24, face="bold", colour = "black", angle = 90),
        legend.text = element_text(size=20, colour = "black"),
        legend.position = "none",
        strip.text.x = element_text(size = 15))
ggsave(plot = g, filename = "results/time/Country_publication_year.png", width = 17, height = 12, units = "in", dpi = 300)

# Statistics
zz1 <- df_plot %>% 
  group_by(Country) %>%
  do({
    mod = lm(n ~ publication_year, data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2])
  })
print(zz1)


# Same plot for cities
cities_to_analyse <- df_city1_sr_1[order(df_city1_sr_1$n, decreasing = TRUE),]$City[1:25]
cities_to_analyse <- gsub("\\ \\(", "_", gsub("\\)", "", cities_to_analyse))
cities_to_analyse <- gsub("FR", "France", 
                           gsub("ES", "Spain", 
                                gsub("IT", "Italy", 
                                     gsub("BE", "Belgium", 
                                          gsub("NL", "Netherlands", 
                                               gsub("AU", "Australia", 
                                                    gsub("US", "USA", 
                                                         gsub("IE", "Ireland", 
                                                              gsub("DE", "Germany", cities_to_analyse)))))))))

df_city <- df %>% 
  dplyr::select(one_of(cities_to_analyse), times_cited_all_databases, publication_year) %>%
  melt(id.vars=c("times_cited_all_databases", "publication_year"), variable.name="City") %>%
  dplyr::filter(value==1) %>%
  dplyr::select(-value)

# Summarise
df_plot <- df_city %>%
  group_by(City, publication_year) %>%
  summarise(times_cited_all_databases = median(times_cited_all_databases),
            n=n()) %>%
  ungroup() %>%
  dplyr::mutate(City = factor(City, levels = cities_to_analyse)) %>%
  arrange(City)
df_plot$rown <- 1:nrow(df_plot)

# Correct City names
df_plot$City <- ifelse(str_detect(df_plot$City, "_"), as.character(paste0(df_plot$City, ")")), as.character(df_plot$City))
df_plot$City <- ifelse(str_detect(df_plot$City, "_"), gsub("_", " (", df_plot$City), df_plot$City)
df_plot$City <- gsub("France", "FR", 
                           gsub("Spain", "ES", 
                                gsub("Italy", "IT",
                                     gsub("Belgium", "BE", 
                                          gsub("Netherlands", "NL", 
                                               gsub("Australia", "AU", 
                                                    gsub("USA", "US", 
                                                         gsub("Ireland", "IE", 
                                                              gsub("Germany", "DE", df_plot$City)))))))))
df_plot <- df_plot %>%
  dplyr::mutate(City = factor(City, levels=unique(City)))

# Plot by cities
g <- ggplot(data = df_plot, aes(x = publication_year, y = n, group = City)) +
  geom_line(aes(col = City), size = 1.5) +
  xlab("Publication year") +
  ylab("# Publications by year") +
  geom_point(col="black", size=2) +
  guides(col = guide_legend(ncol = 7)) +
  theme_bw() +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  facet_wrap("City", scales = "free", ncol = 7) +
  theme(axis.text.x = element_text(size=20, colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size=20, colour = "black"),
        axis.title.y = element_text(size=24, face="bold", colour = "black"),
        axis.title.x = element_text(size=24, face="bold", colour = "black"),
        legend.title = element_text(size=24, face="bold", colour = "black", angle = 90),
        legend.text = element_text(size=20, colour = "black"),
        legend.position = "none",
        strip.text.x = element_text(size = 15))
ggsave(plot = g, filename = "results/time/City_publication_year.png", width = 17, height = 12, units = "in", dpi = 300)

g <- ggplot(data = df_plot, aes(x = publication_year, y = times_cited_all_databases, group = City)) +
  geom_line(aes(col = City), size = 1.5) +
  xlab("Publication year") +
  ylab("# Citations/Publication by year") +
  geom_point(col="black", size=2) +
  guides(col = guide_legend(ncol = 7)) +
  theme_bw() +
  scale_x_continuous(breaks = c(2010, 2012, 2014, 2016, 2018), limits = c(2010, 2019)) +
  facet_wrap("City", scales = "free", ncol = 7) +
  theme(axis.text.x = element_text(size=20, colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size=20, colour = "black"),
        axis.title.y = element_text(size=24, face="bold", colour = "black"),
        axis.title.x = element_text(size=24, face="bold", colour = "black"),
        legend.title = element_text(size=24, face="bold", colour = "black", angle = 90),
        legend.text = element_text(size=20, colour = "black"),
        legend.position = "none",
        strip.text.x = element_text(size = 15))
ggsave(plot = g, filename = "results/time/City_citation_year.png", width = 17, height = 12, units = "in", dpi = 300)

# Statistics
zz1 <- df_plot %>% 
  group_by(City) %>%
  do({
    mod = lm(n ~ publication_year, data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2])
  })
print(zz1)


################################# CITING PATTERNS #########################################################################################################


# Export DOIs
if (exists("zz1")) { rm(zz1) }
for (i in a$country.etc) {
  df$tmp <- df[[i]]
  zz <- df %>%
    filter(tmp == 1) %>%
    dplyr::select(doi) %>%
    summarise(doi = paste(doi, collapse = " OR "))
  zz$Country <- i
  if (exists("zz1")) {
    zz1 <- rbind(zz1, zz)
  } else {
    zz1 <- zz
  }
}
write_tsv(as.data.frame(zz1), "data_export/doi.tsv")

#### Find citing countries from Web of Science and read the resulting data
citing <- readxl::read_xlsx("data_export/doi_post.xlsx")
## Process data with for loop
if (exists("citing2")) { rm(citing2) }
for (i in 1:ncol(citing)) {
  
  ## Extract names of the citing country
  citing1 <- data.frame(citing[seq(from=1, to = nrow(citing), by = 2), i])
  ## Create variable of the publication country
  citing1$source_country <- colnames(citing1)
  ## Rename citing country variable
  colnames(citing1)[1] <- "citing_country"
  ## Extract citation frequency of the citing country
  citations_freq <- data.frame(citing[seq(from=2, to = nrow(citing), by = 2), i])
  colnames(citations_freq)[1] <- "citations_freq"
  ## Join
  citing1 <- cbind(citing1, citations_freq)
  
  ## Save
  if (exists("citing2")) {
    citing2 <- full_join(citing2, citing1)
  } else {
    citing2 <- citing1
  }
  
}
## Convert , to .
citing2 <- citing2 %>%
  dplyr::mutate(citations_freq = as.numeric(gsub("\\,", "", citations_freq)),
                citing_country = tools::toTitleCase(tolower(citing_country)),
                source_country = tools::toTitleCase(source_country),
                citing_country = gsub("Usa", "USA",
                                      gsub("United Kingdom", "UK",
                                           gsub("England", "UK",
                                                gsub("Wales", "UK",
                                                     gsub("North Ireland", "UK",
                                                          gsub("Scotland", "UK",
                                                               gsub("Peoples r China", "China", citing_country))))))),
                source_country = gsub("Usa", "USA",
                                      gsub("\\.", " ",
                                           gsub("United Kingdom", "UK",
                                                gsub("Peoples r China", "China", source_country)))))

# Summarise if duplicates (mainly due to the combination of multiple UK countries)
citing2 <- citing2 %>%
  dplyr::group_by(citing_country, source_country) %>%
  summarise(citations_freq = sum(citations_freq)) %>%
  ungroup()

## Double check for errors. This should be empty
a$country.etc[!a$country.etc %in% unique(citing2$citing_country)]
a$country.etc[!a$country.etc %in% unique(citing2$source_country)]


# Statistics
## Second most common citing country
table(t(citing[3,]))
## Most self-citing countries
zz <- citing2 %>%
  dplyr::group_by(citing_country) %>%
  summarise(citations_freq_sum = sum(citations_freq))
zz <- citing2 %>%
  filter(citing_country == source_country) %>%
  inner_join(zz) %>%
  mutate(self_citations_prop = citations_freq / citations_freq_sum * 100)
zz <- citing2 %>%
  filter(citing_country != source_country) %>%
  dplyr::group_by(source_country) %>%
  summarise(non_self_citations_freq_sum1 = sum(citations_freq)) %>%
  inner_join(zz)
zz <- citing2 %>%
  filter(citing_country != source_country) %>%
  dplyr::group_by(citing_country) %>%
  summarise(non_self_citations_freq_sum2 = sum(citations_freq)) %>%
  dplyr::rename(source_country = citing_country) %>%
  inner_join(zz)
zz <- zz %>%
  mutate(non_self_citations_freq_prop = non_self_citations_freq_sum1 / non_self_citations_freq_sum2*100)
zz <- zz %>%
  mutate(self_citation_index = self_citations_prop/non_self_citations_freq_prop)
## Add number of publications
zz <- zz %>%
  dplyr::left_join(a %>% dplyr::select(source_country = country.etc, n_articles, n_articles_diff, n_articles_diff_prop))

head(zz)
cor.test(zz$citations_freq, zz$citations_freq_sum, method="spearman")
cor.test(zz$citations_freq, zz$self_citation_index, method="spearman")
cor.test(zz$n_articles, zz$self_citation_index, method="spearman")
cor.test(zz$n_articles_diff, zz$self_citation_index, method="spearman")
cor.test(zz$n_articles_diff_prop, zz$self_citation_index, method="spearman")


## Plot 1
g <- ggplot(zz, aes(x = reorder(citing_country, -self_citations_prop), y = self_citations_prop, fill = citing_country)) +
  geom_bar(stat = "identity", color="black") +
  labs(x="Citing Country", y="Proportion of Domestic Self-Citation (%)") +
  geom_text(aes(label = ifelse(str_detect(round(self_citations_prop, 1), "\\."),
                               round(self_citations_prop, 1),
                               paste0(round(self_citations_prop, 1), ".0"))),
            vjust=-0.5,
            size = 3, colour = "black"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/publications/Barplot_country_citation1.png", width = 10, height = 5, units = "in", dpi = 300)

g <- ggplot(zz, aes(x = reorder(citing_country, -self_citation_index), y = self_citation_index, fill = citing_country)) +
  geom_bar(stat = "identity", color="black") +
  labs(x="Citing Country", y="Domestic Self-Citation Index") +
  geom_text(aes(label = ifelse(str_detect(round(self_citation_index, 2), "\\.[[:digit:]]{2}"),
                               round(self_citation_index, 2),
                               paste0(round(self_citation_index, 2), "0"))),
            vjust=-0.5,
            size = 3, colour = "black"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(size=14, colour = "black", angle = 45, hjust = 1),
        axis.text.y = element_text(size=14, colour = "black"),
        axis.title.y = element_text(size=14, face="bold", colour = "black"),
        axis.title.x = element_blank(),
        legend.title = element_text(size=14, face="bold", colour = "black"),
        legend.text = element_text(size=14, colour = "black"),
        legend.key = element_rect(color="black"),
        legend.position = "NULL")
ggsave(plot = g, "results/publications/Barplot_country_citation2.png", width = 10, height = 5, units = "in", dpi = 300)


# Join
a1 <- a %>%
  left_join(citing2 %>% dplyr::rename(country.etc = source_country))
## Keep only same 32 countries
a1 <- a1 %>% 
  dplyr::filter(citing_country %in% a$country.etc)
## Calculate citation sum
a1 <- a1 %>% 
  group_by(country.etc) %>%
  mutate(citations_freq_sum = sum(citations_freq)) %>% 
  ungroup()
## Normalize freq by sum
a1 <- a1 %>%
  mutate(citations_freq_prop = 100*citations_freq/citations_freq_sum,
         citations_freq_prop1 = ifelse(citations_freq_prop<2, NA, citations_freq_prop))


# Cluster
hc <- a1 %>%
  dplyr::select(citing_country, country.etc, citations_freq_prop) %>%
  reshape2::dcast(country.etc~citing_country, value.var="citations_freq_prop")
rownames(hc) <- hc$country.etc
hc$country.etc <- NULL


## Order x axis

a1$citing_country = factor(a1$citing_country, levels = c(colnames(hc)[hclust(dist(hc), method = "ward.D2")$order]))
a1$country.etc = factor(a1$country.etc, levels = levels(a1$citing_country))
a1$citations_freq1 <- log(a1$citations_freq, 10)

# Y-axis color
ycol = cutree(hclust(dist(hc), method = "ward.D2"), k = 3) %>%
  as.data.frame() %>%
  rename(Cluster=".") %>%
  rownames_to_column(var="Country")
ycol = levels(a1$citing_country) %>%
  as.data.frame() %>%
  rename(Country=".") %>%
  full_join(ycol)
ycol$col = ifelse(ycol$Cluster==1, "#377eb8",
              ifelse(ycol$Cluster==2, "#4daf4a",
                     ifelse(ycol$Cluster==3, "#e41a1c", "black")))


# Plot
g <- ggballoonplot(a1, x = "country.etc", y = "citing_country",
              fill = "citations_freq1",
              size = "citations_freq_prop1",
              ggtheme = theme_bw()) +
  scale_size(range = c(1, 10), labels=c(10,20,30), breaks=c(10,20,30)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 3.5,
                       na.value = "gray") +
  xlab("Cited Country") + ylab("Citing Country") +
  guides(size = guide_legend(title="Citations/\nTotal Citations (%)", nrow = 3),  #title.vjust = 0.75, keyheight = 1, label.vjust = 0.75
         fill = guide_colorbar(title="# Citations\n(LOG10)")) +   # , title.vjust = 0.75
  font("xy.text", size = 10, color = "black", face="plain") +
  theme(axis.title.x = element_text(size=12, colour="black", face="bold"),
        axis.text.x = element_text(colour="black", face="bold", angle = 45, hjust = 1),
        axis.title.y = element_text(size=12, colour="black", face="bold", angle = 90),
        axis.text.y = element_text(face="bold", colour = ycol$col),
        legend.title = element_text(size=12, colour="black", face="bold"),
        legend.text = element_text(size=12, colour="black"),
        legend.position = "right",
        legend.direction = "vertical",
        legend.box="vertical",
        legend.margin=margin())
ggsave(plot = g, filename = "results/publications/Balloonplot_citations_country.png", width = 9, height = 7, units = "in", dpi = 300)


# Cluster
dend <- hclust(dist(hc), method = "ward.D2") %>% as.dendrogram %>%
  set("branches_k_color", value = c("#e41a1c", "#377eb8", "#4daf4a"), k=3) %>%
  set("labels_col", value = c("#e41a1c", "#377eb8", "#4daf4a"), k=3)
ggd1 <- as.ggdend(dend)
ggd1 <- ggplot(ggd1, horiz = TRUE)
ggsave(plot = ggd1, filename = "results/publications/Balloonplot_citations_country_clusters1.png", width = 2, height = 5, units = "in", dpi = 300)

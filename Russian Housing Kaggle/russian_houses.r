####### admin #########
library(tidyverse)
library(caret)
library(ggfortify)
library(GGally)
library(lubridate)
library(plotly)
library(ranger)
library(magrittr)
library(splines)
library(Metrics)
library(sp)
library(rgdal)
library(geoR)
library(tmap)
library(xgboost)

setwd("Documents/Russian Housing Kaggle")

full_train_set <- read_csv("train.csv", col_types = list(kitch_sq = col_double(),
                                                         build_year = col_double(),
                                                         max_floor = col_double(),
                                                         num_room = col_double()))
full_test_set <- read_csv("test.csv", col_types = list(kitch_sq = col_double(),
                                                       build_year = col_double(),
                                                       max_floor = col_double(),
                                                       num_room = col_double()))
macro_environment <- read_csv("macro.csv")
full_test_set$price_doc <- NA
full_train_set <- rbind(full_train_set, full_test_set)

d <- function(df){
### I want to keep the train and test_set together, so I only make changes once.
### Just a short hand for keeping track of the train set.
    df[1:30471,]
}

full_train_set$year <- year(full_train_set$timestamp)
full_train_set$month <- month(full_train_set$timestamp)

#### Maps ####
russian_map <- readRDS("mo_SPD.rds")
to_map <- d(full_train_set) %>%
  filter(full_sq > 0) %>%
  mutate(price_sq = price_doc/full_sq) %>%
  group_by(sub_area) %>%
  summarise(nb = n(),
            mean_price_doc = mean(price_doc),
            mean_price_sq = mean(price_sq))
russian_map <- sp::merge(russian_map, to_map)
tmap_mode("view")
col_aes <- "mean_price_sq"
col_min <- min(russian_map[[col_aes]])
col_max <- max(russian_map[[col_aes]])
col_inc <- (col_max - col_min)/20

tm_shape(russian_map) + 
  tm_fill(col = col_aes,
          style = "fixed",
          breaks = seq(col_min, col_max, by = col_inc),
          popup.vars = c("mean_price_doc", "mean_price_sq")) + 
  tm_borders()

##### imputation #####
### break year built into buckets
full_train_set$build_year <- ifelse(full_train_set$build_year < 1000, NA, ifelse(full_train_set$build_year > 2018, NA, full_train_set$build_year))
surrounding_area_year_built <- full_train_set [which(names(full_train_set) %in% c("build_count_before_1920", "build_count_1921-1945", "build_count_1946-1970", "build_count_1971-1995", "build_count_after_1995"))]
year_build_imp <- by_row(surrounding_area_year_built, which.max)[".out"]
year_build_imp <- unlist(year_build_imp)
year_build_imp <- names(surrounding_area_year_built)[year_build_imp]

full_train_set$build_year2 <- ifelse(full_train_set$build_year < 1921, "build_count_before_1920",
                              ifelse(full_train_set$build_year < 1946, "build_count_1921-1945",
                              ifelse(full_train_set$build_year < 1971, "build_count_1946-1970",
                              ifelse(full_train_set$build_year < 1996, "build_count_1971-1995",
                                                                       "build_count_after_1995"))))
full_train_set$build_year2[is.na(full_train_set$build_year2)] <- year_build_imp[is.na(full_train_set$build_year2)]
###
full_train_set$full_sq[full_train_set$full_sq > 1000] <- full_train_set$life_sq[full_train_set$full_sq > 1000]


ggplotly(ggplot(d(full_train_set), aes(x = as.factor(year), y = price_doc)) + geom_boxplot())

ggplotly(ggplot(d(full_train_set), aes(x = build_year2, y = price_doc)) + geom_boxplot())

ggplotly(ggplot(d(full_train_set), aes(x = state, y = price_doc)) + geom_boxplot())
## Owner occupied results in lower sale price than investment
## need to impute metro_km and railroad_km
median_station_distance <- full_train_set %>%
  group_by(sub_area) %>%
  summarise(med_metro_km_walk = median(metro_km_walk, na.rm = T),
            med_railroad_station_walk_km = median(railroad_station_walk_km, na.rm = T))
full_train_set %<>% left_join(median_station_distance, by = c("sub_area" = "sub_area"))
full_train_set$metro_km_walk[is.na(full_train_set$metro_km_walk)] <- full_train_set$med_metro_km_walk[is.na(full_train_set$metro_km_walk)]
full_train_set$railroad_station_walk_km[is.na(full_train_set$railroad_station_walk_km)] <- full_train_set$med_railroad_station_walk_km[is.na(full_train_set$railroad_station_walk_km)]

###
full_train_set$state <- ifelse(is.na(full_train_set$state), "missing", full_train_set$state)
##neighboring materials
full_train_set <- mutate(full_train_set,
                         build_percent_block = build_count_block/raion_build_count_with_material_info,
                         build_percent_wood = build_count_wood/raion_build_count_with_material_info,
                         build_percent_frame = build_count_frame/raion_build_count_with_material_info,
                         build_percent_brick = build_count_brick/raion_build_count_with_material_info,
                         build_percent_monolith = build_count_monolith/raion_build_count_with_material_info,
                         build_percent_panel = build_count_panel/raion_build_count_with_material_info,
                         build_percent_foam = build_count_foam/raion_build_count_with_material_info,
                         build_percent_slag = build_count_slag/raion_build_count_with_material_info,
                         build_percent_mix = build_count_mix/raion_build_count_with_material_info)
## build_percent_brick looks the most impactful, but still not too much
## elder/working percent
full_train_set$work_to_elder <- full_train_set$work_all/full_train_set$ekder_all
## incorrect state value
full_train_set$state[full_train_set$state == 33] <- 3
## full_sq to kitch



ggplot(d(full_train_set), aes(x = build_year, y = price_doc)) + geom_point(alpha = 0.3)
ggplotly(ggplot(d(full_train_set), aes(x = as.factor(month), y = price_doc)) + geom_boxplot() + facet_grid(~year))
## life adds no benefit, full_sq is very important

#change max_floor, kitch_sq, and max_floor to dbl

#2781, 22786, 2119


#### connect macro environment to housing #####
macro_environment$year <- year(macro_environment$timestamp)
macro_environment$month <- month(macro_environment$timestamp)
macro_monthly_orig <- macro_environment %>%
  group_by(year, month) %>%
  summarise_each(funs(mean(., na.rm = T)))
avg_monthly_home_price <- d(full_train_set) %>%
  group_by(year, month) %>%
  summarise(price_doc = mean(price_doc))



create_some_splines <- function(df, col_no){
  if (sum(is.na(df[[col_no]])) == 82) {
    return(rep(0, 82))
  }
  min_of_vector <- min(df[[col_no]], na.rm = T) * .8
  max_of_vector <- max(df[[col_no]], na.rm = T) * 1.2
  fit_spline <- lm(df[[col_no]] ~ bs(1:82, df = 3))
  fit_spline_predict <- predict(fit_spline, newdata = bs(1:82, df = 3))
  replacement_values <- ifelse(fit_spline_predict > max_of_vector, max_of_vector,
                        ifelse(fit_spline_predict < min_of_vector, min_of_vector,
                               fit_spline_predict))
  replacement_values
}
macro_monthly_spline_impute <- macro_monthly_orig
for (i in 4:ncol(macro_monthly_spline_impute)){
  macro_monthly_spline_impute[is.na(macro_monthly_spline_impute[i]), i] <-
    create_some_splines(macro_monthly_spline_impute, i)[is.na(macro_monthly_spline_impute[i])]
}

macro_monthly <- left_join(avg_monthly_home_price, macro_monthly_spline_impute)
macro_target <- macro_monthly$price_doc
macro_monthly_matrix <- macro_monthly[which(!(names(macro_monthly) %in% c("price_doc", "year", "month", "timestamp")))]
macro_control <- trainControl(method = "cv",
                            number = 5)
macro_monthly_rf <- train(x = macro_monthly_matrix,
                          y = macro_target,
                          method = "ranger",
                          trControl = macro_control,
                          tuneGrid = data.frame(mtry = seq(3, 30,3)),
                          importance = "impurity")

macro_predict_matrix <- macro_monthly_spline_impute[which(!(names(macro_monthly_spline_impute) %in% c("year", "month", "timestamp")))]
macro_predict_avg_price <- predict(macro_monthly_rf, newdata = macro_predict_matrix)
avg_monthly_df <- data_frame(date = macro_monthly_spline_impute$timestamp,
                             predicted = macro_predict_avg_price)
ggplot(avg_monthly_df, aes(x = date, y = predicted)) + geom_line()
avg_monthly_df$year <- year(avg_monthly_df$date)
avg_monthly_df$month <- month(avg_monthly_df$date)

full_train_set <- left_join(full_train_set, avg_monthly_df, by = c("year" = "year", "month" = "month"))

#product type is significant, but something else is picking that up now.
lm_both <- lm(price_doc ~ full_sq + 
                as.factor(culture_objects_top_25_raion) +
                railroad_station_walk_km + 
                metro_km_walk + 
                as.factor(university_top_20_raion) + 
                ecology + 
                state * full_sq +
                work_to_elder + 
                radiation_raion +
                school_km + 
                sadovoe_km + 
                bulvar_ring_km + 
                office_km + 
                predicted,
              full_train_set)
summary(lm_both)
BIC(lm_both)
#### ensembles ####
# removed build_year so I could get it running
xg_matrix <- model.matrix(~full_sq +
  as.factor(culture_objects_top_25_raion) +
  railroad_station_walk_km + 
  metro_km_walk + 
  as.factor(university_top_20_raion) + 
  ecology + 
  state * full_sq +
  work_to_elder + 
  radiation_raion +
  school_km + 
  sadovoe_km + 
  bulvar_ring_km + 
  office_km +
  predicted + 
  sub_area,
data = d(full_train_set))
xg_target <- d(full_train_set)$price_doc

xg_test_matrix <- model.matrix(~full_sq +
                            as.factor(culture_objects_top_25_raion) +
                            railroad_station_walk_km + 
                            metro_km_walk + 
                            as.factor(university_top_20_raion) + 
                            ecology + 
                            state * full_sq +
                            work_to_elder + 
                            radiation_raion +
                            school_km + 
                            sadovoe_km + 
                            bulvar_ring_km + 
                            office_km +
                            predicted + 
                            sub_area,
                          data = full_train_set[30472:38133,])






#Swap to RMSLE
custom_summary = function(data, lev = NULL, model = NULL){
    out = rmsle(data[, "obs"], data[, "pred"])
    names(out) = c("rmsle")
    out
}

ctrl <- trainControl(method = "cv",
                     number = 5,
                     summaryFunction = custom_summary,
                     verboseIter = FALSE,
                     ##running into memory issues on my computer
                     allowParallel = FALSE)
xg_grid <- expand.grid(eta = seq(0.01, 0.21, by = 0.05),
                       max_depth = seq(2, 10, by = 2),
                       nrounds = seq(2000, 4000, by = 500),
                       gamma = 0,
                       colsample_bytree = 1,
                       min_child_weight = 1,
                       subsample = 1)

xg_model <- train(y = xg_target, x =xg_matrix,
                      method = "xgbTree",
                      tuneGrid = xg_grid,
                      trControl = ctrl,
                      metric = "rmsle",
                      maximize = FALSE)

xg_pred <- predict(xg_model, xg_test_matrix)

to_export <- data.frame(id = full_test_set$id, price_doc = xg_pred) 
write_csv(to_export, "submission1_xgboost.csv")




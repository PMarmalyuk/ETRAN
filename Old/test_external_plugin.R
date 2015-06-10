setwd("F:\\Институт\\Проекты\\EyeTrackingPackage\\Test")

df <- cars

selectRows <- function(df, selectorPath, settingsPath)
{
  funct <- source(selectorPath)$value
  setting <- source(settingsPath)$value
  funct(df, setting)
}
selectRows(df, selectorPath = "selector.R", settingsPath = "sett1.R")


# create 'selector' objects
filter1 <- list(id = 1, name = "Filter 1", settings = source("sett1.R")$value, fun = source("selector.R")$value)
filter2 <- list(id = 2, name = "Filter 2", settings = source("sett2.R")$value, fun = source("selector.R")$value)

# create abstract select method intended to work with 'df' objects using 'selector' objects
select <- function(df, filter)
{
  res <- filter$fun(df, filter$settings)
  list(filtered_df = res, filterID = filter$id)
}

# use selector
select(df, filter2)



function(df, settings)
{
  df[which(df[, settings$column] > settings$lower & df[, settings$column] < settings$upper), ]
}
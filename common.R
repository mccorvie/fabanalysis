


quarter_levels <- paste0( rep(1:4, times=10), "Q", rep(10:27, each=4) )
paste(quarter_levels, collapse=",")


datify <- \(tt)
{
  tt |> mutate( Date = quarter_to_date( Quarter ))
}

quarter_to_date <- \( quarters )
{
  QQ = str_extract( quarters, "([1-4])Q([0-9]{2})",group=1) |> as.numeric()
  YY = str_extract( quarters, "([1-4])Q([0-9]{2})",group=2) |> as.numeric()
  ymd( paste0( YY, "-",QQ*3-1, "-", 15))
}

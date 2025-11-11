
##
## TSMC units from 
##

tsmc_revenue0 <- read_csv( "tsmc revenue breakdown.csv") |> 
  mutate( across( `3nm`:`0.25um+`, \(x) x/100)) |> 
  mutate( Quarter = factor( Quarter, quarter_levels, ordered=T))

node_levels <- c(  "3nm", "5nm", "7nm", "10nm", "16/20nm", "16nm-20nm", "20nm", "28nm", "40/45nm", "40nm-90nm", "65nm", "90nm",  "0.1um+", "0.11/0.13um", "0.15/0.18um", "0.25um+")

wafer_asp <- tibble( 
  Node = factor( c( "3nm", "5nm", "7nm", "10nm", "16nm-20nm",  "28nm","40nm-90nm", "0.1um+" ), node_levels, ordered=T),
  ASP  = c( 20,	16,	10,	6, 4,	3,	2.6,	2 )
)

##  Semiconductor PPI 

QQ <- rep( 1:4, each=3)
bls_ppi <- read_csv( "BLS semi PPI.csv" ) |>
  mutate( month = month( observation_date ), year = year(observation_date)%%100) |> 
  filter( observation_date >= ymd(20170101), month==2 | month == 5 | month == 8 | month==11) |> 
  mutate( Quarter = paste0( QQ[month], "Q", year ), PPI = PCU334413334413A / 53.134 ) |> 
  mutate( Quarter = factor( Quarter, quarter_levels, ordered=T)) |> 
  select( -observation_date, -month, -year, -PCU334413334413A) 


tsmc_revenue <- tsmc_revenue0 |> 
  mutate( 
    `16nm-20nm` = `16/20nm`+`16nm`+`20nm`,
    `40nm-90nm` = `40/45nm` + `65nm` + `90nm`,
    `0.1um+` = `0.11/0.13um`+`0.15/0.18um`+`0.25um+`
  ) |> 
  select( -`16/20nm`,-`16nm`,-`20nm`, -`40/45nm`,-`65nm`, -`65nm`, -`90nm`, -`0.11/0.13um`,-`0.15/0.18um`,-`0.25um+`) |> 
  rename_with( \(x) paste0( x," %"), `3nm`:`0.1um+`) |> 
  mutate( across( `3nm %`:`0.1um+ %`, \(x) x*Revenue, .names= "{.col}rev" )) |> 
  rename_with( \(x) str_replace( x,"%rev", "rev"), `3nm %rev`:`0.1um+ %rev`)



est_wafers <- tsmc_revenue |> 
  select( Quarter, `3nm rev`:`0.1um+ rev`) |> 
  rename_with( \(x) str_remove( x," rev$" ), `3nm rev`:`0.1um+ rev`) |> 
  pivot_longer( `3nm`:`0.1um+`, names_to = "Node", values_to = "Revenue") |> 
  mutate( Node = factor( Node, node_levels, ordered=T)) |> 
  left_join( wafer_asp, by = "Node") |> 
  left_join( bls_ppi, by = "Quarter") |> 
  mutate( wpm = Revenue / PPI / ASP / 3 ) 

est_wafers

est_total <- est_wafers |> 
  group_by( Quarter ) |> 
  summarize( wpm_est = sum( wpm )) |> 
  ungroup() |> 
  left_join( select(tsmc_revenue, Quarter, `Wafer Shipments`), by="Quarter") |> 
  mutate( wpm_act= `Wafer Shipments`/3, error = (wpm_est/wpm_act-1)*100) |> 
  select( -`Wafer Shipments`)

xx<-est_total |> summarize( wpm_act=sum(wpm_act), wpm_est=sum( wpm_est))
xx$wpm_est/xx$wpm_act

#pp<- filter(est_wafers, Node == "3nm" | Node == "5nm" | Node== "7nm" | Node == "10nm") 
est_wafers

ggplot( pp, aes( x=Quarter, y=wpm, fill = Node) ) + geom_col() + scale_fill_brewer(palette= "Set3")

xx <- est_wafers |> filter( Node == "3nm" | Node == "5nm" | Node== "7nm" ) |> 
  #  xx <- est_wafers |> filter( Node == "3nm" ) |> 
  group_by( Quarter) |> 
  summarize( high_end_wpm = sum( wpm )) |> 
  left_join( filter( EUV_units, Region == "Taiwan" ), by = "Quarter")

xx
ggplot( xx, aes( x=`Cum Units`, y= high_end_wpm)) + geom_point()



##
##  Try to figure out ASP by regression
##
tsmc_revenue_adj
tsmc_revenue_adj <- tsmc_revenue |> 
  left_join( bls_ppi, by = "Quarter") |> 
  mutate( target = `Wafer Shipments` * PPI ) 

tsmc_revenue_adj

ll <- lm( 
  `Wafer Shipments` ~`3nm rev` + `5nm rev` + `7nm rev` + `10nm rev` +  `16nm-20nm rev` + `28nm rev` + `40nm-90nm rev` + `0.1um+ rev` ,
  tsmc_revenue_adj
)

ll <- lm( 
  target ~`3nm rev` + `5nm rev` + `7nm rev` + `28nm rev` + `10nm-20nm rev` + `40nm-90nm rev` + `0.1um+ rev` ,
  tsmc_revenue_adj
)

ss <- summary( ll)
ss
coeff <- ss$coefficients

1/ll$coefficients

1/(coeff[,1]+coeff[,2])
1/(coeff[,1]-coeff[,2])
wafer_asp

ggplot( tibble( x=1:35, y=ll$residuals), aes( x=x,y=y) )+ geom_point()


##
## Analysis of anysilicon price graph 
##  https://anysilicon.com/wafer-cost/
## 

tt <- tibble( 
  x=c(2,3,5,7,10,12,20,28,40,65,90),
  y=c( 25001, 18495, 12101, 9237, 5999, 4001, 3665, 2889, 2275, 1941, 1653 )
) |> mutate( idx = n():1)

ggplot( tt, aes( x=idx, y=log(y)))+geom_point()
ggplot( tt, aes( x=log(x), y=log(y)))+geom_point()


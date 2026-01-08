library( tidyverse )
source( "common.R")

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
wafer_asp

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



## inflation adjust

##  Semiconductor PPI 

QQ <- rep( 1:4, each=3)
bls_ppi <- read_csv( "BLS semi PPI.csv" ) |>
  mutate( month = month( observation_date ), year = year(observation_date)%%100) |> 
  filter( observation_date >= ymd(20170101), month==2 | month == 5 | month == 8 | month==11) |> 
  mutate( Quarter = paste0( QQ[month], "Q", year ), PPI1 = PCU334413334413A, PPI2=PCU33443344 ) |> 
  mutate( Quarter = factor( Quarter, quarter_levels, ordered=T)) |> 
  mutate( Date = quarter_to_date( Quarter )) |> 
  select( -observation_date, -month, -year ) 

index_baseline_quarter = "3Q22" # This is the date of my ASP report
index_baseline <- filter( bls_ppi, Quarter == index_baseline_quarter )

bls_ppi <- bls_ppi |> 
  mutate( 
    PPI1 = PPI1 / index_baseline$PPI1,  
    PPI2 = PPI2 / index_baseline$PPI2,
    PCU334413334413A = PPI1,
    PCU33443344 = PPI2
  )

# pivot_longer( bls_ppi, PCU334413334413A:PCU33443344, names_to = "Index", values_to = "Level") |>
#   datify() |>
#   ggplot( aes( x=Date, y=Level, color=Index, group=Index )) +geom_line(size=1)+scale_color_brewer(palette="Set2")+theme_minimal()
113/11


tsmc_wafer_production <- tsmc_revenue |> 
  select( Quarter, `3nm rev`:`0.1um+ rev`) |> 
  rename_with( \(x) str_remove( x," rev$" ), `3nm rev`:`0.1um+ rev`) |> 
  pivot_longer( `3nm`:`0.1um+`, names_to = "Node", values_to = "Revenue") |> 
  mutate( Node = factor( Node, node_levels, ordered=T)) |> 
  left_join( wafer_asp, by = "Node") |>
  left_join( bls_ppi, by = "Quarter") |> 
  mutate( 
    kWPM = Revenue / ASP / PPI1 / 3,
    `kWPM unadj` = Revenue / ASP  / 3 
  ) 


est_total <- tsmc_wafer_production |> 
  group_by( Quarter ) |> 
  summarize( 
    `Total kWPM (Estimate)` = sum( `kWPM unadj` ),
    `Total kWPM (Adj Estimate)` = sum( kWPM )
  ) |> 
  ungroup() |> 
  left_join( select(tsmc_revenue, Quarter, `Wafer Shipments`), by="Quarter") |> 
  mutate( 
    `Actual kWPM`= `Wafer Shipments`/3, 
    Error = (`Total kWPM (Estimate)`/`Actual kWPM`-1)*100,
    `Error adj` = (`Total kWPM (Adj Estimate)`/`Actual kWPM`-1)*100
  ) |> 
  select( -`Wafer Shipments`) 

## plots moved to report
# 
# ggplot( datify( est_total), aes( x=Date)) +
#   geom_segment(
#     aes( xend = Date, y = Error, yend = `Error adj`),
#     arrow = arrow(length = unit(2, "mm"), type = "closed"),
#     color="darkred"
#   ) +
#   geom_hline(yintercept=0, color="darkgrey")+theme_minimal()
# 
# plotme<- est_total |> pivot_longer( `Total kWPM (Estimate)`:`Actual kWPM`, names_to = "Series", values_to = "kWPM") |> datify() |>
#   mutate( width = if_else( Series == "Actual kWPM", 1, 0.5 )) 
# 
# ggplot( plotme, aes( x=Date)) +
# #  geom_line( aes(y=`Actual kWPM`, linewidth = width), color="darkgrey", size=1)+
#   geom_line(data = filter(plotme, Series == "Actual kWPM" ), aes(y=kWPM, color=Series), size=1) +
#   geom_line( aes(y=kWPM, color=Series)) +
#   theme_minimal() + ylab("kWPM") + labs(title="Total Wafer Output vs Estimate")
# 

## visualize

# filter( tsmc_wafer_production, Node %in% c( "3nm", "5nm","7nm","10nm")) |> datify() |> 
#   ggplot( aes( x=Date, y=WPM, fill = Node) ) + geom_col() + scale_fill_brewer(palette= "Set3")


##
##  Try to figure out ASP by regression
##

tsmc_revenue_adj <- tsmc_revenue |> 
  left_join( bls_ppi, by = "Quarter") |> 
  #mutate( target = `Wafer Shipments` ) 
  mutate( target = `Wafer Shipments`*PPI1 ) 

tsmc_revenue_adj

ll <- lm( 
  target ~`3nm rev` + `5nm rev` + `7nm rev` + `10nm rev` +  `16nm-20nm rev` + `28nm rev` + `40nm-90nm rev` + `0.1um+ rev`+0 ,
  tsmc_revenue_adj
)

coeff <- summary(ll)$coefficients



ASP_regression <- tibble( std= coeff[,2], inv_ASP = coeff[,1], Node = names(ll$coefficients)) |> 
  mutate( Node = str_sub( str_sub( Node, end=-6),start=2)) |> 
  mutate( Node= factor(Node,node_levels, ordered=T) ) |> 
  left_join( wafer_asp, by="Node") |> 
  mutate( inv_ASP_act = 1/ASP)

# ggplot( ASP_regression, aes( x=Node )) +
#   geom_errorbar( aes(y=inv_ASP, ymax = inv_ASP+std, ymin=inv_ASP-std), color="purple") +
#   geom_point( aes(y=inv_ASP ), size=0.25,color="purple") +
#   geom_point(aes( y=inv_ASP_act), size=2) + theme_minimal() + labs(title="Regression-inferred ASP") +
#   ylab( "Inverse ASP")


# plots moved 

# ggplot( tibble( x=1:35, y=ll$residuals), aes( x=x,y=y) )+ geom_point()


##
## Analysis of anysilicon price graph 
##  https://anysilicon.com/wafer-cost/
## 

# tt <- tibble( 
#   x=c(2,3,5,7,10,12,20,28,40,65,90),
#   y=c( 25001, 18495, 12101, 9237, 5999, 4001, 3665, 2889, 2275, 1941, 1653 )
# ) |> mutate( idx = n():1)
# 
# ggplot( tt, aes( x=idx, y=log(y)))+geom_point()
# ggplot( tt, aes( x=log(x), y=log(y)))+geom_point()


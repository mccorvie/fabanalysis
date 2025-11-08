library( tidyverse )

ipf <- \( MM, row_target, col_target)
{
  eps <- 1e-9
  MM_old <- matrix( 0, nrow=nrow( MM), ncol = ncol( MM))
  while( sum( abs( MM_old-MM)) > eps )
  {
    MM_old <- MM
    
    row_marginal <- ifelse( row_target > 0, apply( MM, 1, sum ), 1 )
    MM <- MM * row_target / row_marginal
    
    col_marginal <- ifelse( col_target>0, apply( MM, 2, sum ), 1 )
    MM <- t( t( MM) * col_target / col_marginal )
  }
  MM
}

quarter_to_date <- \( quarters )
{
  QQ = str_extract( quarter_levels, "([1-4])Q([0-9]{2})",group=1) |> as.numeric()
  YY = str_extract( quarter_levels, "([1-4])Q([0-9]{2})",group=2) |> as.numeric()
  ymd( paste0( YY, "-",QQ*3-1, "-", 15))
}

quarter_levels <- paste0( rep(1:4, times=10), "Q", rep(15:25, each=4) )


asml_revenue <- read_csv( "asml revenue breakdown.csv") |> 
  mutate( across( `Net Sales`:`EMEA %`, parse_number)) |> 
  mutate( Quarter = factor( Quarter, quarter_levels, ordered=T )) |> 
  mutate( across( `EUV %`:`EMEA %`, \(x) x/100 ))


sales_by_product <- asml_revenue |> 
  mutate( across( `EUV %`:`EMEA %`, \(x) x * `Net Sales` )) |>
  select( Quarter, `EUV %`:`EMEA %` ) |> 
  rename_with( \(c) str_remove( c, " %$"), !starts_with("Qu") ) |> 
  pivot_longer( !starts_with( "Quarter"), names_to="Product line", values_to = "Sales")

units_by_product <- asml_revenue |> 
  select( Quarter, `EUV #`:`I-line #`) |> 
  rename_with( \(c) str_remove( c, " #$"), !starts_with("Qu") ) |> 
  pivot_longer(  !starts_with( "Quarter" ), names_to="Product line", values_to = "Units")

ASP_by_product <- sales_by_product |> 
  left_join( units_by_product, by=join_by( "Quarter", "Product line")) |> 
  filter( !is.na( Units ) ) |> 
  mutate( ASP = Sales / Units ) 

# |> pivot_wider( id_cols = Quarter, names_from=`Product line`, values_from = ASP )


by_product <- asml_revenue |> select( `EUV %`:`Metrology & Inspection %`)
by_region  <- asml_revenue |> select( `China %`:`EMEA %`)

region_names  <- str_remove( colnames( by_region ), " %$")
product_names <- str_remove( colnames( by_product ), " %$")
quarter_names    <- pull( asml_revenue, "Quarter")


rel_strength <- matrix( 1, nrow=ncol( by_product), ncol = ncol( by_region) )


rownames( rel_strength ) <- product_names
colnames( rel_strength ) <- region_names

# export controls
rel_strength[ "EUV", "China"]  = 0
rel_strength[ "ArFi", "China"] = 0.5
# TSMC gets a lot of EUV scanners
rel_strength[ "EUV", "Taiwan"] = 2


fine_attribution <- array( NA, dim=c(  ncol( by_product), ncol( by_region ),  nrow( asml_revenue ) ))
dimnames( fine_attribution ) <- list( product_names, region_names, quarter_names)


for( date_idx in 1:nrow( asml_revenue ))
{
  region_target  <- by_region[date_idx,] |> as.numeric()
  product_target <- by_product[date_idx,] |> as.numeric()
  
  fine_attribution[ ,,date_idx ] <- ipf( rel_strength, product_target, region_target )  
}


EUV_rev  <- as_tibble( t( fine_attribution[ "EUV",,] )) |> 
  mutate( norm = rowSums( across( everything() ))) |> 
  mutate( across( everything(), \(x) x/norm )) |> 
  select( -norm ) |> 
  mutate( 
      Quarter = factor( quarter_names, levels = quarter_levels, ordered=T),
      `Product line` = "EUV" 
  ) |> 
  pivot_longer( `China`:`EMEA`,names_to="Region", values_to = "%")
  
ggplot( EUV_rev, aes( x=Quarter, y=`%`,fill=Region)) +geom_col() + scale_fill_brewer(palette= "Set3")




EUV_units <- EUV_rev |> 
  left_join( units_by_product, by = join_by( Quarter, `Product line`) ) |> 
  mutate( Units = Units * `%`) |> 
  select( -`%`) |> 
  group_by( Region ) |> 
  mutate( `Cum Units` = cumsum( Units )) |> 
  ungroup()

ggplot( EUV_units, aes( x=Quarter, y=`Cum Units`,color=Region)) +geom_line() + geom_point()+ scale_color_brewer(palette= "Set3")

#1Q19 11
#4Q19 26
#
#3Q23 113
#ut of 202  (113/202 = 56%)
EUV_units |> filter( Quarter == "1Q19", Region == "Taiwan" ) |> pull( `Cum Units`)
EUV_units |> filter( Quarter == "3Q23", Region == "Taiwan" ) |> pull( `Cum Units`)
EUV_units |> filter( Quarter == "3Q23") |> pull( `Cum Units`) |> sum()

EUV_units





##
## TSMC units from 
##

tsmc_revenue0 <- read_csv( "tsmc revenue breakdown.csv") |> 
  mutate( across( `3nm`:`0.25um+`, \(x) x/100)) |> 
  mutate( Quarter = factor( Quarter, quarter_levels, ordered=T))
  
node_levels <- c(  "3nm", "5nm", "7nm", "10nm", "16/20nm", "10nm-20nm", "20nm", "28nm", "40/45nm", "40nm-90nm", "65nm", "90nm",  "0.1um+", "0.11/0.13um", "0.15/0.18um", "0.25um+")

wafer_asp <- tibble( 
  Node = factor( c( "3nm", "5nm", "7nm", "10nm-20nm",  "28nm","40nm-90nm", "0.1um+" ), node_levels, ordered=T),
  ASP  = c( 20,	16,	10,	6,	3,	2.6,	2 )
)

# price_factor <- tibble( 
#   Quarter = pull( tsmc_revenue0, "Quarter"),
#   Factor = c( 0.8,0.8,0.8,0.9,0.95,1,1,1,1,1,1,1,1,1,1,1,1,1,1.1 )
# )


tsmc_revenue <- tsmc_revenue0 |> 
  mutate( 
      `10nm-20nm` = `10nm`+`16/20nm`+`16nm`+`20nm`,
      `40nm-90nm` = `40/45nm` + `65nm` + `90nm`,
      `0.1um+` = `0.11/0.13um`+`0.15/0.18um`+`0.25um+`
  ) |> 
  select( -`10nm`,-`16/20nm`,-`16nm`,-`20nm`, -`40/45nm`,-`65nm`, -`65nm`, -`90nm`, -`0.11/0.13um`,-`0.15/0.18um`,-`0.25um+`) |> 
  rename_with( \(x) paste0( x," %"), `3nm`:`0.1um+`) |> 
  mutate( across( `3nm %`:`0.1um+ %`, \(x) x*Revenue, .names= "{.col}rev" )) |> 
  rename_with( \(x) str_replace( x,"%rev", "rev"), `3nm %rev`:`0.1um+ %rev`)

est_wafers <- tsmc_revenue |> 
  select( Quarter, `3nm rev`:`0.1um+ rev`) |> 
  rename_with( \(x) str_remove( x," rev$" ), `3nm rev`:`0.1um+ rev`) |> 
  pivot_longer( `3nm`:`0.1um+`, names_to = "Node", values_to = "Revenue") |> 
  mutate( Node = factor( Node, node_levels, ordered=T)) |> 
  #left_join( price_factor, by = "Quarter") |> 
  left_join( wafer_asp, by = "Node") |> 
  #mutate( wpm = Revenue / Factor / ASP / 3 ) 
  mutate( wpm = Revenue / ASP  ) 
  

pp<- filter(est_wafers, Node == "3nm" | Node == "5nm" | Node== "7nm" )

ggplot( pp, aes( x=Quarter, y=wpm, fill = Node) ) + geom_col() + scale_fill_brewer(palette= "Set3")

#xx <- est_wafers |> filter( Node == "3nm" | Node == "5nm" | Node== "7nm" ) |> 
  xx <- est_wafers |> filter( Node == "3nm" ) |> 
  group_by( Quarter) |> 
  summarize( high_end_wpm = sum( wpm )) |> 
  left_join( filter( EUV_units, Region == "Taiwan" ), by = "Quarter")

xx
ggplot( xx, aes( x=`Cum Units`, y= high_end_wpm)) + geom_point()

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



asml_revenue <- read_csv( "asml revenue breakdown.csv") |> 
  mutate( across( `Net Sales`:`EMEA %`, parse_number)) |> 
  mutate( across( `EUV %`:`EMEA %`, \(x) x/100 ))


sales_by_product <- asml_revenue |> 
  mutate( across( `EUV %`:`EMEA %`, \(x) x * `Net Sales` )) |>
  select( Date, `EUV %`:`EMEA %` ) |> 
  rename_with( \(c) str_remove( c, " %$"), !starts_with("Date") ) |> 
  pivot_longer( !starts_with( "Date"), names_to="Product line", values_to = "Sales")

units_by_product <- asml_revenue |> 
  select( Date, `EUV #`:`I-line #`) |> 
  rename_with( \(c) str_remove( c, " #$"), !starts_with("Date") ) |> 
  pivot_longer(  !starts_with( "Date" ), names_to="Product line", values_to = "Units")

ASP_by_product <- sales_by_product |> 
  left_join( units_by_product, by=join_by( "Date", "Product line")) |> 
  filter( !is.na( Units ) ) |> 
  mutate( ASP = Sales / Units ) 

# |> pivot_wider( id_cols = Date, names_from=`Product line`, values_from = ASP )


by_product <- asml_revenue |> select( `EUV %`:`Metrology & Inspection %`)
by_region  <- asml_revenue |> select( `China %`:`EMEA %`)

region_names  <- str_remove( colnames( by_region ), " %$")
product_names <- str_remove( colnames( by_product ), " %$")
date_names    <- pull( asml_revenue, "Date")


rel_strength <- matrix( 1, nrow=ncol( by_product), ncol = ncol( by_region) )


rownames( rel_strength ) <- product_names
colnames( rel_strength ) <- region_names

# export controls
rel_strength[ "EUV", "China"]  = 0
rel_strength[ "ArFi", "China"] = 0.5
# TSMC gets a lot of EUV scanners
rel_strength[ "EUV", "Taiwan"] = 2


fine_attribution <- array( NA, dim=c(  ncol( by_product), ncol( by_region ),  nrow( asml_revenue ) ))
dimnames( fine_attribution ) <- list( product_names, region_names, date_names)


for( date_idx in 1:nrow( asml_revenue ))
{
  cat( "** ", date_idx, "\n")
#  date_idx <- 1
  region_target  <- by_region[date_idx,] |> as.numeric()
  product_target <- by_product[date_idx,] |> as.numeric()
  
  fine_attribution[ ,,date_idx ] <- ipf( rel_strength, product_target, region_target )  
}


EUV_rev  <- as_tibble( t( fine_attribution[ "EUV",,] )) |> 
  mutate( norm = rowSums( across( everything() ))) |> 
  mutate( across( everything(), \(x) x/norm )) |> 
  select( -norm ) |> 
  mutate( Date = date_names, idx = 1:n(), `Product line` = "EUV" ) |> 
  pivot_longer( `China`:`EMEA`,names_to="Region", values_to = "%")
  
ggplot( EUV_rev, aes( x=idx, y=`%`,fill=Region)) +geom_col() + scale_fill_brewer(palette= "Set3")


EUV_units <- EUV_rev |> 
  left_join( units_by_product, by = join_by( Date, `Product line`) ) |> 
  mutate( Units = Units * `%`) |> 
  select( -`%`) |> 
  group_by( Region ) |> 
  mutate( `Cum Units` = cumsum( Units )) |> 
  ungroup()

ggplot( EUV_units, aes( x=idx, y=`Cum Units`,color=Region)) +geom_line() + geom_point()+ scale_color_brewer(palette= "Set3")

#1Q19 11
#4Q19 26
#
#3Q23 113
#ut of 202  (113/202 = 56%)
EUV_units |> filter( Date == "1Q19", Region == "Taiwan" ) |> pull( `Cum Units`)
EUV_units |> filter( Date == "3Q23", Region == "Taiwan" ) |> pull( `Cum Units`)
EUV_units |> filter( Date == "3Q23") |> pull( `Cum Units`) |> sum()

EUV_units





##
## TSMC units from 
##

tsmc_revenue0 <- read_csv( "tsmc revenue breakdown.csv")


wafer_asp <- tibble( 
  Node = c( "3nm", "5nm", "7nm", "10nm-20nm",  "28nm","40nm-90nm", "0.1um+" ),
  ASP  = c( 20,	16,	10,	6,	3,	2.6,	2 )
)

wafer_asp

price_factor <- tibble( 
  Date = pull( tsmc_revenue, "Date"),
  Factor = c( 0.8,0.8,0.8,0.9,0.95,1,1,1,1,1,1,1,1,1,1,1,1,1,1.1 )
)



tsmc_revenue <- tsmc_revenue0 |> 
  mutate( 
      `10nm-20nm` = `10nm`+`16/20nm`+`16nm`+`20nm`,
      `40nm-90nm` = `40/45nm` + `65nm` + `90nm`,
      `0.1um+` = `0.11/0.13um`+`0.15/0.18um`+`0.25um+`
  ) |> 
  select( -`10nm`,-`16/20nm`,-`16nm`,-`20nm`, -`40/45nm`,-`65nm`, -`65nm`, -`90nm`, -`0.11/0.13um`,-`0.15/0.18um`,-`0.25um+`) |> 
  mutate( across( `3nm`:`0.1um+`, \(x) x/100)) |> 
  rename_with( \(x) paste0( x," %"), `3nm`:`0.1um+`) |> 
  mutate( across( `3nm %`:`0.1um+ %`, \(x) x*Revenue, .names= "{.col}rev" )) |> 
  rename_with( \(x) str_replace( x,"%rev", "rev"), `3nm %rev`:`0.1um+ %rev`)

est_wafers <- tsmc_revenue |> 
  select( Date, `3nm rev`:`0.1um+ rev`) |> 
  rename_with( \(x) str_remove( x," rev$" ), `3nm rev`:`0.1um+ rev`) |> 
  pivot_longer( `3nm`:`0.1um+`, names_to = "Node", values_to = "Revenue") |> 
  left_join( price_factor, by = "Date") |> 
  left_join( wafer_asp, by = "Node") |> 
  mutate( wpm = Revenue / Factor / ASP / 3 ) |> 
  mutate( idx = 1:n())


ggplot( est_wafers, aes( x=idx, y=wpm, fill = Node) ) + geom_col() + scale_fill_brewer(palette= "Set3")



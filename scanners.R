
library( tidyverse)

scanner_specs<-read_csv( "scanner_specs.csv")

analyst_estimates<-read_csv( "analyst estimates by node.csv") |> 
  mutate( Quarter = factor( Quarter, quarter_levels, ordered=T )) |> 
  rename( `7nm`= `7nm/10nm`) |> 
  pivot_longer( !starts_with("Quarter"), names_to= "Node", values_to="wpm_analyst")

analyst_estimates

node_layers <- tibble(
  Node = c( "3nm", "5nm", "7nm" ),
  Layers = c( 22, 12, 4 )
)

pnorm( 2, 0,1)
f_NXE3400 = approxfun( c(100,160,240,350), c(1,0,0,0), yleft=1, yright=0)
f_NXE3600 = approxfun( c(100,160,240,350), c(0,1,0,0), yleft=0, yright=0)
f_NXE3800 = approxfun( c(100,160,240,350), c(0,0,1,0), yleft=0, yright=0)
f_EXE5000 = approxfun( c(100,160,240,350), c(0,0,0,1), yleft=0, yright=1)


#EUV_breakdown <- ASP_by_product

NXE_allocation <- ASP_by_product |> 
  filter( `Product line` == "EUV") |> 
  mutate( NXE3400 = f_NXE3400( ASP ), NXE3600 = f_NXE3600(ASP), NXE3800 = f_NXE3800(ASP), EXE5000 = f_EXE5000(ASP)) |> 
  select( -Sales, -Units, -ASP )

sigma=30

# 100
# 160
# 230

NXE_allocation <-  ASP_by_product |> 
  filter( `Product line` == "EUV") |> 
  mutate( 
    NXE3400 = dnorm( ASP,100,sigma ), NXE3600 = dnorm( ASP,160,sigma ), NXE3800 = dnorm( ASP,230,sigma ), EXE5000 = dnorm( ASP, 350, sigma ),
    pp = rowSums( pick(`NXE3400`:`EXE5000`)  ),
    NXE3400 = NXE3400/pp, NXE3600 = NXE3600/pp, NXE3800 = NXE3800/pp, EXE5000 = EXE5000/pp
  ) |> 
  select( -Sales, -Units )

hours_per_month = 365.25/12*24

OOE = 0.80
wpm_EXE5000 = 185 * hours_per_month / 25/1000 * OOE
wpm_NXE3800 = 220 * hours_per_month / 25/1000 * OOE
wpm_NXE3600 = 160 * hours_per_month / 14/1000 * OOE
wpm_NXE3400 = 135 * hours_per_month / 5/1000 * OOE

scanner_output <- asml_units_attribution |> 
  filter( `Product line` == "EUV") |> 
  left_join( NXE_allocation, by = join_by("Product line", "Quarter")) |> 
  mutate( Quarter = factor( Quarter, quarter_levels, ordered=T )) |> 
  mutate( NXE3400 = NXE3400*Units, NXE3600=NXE3600*Units, NXE3800 = NXE3800*Units, EXE5000 = EXE5000*Units) |> 
  group_by( Region ) |> 
  arrange( Quarter ) |> 
  mutate( `7nm` = cumsum( NXE3400)* wpm_NXE3400 , `5nm` = cumsum( NXE3600) * wpm_NXE3600,`3nm` = cumsum( NXE3800)*wpm_NXE3400, `2nm`=cumsum( EXE5000)*wpm_EXE5000) |> 
  ungroup()


tsmc_maybe = filter( scanner_output, Region == "Taiwan") |> 
  select( Quarter, `7nm`:`2nm`) |> 
  pivot_longer(`7nm`:`2nm`, names_to = "Node", values_to = "Scanner wpm")  |> 
  left_join( est_wafers, by = join_by("Node", "Quarter")) |> 
  left_join( analyst_estimates, by = join_by("Node", "Quarter")) |> 
  ungroup()


filter( tsmc_maybe, Node!= "2nm" ) |> 
  ggplot( aes( x=Quarter, y=wpm ) ) + 
    geom_point( color="blue") + 
    geom_step( aes(x=Quarter,y=`Scanner wpm`, group=1), color="purple")  + 
    geom_step( aes(x=Quarter,y=`wpm_analyst`, group=1), color="darkgrey")  + 
    facet_grid( rows=vars(Node))



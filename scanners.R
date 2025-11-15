
library( tidyverse )
source( "common.R")


analyst_estimates<-read_csv( "analyst estimates by node.csv") |> 
  mutate( Quarter = factor( Quarter, quarter_levels, ordered=T )) |> 
  rename( `7nm`= `7nm/10nm`) |> 
  pivot_longer( !starts_with("Quarter"), names_to= "Node", values_to="Analyst WPM")

# Node Eff is the "effective node", a crude fudge to approximate actual usage rather than spec

effective_node <- tibble( 
    Model = c( "EXE:5200B", "EXE:5000", "NXE:3800E", "NXE:3600D", "NXE:3400C" ),
    `Node Eff` = c( "<2nm", "2nm", "3nm", "5nm", "7nm")
  )

EUV_specs <- read_csv( "scanner_specs.csv") |> 
  filter( Type == "EUV") |> 
  left_join( effective_node, by="Model")


node_pricepoint <- EUV_specs |> 
  select( `Node Eff`, ASP) |> 
  pivot_wider( names_from = `Node Eff`, values_from = ASP)





# linearly interp ASP between the price points
# 
# f_NXE3400 = approxfun( c(100,160,240,350), c(1,0,0,0), yleft=1, yright=0)
# f_NXE3600 = approxfun( c(100,160,240,350), c(0,1,0,0), yleft=0, yright=0)
# f_NXE3800 = approxfun( c(100,160,240,350), c(0,0,1,0), yleft=0, yright=0)
# f_EXE5000 = approxfun( c(100,160,240,350), c(0,0,0,1), yleft=0, yright=1)
# 
# NXE_allocation <- ASP_by_product |> 
#   filter( `Product line` == "EUV") |> 
#   mutate( NXE3400 = f_NXE3400( ASP ), NXE3600 = f_NXE3600(ASP), NXE3800 = f_NXE3800(ASP), EXE5000 = f_EXE5000(ASP)) |> 
#   select( -Sales, -Units, -ASP )


node_layers <- tibble( 
  Node = c( "3nm", "5nm", "7nm" ),
  Layers = c( 22, 12, 4 )
)

scanner_model_allocation <- ASP_by_product |> 
  filter( `Product line` == "EUV") |> 
  cbind( node_pricepoint) |> 
  mutate( across( ends_with( "nm"), \(x) dnorm( ASP, x, sigma ))) |> 
  mutate( pnorm = rowSums( pick( ends_with( "nm")))) |> 
  mutate( across( ends_with( "nm"), \(x) x/pnorm)) |> 
  select( `Product line`, Quarter, ends_with( "nm"))
  

scanner_allocation <- asml_units_attribution |> 
  filter( `Product line` == "EUV") |> 
  left_join( scanner_model_allocation, by=join_by( Quarter, `Product line`)) |> 
  mutate( across( ends_with( "nm"), \(x) Units * x ))


hours_per_month = 365.25/12*24
efficiency = 0.80 # includes uptime, wafer rescan, everything

scanner_allocation


wpm_est <- scanner_allocation |> select( -`%`, -`Units`, -`Cum Units`) |>
  pivot_longer( ends_with( "nm"), names_to = "Node Eff", values_to = "Units") |> 
  left_join( select( EUV_specs, Throughput, `Node Eff`), by = "Node Eff") |> 
  left_join( node_layers, by=c( `Node Eff` = "Node")) |> 
  mutate( `Estimate WPM` = Units * Throughput * hours_per_month * efficiency / Layers  *1e-3 ) |> 
  group_by( `Product line`, Region, `Node Eff` ) |> 
  arrange( Quarter ) |> 
  mutate( `Estimate WPM` = cumsum( `Estimate WPM`)) |> 
  ungroup()


tsmc_maybe = filter( wpm_est, Region == "Taiwan", `Node Eff` %in% c( "3nm", "5nm", "7nm")) |> 
  left_join( analyst_estimates, by = c( `Node Eff` = "Node", "Quarter"))

tsmc_maybe

filter( tsmc_maybe ) |> datify() |> 
  ggplot( aes( x=Date ) ) + 
  geom_step( aes( y=`Estimate WPM`), color="purple" )  + 
  geom_step( aes( y=`Analyst WPM` ), color="grey50" )  + 
  facet_grid( rows=vars(`Node Eff`))


# theme(
#   axis.text.x = element_text(angle = 60, hjust = 1)  # 45° tilt
# )
  


library( tidyverse )
source( "common.R")


analyst_estimates<-read_csv( "analyst estimates by node.csv") |> 
  mutate( Quarter = factor( Quarter, quarter_levels, ordered=T )) |> 
  rename( `7nm`= `7nm/10nm`) |> 
  pivot_longer( !starts_with("Quarter"), names_to= "Node", values_to="Analyst WPM")

analyst_estimates |> datify() |> 
  ggplot( aes( x=Date, y=`Analyst WPM`, color=Node)) +geom_line()

effective_node <- tibble( 
    Model = c( "EXE:5200B", "EXE:5000", "NXE:3800E", "NXE:3600D", "NXE:3400C" ),
    `2nm` = c( 0.5, 1, 0, 0, 0 ),
    `3nm` = c( 0, 0, 1, 0.5, 0 ),
    `5nm` = c( 0, 0, 0, 0.5, 0.5 ),
    `7nm` = c( 0, 0, 0, 0,   0.5 ),
)

EUV_specs <- read_csv( "scanner_specs.csv") |> 
  filter( Type == "EUV") 

node_pricepoint <- EUV_specs |> 
  select( Model, ASP) |>
  pivot_wider( names_from = Model, values_from = ASP )
  
node_pricepoint
node_layers <- tibble( 
  Node = c( "2nm", "3nm", "5nm", "7nm" ),
  Layers = c( 25, 22, 12, 4 )
)


ss <- 30

scanner_model_allocation <- ASP_by_product |> 
  filter( `Technology` == "EUV") |> 
  bind_cols( node_pricepoint) |> 
  mutate( across( contains( ":"), \(x) dnorm( ASP, x, ss ))) |> 
  mutate( pnorm = rowSums( pick( contains( ":")))) |> 
  mutate( across( contains( ":"), \(x) x/pnorm)) |> 
  select( Quarter, `Technology`, contains( ":"))

# scanner_model_allocation |> pivot_longer( `EXE:5200B`:`NXE:3400C`, names_to = "Product line", values_to = "%") |> 
#   datify() |> 
#   ggplot( aes( x=Date, y=`%`, fill=`Product line`)) + geom_col() + scale_fill_brewer(palette="Set3") +
#   theme_minimal() + labs(title="Scanner Model Allocation")

scanner_allocation <- asml_units_attribution |> 
  filter( `Technology` == "EUV") |> 
  left_join( scanner_model_allocation, by=join_by( Quarter, `Technology`)) |> 
  mutate( across( contains( ":"), \(x) Units * x )) |> 
  select( Region, Quarter, contains( ":")) |> 
  pivot_longer( contains(":"), names_to = "Model", values_to = "Units" ) 
  

hours_per_month = 365.25/12*24
efficiency = 0.80 # includes uptime, wafer rescan, everything

wpm_est <-  scanner_allocation |> 
  left_join( select( EUV_specs, Model, Throughput ), by = "Model") |> 
  mutate( `Scanner WPM` = Units * Throughput * hours_per_month * efficiency * 1e-3 ) |> 
  left_join( effective_node, by = "Model") |> 
  mutate( across( ends_with("nm"), \(x) x*`Scanner WPM` ) ) |> 
  pivot_longer( ends_with("nm"), names_to = "Node", values_to = "Estimate WPM") |> 
  left_join( node_layers, by="Node") |> 
  mutate( `Estimate WPM` = `Estimate WPM` / Layers) |> 
  group_by( Region, Quarter, Node ) |> 
  summarize( `Estimate WPM` = sum( `Estimate WPM`) ) |> 
  group_by( Region, Node ) |> 
  arrange( Quarter ) |> 
  mutate( `Estimate WPM` = cumsum( `Estimate WPM`)) |> 
  ungroup()


tsmc_capacity1 = filter( wpm_est, Region == "Taiwan", Node %in% c( "3nm", "5nm", "7nm")) |> 
  left_join( analyst_estimates, by = c( "Node", "Quarter"))

tsmc_production1 = filter(tsmc_wafer_production, Node %in%c( "3nm", "5nm", "7nm"))
# 
# filter( tsmc_capacity1 ) |> datify() |>
#   ggplot( aes( x=Date ) ) +
#   geom_step( aes( y=`Estimate WPM`), color="purple" )  +
#   geom_step( aes( y=`Analyst WPM` ), color="grey50" )  +
#   geom_point( data= tsmc_production1, aes( y=kWPM), color="maroon", size=0.5) +
#   facet_grid( rows=vars(`Node`)) +
#   ggtitle( "TSMC Production Capacity", subtitle = "Scanner Tracker Model") + theme_minimal()


# theme(
#   axis.text.x = element_text(angle = 60, hjust = 1)  # 45° tilt
# )
  

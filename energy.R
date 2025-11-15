library( tidyverse )
source( "common.R")

# exposure energy

wph = 135 
wafer_size = 30 # cm
dose = 0.030 # 30 mJ / cm^2
resist_energy = pi * wafer_size^2 / 4 * dose # Joules
resist_W = wph / 60/60 * resist_energy  
resist_W  # < 1 Watt

resist_energy
1000*60*60 / 1e6

# throughput

pass_energy = 9.64 # kWh / pass
power = pass_energy * wph / 1e3
power # 1.3 MW


fab18_phases <- tibble(
  Fab = "Fab18",
  Quarter = c( "3Q20", "4Q20", "4Q21", "2Q22", "4Q22", "2Q23", "2Q24", "4Q25"),
  node_3nm = c( 0,0,0,0.5,1,1,1,1),
  towers = c( 12,11,11,11,11,11,11,11)
) 

fab21_phases <- tibble( Fab= "Fab21", Quarter = "3Q25", node_3nm = 0, towers = 11)

fab_construction <- bind_rows( fab18_phases, fab21_phases ) |> 
  mutate( Quarter = factor( Quarter, level=quarter_levels, ordered=T)) |> 
  mutate( node_5nm = 1-node_3nm) 


scanner_power_fraction <- 0.1

efficiency = 0.80 # includes uptime, wafer rescan, everything
wpm_NXE3800_3nm = 220 * hours_per_month / 22/1000 * efficiency
wpm_NXE3600_5nm = 160 * hours_per_month / 12/1000 * efficiency

power_NXE3800 <- 1.3
power_NXE3600 <- 1.3

tsmc_capacity2 <- fab_construction |> 
  mutate( 
    power_3nm = towers * 200/11 * scanner_power_fraction * node_3nm,
    power_5nm = towers * 200/11 * scanner_power_fraction * node_5nm,
    `3nm` = power_3nm / power_NXE3800 * wpm_NXE3800_3nm,
    `5nm` = power_5nm / power_NXE3600 * wpm_NXE3600_5nm
  ) |>  
  pivot_longer( `3nm`:`5nm`, names_to = "Node", values_to = "Estimate WPM" ) |> 
  group_by( Quarter, Node ) |> 
  summarize( `Estimate WPM` = sum( `Estimate WPM`)) |> 
  group_by( Node ) |> 
  arrange( Quarter ) |> 
  mutate( `Estimate WPM` = cumsum( `Estimate WPM`)) 

tsmc_capacity2 <- tsmc_capacity2 |> 
  full_join( analyst_estimates, by=join_by(Quarter, Node)) |> 
  filter( Quarter >= "2Q19", Quarter<= "4Q25", Node %in% c( "3nm", "5nm"))

tsmc_production2 = filter(tsmc_wafer_production, Node %in%c( "3nm", "5nm"), Date>=ymd( 20200101))

tsmc_capacity2 |> datify() |> 
  ggplot( aes( x=Date ) ) + 
  geom_step( aes( y=`Estimate WPM`), color="purple" )  + 
  geom_step( aes( y=`Analyst WPM` ), color="grey50" )  + 
  geom_point( data=tsmc_production2, aes( y=wpm), color="maroon", size=0.5) +
  facet_grid( rows=vars(`Node`)) +
  ggtitle( "TSMC Production Capacity", subtitle = "Energy Construction Model")



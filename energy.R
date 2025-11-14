library( tidyverse)

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



fab_phases <- tibble(
  HMV_date = mdy( c( 
    "6/30/20",
    "12/31/20",
    "12/31/21",
    "6/30/22",
    "12/31/22",
    "6/30/23",
    "6/30/24",
    "12/31/2025"
  )),
  Quarter = c( "3Q20", "4Q20", "4Q21", "2Q22", "4Q22", "2Q23", "2Q24", "4Q25"),
  node_3m = c( 0,0,0,0.5,1,1,1,1),
  towers = c( 12,11,11,11,11,11,11,11)
) |> mutate( Quarter = factor( Quarter, level=quarter_levels, ordered=T))

scanner_power_fraction <- 0.1
power_3nm
throughput_3nm


efficiency = 0.80 # includes uptime, wafer rescan, everything
wpm_EXE5000 = 185 * hours_per_month / 25/1000 * OOE
wpm_NXE3800 = 220 * hours_per_month / 22/1000 * OOE
wpm_NXE3600 = 160 * hours_per_month / 14/1000 * OOE
wpm_NXE3400 = 135 * hours_per_month / 5/1000 * OOE

power_NXE3400 <- 1.3
power_NXE3600 <- 1.3

wpm_est <- fab_phases |> 
  mutate( 
    power_3nm = towers * 200/11 * scanner_power_fraction * node_3m,
    power_5nm = towers * 200/11 * scanner_power_fraction * (1-node_3m),
    `3nm` = cumsum( power_3nm / power_NXE3600 * wpm_NXE3600 ),
    `5nm` = cumsum( power_5nm / power_NXE3400 * wpm_NXE3400 )
  ) |> 
  pivot_longer( `3nm`:`5nm`, names_to = "Node", values_to = "wpm_energy" ) |> 
  left_join( analyst_estimates, by=join_by(Quarter, Node))


ggplot( wpm_est, aes( x=Quarter)) + 
  geom_step( aes(x=Quarter,y=`wpm_energy`, group=1), color="magenta")  + 
  geom_step( aes(x=Quarter,y=`wpm_analyst`, group=1), color="darkgrey") +
  facet_wrap( ~ Node )

analyst_estimates

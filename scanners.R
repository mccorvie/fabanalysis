


scanner_specs<-read_csv( "scanner_specs.csv")
scanner_specs

analyst_estimates<-read_csv( "analyst estimates.csv")



node_layers <- tibble(
  Node = c( "3nm", "5nm", "7nm" ),
  Layers = c( 22, 12, 4 )
)


f_NXE3400 = approxfun( c(100,160,230), c(1,0,0), yleft=1, yright=0)
f_NXE3600 = approxfun( c(100,160,230), c(0,1,0), yleft=0, yright=0)
f_NXE3800 = approxfun( c(100,160,230), c(0,0,1), yleft=0, yright=1)


#EUV_breakdown <- ASP_by_product

NXE_allocation <- ASP_by_product |> 
  filter( `Product line` == "EUV") |> 
  mutate( NXE3400 = f_NXE3400( ASP ), NXE3600 = f_NXE3600(ASP), NXE3800 = f_NXE3800(ASP)) |> 
  select( -Sales, -Units, -ASP )


hours_per_month = 365.25/12*24

OOE = 0.85
wpm_NXE3800 = 220 * hours_per_month / 25/1000 * OOE
wpm_NXE3600 = 160 * hours_per_month / 14/1000 * OOE
wpm_NXE3400 = 135 * hours_per_month / 4/1000 * OOE

scanner_output <- asml_units_attribution |> 
  filter( `Product line` == "EUV") |> 
  left_join( NXE_allocation, by = join_by("Product line", "Quarter")) |> 
  mutate( NXE3400 = NXE3400*Units, NXE3600=NXE3600*Units, NXE3800 = NXE3800*Units) |> 
  group_by( Region ) |> 
  arrange( Quarter ) |> 
  mutate( `7nm` = cumsum( NXE3400)* wpm_NXE3800 , `5nm` = cumsum( NXE3600) * wpm_NXE3600,`3nm` = cumsum( NXE3800)*wpm_NXE3400) |> 
  ungroup()


tsmc_maybe = filter( scanner_output, Region == "Taiwan") |> 
  select( Quarter, `7nm`:`3nm`) |> 
  pivot_longer(`7nm`:`3nm`, names_to = "Node", values_to = "Scanner wpm")  |> 
  left_join( est_wafers, by = join_by("Node", "Quarter"))


ggplot( filter( tsmc_maybe, Node == "3nm" ), aes( x=Quarter, y=wpm) ) + geom_point( color="blue") + geom_point( aes(y=`Scanner wpm`), color="black")
ggplot( filter( tsmc_maybe, Node == "5nm" ), aes( x=Quarter, y=wpm) ) + geom_point( color="blue") + geom_point( aes(y=`Scanner wpm`), color="black")
ggplot( filter( tsmc_maybe, Node == "7nm" ), aes( x=Quarter, y=wpm) ) + geom_point( color="blue") + geom_point( aes(y=`Scanner wpm`), color="black")




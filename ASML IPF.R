
library( tidyverse )
source( "common.R")

##
##  Iterative proportional fitting aka raking
##   

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


asml_rev_attribution <- as_tibble( as.data.frame.table( fine_attribution)) |> 
  rename( `Product line`= Var1, Region=Var2, Quarter=Var3, `%` = Freq) |> 
  mutate( Quarter = factor( Quarter, levels = quarter_levels, ordered=T))

EUV_rev  <- asml_rev_attribution |> 
  filter( `Product line` == "EUV" ) |> 
  group_by( `Quarter`) |> 
  mutate( `%` = `%` / sum( `%`)) |> 
  ungroup()

# datify(EUV_rev) |>  
#   ggplot( aes( x=Date, y=`%`,fill=Region)) +geom_col() + scale_fill_brewer(palette= "Set3") +
#   scale_y_continuous(labels = scales::percent)+
#   theme_minimal() + labs( title="Allocation of EUV Scanners")


asml_units_attribution <- asml_rev_attribution |> 
  group_by( `Product line`, Quarter ) |> 
  mutate( `%` = `%` / sum( `%`)) |> 
  left_join( units_by_product, by = join_by( Quarter, `Product line`) ) |> 
  mutate( Units = Units * `%`) |> 
  group_by( Region, `Product line` ) |> 
  arrange( Quarter ) |> 
  mutate( `Cum Units` = cumsum( Units ))  |> 
  ungroup()

# asml_units_attribution |> datify() |> 
#   filter( `Product line` == "EUV") |> 
#   ggplot( aes( x=Date, y=`Cum Units`,color=Region)) +geom_line() + geom_point()+ scale_color_brewer(palette= "Set2") + theme_minimal()
# 
# asml_units_attribution |> datify() |> 
#   filter( `Region` == "Taiwan") |> 
#   ggplot( aes( x=Date, y=`Cum Units`,color=`Product line`)) +geom_line() + geom_point()+ scale_color_brewer(palette= "Set2") + theme_minimal()

asml_units_attribution |> filter( Region == "Taiwan", `Product line`=="EUV", Quarter ==max(Quarter))


#1Q19 11
#4Q19 26
#
#3Q23 113
#ut of 202  (113/202 = 56%)


asml_units_attribution |> filter( Quarter == "1Q19", Region == "Taiwan", `Product line`=="EUV") |> pull( `Cum Units`)
asml_units_attribution |> filter( Quarter == "3Q23", Region == "Taiwan", `Product line`=="EUV" ) |> pull( `Cum Units`)
asml_units_attribution |> filter( Quarter == "3Q23", `Product line`=="EUV") |> pull( `Cum Units`) |> sum()


# IPF heatmap 

heatmap_with_marginals_onepanel <- function(P, option="D", quarter) 
{
  stopifnot(is.matrix(P))
  if( abs(sum(P) - 1) > 1e-10) P <- P / sum(P)
  
  nr <- nrow(P); nc <- ncol(P)
  if (is.null(rownames(P))) rownames(P) <- paste0("r", 1:nr)
  if (is.null(colnames(P))) colnames(P) <- paste0("c", 1:nc)
  
  # Main matrix tiles
  main <- expand.grid(row = seq_len(nr), col = seq_len(nc), KEEP.OUT.ATTRS = FALSE) |>
    mutate(p = as.vector(P))
  
  # Extra top row (column marginals) at row = nr + 1
  top  <- tibble(row = nr + 1L, col = seq_len(nc), p = colSums(P))
  # Extra right column (row marginals) at col = nc + 1
  side <- tibble(row = seq_len(nr), col = nc + 1L, p = rowSums(P))
  # Corner (leave blank)
  corner <- tibble(row = nr + 1L, col = nc + 1L, p = NA_real_)
  
  all <- bind_rows(main, top, side, corner)
  
  ggplot(all, aes(col, row, fill = p)) +
    geom_tile(color = "grey85", linewidth = 0.2) +
    scale_fill_viridis_c(name = "Proportion", option=option, na.value = NA) +
    # Only label the original matrix rows/cols; the extra row/col are unlabeled:
    scale_x_continuous(breaks = 1:nc, labels = colnames(P), expand = c(0, 0)) +
    scale_y_reverse(breaks = 1:nr, labels = rownames(P), expand = c(0, 0)) +
    coord_fixed() +
    labs(x = NULL, y = NULL, title=paste("Attribution Matrix", quarter)) +
    theme_minimal(base_size = 12) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 20, hjust = 0.2, vjust = 0.3)) +
    # Optional dark separators to “frame” the marginal strips:
    geom_hline(yintercept = nr + 0.5, linewidth = 1.5, colour = "azure") +
    geom_vline(xintercept = nc + 0.5, linewidth = 1.5, colour = "azure")
}

# date_idx <- 18
# quarter <- dimnames( fine_attribution)[[3]][date_idx]
# P <- fine_attribution[,,date_idx]
# heatmap_with_marginals_onepanel(P,"D",quarter)
# asml_revenue$Quarter[date_idx ]
# asml_revenue


library(tidyverse)
library(lavaan)
library(semPlot)
library(semptools)


rawdata <- readxl::read_excel("./rawdata/Table 1.XLS")
rawdata %>% sjPlot::view_df()



########################################################################
model <- '

   PCR   =~ PCR1 + PCR2 + PCR3 + PCR4 + PCR5 + PCR6
   LE    =~ LE1 + LE2 + LE3 + LE4
   ASE   =~ ASE1 + ASE2 + ASE3 + ASE4 + ASE5 
   LM    =~ LM1 + LM2 + LM3 + LM4

   LM    ~  H2*PCR  
   ASE   ~  H5*PCR + H6*LM
   LE    ~  H1*PCR + H3*LM + H7*ASE


'

fit <- sem(model,  data = rawdata)


fit %>%  
  parameterEstimates(standardized = TRUE) %>% 
  filter(op %in% c("~")) %>%  
  filter(str_detect(label, "^H")) %>% 
  mutate(" " = gtools::stars.pval(pvalue), .after = pvalue) %>%
  arrange(label) %>% 
  flextable::flextable() %>% 
  flextable::color(j = "std.all", color = "red") %>% 
  flextable::colformat_double(digits = 3) %>% 
  flextable::autofit()  
########################################################################





########################################################################
indicator_order <- c(
  paste0("LM",   4:1),
  paste0("ASE",  5:1),
  paste0("PCR",  6:1),
  paste0("LE",   4:1)
)


indicator_factor <- c(
  rep("LM",  4),
  rep("ASE", 5),
  rep("PCR", 6),
  rep("LE",  4)
  )



factor_layout <- matrix(
  c(NA,    "LM", NA,   "ASE", NA,  
    "PCR", NA,   NA,   NA ,   "LE"), 
  byrow = TRUE, 2, 5)


factor_point_to <- matrix(
  c(NA,    "left", NA,   "right",   NA,  
    "left", NA,    NA,   NA ,   "right"), 
  byrow = TRUE, 2, 5)


indicator_push <- c(LM  = 2.5,
                    ASE = 2.5,
                    PCR = 2.5,
                    LE  = 2.5
                    )


indicator_spread <- c(LM  = 1,
                      ASE = 1,
                      PCR = 1.2,
                      LE  = 1)


loading_position <- c(ASE = .5,
                      LE  = .5)



p <- fit %>% 
  semPaths(
    what           = "path", 
    whatLabels     = "std",       
    style          = "ram",    # lisrel / ram 
    edge.label.cex = 0.8,        
    edge.color     = "blue",     
    exoVar         = FALSE,
    residScale     = 8,   
    rotation       = 2,
    nCharEdges     = 0,
    nCharNodes     = 0,          
    residuals      = TRUE,       
    intercepts     = FALSE,       
    
    groups         = "latents", 
    pastel         = TRUE, 
    borders        = TRUE,
    border.color   = "white",
    color = list(
       lat = c("#EFA39F", "#A0D8EA", "#F7CB65", "#C1E1C4")
    ),
    
    DoNotPlot      = TRUE,
    mar            = c(8, 8, 8, 8)
    
    # filename       = "myplot",   #<<
    # filetype       = "pdf",      #<<
    # width          = 5,          #<<
    # height         = 3.5         #<<
  ) 


p %>% 
  set_sem_layout(
    indicator_order  = indicator_order,
    indicator_factor = indicator_factor,
    factor_layout    = factor_layout,
    factor_point_to  = factor_point_to,
    indicator_push   = indicator_push,
    indicator_spread = indicator_spread,
    loading_position = loading_position
  ) %>% 
  
  mark_sig(object = fit) %>% 
  set_curve( c("LE ~ LM" = 0) ) %>% 
  rotate_resid( c(LM = 0, ASE = 0, LE = -180) ) %>% 
  set_edge_label_position( c("ASE ~ PCR" = 0.45) ) %>% 
  plot()
########################################################################


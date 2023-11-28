# Outcomes Survs formulas
surv.form <-  c(OS = "Surv(SURVY,STATE)~",
                PFS = "Surv(PFSY,STATEPFS)~ ",
                RI = "Surv(PFSY,factor(CRELAPSE))", #1
                NRM ="Surv(PFSY,factor(CRELAPSE))", #2
                GRFS ="Surv(BMTGRFSY,GRFS)",
                CGVH = "Surv(BMTCGVH,factor(CCGVH))~", #"1"
                CGVH_EXT = "Surv(BMTCGVHEXT,factor(CCGVHEXT))~", #"1"
                CAGVH34 = "(Surv(BMTAGV3,factor(CAGVH3))~", #"2"
                CAGVH24 = "(Surv(BMTAGV2,factor(CAGVH2))~" #"2"
)

# Usual Adjustments
covars.adj <- c(   "PATSEX", "DONSEX1" ,  "AGETX","KARN90" ,"DRI_ADJ" , "YEARTX","CMVDP1", "VRADICON","MYELOABRJEG","frailty(CENTRE)" )

# Sample Form  OS exemple
cox_OS_9 <- coxph(as.formula(   paste0( event , paste(covars.adj, collapse=" + ")) ) ,data=indata )  

# Table Build
t1 <- cox_OS_9 %>% tbl_regression(exponentiate = TRUE,label = cox_labels ,include = !starts_with("frailty(")) %>% bold_labels() %>% bold_p(t = 0.10) %>%add_significance_stars(
    hide_p = FALSE,
    pattern = "{p.value}{stars}",
    hide_se = TRUE
  )

## Stacking Multiple Outcomes HR Tables
tbl_merge_ex1 <-tbl_merge(
    tbls = list(t3,t4,t1, t2 ),
    tab_spanner = c("**Non Relapse Mortality NRM** *(N events = {cox_NRM_9$nevent})*","**Relapse Incidence RI** *(N events = {cox_RPSE_9$nevent})*","**Overall Survival OS** *(N events = {cox_OS_9$nevent})*", "**Progression Free Survival PFS** *(N events = {cox_PFS_9$nevent})*" )
  )%>% modify_caption("**Multivariate Cox Analysis Survival Relapse related Outcomes** ")

## Aesthetics
tbl_merge_ex1  %>% as_gt() %>%  gt::tab_style( style =  gt::cell_borders(
                                                                          sides =c( "l"),color = "royalblue",weight = gt::px(0.06)),
                                                locations =  list(gt::cells_body(  columns = starts_with("estimate")))
                          ) %>%  gt::tab_style(    style =  list(gt::cell_borders(
                                                                   sides =c("b", "t"),color = "red",weight = gt::px(0.2)),
                                                                 gt::cell_fill(color = "#F9E3D6")),
                                                   locations =  list(gt::cells_body( rows = c(3)))
                                              )

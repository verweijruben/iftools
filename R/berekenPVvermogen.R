#'  Functie om de opbrengst van PV te berekenen
#'


# BEREKEN DE OPBRENGST VAN PV PANELEN
F_bereken_pv_kwh <- function(buurtOppervlakGrond,geschikt_dakoppervlak,opp_paneel,e_opbrengst,woonfunctie){
  sum(buurtOppervlakGrond) * geschikt_dakoppervlak / opp_paneel * e_opbrengst / nrow(woonfunctie)
}


#'  Functie om de kosten voor de gasverbruikbesparing per pand te berekenen
#'

berekenKostenBesparingsPotentieWoningen <- function(TabelMetPanden){

TabelMetPanden <- PandRigo
lijst_panden = unique(TabelMetPanden$PAND_ID)
data_IF = besparingspotentieel_tabel # RVO tabel met besparingspotentieel

for(i in 1:length(lijst_panden)){

  te_beschouwen_panden = TabelMetPanden[TabelMetPanden$PAND_ID==lijst_panden[i],] # 208 is flat
  aantal_woningen = nrow(te_beschouwen_panden[te_beschouwen_panden$DOEL == "woonfunctie",])
  telling = count(te_beschouwen_panden$TYPE)
  woning <- te_beschouwen_panden[1,] # alleen één pand is interessant
  besparing <- data_IF[data_IF$Bouwperiode == paste(woning$PERIODE),]
  besparing <- besparing[besparing$Woningtype == paste(woning$RVO),]

  if(woning$TYPE == 1){
    besparing <- besparing[besparing$Subtype == "gemiddeld",]
    besparing <- besparing[besparing$Bewonersgedrag == "werkelijk energiegebruik",]
  } else if(woning$TYPE == 2){
    besparing <- besparing[besparing$Subtype == "tussen",]
    besparing <- besparing[besparing$Bewonersgedrag == "werkelijk energiegebruik",]
  } else if(woning$TYPE == 3){
    besparing <- besparing[besparing$Subtype == "hoek",]
    besparing <- besparing[besparing$Bewonersgedrag == "werkelijk energiegebruik",]
  } else if(woning$TYPE == 4){
    besparing <- besparing[besparing$Bewonersgedrag == "werkelijk energiegebruik",]
  } else if(woning$TYPE == 5){
    besparing <- besparing[besparing$Bewonersgedrag == "werkelijk energiegebruik",]
  } else if(woning$TYPE == 0){
    besparing <- besparing[besparing$Subtype == "gemiddeld",]
    besparing <- besparing[besparing$Bewonersgedrag == "werkelijk energiegebruik",]
  }


  if(woning$TYPE != 0){
  pand_kosten_isolatie=
  besparing$m2_platdak[besparing$Pakket == "huidig"] * kosten_dakisolatie_plat +
  besparing$m2_hellenddak[besparing$Pakket == "huidig"] * kosten_dakisolatie_hellend +
  besparing$m2_zijgevel[besparing$Pakket == "huidig"] * kosten_spouwmuurisolatie +
  mean(te_beschouwen_panden$OPPERVLAK_GROND) * kosten_vloerisolatie / aantal_woningen

  pand_kosten_hrglas =
  besparing$m2_enkelzijgevel[besparing$Pakket == "huidig"] * kostenhrplusplusglas_enkel +
  besparing$m2_dubbelzijgevel[besparing$Pakket == "huidig"] * kostenhrplusplusglas_dubbel +
  besparing$m2_enkelglasvoorachtergevel[besparing$Pakket == "huidig"] * kostenhrplusplusglas_enkel +
  besparing$m2_dubbelglasvoorachtergevel[besparing$Pakket == "huidig"] * kostenhrplusplusglas_dubbel

  pand_kosten_isolatie_totaal = pand_kosten_isolatie + pand_kosten_hrglas

  TabelMetPanden$kosten_besparing1[TabelMetPanden$PAND_ID == woning$PAND_ID] = pand_kosten_isolatie_totaal


  } else {pand_kosten_isolatie_totaal=0}


}


TabelMetPanden$kosten_besparing1[TabelMetPanden$TYPE == 0] = 0


for(i in 1:nrow(TabelMetPanden)){
  woning <- TabelMetPanden[i,]
  TabelMetPanden$pand_isolatie_besparing[i] = TabelMetPanden$besparing1[i] * sum(as.vector(na.exclude(prijsreeks$euro_kuub[1:levensduur_isolatie])))
  TabelMetPanden$pand_isolatie_rendabel[i] = TabelMetPanden$pand_isolatie_besparing[i] > TabelMetPanden$kosten_besparing1[i]
}


return(TabelMetPanden)
remove(lijst,te_beschouwen_panden,aantal_woningen)
remove(data_IF)
}













#'  Functie om de gasverbruikbesparing per pand te berekenen
#'

berekenBesparingsPotentieWoningen <- function(TabelMetPanden){

data_IF = besparingspotentieel_tabel # RVO tabel met besparingspotentieel
for(i in 1:nrow(TabelMetPanden)){
woning <- TabelMetPanden[i,]
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

if(woning$TYPE != 0){TabelMetPanden$besparing1[i] <- as.integer(abs(woning$VERBRUIK_G * (besparing$m3_gas[besparing$Pakket == "besparingspakket"] - besparing$m3_gas[besparing$Pakket == "huidig"]) / besparing$m3_gas[besparing$Pakket == "huidig"]))} else{TabelMetPanden$besparing1[i] = 0}
if(woning$TYPE != 0){TabelMetPanden$besparing2[i] <- as.integer(abs(woning$VERBRUIK_G * (besparing$m3_gas[besparing$Pakket == "besparingspakket extra"] - besparing$m3_gas[besparing$Pakket == "huidig"]) / besparing$m3_gas[besparing$Pakket == "huidig"]))}  else{TabelMetPanden$besparing2[i] = 0}


}
TabelMetPanden$besparing1[TabelMetPanden$TYPE == 0] = 0
return(TabelMetPanden)
}




#Analiza wyborów do Sejmu w 2023: wyniki faktyczne vs. symulacja dla "równości" głosów między okręgami
# Autor: Krzysztof Pytka
# Data:  19/10/2023

# 
# __        __                    _            _                        _
# \ \      / /_ _ _ __ __ _ _ __ (_) ___    __| | __ _ _ __  _   _  ___| |__
# \ \ /\ / / _` | '__/ _` | '_ \| |/ _ \  / _` |/ _` | '_ \| | | |/ __| '_ \
#   \ V  V / (_| | | | (_| | | | | |  __/ | (_| | (_| | | | | |_| | (__| | | |
#    \_/\_/ \__, |_|  \__,_|_| |_|_|\___|  \__,_|\__,_|_| |_|\__, |\___|_| |_|
#           |___/                                            |___/

require("tidyverse")
require("magrittr")
require("coalitions")



okregi <- read_csv2("okregi_sejm_utf8.csv") %>% 
          select("Numer okręgu", 
                 "Liczba mandatów",
                 "Siedziba OKW",
                 "Opis granic") %>% 
          rename("Nr okręgu" = "Numer okręgu")
  

kandydaci <- read_csv2("wyniki_gl_na_listy_po_okregach_sejm_utf8.csv") %>% 
  select(
  "Nr okręgu"                                                                                                                ,
  "Liczba głosów ważnych oddanych łącznie na wszystkie listy kandydatów"                                                     ,
  "Liczba wyborców uprawnionych do głosowania",
  "KOALICYJNY KOMITET WYBORCZY TRZECIA DROGA POLSKA 2050 SZYMONA HOŁOWNI - POLSKIE STRONNICTWO LUDOWE"                       ,
  "KOMITET WYBORCZY NOWA LEWICA"                                                                                             ,
  "KOMITET WYBORCZY PRAWO I SPRAWIEDLIWOŚĆ"                                                                                  ,
  "KOMITET WYBORCZY KONFEDERACJA WOLNOŚĆ I NIEPODLEGŁOŚĆ"                                                                    ,
  "KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI") %>% 
  rename("Liczba głosów" = "Liczba głosów ważnych oddanych łącznie na wszystkie listy kandydatów",
         "Liczba uprawnionych" = "Liczba wyborców uprawnionych do głosowania")



wyniki <- kandydaci %>% left_join(
                  okregi,
                  by = "Nr okręgu"
                  )
   
                   
partie <- c("KOALICYJNY KOMITET WYBORCZY TRZECIA DROGA POLSKA 2050 SZYMONA HOŁOWNI - POLSKIE STRONNICTWO LUDOWE"                       ,
            "KOMITET WYBORCZY NOWA LEWICA"                                                                                             ,
            "KOMITET WYBORCZY PRAWO I SPRAWIEDLIWOŚĆ"                                                                                  ,
            "KOMITET WYBORCZY KONFEDERACJA WOLNOŚĆ I NIEPODLEGŁOŚĆ"                                                                    ,
            "KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI")


# __        __           _ _    _          __ _       _       _
# \ \      / /   _ _ __ (_) | _(_)   ___  / _(_) ___ (_) __ _| |_ __   ___
#  \ \ /\ / / | | | '_ \| | |/ / |  / _ \| |_| |/ __|| |/ _` | | '_ \ / _ \
#   \ V  V /| |_| | | | | |   <| | | (_) |  _| | (__ | | (_| | | | | |  __/
#    \_/\_/  \__, |_| |_|_|_|\_\_|  \___/|_| |_|\___|/ |\__,_|_|_| |_|\___|
#                                                          |___/                                 |__/
# Założenie: każdy okręg ma egzogenicznie ustaloną liczbę mandatów 
 
#Liczenie głosów dla każdej partii w każdym okręgu według FAKTYCZNEJ liczby mandatów dla każdego okręgu
mandaty_faktycznie <- wyniki %>% group_by(`Nr okręgu`) %>% 
            summarise(
               dHondt = dHondt(
                        votes   = c(`KOALICYJNY KOMITET WYBORCZY TRZECIA DROGA POLSKA 2050 SZYMONA HOŁOWNI - POLSKIE STRONNICTWO LUDOWE`                       ,
                        `KOMITET WYBORCZY NOWA LEWICA`                                                                                             ,
                        `KOMITET WYBORCZY PRAWO I SPRAWIEDLIWOŚĆ`                                                                                  ,
                        `KOMITET WYBORCZY KONFEDERACJA WOLNOŚĆ I NIEPODLEGŁOŚĆ`                                                                    ,
                        `KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI`)
                        ,
                        parties = partie,
                        n_seats = `Liczba mandatów`
                    ),
                partie = partie
                )
 
 mandaty_faktycznie %>% ungroup %>% group_by(partie) %>% 
                    summarise(Mandaty = sum(dHondt))
 
 
 
 
 #  ____                        _             _
 # / ___| _   _ _ __ ___  _   _| | __ _  ___ (_) __ _
 # \___ \| | | | '_ ` _ \| | | | |/ _` |/ __|| |/ _` |
 # _ __) | |_| | | | | | | |_| | | (_| | (__ | | (_| |
 # |____/ \__, |_| |_| |_|\__,_|_|\__,_|\___|/ |\__,_|
 #       |___/                            |__/
 # Założenie: każdy okręg ma liczbę mandatów proporcjonalną do oddanych głosów w 2023
 
 wyniki <- wyniki %>% mutate("Liczba mandatów wg oddanych głosów" = 
                      kandydaci %$% dHondt(votes = `Liczba głosów` ,
                      parties = `Nr okręgu`,
                      n_seats = 460
                      ))



 mandaty_hipotetyczne <- wyniki %>% group_by(`Nr okręgu`) %>% 
   summarise(
     dHondt = dHondt(
       votes   = c(`KOALICYJNY KOMITET WYBORCZY TRZECIA DROGA POLSKA 2050 SZYMONA HOŁOWNI - POLSKIE STRONNICTWO LUDOWE`                       ,
                   `KOMITET WYBORCZY NOWA LEWICA`                                                                                             ,
                   `KOMITET WYBORCZY PRAWO I SPRAWIEDLIWOŚĆ`                                                                                  ,
                   `KOMITET WYBORCZY KONFEDERACJA WOLNOŚĆ I NIEPODLEGŁOŚĆ`                                                                    ,
                   `KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI`)
       ,
       parties = partie,
       n_seats = `Liczba mandatów wg oddanych głosów`
     ),
     partie = partie
   )
 
 #  ____           _                                              _
 # |  _ \ ___   __| |___ _   _ _ __ ___   _____      ____ _ _ __ (_) ___
 # | |_) / _ \ / _` / __| | | | '_ ` _ \ / _ \ \ /\ / / _` | '_ \| |/ _ \
 # |  __/ (_) | (_| \__ \ |_| | | | | | | (_) \ V  V / (_| | | | | |  __/
 # |_|   \___/ \__,_|___/\__,_|_| |_| |_|\___/ \_/\_/ \__,_|_| |_|_|\___|
 
 require(stargazer)
 wyn1 <- mandaty_hipotetyczne %>% ungroup %>% group_by(partie) %>% 
   summarise("dHondt^2" = sum(dHondt)) 
 
 wyn2 <-  mandaty_faktycznie %>% ungroup %>% group_by(partie) %>% 
   summarise("Mandaty faktyczne"= sum(dHondt))
   
    
  
 


  
  wyn2 %>% left_join(wyn1, by="partie") %>% 
    rename(`Oficjalne` = `Mandaty faktyczne`, `Proporcjonalne` = `dHondt^2`) %>% 
    mutate(Komitet = case_when( 
        partie == "KOALICYJNY KOMITET WYBORCZY TRZECIA DROGA POLSKA 2050 SZYMONA HOŁOWNI - POLSKIE STRONNICTWO LUDOWE" ~ "3D", 
        partie == "KOMITET WYBORCZY NOWA LEWICA"~"Lewica"                                                                                             ,
        partie == "KOMITET WYBORCZY PRAWO I SPRAWIEDLIWOŚĆ"~"PiS"                                                                                  ,
        partie == "KOMITET WYBORCZY KONFEDERACJA WOLNOŚĆ I NIEPODLEGŁOŚĆ"~"Konfederacja"                                                                    ,
        partie == "KOALICYJNY KOMITET WYBORCZY KOALICJA OBYWATELSKA PO .N IPL ZIELONI"~"Koalicja Obywatelska")) %>% 
    select(-"partie", "Komitet", "Oficjalne", "Proporcjonalne") %>% 
    mutate("Niedoreprezentowanie" = `Proporcjonalne` - `Oficjalne` )%>% arrange(desc(`Niedoreprezentowanie`)) %>% 
    select("Komitet","Niedoreprezentowanie", "Oficjalne",  "Proporcjonalne") %>% 
    stargazer(style='qje', summary=FALSE)
  
  
  
  
  wyniki  %>% 
    rename("Proporcjonalne" = "Liczba mandatów wg oddanych głosów",
           "Oficjalne" = "Liczba mandatów" ) %>% 
    mutate("Niedoreprezentowanie" = `Proporcjonalne` - `Oficjalne` ) %>% 
    select("Nr okręgu"                           ,
           "Siedziba OKW"                        , 
           "Niedoreprezentowanie",
               "Oficjalne"                     , 
               "Proporcjonalne") %>% 
    arrange(desc(`Niedoreprezentowanie`)) %>% stargazer(style='qje', summary=FALSE)

#WCZYTANIE BIBLIOTEK
library(survey) #główna biblioteka do uwzględnienia różnych schematów losowania jednostek
library(srvyr)  #wsparcie dla biblioteki survey poprzez syntaksy zbudowane w oparciu o bibliotekę dplyr
library(dplyr)  #biblioteka umożliwiająca manipulację ramkami danych 
library(flextable) #biblioteka umożliwiająca wygenerowanie tabeli z ramki danych

#PRZEDSTAWIENIE ZBIORU DANYCH
#Posłużymy się dostępnym zbiorem danych, który jest stosowany w przykładach biblioteki survey. Zbiór danych dotyczy wyników kształcenia uczniów w Kalifornii. Główna populacja zawiera dane o wszystkich szkołach z przynajmniej 100 uczniami.

#data(api) zbiór składający się z 5 baz danych:
#apipop    baza danych dla całej populacji (N=6194)
#apisrs    baza danych dla szkół wylosowanych w sposób losowy prosty(N=200)
#apistrat  baza danych dla szkół wylosowanych w sposób losowy prosty w określonych warstwach (typ szkoły) (N=200)
#apiclus1  baza danych dla szkół w wylosowanych dzielnicach (N=183)
#apiclus2  baza danych dla wylosowanych szkół w wylosowanych dzielnicach (N=126)

#OPIS ISTOTNYCH ZMIENNYCH
#cds   ID
#stype typ szkoły (E/M/H)
#snum  numer szkoły (N=6194)
#dnum  numer dzielnicy (N=757)
#cnum  numer hrabstwa (N=57)
#api99 wynik kształcenia uczniów w 1999 roku (skala: 200-1000)
#api00 wynik kształcenia uczniów w 2000 roku (skala: 200-1000)
#pw    waga dla próbkowania

data(api) #wczytujemy zbiór danych

#UTWORZENIE PROJEKTU BADANIA
#Tworzymy design dla bazy danych apiclus2 - uwzględniamy losowanie, gdzie spośród 757 dzielnic wylosowano 40 w pierwszym kroku,a w drugim kroku wylosowano 126 szkół spośród 271.
dclus2_srvyr<-apiclus2 %>%
  as_survey_design(id = c(dnum,snum), #wskazanie klastrów pierwszego i drugiego stopnia 
                   strata = NULL, 
                   weights= pw) #wskazanie wagi próbkowania

#Zakładamy, że dane przedstawione w bazie danych apiclus2 wylosowano w sposób prosty - domyślne ustawienie programów statystycznych
dclus2_srs_srvyr<-apiclus2 %>%
  as_survey_design(id = 1, 
                   strata = NULL, 
                   weights= NULL) 


#PRZEDSTAWIENIE WYNIKÓW DLA RÓŻNYCH PROJEKTÓW BADANIA
#Podsumowanie zmiennej api00

#Złożone losowanie (dclus2_srvyr)
tabela1a <- dclus2_srvyr %>% 
  summarize(mean = survey_mean(api00,vartype=c("ci","se"),level=0.95))%>% #wyliczenie średniej, błędu standardowego oraz dolnej i górnej wartości przedziału ufności
  mutate(CI=paste(round(mean_low,2),"-",round(mean_upp,2),sep=""))%>% #utworzenie łącznego przedziału ufności oraz zaokrąglenie wartości liczbowych do drugiego miejsca po przecinku
  select(mean,mean_se,CI)%>% #wybranie zmiennych z ramki danych
  rename('95% CI'=CI,SE=mean_se) #zmiana nazw zmiennych

#Proste losowanie (dclus2_srs_srvyr)
tabela1b <- dclus2_srs_srvyr %>% 
  summarize(mean = survey_mean(api00,vartype=c("ci","se"),level=0.95))%>%
  mutate(CI=paste(round(mean_low,2),"-",round(mean_upp,2),sep=""))%>%
  select(mean,mean_se,CI)%>%
  rename('95% CI'=CI,SE=mean_se)

#Połączenie dwóch powyższych ramek danych
tabela1<-rbind(tabela1a,tabela1b)%>% 
  mutate('Złożony design' = c("Uwzględniono","Nie uwzględniono"))%>% #utworzenie nowej zmiennej
  relocate('Złożony design',.before=mean)%>% #zmiana kolejności kolumn w ramce danych
  rename('Średnia'=mean)%>%
  flextable()%>% #utworzenie tabeli z ramki danych
  autofit() %>%  #dopasowanie wysokości i szerokości tabeli automatycznie
  flextable::set_caption("Tabela 1. Podsumowanie zmiennej api00 z uwzględnieniem projektu badania")%>% #dodanie tytułu tabeli
  colformat_num(digits=2)%>% #wyświetlenie wartości liczbowych do dwóch miejsc po przecinku
  footnote(i=1,j=3:4,value = as_paragraph(
    c("Błąd standardowy","Przedział ufności")),ref_symbols = c("a","b"),part="header",inline=TRUE) #dodanie stopki do tabeli
tabela1
#UTWORZENIE DODATKOWEGO PROJEKTU BADANIA
#W przypadku projektu (dclus2) rozkład typów szkół dla próby różni się nieco od rozkładu populacji. W tym przypadku można zastosować metodę postratyfikacji, która przeważa rozkład próby względem rozkładu populacji.

#Rozkład typów szkoły w próbie
clus2<- apiclus2 %>%
  group_by(stype) %>% 
  summarise(n_clus2 = n()) %>%
  mutate(proc_clus2 = n_clus2/sum(n_clus2))

#Rozkład typów szkoły w populacji
pop <- apipop %>%
  group_by(stype) %>% 
  summarise(n_pop = n()) %>%
  mutate(proc_pop = n_pop /sum(n_pop))

clus2_pop<-cbind(clus2,pop)%>% #łączenie ramek danych kolumnami
  subset(.,select=which(! duplicated(names(.))))%>% #usunięcie powtarzających się kolumn
  mutate(proc_clus2=round(proc_clus2,2),
         proc_pop=round(proc_pop,2))%>%
  rename('Typ szkoły'=stype,'N dla próby'=n_clus2,'Odsetek dla próby'=proc_clus2,'N dla populacji'=n_pop,'Odsetek dla populacji'=proc_pop)%>%
  flextable()%>%
  autofit() %>% 
  flextable::set_caption("Tabela 2. Rozkład typów szkoły w próbie i populacji")
clus2_pop
pop.types<-xtabs(~stype, data=apipop) #Rozkład typów szkoły dla populacji

#Złożone losowanie z zastosowaniem metody postratyfikacji (przeważenia próby względem populacji)
dclus2post_srvyr<-apiclus2 %>%
  as_survey_design(id = c(dnum,snum), 
                   strata = NULL, 
                   weights= pw)%>%
  postStratify(~stype, pop.types) #uwzględnienie rozkładów typów szkoły dla próby i populacji

#PONOWNE PRZEDSTAWIENIE WYNIKÓW DLA RÓŻNYCH PROJEKTÓW BADANIA
tabela1c <- dclus2post_srvyr %>% 
  summarize(mean = survey_mean(api00,vartype=c("ci","se"),level=0.95))%>%
  mutate(CI=paste(round(mean_low,2),"-",round(mean_upp,2),sep=""))%>%
  select(mean,mean_se,CI)%>%
  rename('95% CI'=CI,SE=mean_se)

tabela3<-rbind(tabela1a,tabela1b,tabela1c)%>%
  mutate('Złożony design' = c("Uwzględniono","Nie uwzględniono","Uwzględniono (poststratyfikacja)"))%>%
  relocate('Złożony design',.before=mean)%>%
  rename('Średnia'=mean)%>%
  flextable()%>%
  autofit() %>% 
  flextable::set_caption("Tabela 3. Podsumowanie zmiennej api00 z uwzględnieniem projektu badania")%>%
  colformat_num(digits=2)%>%
  footnote(i=1,j=3:4,value = as_paragraph(
    c("Błąd standardowy","Przedział ufności")),ref_symbols = c("a","b"),part="header",inline=TRUE)%>%
  hline(i = 2, border = officer::fp_border(color="orange", width = 2)) #dodanie poziomej, kolorowej linii
tabela3
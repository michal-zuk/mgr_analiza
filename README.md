# mgr_analiza
 wyniki znajdziesz w folderze results, sa tam pliki .csv z probką danych po obróbce (sample_data), wyniki staytstyki (test_output) i wyniki dla wiedzy (test_wiedza_output). Wszystkie 3 sa zebranie w plik wyniki.ods. Do tego mnóstwo plików z wykresami - sa to violin ploty - to kolorowe na zewnatrz jest tym szersze im wiecej takich odpowiedzi. W srodku box=plot, standardowy dla statyski testem U Manna Whitneya (robilem nim wszystko).
 
 To, co ze statystyki Cię najbardziej interesuje to kolumny model_XYZ_p.value < 0.05. XYZ to nazwa zmiennej po ktorej grupowalem odpowiedzi, zeby zobaczyc, czy jest roznica w danym pytaniu. Te zmienne to:
 
 Kobieta: 1 - Kobieta, 0 - mezczyzna
 Magister: 1- Magister, 0 - licencjat
 Zajecia: 1 - mial/a zajecia z rehabilitacji seksualnej, 0 - nie mial/a
 zajecia_dodatkowe: 1 - mial/a, 0 - nie mial/a
 Wiedza - najpierw wyciagnalem srednia z odpowiedzi na p.1-10 każdej z osób, następnie zrobiłem scoring na wysoką (1) i niską (0) wiedzę. Wysoka to średnia > 3.5, niska to średnia < 2.5.
 Potem sa po profesjach, gdzie 1 to ktos zadeklarowal, ze pracuje z takimi pacjentami, 0 - ze nie
 
 Skroty profesji to:
 neuro - neurologiczni
 reuma - reumatologiczni
 ozn_ruch - OzN ruchową
 ozn_int - OzN intelektualną
 geria - geriatria
 kardio - kardiologia
 orto - ortopedia
 Uro - urologiczni
 ginek - ginekologiczne
 onko - onkologiczni
 
 Wiec w kazdym z tych testow badalem, czy jest roznica w odpowiedzi na kazde pytanie dla osob będących (1) lub nie bedacych (0) w jakiejs z tych kategorii.
 
 Co do wykresow to pliku nazywaja się plots_XYZ.pdf i ponownie XYZ to to samo, co przy p.values. Na razie nie udalo mi sie wmusic ladnych tytułów, ale działa to tak, że odp. na 1 pytanie są na pierwszej stronie, odp. na drugie na drugiej itp. W przypadku plots_wiedza 1 strona to pytanie 11, czyli 1 z drugiej tabelki itd.

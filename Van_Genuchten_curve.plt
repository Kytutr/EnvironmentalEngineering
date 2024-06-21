    ### Poniższe parametry należy wyedytować w.g. posiadanych danych ###
    n = 1.27
    m = 1 - (1/n)
    alfa = 0.035
    theta_r = 0.045
    theta_s = 0.425

    #### Poniżej już nic nie edytujemy !!!! ####
    whmax = theta_r + (theta_s - theta_r)/(1+(alfa * 50200)**n)**m
    ptw = theta_r + (theta_s - theta_r)/(1+(alfa * 15900)**n)**m
    pkr = theta_r + (theta_s - theta_r)/(1+(alfa * 1590)**n)**m
    ppw = theta_r + (theta_s - theta_r)/(1+(alfa * 320)**n)**m

    # Obliczenia dla punktów pośrednich, dla bardziej realnego przebiegu wykresu
    theta45 = theta_r + (theta_s - theta_r)/(1+(alfa * 32000)**n)**m
    theta40 = theta_r + (theta_s - theta_r)/(1+(alfa * 10000)**n)**m
    theta30 = theta_r + (theta_s - theta_r)/(1+(alfa * 1000)**n)**m
    theta20 = theta_r + (theta_s - theta_r)/(1+(alfa * 100)**n)**m
    theta15 = theta_r + (theta_s - theta_r)/(1+(alfa * 32)**n)**m
    theta10 = theta_r + (theta_s - theta_r)/(1+(alfa * 10)**n)**m

    # Obliczenia dla punktów theta na wykresie
    whmax = real(sprintf("%.3f",whmax))
    ptw = real(sprintf("%.3f",ptw))
    pkr = real(sprintf("%.3f",pkr))
    ppw = real(sprintf("%.3f",ppw))

    #Tutaj przygotowujemy plik z danymi
    set print "./temp.csv"
    print("\"theta\",\"pF\"")
    print(sprintf("%.3f",theta_s) . ",0.0")
    print(sprintf("%.3f",theta10) . ",1.0")
    print(sprintf("%.3f",theta15) . ",1.5")
    print(sprintf("%.3f",theta20) . ",2.0")
    print(sprintf("%.3f",ppw) . ",2.5")
    print(sprintf("%.3f",theta30) . ",3.0")
    print(sprintf("%.3f",pkr) . ",3.2")
    print(sprintf("%.3f",theta40) . ",4.0")
    print(sprintf("%.3f",ptw) . ",4.2")
    print(sprintf("%.3f",theta45) . ",4.5")
    print(sprintf("%.3f",whmax) . ",4.7")

    # Ustalamy jak wyglądał będzie wykres
    set title "Krzywa charakterystiki wodnej (model van Genuchtena)"
    set key autotitle columnhead
    set xlabel '{/Symbol Q}'
    set ylabel 'pF'
    set xrange [0.0:0.45]
    set yrange [0:5]
    set datafile separator ','
    set grid nopolar
    set xtics 0.15
    set ytics 0.5
    set ytics add (2.5)
    set ytics add (3.2)
    set ytics add (4.2)
    set ytics add (4.7)
    set xtics add (whmax)
    set xtics add (ptw)
    set xtics add (pkr)
    set xtics add (ppw)
    set xtics add (theta_s)

    # Dodajemy dodatkowe linie na wysokości zadanych punktów pF i wartości theta
    set arrow from 0.0,4.7 to 0.45,4.7 nohead lw 1.5 lt "-"
    set arrow from 0.0,4.2 to 0.45,4.2 nohead lw 1.5 lt "-"
    set arrow from 0.0,3.2 to 0.45,3.2 nohead lw 1.5 lt "-"
    set arrow from 0.0,2.5 to 0.45,2.5 nohead lw 1.5 lt "-"
    set arrow from theta_s,0.0 to theta_s,5 nohead lw 1.5 lt "-"
    set arrow from ppw,0.0 to ppw,5 nohead lw 1.5 lt "-"
    set arrow from pkr,0.0 to pkr,5 nohead lw 1.5 lt "-"
    set arrow from ptw,0.0 to ptw,5 nohead lw 1.5 lt "-"
    set arrow from whmax,0.0 to whmax,5 nohead lw 1.5 lt "-"

    # Dodajemy etykiety
    offsecik = 0.0025
    set label "{/Symbol Q}_{whmax}" at whmax+ offsecik, 4.7 + offsecik front
    set label "{/Symbol Q}_{PTW}" at ptw+ offsecik, 4.2 + offsecik front
    set label "{/Symbol Q}_{PKR}" at pkr+ offsecik, 3.2 + offsecik front
    set label "{/Symbol Q}_{PPW}" at ppw+ offsecik, 2.5 + offsecik front
    set label "{/Symbol Q}_{S}" at theta_s+ offsecik, 0.0 + offsecik front

    # Rysujemy nasz wykres
    set encoding utf8
    plot "./temp.csv" using 1:2 with lines smooth mcsplines linewidth 2

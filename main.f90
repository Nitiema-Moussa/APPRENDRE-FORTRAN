! program thermique_magasin
!   implicit none
!   ! ------------------------------------------------------------
!   ! Modèle thermique simplifié (ambiance magasin)
!   ! ------------------------------------------------------------
!   integer :: n, nmax
!   real :: dt, alpha1, K, hc, hr
!   real :: Tmur, Ttoit, Tsol
!   real :: TA1n, TA1np1, TA2n, TA2np1
!   real :: a11, a12, Y1
!   real, dimension(100) :: TA1, TA2

!   ! --- Paramètres physiques réalistes pour ambiance magasin ---
!   dt     = 60.0          ! pas de temps = 60 s (1 minute)
!   alpha1 = dt / 6000.0   ! capacité thermique ~ 6000 J/K
!   K      = 0.5           ! conductance thermique (W/K)
!   hc     = 8.0           ! convection modérée dans un local (W/m².K)
!   hr     = 4.0           ! rayonnement modéré (W/m².K)
!   Tmur   = 28.0          ! température mur magasin (°C)
!   Ttoit  = 30.0          ! température toit magasin (°C)
!   Tsol   = 26.0          ! température sol magasin (°C)

!   ! --- Initialisation ---
!   nmax   = 50            ! nombre de pas de temps
!   TA1n   = 22.0          ! température initiale au nœud A1
!   TA2n   = 23.0          ! température initiale au nœud A2

!   ! --- Boucle temporelle ---
!   do n = 1, nmax
!      a11 = 1.0 + alpha1*(K + hc + 2.0*hr)
!      a12 = -alpha1*K
!      Y1  = TA1n + alpha1*(hc*Tmur + hr*Ttoit + hr*Tsol)

!      ! Évolution fictive de TA2 vers la température du mur
!      TA2np1 = TA2n + 0.05*(Tmur - TA2n)
!      TA1np1 = (Y1 - a12*TA2np1)/a11

!      TA1(n) = TA1np1
!      TA2(n) = TA2np1

!      TA1n   = TA1np1
!      TA2n   = TA2np1
!   end do

!   ! --- Écriture des données ---
!   open(unit=10, file="data_magasin.dat", status="replace")
!   do n = 1, nmax
!      write(10,*) n*dt/60.0, TA1(n), TA2(n)   ! temps en minutes
!   end do
!   close(10)

!   ! --- Script Gnuplot intégré ---
!   open(unit=20, file="plot_magasin.gp", status="replace")
!   write(20,*) "set terminal windows"
!   write(20,*) "set title 'Evolution de T_A1 et T_A2 (ambiance magasin)'"
!   write(20,*) "set xlabel 'Temps (minutes)'"
!   write(20,*) "set ylabel 'Température (°C)'"
!   write(20,*) "set yrange [20:35]"
!   write(20,*) "plot 'data_magasin.dat' using 1:2 with lines lw 2 lc rgb 'blue' title 'T_A1', " // &
!               "'data_magasin.dat' using 1:3 with lines lw 2 lc rgb 'red'  title 'T_A2'"
!   close(20)

!   ! --- Appel Gnuplot ---
!   call system("gnuplot -persist plot_magasin.gp")

! end program thermique_magasin




! ########################################################################################





program thermique_combustion
  implicit none
  ! ------------------------------------------------------------
  ! Modèle thermique simplifié (chambre de combustion biomasse)
  ! ------------------------------------------------------------
  ! n      : indice temporel
  ! nmax   : nombre de pas de temps
  ! dt     : pas de temps en secondes
  ! alpha1 : coefficient numérique = dt / C (C = capacité thermique)
  ! K      : conductance thermique entre A1 et A2 (W/K)
  ! hc     : coefficient de convection avec l'air chaud (W/m².K)
  ! hr     : coefficient de rayonnement (W/m².K)
  ! Tmur   : température mur de la chambre (°C)
  ! Ttoit  : température voûte/toit de la chambre (°C)
  ! Tsol   : température sole du four (°C)
  ! TA1n   : température au nœud A1 à l'instant n
  ! TA1np1 : température au nœud A1 à l'instant n+1
  ! TA2n   : température au nœud A2 à l'instant n
  ! TA2np1 : température au nœud A2 à l'instant n+1
  ! a11,a12: coefficients du système linéaire pour A1
  ! Y1     : terme source du système linéaire pour A1
  ! TA1,TA2: tableaux pour stocker l'évolution des températures
  ! ------------------------------------------------------------

  integer :: n, nmax
  real :: dt, alpha1, K, hc, hr
  real :: Tmur, Ttoit, Tsol
  real :: TA1n, TA1np1, TA2n, TA2np1
  real :: a11, a12, Y1
  real, dimension(100) :: TA1, TA2

  ! --- Paramètres physiques réalistes pour combustion ---
  dt     = 60.0          ! pas de temps = 60 s (1 minute)
  alpha1 = dt / 6000.0   ! capacité thermique ~ 6000 J/K
  K      = 0.5           ! conductance thermique (W/K)
  hc     = 15.0          ! convection forte dans la chambre (W/m².K)
  hr     = 6.0           ! rayonnement élevé (W/m².K)
  Tmur   = 350.0         ! mur de la chambre (°C)
  Ttoit  = 380.0         ! voûte/toit (°C)
  Tsol   = 280.0         ! sole du four (°C)

  ! --- Initialisation ---
  nmax = 120
  TA1n = 39.0
  TA2np1 = 23.0

  ! --- Boucle temporelle ---
  do n = 1, nmax
     a11 = 1.0 + alpha1*(K + hc + 2.0*hr)
     a12 = -alpha1*K
     Y1  = TA1n + alpha1*(hc*Tmur + hr*Ttoit + hr*Tsol)

     ! Évolution fictive de TA2 vers la température du mur
     TA2np1 = TA2n + 0.05*(Tmur - TA2n)
     TA1np1 = (Y1 - a12*TA2np1)/a11

     TA1(n) = TA1np1
     TA2(n) = TA2np1

     TA1n   = TA1np1
     TA2n   = TA2np1
  end do

  ! --- Écriture des données ---
  open(unit=10, file="data_combustion.dat", status="replace")
  do n = 1, nmax
     write(10,*) n*dt/60.0, TA1(n), TA2(n)   ! temps en minutes
  end do
  close(10)

  ! --- Script Gnuplot ---
  open(unit=20, file="plot_combustion.gp", status="replace")
  write(20,*) "set title 'Evolution de T_A1 et T_A2 (chambre de combustion)'"
  write(20,*) "set xlabel 'Temps (minutes)'"
  write(20,*) "set ylabel 'Température (°C)'"
  write(20,*) "plot 'data_combustion.dat' using 1:2 with lines lw 2 lc rgb 'blue' title 'T_A1', " // &
              "'data_combustion.dat' using 1:3 with lines lw 2 lc rgb 'red'  title 'T_A2'"
  close(20)

  ! --- Appel Gnuplot ---
  call system("gnuplot -persist plot_combustion.gp")

end program thermique_combustion





! ########################################################################################




! program thermique_magasin
!   implicit none
!   ! ------------------------------------------------------------
!   ! Modèle thermique simplifié (ambiance magasin)
!   ! ------------------------------------------------------------
!   ! n      : indice temporel
!   ! nmax   : nombre de pas de temps
!   ! dt     : pas de temps en secondes
!   ! alpha1 : coefficient numérique = dt / C (C = capacité thermique)
!   ! K      : conductance thermique entre A1 et A2 (W/K)
!   ! hc     : coefficient de convection avec l'air du magasin (W/m².K)
!   ! hr     : coefficient de rayonnement avec toit et sol (W/m².K)
!   ! Tmur   : température du mur du magasin (°C)
!   ! Ttoit  : température du toit du magasin (°C)
!   ! Tsol   : température du sol du magasin (°C)
!   ! TA1n   : température au nœud A1 à l'instant n
!   ! TA1np1 : température au nœud A1 à l'instant n+1
!   ! TA2n   : température au nœud A2 à l'instant n
!   ! TA2np1 : température au nœud A2 à l'instant n+1
!   ! a11,a12: coefficients du système linéaire pour A1
!   ! Y1     : terme source du système linéaire pour A1
!   ! TA1,TA2: tableaux pour stocker l'évolution des températures
!   ! ------------------------------------------------------------

!   integer :: n, nmax
!   real :: dt, alpha1, K, hc, hr
!   real :: Tmur, Ttoit, Tsol
!   real :: TA1n, TA1np1, TA2n, TA2np1
!   real :: a11, a12, Y1
!   real, dimension(100) :: TA1, TA2

!   ! --- Paramètres physiques réalistes pour ambiance magasin ---
!   dt     = 60.0          ! pas de temps = 60 s (1 minute)
!   alpha1 = dt / 6000.0   ! capacité thermique ~ 6000 J/K
!   K      = 0.5           ! conductance thermique (W/K)
!   hc     = 8.0           ! convection modérée dans un local (W/m².K)
!   hr     = 4.0           ! rayonnement modéré (W/m².K)
!   Tmur   = 28.0          ! température mur magasin (°C)
!   Ttoit  = 30.0          ! température toit magasin (°C)
!   Tsol   = 26.0          ! température sol magasin (°C)

!   ! --- Initialisation ---
!   nmax   = 50            ! nombre de pas de temps
!   TA1n   = 22.0          ! température initiale au nœud A1
!   TA2n   = 23.0          ! température initiale au nœud A2

!   ! --- Boucle temporelle ---
!   do n = 1, nmax
!      a11 = 1.0 + alpha1*(K + hc + 2.0*hr)
!      a12 = -alpha1*K
!      Y1  = TA1n + alpha1*(hc*Tmur + hr*Ttoit + hr*Tsol)

!      ! Évolution fictive de TA2 vers la température du mur
!      TA2np1 = TA2n + 0.05*(Tmur - TA2n)
!      TA1np1 = (Y1 - a12*TA2np1)/a11

!      TA1(n) = TA1np1
!      TA2(n) = TA2np1

!      TA1n   = TA1np1
!      TA2n   = TA2np1
!   end do

!   ! --- Écriture des données ---
!   open(unit=10, file="data_magasin.dat", status="replace")
!   do n = 1, nmax
!      write(10,*) n*dt/60.0, TA1(n), TA2(n)   ! temps en minutes
!   end do
!   close(10)

!   ! --- Script Gnuplot ---
!   open(unit=20, file="plot_magasin.gp", status="replace")
!   write(20,*) "set title 'Evolution de T_A1 et T_A2 (ambiance magasin)'"
!   write(20,*) "set xlabel 'Temps (minutes)'"
!   write(20,*) "set ylabel 'Température (°C)'"
!   write(20,*) "plot 'data_magasin.dat' using 1:2 with lines lw 2 lc rgb 'blue' title 'T_A1', " // &
!               "'data_magasin.dat' using 1:3 with lines lw 2 lc rgb 'red'  title 'T_A2'"
!   close(20)

!   ! --- Appel Gnuplot ---
!   call system("gnuplot -persist plot_magasin.gp")

! end program thermique_magasin




! ########################################################################################




! program thermique
!   implicit none
!   ! ------------------------------------------------------------
!   ! Modèle thermique simplifié (2 nœuds : A1 et A2)
!   ! ------------------------------------------------------------
!   ! n      : indice temporel
!   ! nmax   : nombre de pas de temps
!   ! dt     : pas de temps en secondes
!   ! alpha1 : coefficient numérique = dt / C (C = capacité thermique)
!   ! K      : conductance thermique entre A1 et A2 (W/K)
!   ! hc     : coefficient de convection avec l'air (W/m².K)
!   ! hr     : coefficient de rayonnement (W/m².K)
!   ! Tmur   : température du mur (°C)
!   ! Ttoit  : température du toit (°C)
!   ! Tsol   : température du sol (°C)
!   ! TA1n   : température au nœud A1 à l'instant n
!   ! TA1np1 : température au nœud A1 à l'instant n+1
!   ! TA2n   : température au nœud A2 à l'instant n
!   ! TA2np1 : température au nœud A2 à l'instant n+1
!   ! a11,a12: coefficients du système linéaire pour A1
!   ! Y1     : terme source du système linéaire pour A1
!   ! TA1,TA2: tableaux pour stocker l'évolution des températures
!   ! ------------------------------------------------------------

!   integer :: n, nmax
!   real :: dt, alpha1, K, hc, hr
!   real :: Tmur, Ttoit, Tsol
!   real :: TA1n, TA1np1, TA2n, TA2np1
!   real :: a11, a12, Y1
!   real, dimension(100) :: TA1, TA2

!   ! --- Paramètres physiques réalistes ---
!   dt     = 60.0          ! pas de temps = 60 s (1 minute)
!   alpha1 = dt / 6000.0   ! capacité thermique ~ 6000 J/K
!   K      = 0.5           ! conductance thermique (W/K)
!   hc     = 10.0          ! convection réaliste (W/m².K)
!   hr     = 5.0           ! rayonnement réaliste (W/m².K)
!   Tmur   = 25.0          ! température du mur (°C)
!   Ttoit  = 30.0          ! température du toit (°C)
!   Tsol   = 20.0          ! température du sol (°C)

!   ! --- Initialisation ---
!   nmax   = 50            ! nombre de pas de temps
!   TA1n   = 22.0          ! température initiale au nœud A1
!   TA2n   = 23.0          ! température initiale au nœud A2

!   ! --- Boucle temporelle ---
!   do n = 1, nmax
!      a11 = 1.0 + alpha1*(K + hc + 2.0*hr)
!      a12 = -alpha1*K
!      Y1  = TA1n + alpha1*(hc*Tmur + hr*Ttoit + hr*Tsol)

!      ! Résolution simple (ici TA2np1 évolue légèrement vers Tmur)
!      TA2np1 = TA2n + 0.05*(Tmur - TA2n)   ! évolution fictive de TA2
!      TA1np1 = (Y1 - a12*TA2np1)/a11

!      TA1(n) = TA1np1
!      TA2(n) = TA2np1

!      TA1n   = TA1np1
!      TA2n   = TA2np1
!   end do

!   ! --- Écriture des données ---
!   open(unit=10, file="data.dat", status="replace")
!   do n = 1, nmax
!      write(10,*) n*dt/60.0, TA1(n), TA2(n)   ! temps en minutes
!   end do
!   close(10)

!   ! --- Script Gnuplot ---
!   open(unit=20, file="plot.gp", status="replace")
!   write(20,*) "set title 'Evolution de T_A1 et T_A2'"
!   write(20,*) "set xlabel 'Temps (minutes)'"
!   write(20,*) "set ylabel 'Température (°C)'"
!   write(20,*) "plot 'data.dat' using 1:2 with lines lw 2 lc rgb 'blue' title 'T_A1', " // &
!               "'data.dat' using 1:3 with lines lw 2 lc rgb 'red'  title 'T_A2'"
!   close(20)

!   ! --- Appel Gnuplot ---
!   call system("gnuplot -persist plot.gp")

! end program thermique




! ########################################################################################




! ERREUR
! program thermique
!   implicit none
!   ! ------------------------------------------------------------
!   ! Modèle thermique simplifié (2 nœuds : A1 et A2)
!   ! ------------------------------------------------------------
!   ! n      : indice temporel
!   ! nmax   : nombre de pas de temps
!   ! dt     : pas de temps en secondes
!   ! alpha1 : coefficient numérique = dt / C (C = capacité thermique)
!   ! K      : conductance thermique entre A1 et A2 (W/K)
!   ! hc     : coefficient de convection avec l'air (W/m².K)
!   ! hr     : coefficient de rayonnement (W/m².K)
!   ! Tmur   : température du mur (°C)
!   ! Ttoit  : température du toit (°C)
!   ! Tsol   : température du sol (°C)
!   ! TA1n   : température au nœud A1 à l'instant n
!   ! TA1np1 : température au nœud A1 à l'instant n+1
!   ! TA2n   : température au nœud A2 à l'instant n
!   ! TA2np1 : température au nœud A2 à l'instant n+1
!   ! a11,a12: coefficients du système linéaire pour A1
!   ! Y1     : terme source du système linéaire pour A1
!   ! TA1,TA2: tableaux pour stocker l'évolution des températures
!   ! ------------------------------------------------------------

!   integer :: n, nmax
!   real :: dt, alpha1, K, hc, hr
!   real :: Tmur, Ttoit, Tsol
!   real :: TA1n, TA1np1, TA2n, TA2np1
!   real :: a11, a12, Y1
!   real, dimension(100) :: TA1, TA2

!   ! --- Paramètres physiques réalistes ---
!   dt     = 60.0          ! pas de temps = 60 s (1 minute)
!   alpha1 = dt / 6000.0   ! capacité thermique ~ 6000 J/K
!   K      = 0.5           ! conductance thermique (W/K)
!   hc     = 10.0          ! convection réaliste (W/m².K)
!   hr     = 5.0           ! rayonnement réaliste (W/m².K)
!   Tmur   = 25.0          ! température du mur (°C)
!   Ttoit  = 30.0          ! température du toit (°C)
!   Tsol   = 20.0          ! température du sol (°C)

!   ! --- Initialisation ---
!   nmax   = 50            ! nombre de pas de temps
!   TA1n   = 22.0          ! température initiale au nœud A1
!   TA2n   = 23.0          ! température initiale au nœud A2

!   ! --- Boucle temporelle ---
!   do n = 1, nmax
!      a11 = 1.0 + alpha1*(K + hc + 2.0*hr)
!      a12 = -alpha1*K
!      Y1  = TA1n + alpha1*(hc*Tmur + hr*Ttoit + hr*Tsol)

!      ! Résolution simple (ici TA2np1 évolue légèrement vers Tmur)
!      TA2np1 = TA2n + 0.05*(Tmur - TA2n)   ! évolution fictive de TA2
!      TA1np1 = (Y1 - a12*TA2np1)/a11

!      TA1(n) = TA1np1
!      TA2(n) = TA2np1

!      TA1n   = TA1np1
!      TA2n   = TA2np1
!   end do

!   ! --- Écriture des données ---
!   open(unit=10, file="data.dat", status="replace")
!   do n = 1, nmax
!      write(10,*) n*dt/60.0, TA1(n), TA2(n)   ! temps en minutes
!   end do
!   close(10)

!   ! --- Script Gnuplot ---
!   open(unit=20, file="plot.gp", status="replace")
!   write(20,*) "set title 'Evolution de T_A1 et T_A2'"
!   write(20,*) "set xlabel 'Temps (minutes)'"
!   write(20,*) "set ylabel 'Température (°C)'"
!   write(20,*) "plot 'data.dat' using 1:2 with lines lw 2 lc rgb 'blue' title 'T_A1', \ "
!   write(20,*) "     'data.dat' using 1:3 with lines lw 2 lc rgb 'red'  title 'T_A2'"
!   close(20)

!   ! --- Appel Gnuplot ---
!   call system("gnuplot -persist plot.gp")

! end program thermique




! ########################################################################################




! Courbe TA1
! program thermique
!   implicit none
!   ! ------------------------------------------------------------
!   ! Modèle thermique simplifié (2 nœuds : A1 et A2)
!   ! ------------------------------------------------------------
!   ! n      : indice temporel
!   ! nmax   : nombre de pas de temps
!   ! dt     : pas de temps en secondes
!   ! alpha1 : coefficient numérique = dt / C (C = capacité thermique)
!   ! K      : conductance thermique entre A1 et A2 (W/K)
!   ! hc     : coefficient de convection avec l'air (W/m².K)
!   ! hr     : coefficient de rayonnement (W/m².K)
!   ! Tmur   : température du mur (°C)
!   ! Ttoit  : température du toit (°C)
!   ! Tsol   : température du sol (°C)
!   ! TA1n   : température au nœud A1 à l'instant n
!   ! TA1np1 : température au nœud A1 à l'instant n+1
!   ! TA2np1 : température au nœud A2 à l'instant n+1 (ici supposée connue)
!   ! a11,a12: coefficients du système linéaire
!   ! Y1     : terme source du système linéaire
!   ! TA1    : tableau pour stocker l'évolution de T_A1
!   ! ------------------------------------------------------------

!   integer :: n, nmax
!   real :: dt, alpha1, K, hc, hr
!   real :: Tmur, Ttoit, Tsol
!   real :: TA1n, TA1np1, TA2np1
!   real :: a11, a12, Y1
!   real, dimension(100) :: TA1

!   ! --- Paramètres physiques réalistes ---
!   dt     = 60.0          ! pas de temps = 60 s (1 minute)
!   alpha1 = dt / 7800.0   ! capacité thermique ~ 6000 J/K
!   K      = 0.5           ! conductance thermique (W/K)
!   hc     = 13.5          ! convection réaliste (W/m².K)
!   hr     = 5.36           ! rayonnement réaliste (W/m².K)
!   Tmur   = 38.0          ! température du mur (°C)
!   Ttoit  = 38.0          ! température du toit (°C)
!   Tsol   = 40.0          ! température du sol (°C)

!   ! --- Initialisation ---
!   nmax   = 60            ! nombre de pas de temps
!   TA1n   = 45.0          ! température initiale au nœud A1
!   TA2np1 = 38.0          ! température supposée connue au nœud A2

!   ! --- Boucle temporelle ---
!   do n = 1, nmax
!      a11 = 1.0 + alpha1*(K + hc + 2.0*hr)
!      a12 = -alpha1*K
!      Y1  = TA1n + alpha1*(hc*Tmur + hr*Ttoit + hr*Tsol)

!      ! Résolution simple (TA2np1 supposé connu)
!      TA1np1 = (Y1 - a12*TA2np1)/a11

!      TA1(n) = TA1np1
!      TA1n   = TA1np1
!   end do

!   ! --- Écriture des données ---
!   open(unit=10, file="data.dat", status="replace")
!   do n = 1, nmax
!      write(10,*) n*dt/60.0, TA1(n)   ! temps en minutes
!   end do
!   close(10)

!   ! --- Script Gnuplot ---
!   open(unit=20, file="plot.gp", status="replace")
!   write(20,*) "set title 'Evolution de T_A1'"
!   write(20,*) "set xlabel 'Temps (minutes)'"
!   write(20,*) "set ylabel 'Température (°C)'"
!   write(20,*) "plot 'data.dat' with lines lw 2 lc rgb 'blue' title 'T_A1'"
!   close(20)

!   ! --- Appel Gnuplot ---
!   call system("gnuplot -persist plot.gp")

! end program thermique






! ########################################################################################



! TOUS PREMIER PROGRAM
! !!!!!!!!!!!!!!!!!!!!!!!!!!
! program thermique
!   implicit none
!   !   n : indice temporel
!   !   nmax : nombre de pas de temps
!   !   alpha1 : diffusivité thermique
!   !   K : conductivité thermique
!   !   hc : coefficient de convection avec l'air
!   !   hr : coefficient de radiation
!   !   Tmur : température de la muraille
!   !   Ttoit : température du toit
!   !   Tsol : température du sol
!   !   TA1n : température à l'instant n
!   !   TA1np1 : température à l'instant n+1
!   !   TA2np1 : température de l'autre point à l'instant n+1 (supposée connue)
!   !   a11, a12 : coefficients du système linéaire
!   !   Y1 : terme source du système linéaire
!   integer :: n, nmax
!   real :: alpha1, K, hc, hr
!   real :: Tmur, Ttoit, Tsol
!   real :: TA1n, TA1np1, TA2np1
!   real :: a11, a12, Y1
!   real, dimension(100) :: TA1

!   ! --- Paramètres physiques ---
!   alpha1 = 0.01
!   K      = 0.5
!   hc     = 0.2
!   hr     = 0.1
!   Tmur   = 25.0
!   Ttoit  = 30.0
!   Tsol   = 20.0

!   ! --- Initialisation ---
!   nmax = 50
!   TA1n = 22.0
!   TA2np1 = 23.0

!   ! --- Boucle temporelle ---
!   do n = 1, nmax
!      a11 = 1.0 + alpha1*(K + hc + 2.0*hr)
!      a12 = -alpha1*K
!      Y1  = TA1n + alpha1*(hc*Tmur + hr*Ttoit + hr*Tsol)

!      ! Résolution simple (ici 2 inconnues → on suppose TA2np1 connu)
!      TA1np1 = (Y1 - a12*TA2np1)/a11

!      TA1(n) = TA1np1
!      TA1n   = TA1np1
!   end do

!   ! --- Écriture des données ---
!   open(unit=10, file="data.dat", status="replace")
!   do n = 1, nmax
!      write(10,*) n, TA1(n)
!   end do
!   close(10)

!   ! --- Script Gnuplot ---
!   open(unit=20, file="plot.gp", status="replace")
!   write(20,*) "set title 'Evolution de T_A1'"
!   write(20,*) "set xlabel 'Temps (pas)'"
!   write(20,*) "set ylabel 'Température (°C)'"
!   write(20,*) "plot 'data.dat' with lines lw 2 lc rgb 'blue' title 'T_A1'"
!   close(20)

!   ! --- Appel Gnuplot ---
!   call system("gnuplot -persist plot.gp")

! end program thermique

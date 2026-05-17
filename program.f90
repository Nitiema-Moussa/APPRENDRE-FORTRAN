
program thermique_magasin
  implicit none
  ! ------------------------------------------------------------
  ! Modèle thermique simplifié (ambiance magasin)
  ! ------------------------------------------------------------
  ! n      : indice temporel
  ! nmax   : nombre de pas de temps
  ! dt     : pas de temps en secondes
  ! alpha1 : coefficient numérique = dt / C (C = capacité thermique)
  ! K      : conductance thermique entre A1 et A2 (W/K)
  ! hc     : coefficient de convection avec l'air du magasin (W/m².K)
  ! hr     : coefficient de rayonnement avec toit et sol (W/m².K)
  ! Tmur   : température du mur du magasin (°C)
  ! Ttoit  : température du toit du magasin (°C)
  ! Tsol   : température du sol du magasin (°C)
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

  ! --- Paramètres physiques réalistes pour ambiance magasin ---
  dt     = 60.0          ! pas de temps = 60 s (1 minute)
  alpha1 = dt / 6000.0   ! capacité thermique ~ 6000 J/K
  K      = 0.5           ! conductance thermique (W/K)
  hc     = 8.0           ! convection modérée dans un local (W/m².K)
  hr     = 4.0           ! rayonnement modéré (W/m².K)
  Tmur   = 28.0          ! température mur magasin (°C)
  Ttoit  = 30.0          ! température toit magasin (°C)
  Tsol   = 26.0          ! température sol magasin (°C)

  ! --- Initialisation ---
  nmax   = 50            ! nombre de pas de temps
  TA1n   = 22.0          ! température initiale au nœud A1
  TA2n   = 23.0          ! température initiale au nœud A2

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
  open(unit=10, file="data_magasin.dat", status="replace")
  do n = 1, nmax
     write(10,*) n*dt/60.0, TA1(n), TA2(n)   ! temps en minutes
  end do
  close(10)

  ! --- Script Gnuplot ---
  open(unit=20, file="plot_magasin.gp", status="replace")
  write(20,*) "set title 'Evolution de T_A1 et T_A2 (ambiance magasin)'"
  write(20,*) "set xlabel 'Temps (minutes)'"
  write(20,*) "set ylabel 'Température (°C)'"
  write(20,*) "plot 'data_magasin.dat' using 1:2 with lines lw 2 lc rgb 'blue' title 'T_A1', " // &
              "'data_magasin.dat' using 1:3 with lines lw 2 lc rgb 'red'  title 'T_A2'"
  close(20)

  ! --- Appel Gnuplot ---
  call system("gnuplot -persist plot_magasin.gp")

end program thermique_magasin
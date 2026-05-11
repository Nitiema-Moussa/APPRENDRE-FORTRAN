! program main
! 	implicit none
!     print *, "Hello World"
! end program main

program thermique
  implicit none
!   n : indice temporel
!   nmax : nombre de pas de temps
!   alpha1 : diffusivité thermique
!   K : conductivité thermique
!   hc : coefficient de convection avec l'air
!   hr : coefficient de radiation
!   Tmur : température de la muraille
!   Ttoit : température du toit
!   Tsol : température du sol
!   TA1n : température à l'instant n
!   TA1np1 : température à l'instant n+1
!   TA2np1 : température de l'autre point à l'instant n+1 (supposée connue)
!   a11, a12 : coefficients du système linéaire
!   Y1 : terme source du système linéaire
  integer :: n, nmax
  real :: alpha1, K, hc, hr
  real :: Tmur, Ttoit, Tsol
  real :: TA1n, TA1np1, TA2np1
  real :: a11, a12, Y1
  real, dimension(100) :: TA1

  ! --- Paramètres physiques ---
  alpha1 = 0.01
  K      = 0.5
  hc     = 0.2
  hr     = 0.1
  Tmur   = 25.0
  Ttoit  = 30.0
  Tsol   = 20.0

  ! --- Initialisation ---
  nmax = 50
  TA1n = 22.0
  TA2np1 = 23.0

  ! --- Boucle temporelle ---
  do n = 1, nmax
     a11 = 1.0 + alpha1*(K + hc + 2.0*hr)
     a12 = -alpha1*K
     Y1  = TA1n + alpha1*(hc*Tmur + hr*Ttoit + hr*Tsol)

     ! Résolution simple (ici 2 inconnues → on suppose TA2np1 connu)
     TA1np1 = (Y1 - a12*TA2np1)/a11

     TA1(n) = TA1np1
     TA1n   = TA1np1
  end do

  ! --- Écriture des données ---
  open(unit=10, file="data.dat", status="replace")
  do n = 1, nmax
     write(10,*) n, TA1(n)
  end do
  close(10)

  ! --- Script Gnuplot ---
  open(unit=20, file="plot.gp", status="replace")
  write(20,*) "set title 'Evolution de T_A1'"
  write(20,*) "set xlabel 'Temps (pas)'"
  write(20,*) "set ylabel 'Température (°C)'"
  write(20,*) "plot 'data.dat' with lines lw 2 lc rgb 'blue' title 'T_A1'"
  close(20)

  ! --- Appel Gnuplot ---
  call system("gnuplot -persist plot.gp")

end program thermique

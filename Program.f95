PROGRAM geoprog
!================================================== 
! Selve programmet
!
! ENDRET(dd.mm.ее): 23.02.07
!================================================== 
USE constants
IMPLICIT NONE
REAL(nok)   temp1, temp2,lambda,phi
!-------------------------------------------------

WRITE(*,'(A/)') "GeoComp v.0.1"
WRITE(*,'(A)') " ____                   ____                                  "
WRITE(*,'(A)') "/\  _`\                /\  _`\                                "
WRITE(*,'(A)') "\ \ \\_\      __    ___\ \ \/\_\    ___     ___ ___   _____   "
WRITE(*,'(A)') " \ \ \ ___  /'__`\ / __`\ \ \/_/_  / __`\ /' __` __`\/\ '__`\ "
WRITE(*,'(A)') "  \ \ \/, \/\  __//\ \_\ \ \ \_\ \/\ \_\ \/\ \/\ \/\ \ \ \_\ \"
WRITE(*,'(A)') "   \ \____/\ \____\ \____/\ \____/\ \____/\ \_\ \_\ \_\ \ ,__/"
WRITE(*,'(A)') "    \/___/  \/____/\/___/  \/___/  \/___/  \/_/\/_/\/_/\ \ \/ "
WRITE(*,'(A)') "                                                        \ \_\ "
WRITE(*,'(A//)')"    GEOIDAL COMPUTATIONS, A PROGRAM BY SULENG & SVEEN    \/_/ "

!CALL compute_geoid("GGM")
!CALL compute_geoid("EGM")

!lambda = 31.0343967225261
!phi = 70.3336838070937

!CALL compute_geoid_sp(lambda,phi,"GGM",temp1)
!CALL compute_geoid_sp(lambda,phi,"EGM",temp2)
!
!WRITE(*,'(F10.4)') temp1
!WRITE(*,'(F10.4)') temp2

CALL compute_mix()
CALL compare()

WRITE(*,'(A)') "Program has completed, please press Enter to exit"!
READ(*,*)

END PROGRAM geoprog
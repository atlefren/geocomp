SUBROUTINE compute_geoid_sp(lambda,phi,model,N_grav_sp)
!================================================== 
! Subrutine som beregner geoideh�yde for et gitt punkt
! definert av phi og lambda 
! 
! St�tter beregning av geoideh�yde basert p� to
! Global Gravity Models, GGM02S og EGM96
!  
! Input: 
!   - lambda    (vinkel fra fil)
!   - phi       (vinkel fra fil)
!   - model (enten GGM eller EGM)
!
! Output: 
!   - N_grav_sp
!
! Avhengigheter: 
!    Subrutiner: matrise, loop
!
! ENDRET(dd.mm.��): 23.02.07
!================================================== 
USE constants
IMPLICIT NONE
REAL(nok),      INTENT(IN)  :: lambda,phi
CHARACTER(3),   INTENT(IN)  :: model
REAL(nok),      INTENT(OUT) :: N_grav_sp
!interne variable
REAL(nok)                   :: N_temp
!-------------------------------------------------

CALL matrise(phi,model)
CALL loop(model,lambda,N_temp)
N_grav_sp = N_temp*(GM/(R*g))

RETURN
END SUBROUTINE compute_geoid_sp
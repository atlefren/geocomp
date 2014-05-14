SUBROUTINE inner1(n,m,J_nm,k_nm,lambda,inner1_prod)
!================================================== 
! Subrutine som kjører den innerste loopen i (1)
! 
! Input: 
!   - n, m          (tellere)
!   - J_nm, k_nm    (variable i beregningen)    
!   - lambda        (lengdegrad)
! 
! Output: 
!   - inner1_prod   (summen av den indre loopen)
! 
! Avhengigheter: 
!   - Funksjonene: R_nm() og rad
!   - Matrisa P_nm (definert i modulen constants)  
!
! ENDRET(dd.mm.åå): 21.02.07
!==================================================
USE constants
IMPLICIT NONE
INTEGER,     INTENT(IN)         :: n,m
REAL(nok),   INTENT(IN)         :: J_nm,K_nm
REAL(nok),   INTENT(IN)         :: lambda
REAL(nok),   INTENT(OUT)        :: inner1_prod
!interne variable
REAL(nok)                       :: R_nm
REAL(nok)                       :: rad
!-------------------------------------------------

inner1_prod = (R_nm(n,m,J_nm)*cos(m*rad(lambda))+k_nm*sin(m*rad(lambda)))*P_nm(n,m)

RETURN
END SUBROUTINE inner1

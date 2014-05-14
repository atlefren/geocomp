

REAL(nok) FUNCTION J_n_N(n)   
!================================================== 
! Funksjon som beregner Jn^(N)  (likn 4) 
! 
! Input: 
!   - n (integer fra det ytre summetegnet i likn 1) 
! 
! Output: Jn^(N)  
! 
! ENDRET(dd.mm.åå): 21.02.07
!==================================================  
USE constants 
IMPLICIT NONE 
INTEGER, INTENT(IN) :: n 
!interne variable 
REAL(nok) :: a1,b1,c1,d1 
!-------------------------------------------------
 
IF ( MOD(n,2) == 0 ) THEN ! n is even 
    a1 = (-1)**(n/2) 
    b1 = 3*((e2**0.5)**n) 
    c1 = (1-(n/2)+((5*J_2)/(2*e2))*n) 
    d1 = (n+1)*(n+3)*((2.*n)+1.)**0.5 
    J_n_N = a1*((b1*c1)/d1) 
ELSEIF ( MOD(n,2) /= 0 ) THEN ! n is odd 
    J_n_N = 0. 
ENDIF 
 
RETURN  
END FUNCTION J_n_N



REAL(nok) FUNCTION R_nm(n,m,J_nm)
!==================================================
! Funksjon som beregner R_nm  (likn 2) 
! 
! Input: 
!   - n     (integer fra det ytre summetegnet i (1))
!   - m     (integer fra det indre summetegnet i (1))
!   - J_nm  (lest fra fil) 
! 
! Output:
!   - R_nm  (Beregnet på grunnlag av (2))
! 
! ENDRET(dd.mm.åå): 21.02.07
!==================================================
USE constants
IMPLICIT NONE
REAL(nok)   ,   INTENT(IN)  ::J_nm
INTEGER     ,   INTENT(IN)  ::n,m
!interne variable
REAL(nok)                   ::J_n_N
!-------------------------------------------------

IF(m==0)THEN
    R_nm = J_nm-J_n_N(n)
ELSE
    R_nm = J_nm
ENDIF

RETURN
END FUNCTION R_nm

REAL FUNCTION geometric(H_orto,h_ell)
!================================================== 
! Funksjon som beregner geoidehøyde basert på (8)
! 
! Input:  
!   - H_orto    (input fra fil)
!   - h_ell     (input fra fil)
! Output: 
!   - geometric (beregnet geoidehøyde)
!
! ENDRET(dd.mm.åå): 26.02.07
!================================================== 
USE constants
IMPLICIT none
REAL, INTENT(IN)    ::H_orto,h_ell
!-------------------------------------------------

geometric = h_ell - H_orto

RETURN
END FUNCTION



REAL(nok) FUNCTION rad(grad)
!================================================== 
! Funksjon som konverterer fra grader til radianer
! 
! Input: 
!   - grad  (vinkel i grader)
! 
! Output: 
!   - rad   (vinkel i radianer) 
! 
! ENDRET(dd.mm.åå): 23.02.07
!================================================== 
USE constants
IMPLICIT NONE
REAL(nok),INTENT(in)   ::grad
!-------------------------------------------------

rad=(grad*(pi/180.0))

RETURN
END FUNCTION

 


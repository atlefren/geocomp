SUBROUTINE matrise(phi,model)
!================================================== 
! Subrutine som beregner matrisa P_nm på grunnlag av (6)
! avhengig av hvilken modell som brukes
!
! Input: 
!   - phi   (lengdegrad)
!   - model (navnet på modellen vi skal bruke GGM eller EGM)
!
! Output: 
!   ingen (P_nm endres)
!
! Avhengigheter:
!   Matrisa P_nm (definert i Modulen constants)
!
! Kjente "feil":
!   Matrisa P_nm er 360*360 uansett hvilken 
!   modell som brukes, dette gjør at vi overforbruker plass.
!
! 
! ENDRET(dd.mm.åå): 23.02.07
!================================================== 
USE constants
IMPLICIT NONE
REAL(nok),      INTENT(IN)  :: phi
CHARACTER(3),   INTENT(IN)  :: model
!interne variable
INTEGER                     :: i,j,m,n,n_max
REAL(nok)                   :: t,rad
!-------------------------------------------------

!definerer t
t = sin(rad(phi))

IF(model == "GGM") THEN
    n_max = 160
ELSEIF(model == "EGM") THEN
    n_max = 360
ENDIF

!setter alle elementer i matrisa lik 0
DO i = 0,n_max
    DO j = 0,n_max
    P_nm(i,j) = 0.0
    ENDDO
ENDDO

!initialverdier (gitt i (7))
P_nm(0,0) = 1.0
P_nm(1,0) = t*(3.0**0.5)
P_nm(1,1) = (3.0**0.5)*(1.0-t**2)**0.5
P_nm(2,0) = (5.0**0.5)*(((3.0/2.0)*t**2)-(1.0/2.0))
P_nm(2,1) = t*(15.0**0.5)*(1.0-t**2)**0.5

!fyller matrisa på grunnlag av (6)
DO m=0,n_max
    DO n = 2,n_max
        
        IF(m==0) THEN
             P_nm(n,m) = - (((2.0*n+1)**0.5)/(n))*((n-1.0)/((2.0*n-3)**0.5))*P_nm((n-2),m)&
             + t*(((2.0*n+1)**0.5)/n)*((2.0*n-1)**0.5)*P_nm((n-1),m)
        ELSEIF (n>=3 .AND. ( m >= 1 .AND. m <=(n-2))) THEN
        
            P_nm(n,m) = -((((2.0*n+1)*(n+m-1.0)*(n-m-1.0))/((2.0*n-3.0)*(n+m)*(n-m)))**0.5)&
            *P_nm((n-2),m)+t*((((2.0*n+1)*(2.0*n-1))/((n+m)*(n-m)))**0.5)*P_nm((n-1),m)
        
        ELSEIF(n>=1 .AND. m == (n-1)) THEN
        
            P_nm(n,m) = t*((2.0*n+1)**0.5)*P_nm((n-1),(n-1))
        
        ELSEIF(n>=2 .AND. m==n) THEN
        
            P_nm(n,m) = (((2.0*n+1)/(2.0*n))**0.5)*((1-t**2)**0.5)*P_nm((n-1),(n-1))
        
        ENDIF
            
    ENDDO
ENDDO    

END SUBROUTINE matrise
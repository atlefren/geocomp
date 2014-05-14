SUBROUTINE loop(model,lambda,N_temp)
!================================================== 
! Subrutine som kjører den ytterste loopen i (1),
! dvs leser inn J_nm og K_nm fra fil basert på modell 
! som brukes og kaller inner1 for beregning.
!  
! Input: 
!   - model     (navnet på modell som brukes,GMM eller EGM)
!   - lambda    (lengdegrad)
! 
! Output: 
!  - N_temp     (resultatet av kjøring av begge løkker i (1))
! 
! Avhengigheter:
!   Subrutinen inner1
!
! ENDRET(dd.mm.åå): 21.02.07
!================================================== 
USE constants
IMPLICIT NONE
REAL(nok),   INTENT(IN)     :: lambda
CHARACTER(3),INTENT(IN)     :: model
REAL(nok),   INTENT(OUT)    :: N_temp
!interne variable
CHARACTER(88)               :: header !innholdet i linjene i headeren
INTEGER                     :: n,m,head,n2,m2,n_max
REAL(nok)                   :: J_nm,k_nm,inner1_prod,sum_u,sum_i
!-------------------------------------------------

sum_i = 0.
sum_u = 0.

!åpner filer for lesing og skriving avhengig av modell som brukes
IF (model == "GGM") THEN
    n_max = 160
    OPEN(UNIT=10, FILE='GGM02S_Model_TKT4185.dat', STATUS='old') 
ELSEIF (model == "EGM") THEN
    n_max = 360
    OPEN(UNIT=10, FILE='EGM96_Model_TKT4185.dat', STATUS='old')
ENDIF

!Hopper over headeren
head = 1 !i begynnelsen er vi i headeren    
DO WHILE(head==1)   
    IF (head == 1) THEN ! er vi fortsatt i headeren 
        READ(10,*) header 
        IF (header == "end_of_head") THEN ! veit vi at på neste linje begynner infoen vi skal ha 
            head = 0 !markerer at vi er ute av headeren 
        ENDIF
    ENDIF
END DO

! nå begynner vi å lese informasjonen vi vil ha
DO n=2,n_max
sum_i = 0.
    DO m=0,n
        IF(model == "GGM") THEN    
            READ(10,'(i5,i4,ES21.13,ES21.13)') n2,m2,J_nm,k_nm
        ELSEIF(model == "EGM") THEN 
            READ(10,'(i4,i4,E20.12,E20.12)') n2,m2,J_nm,k_nm
        ENDIF
        ! for hver linje i fila kaller vi inner1()
        CALL inner1(n,m,J_nm,k_nm,lambda,inner1_prod)
        sum_i = sum_i+inner1_prod
    END DO
    sum_u=sum_u+sum_i*(a/R)**n
END DO
N_temp=sum_u
CLOSE (UNIT=10) !lukker fila vi leser fra

RETURN
END SUBROUTINE loop

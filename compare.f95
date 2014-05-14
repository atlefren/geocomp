SUBROUTINE compare
!================================================== 
! Subrutine som beregner minimum,maximum, standardavvik 
! og gjennomsnitt av differansene mellom geoidehøyder beregnet 
! med (1) (ggm og egm) og (8) og skriver dette til fil
!  
!   Input:  Ingen
!   Output: Ingen
!
! ENDRET(dd.mm.åå): 26.02.07
!================================================== 
USE constants
IMPLICIT NONE
CHARACTER(88)       :: header        !innholdet i linjene i headeren
INTEGER             :: teller,head   ! variable til loopen
REAL(nok)           :: lambda,phi
REAL                :: N_geo,N_ggm,N_egm,max_ggm,min_ggm,max_egm,min_egm,mean_diff_ggm,mean_diff_egm,sd_ggm,sd_egm
REAL,DIMENSION(40)  :: diff_ggm, diff_egm
!-------------------------------------------------

WRITE(*,'(A)') "Comparing the geoidal heights for 40 given points computed with methods"
WRITE(*,'(A)') "N_geo,N_grav(GGM),N_grav(EGM)"

OPEN(UNIT=10, FILE='Geoid_height_computed.dat', STATUS='old')

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

max_ggm = 0.
min_ggm = 100.
max_egm = 0.
min_egm = 100.

DO teller=1,40
    READ(10,'(F19.15,F18.13,3F10.4)') lambda,phi,N_geo,N_ggm,N_egm
    
    mean_diff_ggm = mean_diff_ggm + (N_geo-N_ggm)**2
    mean_diff_egm = mean_diff_egm + (N_geo-N_egm)**2
    diff_ggm(teller) = (N_geo-N_ggm)**2
    diff_egm(teller) = (N_geo-N_egm)**2
    
    IF(diff_ggm(teller) > max_ggm) THEN
     max_ggm = (N_geo-N_ggm)**2
    ENDIF
    IF(diff_egm(teller) > max_egm) THEN
     max_egm = (N_geo-N_egm)**2
    ENDIF     
    IF(diff_ggm(teller) < min_ggm) THEN
     min_ggm = (N_geo-N_ggm)**2
    ENDIF
    IF(diff_egm(teller) < min_egm) THEN
     min_egm = (N_geo-N_egm)**2
    ENDIF      
    
ENDDO
CLOSE(UNIT=10)

mean_diff_ggm = ((mean_diff_ggm)**0.5)/40
mean_diff_egm = ((mean_diff_egm)**0.5)/40

DO teller=1,40 
    sd_ggm = sd_ggm + ((diff_ggm(teller))**0.5 - mean_diff_ggm)**2
    sd_egm = sd_egm + ((diff_egm(teller))**0.5 - mean_diff_egm)**2
ENDDO

OPEN(UNIT=11, FILE='Geoid_height_compared.dat', STATUS='unknown')

WRITE(*,'(A)') "Values compared,results written to file 'Geoid_height_compared.dat'"

WRITE(11,'(A)') "Comparison N_geometric vs. N_gravimetric(GGM)"
WRITE(11,'(A11,F10.4)') "Max diff:", (max_ggm)**0.5
WRITE(11,'(A11,F10.4)') "Min diff:", (min_ggm)**0.5
WRITE(11,'(A11,F10.4)') "Mean diff:", mean_diff_ggm
WRITE(11,'(A11,F10.4)') "SD:", sd_ggm/40
WRITE(11,*)
WRITE(11,'(A)') "Comparison N_geometric vs. N_gravimetric(EGM)"
WRITE(11,'(A11,F10.4)') "Max diff:", (max_egm)**0.5
WRITE(11,'(A11,F10.4)') "Min diff:", (min_egm)**0.5
WRITE(11,'(A11,F10.4)') "Mean diff:", mean_diff_egm
WRITE(11,'(A11,F10.4)') "SD:", sd_egm/40

CLOSE(UNIT=11)
WRITE(*,*)
WRITE(*,*)

END SUBROUTINE compare
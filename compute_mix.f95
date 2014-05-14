SUBROUTINE compute_mix
!================================================== 
! Subrutine som beregner geoidehøyde for 40 gitte punkter
! vha 3 metoder - N_geometric       (funksjonen geometric)
!               - N_Gravitetric(GGM)(subrutinen compute_geoid_sp) 
!               - N_Gravimetric(EGM)(subrutinen compute_geoid_sp)
! Basert på punktene oppgitt i fila 'GPS-Levelling_Stations_TKT4185.dat'
!
! Input:  
!   - ingen
!
! Output: 
!   - ingen
!
! Avhengigheter:
!   - Subrutinen Compute_geoid_sp()
!   - funksjonen geometric() 
! 
! ENDRET(dd.mm.åå): 26.02.07
!================================================== 
USE constants
IMPLICIT NONE
CHARACTER(88)   :: header        !innholdet i linjene i headeren
INTEGER         :: teller,head   ! variable til loopen
REAL(nok)       :: lambda,phi,N_ggm,N_egm,time1,time2
REAL            :: H_orto,h_ell,geometric
!-------------------------------------------------

!gps-leveling 
OPEN(UNIT=12, FILE='GPS-Levelling_Stations_TKT4185.dat', STATUS='old')
OPEN(UNIT=11, FILE='Geoid_height_computed.dat', STATUS='unknown')

WRITE(*,'(A)') "Computing geoidal heights for 40 given points with methods"
WRITE(*,'(A)') "N_geo,N_grav(GGM),N_grav(EGM)"
WRITE(*,'(A)') "    This may take a while, please wait..."

call cpu_time(time1)

WRITE(11,'(A)')"Longitude(degree)  Latitude(degree)    N_geometric  N_Grav(GGM) N_Grav(EGM)"
WRITE(11,'(A)')"end_of_head ============================================================================="
!Hopper over headeren
head = 1 !i begynnelsen er vi i headeren    
DO WHILE(head==1)   
    IF (head == 1) THEN ! er vi fortsatt i headeren 
        READ(12,*) header 
        IF (header == "end_of_head") THEN ! veit vi at på neste linje begynner infoen vi skal ha 
            head = 0 !markerer at vi er ute av headeren 
        ENDIF
    ENDIF
END DO

DO teller=1,40
    READ(12,'(F17.15,F18.13,F24.4,F24.3)') lambda,phi,H_orto,h_ell
    CALL compute_geoid_sp(lambda,phi,"GGM",N_ggm)
    CALL compute_geoid_sp(lambda,phi,"EGM",N_egm)
    WRITE(11,'(F19.15,F18.13,3F10.4)') lambda,phi,geometric(H_orto,h_ell),N_ggm,N_egm
ENDDO

CLOSE (UNIT=12) !lukker fila vi leser fra
CLOSE (UNIT=11) !lukker fila vi skriver til
call cpu_time(time2)

WRITE(*,'(A)') "Geoidal heights computed, saved to file 'Geoid_height_computed.dat'"
WRITE(*,'(A,I5)') "    Points calculated: ", teller
WRITE(*,'(A,F13.2,A)') "    Time elapsed: ", time2-time1, " s"

write(*,*)
write(*,*)

END SUBROUTINE compute_mix
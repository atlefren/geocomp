SUBROUTINE compute_geoid(model)
!================================================== 
! Subrutine som beregner geoidehøyde for gitte phi 
! og lambda og skriver disse til fil.
! 
! Støtter beregning av geoidehøyde basert på to
! Global Gravity Models, GGM02S og EGM96
!
! Filnavnet geoidehøyden får er på formen
! "Geoid_height_XXX.dat", der XXX er enten GGM eller EGM
!  
! Input: 
!   model (navnet på modellen som skal brukes, enten GGM eller EGM)
!
! Output: 
!   ingen (resultatet skrives til fil)
!
! Avhengigheter: 
!    Subrutiner: matrise, loop
!
! ENDRET(dd.mm.åå): 23.02.07
!================================================== 
USE constants
IMPLICIT NONE
CHARACTER(3),   INTENT(IN)  :: model
!interne variable
REAL(nok)                   :: N_temp,N_grav
REAL(nok)                   :: time1,time2
REAL(nok)                   :: lambda,phi
REAL                        :: lambda_start,lambda_end,phi_start,phi_end,step
INTEGER                     :: counter
!-------------------------------------------------

IF(model == "GGM") THEN
    WRITE(*,'(A)') "Computing geoidal height based on Global Gravity Model GGM02S"
    OPEN(UNIT=11, FILE='Geoid_height_GGM.dat', STATUS='unknown')
ELSEIF(model == "EGM") THEN
    WRITE(*,'(A)') "Computing geoidal height based on Global Gravity Model EGM96"
    OPEN(UNIT=11, FILE='Geoid_height_EGM.dat', STATUS='unknown')
ENDIF
    
WRITE(*,'(A)') "    This may take a while, please wait..."
!Values to be used in final program
!lambda_start    = 0.
!lambda_end      = 35.
!phi_start       = 55.
!phi_end         = 75.
!step            = 0.5

!testing values
lambda_start    = 0.
lambda_end      = 2.
phi_start       = 55.
phi_end         = 56.
step            = 0.5

phi = phi_start
call cpu_time(time1)
counter = 0 
DO WHILE(phi <=phi_end) 
    CALL matrise(phi,model)
   
    lambda = lambda_start 
    DO WHILE(lambda <=lambda_end)
        CALL loop(model,lambda,N_temp)
        N_grav = N_temp*(GM/(R*g))
        WRITE(11,'(F4.1,F7.1,F10.4)') lambda,phi,N_grav
        !WRITE(*,'(A\)') "-"
        lambda = lambda + step
        counter = counter + 1
        
    ENDDO 
   phi = phi + step 
   
ENDDO 
call cpu_time(time2)
CLOSE(UNIT=11) ! lukker fila vi skriver til

WRITE(*,*)
IF(model == "GGM") THEN
    WRITE(*,'(A)') "Geoidal height computed, saved to file 'Geoid_height_GGM.dat'"
ELSEIF(model == "EGM") THEN
    WRITE(*,'(A)') "Geoidal height computed, saved to file 'Geoid_height_EGM.dat'"
ENDIF
WRITE(*,'(A,I5)') "    Points calculated: ", counter
WRITE(*,'(A,F13.2,A)') "    Time elapsed: ", time2-time1, " s"

write(*,*)
write(*,*)

END SUBROUTINE compute_geoid
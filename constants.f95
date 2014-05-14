MODULE constants
!================================================== 
! Modul som inneholder div. konstanter
!
! ENDRET(dd.mm.åå): 19.02.07
!================================================== 
IMPLICIT NONE
    
!datatype med nok plass til våre behov
INTEGER,    PARAMETER               ::nok=SELECTED_REAL_KIND(6,100)
  
!matrisa P_nm
REAL(nok),  DIMENSION(0:360,0:360)  ::P_nm
   
!GRS80 quantities
REAL        J_2,a,g,GM
REAL(nok)   b,e2,f,R,pi
PARAMETER   (J_2          = 0.108263E-2)            ! ubenevnt
PARAMETER   (a            = 6378137.0000)           ! m
PARAMETER   (b            = 6356752.3141D0)         ! m
PARAMETER   (e2           = 0.006694380023D0)       ! ubenevnt    
PARAMETER   (f            = 298.257222101D0)        ! m
PARAMETER   (R            = 6371000.7900D0)         ! m    
PARAMETER   (GM           = 3986005.0E+08)          ! m^3/s^2
          
!General values
PARAMETER   (g            = 9.81)                   ! m/s^2   (tyngdens akselerasjon)     
PARAMETER   (pi           = 3.14159265358979323846264338327950288419716939937510)
END MODULE
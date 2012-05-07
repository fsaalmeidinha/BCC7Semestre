program ola_4
!$ use omp_lib
implicit none
integer::np
call omp_set_num_threads(8) 
!$OMP PARALLEL PRIVATE(NP)
NP =omp_get_thread_num()
CALL WORK('em paralelo',NP)
!$OMP MASTER
NP=omp_get_thread_num()
CALL WORK('no master  ',NP)
!$OMP END MASTER
!$OMP END PARALLEL
end program ola_4                              
subroutine  work(msg,THD_NUM)
integer::THD_NUM
character(*)::msg
PRINT *,msg,THD_NUM
end subroutine work                  

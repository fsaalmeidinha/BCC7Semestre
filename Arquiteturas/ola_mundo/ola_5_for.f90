program ola_5_for
        !$ use omp_lib
        implicit none
        integer::i,np
        np=1
        !$omp parallel
        !$omp do  
            do i=1,16
               !$ np=omp_get_thread_num()
               write(*,*)"i=",i,"threads",np
            enddo
        !   !$omp end do  
        !$omp end parallel
end program ola_5_for

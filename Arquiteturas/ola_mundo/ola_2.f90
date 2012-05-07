program ola
        !$ use omp_lib
        implicit none
        write(*,*)
        write(*,*)"imprime",omp_in_parallel()
        write(*,*)
        !$omp parallel 
        write(*,*)"imprime",omp_in_parallel()
        !$omp end parallel 
end program ola

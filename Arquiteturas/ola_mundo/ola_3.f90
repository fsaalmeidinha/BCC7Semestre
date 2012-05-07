program ola_3
        !$ use omp_lib
        implicit none
        integer::N1,N2
        N1 = omp_get_num_threads()
        PRINT *, N1
        PRINT *
        !$OMP PARALLEL PRIVATE(N2)
        N2 = omp_get_num_threads()
        PRINT *, N2
        !$OMP END PARALLEL
end program ola_3

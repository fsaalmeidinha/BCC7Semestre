program ola
        !$ use omp_lib
        implicit none
        integer::total,rank
        total=1
        rank=0
        !$omp parallel private(rank) shared(total)
        !$   total=omp_get_num_threads()
        !$   rank =omp_get_thread_num()
        write(*,*) 'Oi da thread',rank,'no grupo',total,'threads' 
        !$omp end parallel 
end program ola

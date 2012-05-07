program ola_1
        !$ use omp_lib
        implicit none
        !$omp parallel 
        write(*,*) 'Ola pessoal' 
        !$omp end parallel 
end program ola_1

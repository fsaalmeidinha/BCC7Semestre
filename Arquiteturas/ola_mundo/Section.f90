program Section
!$ use omp_lib
        !implicit none

        !$omp parallel
        !$omp sections
	!$omp section
	write(*,*),"Teste"
	!$omp section
	write(*,*),"Teste2"
        !$omp end sections
        !$omp end parallel

end program Section

program Barrier_Single_Nowait
!$ use omp_lib
        !implicit none
	integer::variavel = 0

        !$omp parallel
        !$omp sections
	!$omp section
	variavel = variavel + 1
	write(*,*),"Teste"
	!$omp section
	call sleep(1)
	variavel = variavel + 2
	write(*,*),"Teste2"
        !$omp end sections nowait
	!sem a barreira, iria imprimir errado
	!$omp barrier	

	!$omp single
	write(*,*),"Variavel: ",variavel
	!$omp end single

        !$omp end parallel

end program Barrier_Single_Nowait

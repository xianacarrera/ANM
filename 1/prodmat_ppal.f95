program summat_ppal
    !*************************
    !Programa que lee y escribe:
    !   el número de filas, m
    !   el número de columnas, n
    !   y los elementos
    !de dos matrices compatibles para la suma.
    !Después, realiza y escribe su suma.
    !*************************

    implicit none
    integer::m, n, p, i, j, k, reserva
    real, allocatable, dimension(:,:)::a, b, c
    real::aux    !Variable auxiliar para las sumas

    character(len=4)::formato1 = '(i4)'
    character(len=10)::formato2 = '(100e12.4)'

    print*
    print*, 'El numero de filas de A es:'
    read *, m
    print formato1, m

    print*
    print*, 'El numero de columnas de A y de filas de B es:'
    read *, n
    print formato1, n    

    print*
    print*, 'El numero de columnas de B es:'
    read *, p
    print formato1, p

    allocate(a(m,n), b(n,p), c(m,p), stat=reserva)
	
    if (reserva /= 0) then
        print*,'No se pudo reservar memoria'
        stop
    end if

    print*
    print*,'La matriz A es:'
    do i = 1, m
        read*,a(i,:)
        print formato2, a(i,:)
    end do

    print*
    print*,'La matriz B es:'
    do i = 1, n
        read*,b(i,:)
        print formato2, b(i,:)
    end do

    do i = 1, m
        do j = 1, p
            aux = 0.
            do k = 1, n
                aux = aux + a(i, k) * b(k, j)
            end do
            c(i,j) = aux
        end do
    end do

    print*
    print*,'Multiplicación elemento a elemento de las matrices'
    do i = 1, m
        print *, c(i, :)
    end do
    
    c = matmul(a, b)

    print*
    print*,'Multiplicación directa de las matrices'
    print*, c

    deallocate(a, b, c)

end program summat_ppal
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
    integer::m, n, i, j, reserva
    real, allocatable, dimension(:,:)::a,b

    character(len=4)::formato1='(i4)'
    character(len=10)::formato2='(100e12.4)'

    print*
    print*, 'El numero de filas de A y B es:'
    read *, m
    print formato1, m

    print*
    print*, 'El numero de columnas de A y B es:'
    read *, n
    print formato1, n     

    allocate(a(m,n), b(m,n), stat=reserva)
	
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
    do i = 1, m
        read*,b(i,:)
        print formato2, b(i,:)
    end do

    !do i=1,m
    !    do j=1,n
    !        a(i, j) = a(i, j) + b(i, j)
    !    end do
    !end do

    !print*
    !print*,'Suma elemento a elemento de las matrices'
    !do i=1,m
    !    print *, a(i, :)
    !end do

    a = a + b

    print*
    print*,'Suma directa de las matrices'
    print*, a

    !Los escalares son compatibles para operaciones con todas las matrices
    a = a + 1

    print*
    print*,'Resultado de sumar 1 a todos los elementos (A + B + 1):'
    print*, a

    deallocate(a,b)

end program summat_ppal
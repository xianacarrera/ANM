program prodmat_ppal
    !*************************
    !Programa que lee y escribe:
    !   el número de filas, m, de una matriz A
    !   el número de columnas de A, que se toma como el número
    !           de columnas de B
    !   el número de columnas, p, de B
    !   y sus correspondientes elementos.
    !Después, realiza y escribe su multiplicación.
    !*************************

    implicit none
    integer::m, n, p, i, j, k, reserva
    real, allocatable, dimension(:,:)::a, b, c
    real::aux    !Variable auxiliar para las sumas

    character(len=4)::formato1 = '(i4)'
    character(len=10)::formato2 = '(100e12.4)'

    print*
    print*,'****************************************'
    print*,'Programa que lee y escribe:'
    print*,'        el número de filas, m, de una matriz A'
    print*,'        el número de columnas de A, que se toma como el número'
    print*,'                  de columnas de B'
    print*,'        y sus correspondientes elementos elementos'
    print*,'Después, realiza y escribe su multiplicación.'
    print*,'*****************************************'
    print*

    !Lectura y escritura del numero de filas de la matriz A
    print*
    print*, 'El numero de filas de A es:'
    read *, m
    print formato1, m

    !Lectura y escritura del numero de columnas de A
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

end program prodmat_ppal
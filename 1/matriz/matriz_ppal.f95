program matriz_ppal
    !*************************
    !Programa que lee y escribe:
    !   el número de filas, m
    !   el número de columnas, n
    !   y los elementos
    !de una matriz A
    !*************************

    implicit none
    integer::m, n, i
    real, allocatable, dimension(:,:)::a

    character(len=4)::formato1='(i4)'
    character(len=10)::formato2='(100e12.4)'

    print*
    print*,'****************************'
    print*,'Programa que lee y escribe:'
    print*,'   el numero de filas, m'
    print*,'   el numero de columnas, n'
    print*,'   y los elementos'
    print*,'de una matriz A'
    print*,'****************************'
    print*

    print*
    print*, 'El numero de filas de A es:'
    read *, m
    print formato1, m

    print*
    print*, 'El numero de columnas de A es:'
    read *, n
    print formato1, n    

    allocate(a(m,n))

    print*
    print*,'La matriz A es:'
    do i = 1, m
        read*,a(i,:)
        print formato2, a(i,:)
    end do

    print*
    print*,'Segunda forma de imprimir una matriz, con print*'
    print*,a

    print*
    print*, 'El tamaño de A (size(A)) es:'
    print*, size(a)
    !Como no se indica dimensión, size(a) devuelve el número de elementos de a (el producto de sus extensiones)

    print*
    print*, 'El tamaño de shape(A) es:'
    print*, 'size(shape(A)) = ', size(shape(a))
    !size(shape(a)) será el número de dimensiones de A (su rango)

    print*
    print*, 'El vector shape(A) es:'
    print*, shape(a)
    !shape(a) es un vector unidimensional que indica la extensión de cada una de las dimensiones de a

    deallocate(a)

end program matriz_ppal
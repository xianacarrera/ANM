program prodesc_ppal
    !*************************
    !Producto escalar de dos vectores:
    !mediante definición
    !mediante la función intrínseca dot_product
    !*************************

    implicit none
    integer::n, i
    real::prodesc
    real, allocatable, dimension(:)::u, v

    print*
    print*, 'El numero de componentes de u y v es:'
    read *, n
    print *, n

    allocate(u(n),v(n))

    print*
    print*, 'El vector u es:'
    read*, u
    print*, u

    print*
    print*, 'El vector v es:'
    read*, v
    print*, v

    !prodesc = 0.    !Al ser un real, se inicializa con un punto
    !do i = 1, n
    !    prodesc = prodesc + u(i) * v(i)
    !end do

    prodesc = dot_product(u,v)

    print*
    print*, 'El producto escalar de u y v es:'
    print*, prodesc

    deallocate(u, v)


end program prodesc_ppal
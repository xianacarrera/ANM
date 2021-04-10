program matdve_ppal
    ! Programa que lee y escribe una matriz tridiagonal A y un vector v y calcula su producto Av.

    ! En el examen habrá que desarrollar la fórmula y luego escribir el programa
    ! Se pueden extender au y al a vectores de tamaño n y poner la componente restante a 0, para juntar los cálculos en un solo bucle
    implicit none

    real,allocatable,dimension(:)::ad,al,au,v,w
    integer::i,n,reserva

    character(len=4)::formato1 = '(i4)'
    character(len=10)::formato2 = '(100e12.4)'

    print*
    print*,'El orden de la matriz tridiagonal A es'
    read*,n
    print formato1, n

    allocate(ad(n), au(n-1), al(n-1), v(n), w(n), stat=reserva)

    if (reserva /= 0) then
        print*,'No se pudo reservar memoria'
        stop
    end if

    print*
    print*,'La diagonal de la matriz A es:'
    read*,ad
    print formato2, ad

    print*
    print*,'La subdiagonal de la matriz A es:'
    read*,al
    print formato2, al

    print*
    print*,'La superdiagonal de la matriz A es:'
    read*,au
    print formato2, au

    print*
    print*,'El vector v es:'
    read*,v
    print formato2, v

    w(1) = ad(1)*v(1)+au(1)*v(2)

    do i=2,n-1
        w(i)=al(i-1)*v(i-1)+ad(i)*v(i)+au(i)*v(i+1)
    end do
    !Se puede sustituir por
    !w(2:n-1)=al(1:n-2)*v(1:n-2)+ad(2:n-1)*v(2:n-1)+au(2:n-1)*v(3:n)

    w(n) = al(n-1)*v(n-1)+ad(n)*v(n)

    print*
    print*,'El vector producto Av es:'
    !print formato2, w
    print*,w

    deallocate(ad, al, au, v, w)

    !Otra opción
    ! ...
    ! allocate(ad(n),al(0:n-1),au(n),v(0:n+1),w(n))
    !
    ! read*,ad(1:n)
    ! read*,al(1:n-1)
    ! read*,au(1:n-1)
    ! read*,v(1:n)
    !
    ! al(0) = 0.
    ! au(n) = 0.
    ! v(0) = 0.
    ! v(n+1) = 0.
    !
    ! w(1:n) = al(0:n-1)*v(0:n-1)+ad(1:n)*v(1:n)+au(1:n)*v(2:n+1)

    ! También se puede guardar el producto en el arreglo v

end program matdve_ppal
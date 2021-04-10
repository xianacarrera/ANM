program redisuo_ppal
    ! Cuanto menor es el residuo, mejor es la aproximación de la solución

    use mod_clreal      !Módulo donde está la clase definida de reales (clreal)

    implicit none

    integer::m,n,reserva,i
    real(kind=clreal),allocatable,dimension(:,:)::a
    real(kind=clreal),allocatable,dimension(:)::b,u,r

    !Otra opción
    !real,allocatable::a(:,:),b(:),u(:),r(:)

    character(len=4)::formato1 = '(i4)'
    character(len=10)::formato2 = '(100e12.4)'

    print*
    print*,'****Calculo del residuo de un sistema r=Au-b****'
    print*

    print*
    print*,'El numero de filas de A es:'
    read*,m
    print formato1, m

    print*
    print*,'El numero de columnas de A es:'
    read*,n
    print formato1, n

    allocate(a(m,n), b(m), u(n), r(m), stat=reserva)

    if (reserva /= 0) then
        print*,'No se pudo reservar memoria'
        stop
    end if

    print*
    print*,'La matriz A es:'
    do i = 1,m
        read*,a(i,:)
        print formato2, a(i,:)
    end do

    print*
    print*,'El termino independiente b es:'
    read*,b
    print formato2, b

    print*
    print*,'El vector solucion u es:'
    read*,u
    print formato2,u

    call residuo(m,n,a,b,u,r)

    print*
    print*,'El residuo del sistema r=Au-b es:'
    !print formato4,r
    print*,r

    deallocate(a,b,u,r)

end program redisuo_ppal
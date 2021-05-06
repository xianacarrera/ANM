program residuo_ppal
    !***********************************************************************
    ! Programa principal para el calculo del residuo de un sistema r=Au-b
    ! Cuanto menor es el residuo, mejor es la aproximación de la solucion
    !***********************************************************************

    use mod_clreal      !Modulo donde esta la clase definida de reales (clreal)

    implicit none

    !Argumentos
    integer::m,n,reserva,i
    real(kind=clreal),allocatable,dimension(:,:)::a
    real(kind=clreal),allocatable,dimension(:)::b,u,r

    !Otra opción
    !real,allocatable::a(:,:),b(:),u(:),r(:)

    !Formatos
    character(len=4)::formato1 = '(i4)'
    character(len=10)::formato2 = '(100e12.4)'

    print*
    print*,'****Calculo del residuo de un sistema r=Au-b****'
    print*

    print*
    print*,'            *Lectura de datos*'
    print*

    !Lectura del numero de filas de A
    print*
    print*,'El numero de filas de A es:'
    read*,m
    print formato1, m

    !Lectura del numero de columnas de A
    print*
    print*,'El numero de columnas de A es:'
    read*,n
    print formato1, n

    !Reserva de memoria
    allocate(a(m,n), b(m), u(n), r(m), stat=reserva)

    !Comprobacion de la reserva correcta de memoria
    if (reserva /= 0) then
        print*,'No se pudo reservar memoria'
        stop
    end if

    !Lectura y escritura de la matriz del S.E.L.
    print*
    print*,'La matriz A es:'
    do i = 1,m
        read*,a(i,:)
        print formato2, a(i,:)
    end do

    !Lectura y escritura del termino independiente
    print*
    print*,'El termino independiente b es:'
    read*,b
    print formato2, b

    !Lectura y escritura de la solucion aproximada
    print*
    print*,'El vector solucion u es:'
    read*,u
    print formato2,u

    print*
    print*,'            *Fin de la lectura de datos*'
    print*

    !Calculo del residuo del S.E.L.
    call residuo(m,n,a,b,u,r)

    !Escritura del residuo
    print*
    print*,'El residuo del sistema r=Au-b es:'
    !print formato4,r
    print formato2,r

    !Liberacion de memoria
    deallocate(a,b,u,r)

end program residuo_ppal
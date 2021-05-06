program sistu_ppal
    !******************************************************************
    !Programa principal para la resolución de un sistema con matriz triangular
    !superior
    !********************************************************************

    use mod_clreal

    implicit none

    integer::n,i,j,reserva
    real(kind=clreal),allocatable,dimension(:)::b,u
    real(kind=clreal),allocatable,dimension(:,:)::a

    !Formatos
    character(len=4)::formato1 = '(i4)'
    character(len=10)::formato2 = '(100e12.4)'

    print*
    print*,'****************************************'
    print*,'Calculo de la solucion de un sistema con matriz triagular superior'
    print*,'                       Au=b'
    print*,'*****************************************'
    print*

    print*
    print*,'El orden de A es:'
    read*,n
    print formato1,n

    allocate(a(n,n), b(n), u(n), stat=reserva)

    if (reserva /= 0) then
        print*,'No se pudo reservar memoria'
        stop
    end if

    print*
    print*,'La matriz triangular superior A es:'
    do i = 1,n
        !No hace falta leer todos los elementos. De cada fila, leo desde i hasta n
        read*,a(i,i:n)
        if(abs(a(i,i)) < 1.e-12) then
            print*, 'Matriz singular'
            stop
        end if
        !Para que no se distorsione el formato de salida, imprimo 0's desde el inicio de cada fila hasta la diagonal
        print formato2, (0._clreal,j=1,i-1), a(i,i:n)
    end do

    !lectura y escritura del termino independiente
    print*
    print*,'El termino independiente b es:'
    read*,b
    print formato2, b

    !resolucion del sistema triangular superior
    call sistu(n,a,b,u)

    !solucion del sistema
    print*
    print*,'El vector solución u del sistema Au=b es:'
    print formato2,u

    !liberacion de memoria
    deallocate(a,b,u)

end program sistu_ppal
program gaussh_ppal
    !*********************************************
    !Programa principal para la resolución de S.E.L. con matrices Hessenberg superiores.
    !*********************************************

    use mod_clreal
    implicit none

    integer::                                           n     !Orden del S.E.L.                       
    real(kind=clreal)::                                 deter !Determinante de la matriz del S.E.L.
    real(kind=clreal), allocatable, dimension(:,:)::    a     !Matriz del S.E.L.
    real(kind=clreal), allocatable, dimension(:,:)::    aa    !Copia de a
    real(kind=clreal), allocatable, dimension(:)::      b     !Término independiente del S.E.L.
    real(kind=clreal), allocatable, dimension(:)::      bb    !Copia de b  
    real(kind=clreal), allocatable, dimension(:)::      u     !Solución del S.E.L.
    real(kind=clreal), allocatable, dimension(:)::      r     !Residuo


    integer::i,j     !Indices de iteracion


    print*, '****S.E.L. con matriz Hessenberg superior****'
    print*,'****Resolución del sistema mediante el método de Gauss****'

    !Lectura y escrritura del orden del sistema
    print*
    print*,'Introducir orden del S.E.L.: '
    print*
    read*, n
    print*, n
    print*

    !Reserva de memoria
    allocate(a(n,n), aa(n,n), b(n), bb(n), u(n), r(n))

    !Se copia a y b para el posterior cálculo del residuo
    aa = a
    bb = b

    !Lectura de datos
    call datsishessup(n, a, b)

    !Transformación del S.E.L. en uno equivalente con matriz triangular superior por Gauss para matrices Hessenberg superiores
    call gaussh(n, a, b, deter)

    !Escritura de la matriz resultante de la eliminación de Gauss
    print*
    print*,'La matriz triangular superior U es:'
    print*
    do i = 1,n
        !Para que no se distorsione el formato de salida, imprimo 0's desde el inicio de cada fila hasta la diagonal
        print*, (0._clreal, j=1,i-1), a(i,i:n)
    end do

    !Escritura del término independiente modificado
    print*
    print*, 'El término independiente del sistema equivalente b es: '
    print*
    print*, b
    print*

    !Escritura del determinante de A
    print*
    print*, 'El determinante de la matriz equivalente es: '
    print*
    print*, deter
    print*

    !Resolución del sistema con matriz triangular superior
    call sistu(n, a, b, u)

    !Escritura de la solución del S.E.L.
    print*
    print*, 'La solución del S.E.L. es: '
    print*
    print*, u
    print*

    !Residuo del cálculo de la solución
    call residuo(n, n, aa, bb, u, r)

    !Escritura del residuo resultante
    print*
    print*, 'El residuo es: '
    print*
    print*, r
    print*

    !Liberación de memoria
    deallocate(a, aa, b, bb, u, r)

end program gaussh_ppal
program gauss_ppal
    !*********************************************
    !Programa principal para la resolución de S.E.L. con matrices
    !de estructura igual a la del ejercicio 2 del boletín 3
    !*********************************************

    use mod_clreal
    implicit none

    integer::                                         n    !Orden del S.E.L.
    real(kind=clreal), allocatable, dimension(:)::    a    !Diagonal de la matriz del S.E.L.
    real(kind=clreal), allocatable, dimension(:)::    c    !Primera fila de la matriz del S.E.L., exceptuando el primer elemento
    real(kind=clreal), allocatable, dimension(:)::    b    !Termino independiente del S.E.L.
    real(kind=clreal), allocatable, dimension(:)::    u    !Solución del S.E.L.
    real(kind=clreal)::                            alfa    !Elemento a_1n
    real(kind=clreal)::                           deter    !Determinante

    integer::reserva !Comprobacion de reserva de memoria correcta
    integer::i,j     !Indices de iteracion

    
    print*, '************S.E.L. - Boletín 3, ejercicio 2**************'
    print*,'****Resolución del sistema mediante el método de Gauss****'

    !Lectura del orden del sistema
    print*
    print*,'Introducir orden del S.E.L.: '
    read*, n
    print*, n
    print*

    !Asignacion de memoria dinamica
    allocate(a(n), b(n), c(n-1), u(n), stat = reserva)

    !Lectura de la diagonal de la matriz
    print*
    print*,'Introducir diagonal de la matriz: '
    read*, a
    print*, a
    print*

    !Lectura de los elementos restantes de la primera fila
    print*
    print*,'Introducir primera fila de la matriz (salvo el primer elemento): '
    read*, c
    print*, c
    print*

    !Lectura de alfa
    print*
    print*,'Introducir alfa (elemento a_1n): '
    read*, alfa
    print*, alfa
    print*

    !Matriz A resultante
    print*
    print*,'La matriz del S.E.L. sera: '
    print*
    print*, a(1), c(1:n-1)
    do i = 2,n-1
        !Para que no se distorsione el formato de salida, imprimo 0's desde el inicio de cada fila hasta la diagonal
        print*, (0._clreal, j=1,i-1), a(i), (0._clreal, j=i+1,n)
    end do
    print*, alfa, (0._clreal, j=2,n-1), a(n)

    !Lectura del termino independiente
    print*
    print*,'Introducir termino independiente del S.E.L.: '
    read*, b
    print*, b
    print*

    !Eliminacion utilizando el metodo de Gauss
    call gauss_acalfa(n, a, c, alfa, b, deter)

    !Escritura de la matriz triangular superior resultante
    print*
    print*,'La matriz triangular superior U es:'
    print*
    print*, a(1), c(1:n-1)
    do i = 2,n
        !Para que no se distorsione el formato de salida, imprimo 0's desde el inicio de cada fila hasta la diagonal
        print*, (0._clreal, j=1,i-1), a(i), (0._clreal, j=i+1,n)
    end do

    !Escritura del termino independiente del sistema equivalente
    print*
    print*, 'El termino independiente del sistema equivalente b es: '
    print*
    print*, b
    print*

    !Escritura del determinante de la matriz U
    print*
    print*, 'El determinante de la matriz equivalente es: '
    print*
    print*, deter
    print*

    !Resolucion del sistema con matriz triangular superior
    call sistu_acalfa(n, a, c, b, u)

    !Escritura de la solucion del sistema
    print*
    print*, 'La solución del S.E.L. es: '
    print*
    print*, u
    print*

    !Liberacion de memoria
    deallocate(a, b, c, u)

end program gauss_ppal
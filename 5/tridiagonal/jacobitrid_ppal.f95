program jacobitrid_ppal
    !Programa principal relativo al metodo de Jacobi

    use mod_clreal
    use mod_formatos
    implicit none

    integer::                                          n        !orden del S.E.L.
    real(kind=clreal), allocatable, dimension(:)::     ad       !diagonal de la matriz
    real(kind=clreal), allocatable, dimension(:)::     as       !subdiagonal de la matriz
    real(kind=clreal), allocatable, dimension(:)::     au       !superdiagonal de la matriz
    real(kind=clreal), allocatable, dimension(:)::     b        !termino independiente
    real(kind=clreal), allocatable, dimension(:)::     u        !iterante inicial
    real(kind=clreal)::                                eps      !parametro para el test de parada:
                                                         !  (k)    (k-1)
                                                         !|u    - u     | < eps
                                                         !               inf
    integer::                           nitmax   !numero maximo de iteraciones permitido
    real(kind=clreal), allocatable, dimension(:)::      r       !residuo del S.E.L.

    !variables locales
    integer:: i,j

    print*,'****Resolución del sistema mediante el metodo de Jacobi****'
    print*,'         Generalizacion para matrices tridiagonales'

    print*
    print*,'Introducir orden del S.E.L: '
    print*
    read*, n
    print*, n
    print*

    !reserva de memoria
    allocate(ad(n), as(n-1), au(n-1), b(n), u(n), r(n))

    !lectura de datos
    call initertrid(n, ad, as, au, b, u, eps, nitmax)

    print*
    print*,'******Fin de la lectura de datos*********'
    print*

    !aplicacion del metodo de Jacobi
    call jacobitrid(n,ad,as,au,b,u,eps,nitmax)

    !escritura de la solucion aproximada del sistema (ultimo iterante calculado)
    print*
    print*, ' El ultimo iterante calculado es: '
    print*
    print*, u
    print*

    !residuo de la solucion aproximada
    call residuotrid(n,ad,as,au,b,u,r)

    !escritura del residuo
    print*
    print*, 'El residuo es: '
    print*
    print*, r
    print*
    
    !escritura de la norma 2 del residuo del sistema
    print*
    print*, 'La norma ||r|| es:', sqrt(dot_product(r,r))
    print*, '              2'

    !liberacion de memoria
    deallocate(ad, as, au, b, u, r)

end program jacobitrid_ppal
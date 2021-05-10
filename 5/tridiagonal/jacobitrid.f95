subroutine jacobitrid(n,ad,as,au,b,u,eps,nitmax)
    !***********************************************************
    !Subrutina relativa al metodo iterativo de Jacobi en el caso de una matriz tridiagonal
    !***********************************************************

    use mod_clreal
    implicit none

    integer::                                         n       !orden del S.E.L.
    real(kind=clreal), dimension(n), intent(in)::     ad      !diagonal de la matriz del S.E.L.
    real(kind=clreal), dimension(n-1), intent(in)::   as      !subdiagonal de la matriz del S.E.L.
    real(kind=clreal), dimension(n-1), intent(in)::   au      !superdiagonal de la matriz del S.E.L.
    real(kind=clreal), dimension(n), intent(in)::     b       !termino independiente del S.E.L.
    real(kind=clreal), dimension(n), intent(inout)::  u       !iterante inicial 
                                                              !o ultimo
                                                              !iterante hallado
    real(kind=clreal)::                               eps     !parametro para el test de parada
                                                    !  (k)    (k-1)
                                                    !|u    - u     |   <  eps
                                                    !               inf
    integer::                                       nitmax  !numero de iteraciones máximo permitido
    
    !variables auxiliares
    integer:: i,iter
    !integer:: j, iter
    real(kind=clreal), dimension(n)::uold
    real(kind=clreal)::error

    !interrupcion del metodo si el elemento diagonal i-esimo es nulo
    do i=1,n
        if(abs(ad(i)) < 1.e-12) then
            print*,'Atencion, elemento diagonal',i,' < 1.e-12,'
            print*,'¡se interrumpe el algoritmo!'
            stop
        endif
    enddo

    print*, '            |                      resultados'
    print*, '******************************************************************'

    !calculo de los iterantes
    do iter=1,nitmax        !nitmax es el numero de iteraciones maximo
        uold=u
        u(1)=(b(1) - au(1)*uold(2))/ad(1)
        do i=2,n-1
            u(i)=(b(i) - as(i-1)*uold(i-1) - au(i)*uold(i+1))/ad(i)
        end do
        u(n)=(b(n) - as(n-1)*uold(n-1))/ad(n)

        !se calcula el error utilizando la norma infinito
        error=maxval(abs(u-uold))

        print*, iter, '|', u
        print*, '         err|', error
        print*,'******************************************************************'


        if(error < eps) then
            print*,'Se satisface el test de parada para iter = ',iter
            return
        endif
    end do
    
    !se ha llegado hasta el final del bucle sin que el error sea menor que eps en ninguna iteracion
    print*
    print*,'******************************************************************'
    print*,'Efectuadas ',nitmax,' iteraciones sin que se cumpla'
    print*,'el test de parada'

end subroutine jacobitrid
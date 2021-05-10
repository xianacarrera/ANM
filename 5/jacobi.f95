subroutine jacobi(n,a,b,u,eps,nitmax)
    !***********************************************************
    !Subrutina relativa al metodo iterativo de Jacobi
    !***********************************************************

    use mod_clreal
    implicit none

    integer::                                         n       !orden del S.E.L.
    real(kind=clreal), dimension(n,n), intent(in)::   a       !matriz del S.E.L.
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
        if(abs(a(i,i)) < 1.e-12) then
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
        do i=1,n
            u(i)=(b(i) - sum(a(i,1:i-1)*uold(1:i-1)) &
                       - sum(a(i,i+1:n)*uold(i+1:n)))/a(i,i)
        end do

        !u=b
        !do j=1,n
        !   u(1:j-1) = u(1:j-1) - a(1:j-1,j)*uold(j)
        !   u(j+1:n) = u(j+1:n) - a(j+1:n,j)*uold(j)
        !end do
        !do j=1,n
        !   u(j)=u(j)/a(j,j)
        !end do

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

end subroutine jacobi
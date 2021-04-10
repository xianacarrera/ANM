subroutine residuo(m,n,a,b,u,r)
    implicit none

    integer, intent(in)::m,n
    real,intent(in)::a(m,n),b(m),u(n)
    real,intent(out)::r(m)

    real::aux(m)
    integer::j

    aux=0.
    do j=1,n
        aux = aux + a(:,j)*u(j)
    end do
    r=aux-b
end subroutine residuo
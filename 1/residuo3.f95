subroutine residuo(m,n,a,b,u)
    implicit none

    integer,intent(in)::m,n
    real,intent(in)::a(m,n),u(n)
    real,intent(inout)::b(m)

    integer::j

    do j=1,n
        b(:)=b(:)-a(:,j)*u(j)
    end do
    b = -b
    
end subroutine residuo
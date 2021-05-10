% Metodo de Newton discretizado para un sistema no lineal
clc
clear all

% Se importan los datos para el calculo
newton_dis_datos
for it=1:niter
   
    % Se evalua la funcion en x0
    fx=feval(funcion,x0);
   
    % Matriz jacobiana discretizada
    for j=1:n 
        x0(j)=x0(j)+h;
        fxh=feval(funcion,x0);
        x0(j)=x0(j)-h;
        % Aproximación de la derivada
        df(:,j)=(fxh-fx)/h;
    end
    
    % Test sobre la matriz jacobiana discretizada
    detdf=det(df);     % Determinante
    if(abs(detdf)<1.e-12)
        disp(['det(df)= ',num2str(detdf)])
        disp('Matriz jacobiana discretizada singular')
        return
    end
    
    % Resolucion del sistema
    dx=df\fx;
    x1=x0-dx;
    
    % Test de parada
    error=norm(dx);
    
    % Imprimimos el numero de la iteracion actual, el iterante y su error
    % num2str devuelve la representación textual de un número
    disp(['iteracion numero: ',num2str(it)])
    disp(['iterante: ',num2str(x1')])
    disp(['error: ', num2str(error)])
    
    %if(error<=tol) % test error absoluto
    %if(error<=tol*norm(x0)) % test error relativo
        % diferencia en norma 2 
        % test muy exigente si x0 es pequeño
     if(error<=tol*(norm(x0)+1)) % test error absoluto/relativo
        % si estamos convergiendo a 0, norm(x0+1) converge a 1
        % se convierte en un test de error absoluto
        disp('Convergencia alcanzada')
        disp(['La solucion es: ',num2str(x1')])
        disp(['Se obtuvo en la iteracion: ',num2str(it)])
        residuo=norm(feval(funcion,x1));
        disp(['El residuo es: ',num2str(residuo)])
        return
    else
        x0=x1;
    end
end

disp('Se supero el numero maximo de iteraciones sin convergencia')
disp(['El ultimo iterante calculado es: ',num2str(x1')])
residuo=norm(feval(funcion,x1));
disp(['El residuo es: ',num2str(residuo)])
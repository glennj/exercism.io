create or replace package prime#
is
    function is_prime ( n pls_integer ) return boolean;
    function nth      ( n pls_integer ) return pls_integer;

    invalid_argument_error      exception;
end prime#;
/

create or replace package body prime#
is
    function is_prime ( n pls_integer )
    return boolean
    is
        root    pls_integer := trunc(sqrt(n));
        factor  pls_integer;
    begin
        if n <= 1 then
            return false;
        elsif n = 2 then
            return true;
        elsif mod(n, 2) = 0 then
            return false;
        end if;

        factor := 3;
        while factor <= root loop
            if mod(n, factor) = 0 then
                return false;
            end if;
            factor := factor + 2;
        end loop;
        return true;

    exception when others then raise;
    end is_prime;

    --
    function nth ( n pls_integer ) return pls_integer
    is
        counter pls_integer;
        prime   pls_integer;
    begin
        if n < 1 then
            raise invalid_argument_error;
        end if;

        if n = 1 then
            return 2;
        end if;

        counter := 1;
        prime := 1;
        while counter < n loop
            prime := prime + 2;
            if is_prime(prime) then
                counter := counter + 1;
            end if;
        end loop;

        return prime;

    exception when others then raise;
    end nth;
end prime#;
/

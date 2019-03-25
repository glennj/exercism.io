create or replace package grains#
is
    function at_square ( n pls_integer ) return number;
    function total                       return number;
    function total_naive                 return number;

    invalid_square  exception;
end grains#;
/

create or replace package body grains#
is
    function at_square ( n pls_integer ) 
    return number
    is
    begin
        if n < 1 or n > 64 then
            raise invalid_square;
        end if;

        return power(2, n-1);

    exception when others then raise;
    end at_square;

    --
    function total 
    return number
    is
    begin

        return power(2, 64) - 1;

    exception when others then raise;
    end total;

    --
    function total_naive  
    return number
    is
        s number := 0;
        i pls_integer;
    begin
        for i in 1 .. 64 loop
            s := s + at_square(i);
        end loop;
        return s;
    exception when others then raise;
    end total_naive;

end grains#;
/


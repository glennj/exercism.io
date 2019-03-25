create or replace package series#
is
    function square_of_sums  ( n pls_integer ) return pls_integer;
    function sum_of_squares  ( n pls_integer ) return pls_integer;
    function diff_of_squares ( n pls_integer ) return pls_integer;
end series#;
/

create or replace package body series#
is
    --
    function square_of_sums ( n pls_integer ) return pls_integer
    is
        summ    pls_integer := 0;
        i       pls_integer;
    begin
        if i <= 0 then
            raise_application_error(-20001, 'Natural numbers only');
        end if;

        for i in 1 .. n loop
            summ := summ + i;
        end loop;

        return summ * summ;

    exception when others then raise;
    end;

    --
    function sum_of_squares ( n pls_integer ) return pls_integer
    is
        summ    pls_integer := 0;
        i       pls_integer;
    begin
        if i <= 0 then
            raise_application_error(-20001, 'Natural numbers only');
        end if;

        for i in 1 .. n loop
            summ := summ + i * i;
        end loop;

        return summ;

    exception when others then raise;
    end;

    --
    function diff_of_squares ( n pls_integer ) return pls_integer
    is
    begin

        return square_of_sums(n) - sum_of_squares(n);

    exception when others then raise;
    end;

end series#;
/


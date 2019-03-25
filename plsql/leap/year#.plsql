create or replace package year#
is
    function is_leap(
        year  number
    )
    return varchar2;
end year#;
/

create or replace package body year#
is
    function is_leap(
        year  number
    )
    return varchar2
    as
        result varchar2(100);
    begin
        if mod(year, 400) = 0 or (mod(year, 4) = 0 and mod(year, 100) <> 0) then
            result := 'Yes, ' || year || ' is a leap year';
        else
            result := 'No, ' || year || ' is not a leap year';
        end if;
        return result;
    end;
end year#;
/

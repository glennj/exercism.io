create or replace package raindrops#
is
    function convert ( num number ) return varchar2;
end raindrops#;
/

create or replace package body raindrops#
is
    function convert (
        num             number
    )
    return varchar2
    as
        result          varchar2(15);
    begin

        if mod(num, 3) = 0 then
            result := result || 'Pling';
        end if;
        if mod(num, 5) = 0 then
            result := result || 'Plang';
        end if;
        if mod(num, 7) = 0 then
            result := result || 'Plong';
        end if;

        if result is null then
            result := to_char(num);
        end if;

        return result;
        
    exception
        when others
            then raise;

    end convert;

end raindrops#;
/

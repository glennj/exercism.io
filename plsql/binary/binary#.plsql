create or replace package binary#
is
    function to_decimal         (binary_string varchar2) return pls_integer;
    function to_dec_loop        (binary_string varchar2) return pls_integer;
    function to_dec_bin_to_num  (binary_string varchar2) return pls_integer;
end binary#;
/

create or replace package body binary#
is
    function to_dec_loop (binary_string varchar2)
    return pls_integer
    is
        digit   varchar2(1);
        answer  pls_integer := 0;
        i       pls_integer;
    begin
        for i in 1 .. length(nvl(binary_string, '')) loop
            digit := substr(binary_string, i, 1);
            answer := answer * 2;
            if digit = '1' then
                answer := answer + 1;
            elsif digit <> '0' then
                -- invalid binary digit
                return 0;
            end if;
        end loop;

        return answer;

    exception when others then raise;
    end to_dec_loop;

    -- https://stackoverflow.com/a/49718779/7552
    function to_dec_bin_to_num (binary_string varchar2)
    return pls_integer
    is
        vector      varchar2(255);
        answer      pls_integer;
    begin

        vector := replace(replace(nvl(binary_string, ''), '1', '1,'), '0', '0,');
        vector := trim(trailing ',' from vector);
        execute immediate 'select bin_to_num(' || vector || ') from dual' into answer;
        return answer;

    exception when others then
        if SQLCODE in (-904, -1428) then
            -- ORA-00904: "invalid identifier" -- non-digit in vector
            -- ORA-01428: "out of range" error -- vector contains number not 1 or 0
            return 0;
        else
            raise;
        end if;
    end to_dec_bin_to_num;

    --
    function to_decimal (binary_string varchar2)
    return pls_integer
    is
    begin
        --return to_dec_loop(binary_string);
        return to_dec_bin_to_num(binary_string);
    exception when others then raise;
    end to_decimal;
end binary#;
/

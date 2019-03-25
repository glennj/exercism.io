create or replace package numeral#
is
    function to_roman        ( num pls_integer ) return varchar2;
    function to_roman_manual ( num pls_integer ) return varchar2;
end numeral#;
/

create or replace package body numeral#
is
    function to_roman ( num pls_integer ) return varchar2
    is begin
        return trim(to_char(num, 'RN'));
    exception when others then raise;
    end to_roman;

    --
    function to_roman_manual ( num pls_integer ) return varchar2
    is
        roman   varchar2(16);
        n       pls_integer := num;
    begin

        while n >= 1000 loop roman := roman ||  'M'; n := n - 1000; end loop;
        if    n >=  900 then roman := roman || 'CM'; n := n -  900; end if;
        if    n >=  500 then roman := roman ||  'D'; n := n -  500; end if;
        if    n >=  400 then roman := roman || 'CD'; n := n -  400; end if;
        while n >=  100 loop roman := roman ||  'C'; n := n -  100; end loop;
        if    n >=   90 then roman := roman || 'XC'; n := n -   90; end if;
        if    n >=   50 then roman := roman ||  'L'; n := n -   50; end if;
        if    n >=   40 then roman := roman || 'XL'; n := n -   40; end if;
        while n >=   10 loop roman := roman ||  'X'; n := n -   10; end loop;
        if    n >=    9 then roman := roman || 'IX'; n := n -    9; end if;
        if    n >=    5 then roman := roman ||  'V'; n := n -    5; end if;
        if    n >=    4 then roman := roman || 'IV'; n := n -    4; end if;
        while n >=    1 loop roman := roman ||  'I'; n := n -    1; end loop;

        return roman;

    exception when others then raise;
    end to_roman_manual;
end numeral#;
/

create or replace package gigasecond#
is
    GIGASECOND          constant pls_integer := 1e9;
    SECONDS_PER_DAY     constant pls_integer := 86400;

    function since (
        i_birthdate     date
    )
    return date;
end gigasecond#;
/

create or replace package body gigasecond#
is
    function since (
        i_birthdate     date
    ) 
    return date
    as
    begin

        return trunc(i_birthdate + GIGASECOND / SECONDS_PER_DAY, 'DD');    

    exception
        when others
            then raise;
    end since;
end gigasecond#;
/

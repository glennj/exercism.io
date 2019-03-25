create or replace package hamming#
is
  --+--------------------------------------------------------------------------+
  -- Computes the Hamming distance between two starnds.
  --
  -- @param i_first  sequence to compare
  -- @param i_second sequence to compare
  --
  -- @return         Hamming distance between i_first and i_second
  --+--------------------------------------------------------------------------+
  function distance (
    i_first                                       varchar2
   ,i_second                                      varchar2
  ) return pls_integer;

end hamming#;
/

create or replace package body hamming#
is
    function distance (
        i_first         varchar2
      , i_second        varchar2
    )
    return pls_integer
    as
        v_count         pls_integer := 0;
        i               pls_integer;
    begin
        if i_first is null or i_second is null then
            raise_application_error (-20001, 'Parameters must not be null.');
        end if;
        if length(i_first) <> length(i_second) then
            raise_application_error (-20001, 'Parameters must be same length.');
        end if;

        for i in 1 .. length(i_first) loop
            if substr(i_first, i, 1) <> substr(i_second, i, 1) then
                v_count := v_count + 1;
            end if;
        end loop;

        return v_count;
    end;

end hamming#;
/

create or replace package complement#
is
    DNA_NUCLEOTIDES     varchar2(4) := 'GCTA';
    RNA_NUCLEOTIDES     varchar2(4) := 'CGAU';

    function of_dna ( dna_strand varchar2 ) return varchar2;
    function of_rna ( rna_strand varchar2 ) return varchar2;
end complement#;
/

create or replace package body complement#
is
    --
    function of_dna (
        dna_strand      varchar2
    )
    return varchar2
    as
    begin

        if dna_strand is null then
            raise_application_error(-20001, 'Input cannot be null.');
        end if;

        return translate(dna_strand, DNA_NUCLEOTIDES, RNA_NUCLEOTIDES);

    exception
        when others
            then raise;

    end of_dna;

    --
    function of_rna (
        rna_strand      varchar2
    )
    return varchar2
    as
    begin

        if rna_strand is null then
            raise_application_error(-20001, 'Input cannot be null.');
        end if;

        return translate(rna_strand, RNA_NUCLEOTIDES, DNA_NUCLEOTIDES);

    exception
        when others
            then raise;

    end of_rna;

end complement#;
/


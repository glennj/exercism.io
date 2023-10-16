require "spec"
require "../src/*"

describe "ProteinTranslation" do
  it "Empty RNA sequence results in no proteins" do
    ProteinTranslation.proteins("").should eq([] of String)
  end

  pending "Methionine RNA sequence" do
    ProteinTranslation.proteins("AUG").should eq(["Methionine"] of String)
  end

  pending "Phenylalanine RNA sequence 1" do
    ProteinTranslation.proteins("UUU").should eq(["Phenylalanine"] of String)
  end

  pending "Phenylalanine RNA sequence 2" do
    ProteinTranslation.proteins("UUC").should eq(["Phenylalanine"] of String)
  end

  pending "Leucine RNA sequence 1" do
    ProteinTranslation.proteins("UUA").should eq(["Leucine"] of String)
  end

  pending "Leucine RNA sequence 2" do
    ProteinTranslation.proteins("UUG").should eq(["Leucine"] of String)
  end

  pending "Serine RNA sequence 1" do
    ProteinTranslation.proteins("UCU").should eq(["Serine"] of String)
  end

  pending "Serine RNA sequence 2" do
    ProteinTranslation.proteins("UCC").should eq(["Serine"] of String)
  end

  pending "Serine RNA sequence 3" do
    ProteinTranslation.proteins("UCA").should eq(["Serine"] of String)
  end

  pending "Serine RNA sequence 4" do
    ProteinTranslation.proteins("UCG").should eq(["Serine"] of String)
  end

  pending "Tyrosine RNA sequence 1" do
    ProteinTranslation.proteins("UAU").should eq(["Tyrosine"] of String)
  end

  pending "Tyrosine RNA sequence 2" do
    ProteinTranslation.proteins("UAC").should eq(["Tyrosine"] of String)
  end

  pending "Cysteine RNA sequence 1" do
    ProteinTranslation.proteins("UGU").should eq(["Cysteine"] of String)
  end

  pending "Cysteine RNA sequence 2" do
    ProteinTranslation.proteins("UGC").should eq(["Cysteine"] of String)
  end

  pending "Tryptophan RNA sequence" do
    ProteinTranslation.proteins("UGG").should eq(["Tryptophan"] of String)
  end

  pending "STOP codon RNA sequence 1" do
    ProteinTranslation.proteins("UAA").should eq([] of String)
  end

  pending "STOP codon RNA sequence 2" do
    ProteinTranslation.proteins("UAG").should eq([] of String)
  end

  pending "STOP codon RNA sequence 3" do
    ProteinTranslation.proteins("UGA").should eq([] of String)
  end

  pending "Sequence of two protein codons translates into proteins" do
    ProteinTranslation.proteins("UUUUUU").should eq(["Phenylalanine", "Phenylalanine"] of String)
  end

  pending "Sequence of two different protein codons translates into proteins" do
    ProteinTranslation.proteins("UUAUUG").should eq(["Leucine", "Leucine"] of String)
  end

  pending "Translate RNA strand into correct protein list" do
    ProteinTranslation.proteins("AUGUUUUGG").should eq(["Methionine", "Phenylalanine", "Tryptophan"] of String)
  end

  pending "Translation stops if STOP codon at beginning of sequence" do
    ProteinTranslation.proteins("UAGUGG").should eq([] of String)
  end

  pending "Translation stops if STOP codon at end of two-codon sequence" do
    ProteinTranslation.proteins("UGGUAG").should eq(["Tryptophan"] of String)
  end

  pending "Translation stops if STOP codon at end of three-codon sequence" do
    ProteinTranslation.proteins("AUGUUUUAA").should eq(["Methionine", "Phenylalanine"] of String)
  end

  pending "Translation stops if STOP codon in middle of three-codon sequence" do
    ProteinTranslation.proteins("UGGUAGUGG").should eq(["Tryptophan"] of String)
  end

  pending "Translation stops if STOP codon in middle of six-codon sequence" do
    ProteinTranslation.proteins("UGGUGUUAUUAAUGGUUU").should eq(["Tryptophan", "Cysteine", "Tyrosine"] of String)
  end

  pending "Non-existing codon can't translate" do
    expect_raises(ArgumentError) do
      ProteinTranslation.proteins("AAA")
    end
  end

  pending "Unknown amino acids, not part of a codon, can't translate" do
    expect_raises(ArgumentError) do
      ProteinTranslation.proteins("XYZ")
    end
  end

  pending "Incomplete RNA sequence can't translate" do
    expect_raises(ArgumentError) do
      ProteinTranslation.proteins("AUGU")
    end
  end

  pending "Incomplete RNA sequence can translate if valid until a STOP codon" do
    ProteinTranslation.proteins("UUCUUCUAAUGGU").should eq(["Phenylalanine", "Phenylalanine"] of String)
  end
end

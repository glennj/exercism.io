require "spec"
require "../src/*"

describe "Acronym" do
  it "basic" do
    Acronym.abbreviate("Portable Network Graphics").should eq("PNG")
  end

  it "lowercase words" do
    Acronym.abbreviate("Ruby on Rails").should eq("ROR")
  end

  it "punctuation" do
    Acronym.abbreviate("First In, First Out").should eq("FIFO")
  end

  it "all caps word" do
    Acronym.abbreviate("GNU Image Manipulation Program").should eq("GIMP")
  end

  it "punctuation without whitespace" do
    Acronym.abbreviate("Complementary metal-oxide semiconductor").should eq("CMOS")
  end

  it "very long abbreviation" do
    Acronym.abbreviate("Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me").should eq("ROTFLSHTMDCOALM")
  end

  it "consecutive delimiters" do
    Acronym.abbreviate("Something - I made up from thin air").should eq("SIMUFTA")
  end

  it "apostrophes" do
    Acronym.abbreviate("Halley's Comet").should eq("HC")
  end

  it "underscore emphasis" do
    Acronym.abbreviate("The Road _Not_ Taken").should eq("TRNT")
  end
end

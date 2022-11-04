# Rna Transcription

Welcome to Rna Transcription on Exercism's jq Track.
If you need help running the tests or submitting your code, check out `HELP.md`.

## Instructions

Given a DNA strand, return its RNA complement (per RNA transcription).

Both DNA and RNA strands are a sequence of nucleotides.

The four nucleotides found in DNA are adenine (**A**), cytosine (**C**), guanine (**G**) and thymine (**T**).

The four nucleotides found in RNA are adenine (**A**), cytosine (**C**), guanine (**G**) and uracil (**U**).

Given a DNA strand, its transcribed RNA strand is formed by replacing each nucleotide with its complement:

- `G` -> `C`
- `C` -> `G`
- `T` -> `A`
- `A` -> `U`

## jq-specific instructions

Your requirement for this exercise is to define a `toRna` function.
This function will expect a DNA sequence (as a string) as input, and will output the transcribed RNA sequence.

Refer to [Defining Functions][def] in the jq manual.

[def]: https://stedolan.github.io/jq/manual/#DefiningFunctions

## Source

### Created by

- @glennj

### Based on

Hyperphysics - https://web.archive.org/web/20220408112140/http://hyperphysics.phy-astr.gsu.edu/hbase/Organic/transcription.html
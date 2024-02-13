<?php

declare(strict_types=1);

class ProteinTranslation
{
    private const STOP = 'STOP';

    public function getProteins(string $input): array
    {
        $codons = str_split($input, 3);
        $proteins = [];
        foreach ($codons as $codon) {
            $protein = $this->getProtein($codon);
            if ($protein == self::STOP) {
                break;
            }
            $proteins[] = $protein;
        }
        return $proteins;
    }

    public function getProtein(string $codon): string
    {
        return match ($codon) {
            'AUG'                      => 'Methionine',
            'UUU', 'UUC'               => 'Phenylalanine',
            'UUA', 'UUG'               => 'Leucine',
            'UCU', 'UCC', 'UCA', 'UCG' => 'Serine',
            'UAU', 'UAC'               => 'Tyrosine',
            'UGU', 'UGC'               => 'Cysteine',
            'UGG'                      => 'Tryptophan',
            'UAA', 'UAG', 'UGA'        => self::STOP,
            default => throw new InvalidArgumentException('Invalid codon'),
        };
    }
}

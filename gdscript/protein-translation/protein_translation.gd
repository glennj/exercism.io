const STOP = "STOP"


func proteins(strand):
	var ps = []

	while strand != "":
		match to_protein(strand.substr(0, 3)):
			STOP  : break
			null  : return null
			var p : ps.append(p)

		strand = strand.substr(3)

	return ps


func to_protein(codon):
	match codon:
		'AUG'                      : return 'Methionine'
		'UUU', 'UUC'               : return 'Phenylalanine'
		'UUA', 'UUG'               : return 'Leucine'
		'UCU', 'UCC', 'UCA', 'UCG' : return 'Serine'
		'UAU', 'UAC'               : return 'Tyrosine'
		'UGU', 'UGC'               : return 'Cysteine'
		'UGG'                      : return 'Tryptophan'
		'UAA', 'UAG', 'UGA'        : return STOP
		_                          : return null

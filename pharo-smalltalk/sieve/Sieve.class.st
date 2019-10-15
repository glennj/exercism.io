Class {
	#name : #Sieve,
	#superclass : #Object,
	#category : #'Exercise@Sieve'
}

{ #category : #calculating }
Sieve >> primesLimit: limit [
	| sieve |

	"initialize the sieve 'flag' array"
	sieve := (1 to: limit) asArray.
	sieve at: 1 put: nil.
	
	"remove non-primes."
	2	to: limit sqrtFloor 
		do: [ :p | 
			(sieve at: p) isNotNil ifTrue: [ 
				p * p 
					to: limit 
					by: (p = 2 ifTrue: [ p ] ifFalse: [ 2 * p ]) 
					do: [ :m | sieve at: m put: nil ] ] ].
		
	"return the remaining (prime) numbers"
	^ sieve select: #isNotNil
]

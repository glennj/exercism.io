namespace eval diffieHellman {}

proc diffieHellman::privateKey {p} {
    # a random number in the range [2, p)
    expr {2 + int(($p - 2) * rand())}
}

proc diffieHellman::publicKey {p g private} {
    expr {($g ** $private) % $p}
}

proc diffieHellman::secret {p public private} {
    publicKey $p $public $private
}

[
  {value:  1, letters: "AEIOULNRST"},
  {value:  2, letters: "DG"},
  {value:  3, letters: "BCMP"},
  {value:  4, letters: "FHVWY"},
  {value:  5, letters: "K"},
  {value:  8, letters: "JX"},
  {value: 10, letters: "QZ"}
] as $scores

| reduce (.word | ascii_upcase / "")[] as $letter (0;
    . + ($scores[] | select(.letters | contains($letter)) | .value)
  )

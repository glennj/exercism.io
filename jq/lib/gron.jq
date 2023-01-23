# implement `gron` in jq
# : https://github.com/tomnomnom/gron
#
#   $ echo '{"a": [1, 2, {"b c": 3}]}' | jq -r 'include "gron"; gron'
#   .a[0] = 1
#   .a[1] = 2
#   .a[2]["b c"] = 3
#
def gron:
  def topath:
    def isidentifier: test("^[a-zA-Z_][a-zA-Z_0-9]*$");
    def tokey: if isidentifier then ".\(.)" else "[\"\(.)\"]" end;
    def toindex: "[\(.)]";

    [ .[] | if type == "string" then tokey else toindex end ]
    | join("")
    ;

  path(..|scalars) as $p | "\($p|topath) = \(getpath($p) | tojson)"
  ;

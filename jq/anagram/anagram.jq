def encode: explode | sort;

(.subject | ascii_downcase | {lc: ., key: encode}) as $subj
| .candidates
| map(select(ascii_downcase | . != $subj.lc and encode == $subj.key))

using Statistics

rationalize(successes, trials) = [a//b for (a,b) in zip(successes, trials)]
# or: `rationalize(successes, trials) = successes .// trials`

probabilities(successes, trials) = rationalize(successes, trials) .|> float

checkmean(successes, trials) = rats_vs_floats(mean, successes, trials)
checkprob(successes, trials) = rats_vs_floats(prod, successes, trials)

function rats_vs_floats(f, ss, ts)
    result_r = f(rationalize(ss, ts))
    result_f = f(probabilities(ss, ts))

    result_f == float(result_r) ? true : result_r
    # or: `result_f == float(result_r) || result_r`
end

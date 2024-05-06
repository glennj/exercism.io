import gleam/result

pub fn with_retry(experiment: fn() -> Result(t, e)) -> Result(t, e) {
  case experiment() {
    Ok(x) -> Ok(x)
    _ -> experiment()
  }
}

pub fn record_timing(
  time_logger: fn() -> Nil,
  experiment: fn() -> Result(t, e),
) -> Result(t, e) {
  time_logger()
  let result = experiment()
  time_logger()
  result
}

pub fn run_experiment(
  name: String,
  setup: fn() -> Result(t, e),
  action: fn(t) -> Result(u, e),
  record: fn(t, u) -> Result(v, e),
) -> Result(#(String, v), e) {
  use t <- result.try(setup())
  use u <- result.try(action(t))
  use v <- result.try(record(t, u))
  Ok(#(name, v))
}

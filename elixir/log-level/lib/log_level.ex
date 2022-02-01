defmodule LogLevel do
  def to_label(level, legacy?) do
    cond do
      # note to self: use === (or == if floats are ok) here, not =
      level === 0 and not legacy? -> :trace
      level === 1 -> :debug
      level === 2 -> :info
      level === 3 -> :warning
      level === 4 -> :error
      level === 5 and not legacy? -> :fatal
      true -> :unknown
    end
  end

  def alert_recipient(level, legacy?) do
    label = to_label(level, legacy?)
    cond do
      label === :error or label === :fatal -> :ops
      label === :unknown and legacy? -> :dev1
      true -> :dev2

   #   label === :unknown and not legacy? -> :dev2
   #   true -> false
    end
  end
end

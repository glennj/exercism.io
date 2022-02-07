defmodule Grep do
  @type filename :: String.t()
  @type option :: String.t()
  @type line :: String.t()
  @type options :: map()

  @flags ["-i", "-x", "-v", "-l", "-n"]

  @spec grep(String.t(), [option], [filename]) :: String.t()
  def grep(pattern, flags, files) do
    opts = parse_flags(flags)

    # stash whether there are multiple files
    opts = Map.put(opts, :multi, length(files) > 1)

    # stash the pattern
    opts =
      Map.put(
        opts,
        :pattern,
        cond do
          opts.x and opts.i -> ~r/^#{pattern}$/i
          opts.x -> ~r/^#{pattern}$/
          opts.i -> ~r/#{pattern}/i
          true -> ~r/#{pattern}/
        end
      )

    process_files(files, opts, [])
  end

  @spec parse_flags([option]) :: options
  defp parse_flags(flags) do
    # default values
    opts =
      @flags
      |> Enum.map(&{flag_to_atom(&1), false})
      |> Enum.into(%{})

    # incorporate user's flags
    flags
    |> Enum.reduce(opts, &%{&2 | flag_to_atom(&1) => true})
  end

  @spec flag_to_atom(String.t()) :: atom
  defp flag_to_atom(flag), do: String.at(flag, 1) |> String.to_atom()

  @spec process_files([filename], options, [String.t()]) :: String.t()
  defp process_files([], _, result) do
    result
    |> Enum.map(&(&1 <> "\n"))
    |> Enum.join("")
  end

  defp process_files([file | files], opts, result) do
    process_files(
      files,
      opts,
      result ++ process_one_file(file, opts)
    )
  end

  @spec process_one_file(filename, options) :: [line]
  defp process_one_file(file, opts) do
    do_process_one_file(file, lines(file), 1, opts, [])
  end

  # return a list of the lines of a file
  @spec lines(filename) :: [line]
  defp lines(file) do
    File.read!(file)
    |> String.trim_trailing("\n")
    |> String.split("\n")
  end

  @spec do_process_one_file(
          file :: filename,
          lines :: [line],
          line_no :: pos_integer,
          opts :: options,
          lines_matched :: [line]
        ) :: [line]
  defp do_process_one_file(_, [], _, _, result), do: result

  defp do_process_one_file(file, [line | lines], line_no, opts, matched) do
    if matches?(line, opts.pattern, opts.v) do
      if opts.l do
        [file]
      else
        res =
          if(opts.multi, do: "#{file}:", else: "") <>
            if(opts.n, do: "#{line_no}:", else: "") <>
            line

        do_process_one_file(file, lines, line_no + 1, opts, matched ++ [res])
      end
    else
      do_process_one_file(file, lines, line_no + 1, opts, matched)
    end
  end

  defp matches?(line, pattern, inverse) do
    case {Regex.match?(pattern, line), inverse} do
      {true, true} -> false
      {true, false} -> true
      {false, true} -> true
      {false, false} -> false
    end
  end
end

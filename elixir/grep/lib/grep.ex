defmodule Grep do
  @type filename :: String.t()
  @type option :: String.t()
  @type options :: %{atom() => boolean()}

  @flags ["-i", "-x", "-v", "-l", "-n"]

  @spec grep(
          pattern :: String.t(),
          flags :: list(option),
          files :: list(filename)
        ) :: String.t()
  def grep(pattern, flags, files) do
    opts = parse_flags(flags)

    # stash whether there are multiple files
    opts = Map.put(opts, :multi, length(files) > 1)

    regex =
      cond do
        opts.x and opts.i -> ~r/^#{pattern}$/i
        opts.x -> ~r/^#{pattern}$/
        opts.i -> ~r/#{pattern}/i
        true -> ~r/#{pattern}/
      end

    process_files(files, regex, opts, [])
  end

  @spec parse_flags(list(option)) :: options
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

  # flag_to_atom
  #
  #   iex> flag_to_atom("-x")
  #   :x
  #
  @spec flag_to_atom(option) :: atom()
  defp flag_to_atom(flag), do: String.slice(flag, 1..-1) |> String.to_atom()

  @spec process_files(list(filename), Regex.t(), options, list(String.t())) :: String.t()

  # no more files: format the result into a single string
  defp process_files([], _, _, result) do
    result
    |> Enum.map(&(&1 <> "\n"))
    |> Enum.join("")
  end

  defp process_files([file | files], regex, opts, result) do
    process_files(
      files,
      regex,
      opts,
      result ++ process_one_file(file, regex, opts)
    )
  end

  @spec process_one_file(filename, Regex.t(), options) :: list(String.t())
  defp process_one_file(file, regex, opts) do
    do_process_one_file(file, lines(file), 1, regex, opts, [])
  end

  # return a list of the lines of a file
  @spec lines(filename) :: list(String.t())
  defp lines(file) do
    File.read!(file)
    |> String.trim_trailing("\n")
    |> String.split("\n")
  end

  @spec do_process_one_file(
          file :: filename,
          lines :: list(String.t()),
          line_no :: pos_integer,
          regex :: Regex.t(),
          opts :: options,
          result :: list(String.t())
        ) :: list(String.t())

  defp do_process_one_file(_, [], _, _, _, result), do: Enum.reverse(result)

  defp do_process_one_file(file, [line | lines], line_no, regex, opts, result) do
    cond do
      # if this line does not match, carry on with the rest of the file's lines
      not matches?(line, regex, opts) ->
        do_process_one_file(file, lines, line_no + 1, regex, opts, result)

      # this line matches, but option "-l" means we only want the file name.
      opts.l ->
        [file]

      # this line matches, format the grep result based on options
      true ->
        res =
          if(opts.multi, do: "#{file}:", else: "") <>
          if(opts.n, do: "#{line_no}:", else: "") <>
          line
        do_process_one_file(file, lines, line_no + 1, regex, opts, [res | result])
    end
  end

  # a file of a file "matches" if the pattern matches the line,
  # _or_ the "-v" inverse flag is set _but not both_.
  defp matches?(line, regex, opts), do: xor(Regex.match?(regex, line), opts.v)

  # exclusive-or
  defp xor(true, true), do: false
  defp xor(true, false), do: true
  defp xor(false, true), do: true
  defp xor(false, false), do: false
end

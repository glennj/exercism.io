defmodule FileSniffer do
  def type_from_extension(extension) do
    %{
      "exe" => "application/octet-stream",
      "bmp" => "image/bmp",
      "png" => "image/png",
      "jpg" => "image/jpg",
      "gif" => "image/gif"
    }[extension]
  end

  def type_from_binary(file_binary) do
    case file_binary do
      <<0x42, 0x4D, _::binary>> -> "image/bmp"
      <<0xFF, 0xD8, 0xFF, _::binary>> -> "image/jpg"
      <<0x47, 0x49, 0x46, _::binary>> -> "image/gif"
      <<0x7F, 0x45, 0x4C, 0x46, _::binary>> -> "application/octet-stream"
      <<0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A, _::binary>> -> "image/png"
    end
  end

  def verify(file_binary, extension) do
    do_verify(
      type_from_extension(extension),
      type_from_binary(file_binary)
    )
  end

  defp do_verify(type, type), do: {:ok, type}
  defp do_verify(_, _), do: {:error, "Warning, file format and file extension do not match."}
end

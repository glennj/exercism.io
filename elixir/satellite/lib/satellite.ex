defmodule Satellite do
  @typedoc """
  A tree, which can be empty, or made from a left branch, a node and a right branch
  """
  @type tree :: {} | {tree, any, tree}

  @doc """
  Build a tree from the elements given in a pre-order and in-order style
  """
  @spec build_tree(preorder :: [any], inorder :: [any]) :: {:ok, tree} | {:error, String.t()}
  def build_tree(preorder, inorder) do
    len = length(preorder)
    cond do
      len != length(inorder) ->
        {:error, "traversals must have the same length"}

      Enum.any?(preorder, &(&1 not in inorder)) ->
        {:error, "traversals must have the same elements"}

      len != MapSet.size(MapSet.new(preorder)) ->
        {:error, "traversals must contain unique items"}

      true ->
        {:ok, do_build_tree(preorder, inorder)}
    end
  end

  defp do_build_tree([], []), do: {}

  defp do_build_tree(preorder, inorder) do
    [root | preorder_rest] = preorder
    n = Enum.find_index(inorder, &(&1 == root))

    preorder_left = Enum.take(preorder_rest, n)
    preorder_right = Enum.drop(preorder_rest, n)

    inorder_left = Enum.take(inorder, n)
    inorder_right = Enum.drop(inorder, n + 1)

    {
      do_build_tree(preorder_left, inorder_left),
      root,
      do_build_tree(preorder_right, inorder_right)
    }
  end
end
